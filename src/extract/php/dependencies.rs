use tree_sitter::Node;

use super::{extract_namespace_use_clause, normalize_php_name, push_unique, txt};
use crate::model::FileSymbols;

#[derive(Default)]
pub(super) struct Context {
    namespace: String,
    aliases: Vec<(String, String)>,
    declared: Vec<String>,
}

impl Context {
    pub(super) fn resolve(&self, node: Node, src: &[u8]) -> Option<String> {
        if !matches!(node.kind(), "name" | "qualified_name" | "relative_name") {
            return None;
        }
        let raw = txt(node, src).trim();
        if matches!(
            raw.to_ascii_lowercase().as_str(),
            "self" | "static" | "parent"
        ) {
            return None;
        }
        let name = normalize_php_name(raw);
        if raw.starts_with('\\') {
            return Some(name);
        }
        if let Some((_, relative)) = name
            .split_once('\\')
            .filter(|(head, _)| head.eq_ignore_ascii_case("namespace"))
        {
            return Some(self.qualify(relative));
        }
        let (head, tail) = name.split_once('\\').unwrap_or((&name, ""));
        if let Some((_, source)) = self
            .aliases
            .iter()
            .find(|(alias, _)| alias.eq_ignore_ascii_case(head))
        {
            return Some(if tail.is_empty() {
                source.clone()
            } else {
                format!("{source}\\{tail}")
            });
        }
        Some(self.qualify(&name))
    }

    fn qualify(&self, name: &str) -> String {
        if self.namespace.is_empty() {
            name.to_string()
        } else {
            format!("{}\\{name}", self.namespace)
        }
    }
}

/// PHP imports are namespace-scoped (including semicolon namespaces), not file-scoped.
pub(super) fn visit_scopes<'tree>(
    root: Node<'tree>,
    src: &[u8],
    mut visit: impl FnMut(&[Node<'tree>], &Context),
) {
    let mut namespace = String::new();
    let mut nodes = Vec::new();
    let mut cursor = root.walk();
    for node in root.named_children(&mut cursor) {
        if node.kind() != "namespace_definition" {
            nodes.push(node);
            continue;
        }
        visit(&nodes, &context(&nodes, &namespace, src));
        nodes.clear();
        namespace = node
            .child_by_field_name("name")
            .map_or_else(String::new, |name| normalize_php_name(txt(name, src)));
        if let Some(body) = node.child_by_field_name("body") {
            let mut cursor = body.walk();
            let children: Vec<_> = body.named_children(&mut cursor).collect();
            visit(&children, &context(&children, &namespace, src));
            namespace.clear();
        }
    }
    visit(&nodes, &context(&nodes, &namespace, src));
}

fn context(nodes: &[Node], namespace: &str, src: &[u8]) -> Context {
    let mut context = Context {
        namespace: namespace.to_string(),
        ..Context::default()
    };
    for &node in nodes {
        if node.kind() == "namespace_use_declaration" && node.child_by_field_name("type").is_none()
        {
            collect_class_aliases(node, src, "", &mut context.aliases);
        }
        if matches!(
            node.kind(),
            "class_declaration"
                | "interface_declaration"
                | "trait_declaration"
                | "enum_declaration"
        ) {
            if let Some(name) = node.child_by_field_name("name") {
                context.declared.push(context.qualify(txt(name, src)));
            }
        }
    }
    context
}

fn collect_class_aliases(
    node: Node,
    src: &[u8],
    prefix: &str,
    aliases: &mut Vec<(String, String)>,
) {
    if node.kind() == "namespace_use_clause" {
        // PHP has separate alias tables for classes, functions, and constants.
        if node.child_by_field_name("type").is_none() {
            let mut imports = super::super::empty_symbols();
            extract_namespace_use_clause(node, src, prefix, &mut imports);
            aliases.extend(
                imports
                    .import_bindings
                    .into_iter()
                    .map(|binding| (binding.local_name, binding.source)),
            );
        }
        return;
    }
    let group_prefix = super::first_named_child_of_kind(node, "namespace_name")
        .map(|name| normalize_php_name(txt(name, src)));
    let prefix = group_prefix.as_deref().unwrap_or(prefix);
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if matches!(child.kind(), "namespace_use_clause" | "namespace_use_group") {
            collect_class_aliases(child, src, prefix, aliases);
        }
    }
}

pub(super) fn collect(root: Node, src: &[u8], symbols: &mut FileSymbols) {
    visit_scopes(root, src, |nodes, context| {
        // Keep explicit imports/bindings separate from inferred class references.
        for &node in nodes {
            super::collect_imports_recursive(node, src, symbols);
        }
        for &node in nodes {
            collect_references(node, src, context, &mut symbols.imports);
        }
    });
}

fn collect_references(node: Node, src: &[u8], context: &Context, imports: &mut Vec<String>) {
    match node.kind() {
        "namespace_use_declaration" => return,
        "object_creation_expression" | "class_constant_access_expression" | "attribute" => {
            if let Some(name) = node.named_child(0) {
                add_reference(name, src, context, imports);
            }
        }
        "scoped_call_expression" | "scoped_property_access_expression" => {
            if let Some(scope) = node.child_by_field_name("scope") {
                add_reference(scope, src, context, imports);
            }
        }
        "named_type" | "base_clause" | "class_interface_clause" | "use_declaration" => {
            let mut cursor = node.walk();
            for name in node.named_children(&mut cursor) {
                add_reference(name, src, context, imports);
            }
        }
        "binary_expression"
            if node
                .child_by_field_name("operator")
                .is_some_and(|op| txt(op, src) == "instanceof") =>
        {
            if let Some(right) = node.child_by_field_name("right") {
                add_reference(right, src, context, imports);
            }
        }
        _ => {}
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_references(child, src, context, imports);
    }
}

fn add_reference(node: Node, src: &[u8], context: &Context, imports: &mut Vec<String>) {
    if let Some(name) = context.resolve(node, src) {
        if !context
            .declared
            .iter()
            .any(|declared| declared.eq_ignore_ascii_case(&name))
        {
            push_unique(imports, name);
        }
    }
}
