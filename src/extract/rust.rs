use tree_sitter::Node;

use crate::model::{FileSymbols, ImportBinding, Symbol, TestBlock, TypeDef};
use crate::util::txt;

pub(super) fn extract_symbols(root: Node, src: &[u8]) -> FileSymbols {
    let mut symbols = super::empty_symbols();
    let mut pending_attrs: Vec<String> = Vec::new();

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "attribute_item" => {
                if let Some(name) = attribute_name(node, src) {
                    pending_attrs.push(name);
                }
            }
            "use_declaration" => {
                pending_attrs.clear();
                extract_use_declaration(
                    node,
                    src,
                    &mut symbols.imports,
                    &mut symbols.import_bindings,
                );
            }
            "function_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                if has_test_attr(&attrs) {
                    let name = item_name(node, src).unwrap_or_default();
                    symbols.tests.push(TestBlock {
                        kind: "test".to_string(),
                        name,
                        line_start: node.start_position().row + 1,
                        line_end: node.end_position().row + 1,
                        children: Vec::new(),
                    });
                } else {
                    extract_function(node, src, attrs, &mut symbols);
                }
            }
            "struct_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_struct(node, src, &attrs, &mut symbols.types);
            }
            "enum_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_enum(node, src, &attrs, &mut symbols.types);
            }
            "trait_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_trait(node, src, &attrs, &mut symbols.types);
            }
            "type_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_type_alias(node, src, &attrs, &mut symbols.types);
            }
            "const_item" | "static_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_const_or_static(node, src, attrs, &mut symbols);
            }
            "impl_item" => {
                pending_attrs.clear();
                extract_impl(node, src, &mut symbols.types);
            }
            "macro_definition" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_macro_definition(node, src, attrs, &mut symbols);
            }
            "mod_item" => {
                let attrs = std::mem::take(&mut pending_attrs);
                extract_mod(node, src, &attrs, &mut symbols);
            }
            _ => {
                pending_attrs.clear();
            }
        }
    }

    symbols
}

pub(super) fn extract_sources_only(root: Node, src: &[u8]) -> Vec<String> {
    let mut imports = Vec::new();
    let mut scratch_bindings = Vec::new();
    let mut pending_attrs: Vec<String> = Vec::new();

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "attribute_item" => {
                if let Some(name) = attribute_name(node, src) {
                    pending_attrs.push(name);
                }
            }
            "use_declaration" => {
                pending_attrs.clear();
                extract_use_declaration(node, src, &mut imports, &mut scratch_bindings);
            }
            "mod_item" => {
                pending_attrs.clear();
                if !has_body(node) {
                    if let Some(name) = item_name(node, src) {
                        push_unique(&mut imports, name);
                    }
                }
            }
            _ => {
                pending_attrs.clear();
            }
        }
    }

    imports
}

// ── use declarations ──────────────────────────────────────────────────

fn extract_use_declaration(
    node: Node,
    src: &[u8],
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    // The use tree is the child after the visibility modifier (if any)
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        match child.kind() {
            "visibility_modifier" => {}
            _ => flatten_use_tree(child, src, &mut String::new(), imports, bindings),
        }
    }
}

fn flatten_use_tree(
    node: Node,
    src: &[u8],
    prefix: &mut String,
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    match node.kind() {
        "scoped_identifier" | "identifier" | "crate" | "self" | "super"
        | "metavariable" => {
            let path = txt(node, src).trim().to_string();
            if !path.is_empty() {
                push_unique(imports, path.clone());
                let local = path.rsplit("::").next().unwrap_or(&path).to_string();
                bindings.push(ImportBinding {
                    local_name: local,
                    source: path,
                    is_default: false,
                });
            }
        }
        "use_as_clause" => {
            let mut source = None;
            let mut alias = None;
            let mut inner = node.walk();
            for child in node.named_children(&mut inner) {
                match child.kind() {
                    "scoped_identifier" | "identifier" | "crate" | "self" | "super" => {
                        if source.is_none() {
                            source = Some(txt(child, src).trim().to_string());
                        } else {
                            alias = Some(txt(child, src).trim().to_string());
                        }
                    }
                    _ => {}
                }
            }
            if let Some(source) = source {
                push_unique(imports, source.clone());
                let local =
                    alias.unwrap_or_else(|| source.rsplit("::").next().unwrap_or(&source).to_string());
                bindings.push(ImportBinding {
                    local_name: local,
                    source,
                    is_default: false,
                });
            }
        }
        "scoped_use_list" => {
            // e.g. crate::model::{Symbol, TypeDef}
            let mut path_part: Option<String> = None;
            let mut inner = node.walk();
            for child in node.named_children(&mut inner) {
                match child.kind() {
                    "use_list" => {
                        let combined_prefix = match &path_part {
                            Some(p) if !prefix.is_empty() => format!("{prefix}::{p}"),
                            Some(p) => p.clone(),
                            None if !prefix.is_empty() => prefix.clone(),
                            None => String::new(),
                        };
                        let mut sub_prefix = combined_prefix;
                        flatten_use_list(child, src, &mut sub_prefix, imports, bindings);
                    }
                    _ => {
                        path_part = Some(txt(child, src).trim().to_string());
                    }
                }
            }
        }
        "use_list" => {
            flatten_use_list(node, src, prefix, imports, bindings);
        }
        "use_wildcard" => {
            // e.g. use module::*
            let path = txt(node, src).trim().to_string();
            if !path.is_empty() {
                push_unique(imports, path);
            }
        }
        _ => {}
    }
}

fn flatten_use_list(
    node: Node,
    src: &[u8],
    prefix: &mut String,
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    let mut inner = node.walk();
    for child in node.named_children(&mut inner) {
        match child.kind() {
            "identifier" | "self" => {
                let name = txt(child, src).trim().to_string();
                let full = if prefix.is_empty() {
                    name.clone()
                } else {
                    format!("{prefix}::{name}")
                };
                push_unique(imports, full.clone());
                bindings.push(ImportBinding {
                    local_name: name,
                    source: full,
                    is_default: false,
                });
            }
            "use_as_clause" => {
                let mut source_name = None;
                let mut alias = None;
                let mut c = child.walk();
                for grandchild in child.named_children(&mut c) {
                    match grandchild.kind() {
                        "identifier" | "scoped_identifier" | "self" => {
                            if source_name.is_none() {
                                source_name = Some(txt(grandchild, src).trim().to_string());
                            } else {
                                alias = Some(txt(grandchild, src).trim().to_string());
                            }
                        }
                        _ => {}
                    }
                }
                if let Some(name) = source_name {
                    let full = if prefix.is_empty() {
                        name.clone()
                    } else {
                        format!("{prefix}::{name}")
                    };
                    push_unique(imports, full.clone());
                    let local = alias.unwrap_or(name);
                    bindings.push(ImportBinding {
                        local_name: local,
                        source: full,
                        is_default: false,
                    });
                }
            }
            "scoped_use_list" => {
                flatten_use_tree(child, src, prefix, imports, bindings);
            }
            _ => {}
        }
    }
}

// ── functions ─────────────────────────────────────────────────────────

fn extract_function(
    node: Node,
    src: &[u8],
    decorators: Vec<String>,
    symbols: &mut FileSymbols,
) {
    let Some(name) = item_name(node, src) else {
        return;
    };
    let signature = item_signature(node, src);
    let body = node.child_by_field_name("body");
    let calls = extract_calls(body, src);

    let symbol = Symbol {
        signature,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls,
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators,
    };

    if is_pub(node) {
        symbols.exports.push(symbol);
    } else {
        symbols.internals.push(symbol);
    }

    // Drop the name binding — it was only needed for the control flow.
    let _ = name;
}

// ── types ─────────────────────────────────────────────────────────────

fn extract_struct(node: Node, src: &[u8], attrs: &[String], types: &mut Vec<TypeDef>) {
    let Some(name) = type_name(node, src) else {
        return;
    };

    let derives = derive_list(attrs);
    let fields = struct_field_names(node, src);
    let mut summary_parts: Vec<String> = Vec::new();
    if !derives.is_empty() {
        summary_parts.push(format!("derives [{derives}]"));
    }
    let field_summary = summarize_names(&fields);
    if !field_summary.is_empty() {
        summary_parts.push(field_summary);
    }

    types.push(TypeDef {
        name,
        kind: "struct".to_string(),
        extends: String::new(),
        summary: summary_parts.join("  "),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: is_pub(node),
    });
}

fn extract_enum(node: Node, src: &[u8], attrs: &[String], types: &mut Vec<TypeDef>) {
    let Some(name) = type_name(node, src) else {
        return;
    };

    let derives = derive_list(attrs);
    let variants = enum_variant_names(node, src);
    let mut summary_parts: Vec<String> = Vec::new();
    if !derives.is_empty() {
        summary_parts.push(format!("derives [{derives}]"));
    }
    let variant_summary = summarize_names(&variants);
    if !variant_summary.is_empty() {
        summary_parts.push(variant_summary);
    }

    types.push(TypeDef {
        name,
        kind: "enum".to_string(),
        extends: String::new(),
        summary: summary_parts.join("  "),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: is_pub(node),
    });
}

fn extract_trait(node: Node, src: &[u8], _attrs: &[String], types: &mut Vec<TypeDef>) {
    let Some(name) = type_name(node, src) else {
        return;
    };

    let bounds = trait_bounds(node, src);
    let methods = declaration_list_method_names(node, src);
    let summary = summarize_names(&methods);

    types.push(TypeDef {
        name,
        kind: "trait".to_string(),
        extends: bounds,
        summary,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: is_pub(node),
    });
}

fn extract_type_alias(node: Node, src: &[u8], _attrs: &[String], types: &mut Vec<TypeDef>) {
    let Some(name) = type_name(node, src) else {
        return;
    };

    types.push(TypeDef {
        name,
        kind: "type alias".to_string(),
        extends: String::new(),
        summary: String::new(),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: is_pub(node),
    });
}

fn extract_impl(node: Node, src: &[u8], types: &mut Vec<TypeDef>) {
    let (impl_name, extends) = impl_names(node, src);
    let methods = declaration_list_method_names(node, src);
    let summary = summarize_names(&methods);

    types.push(TypeDef {
        name: impl_name,
        kind: "impl".to_string(),
        extends,
        summary,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: false,
    });
}

// ── const / static ────────────────────────────────────────────────────

fn extract_const_or_static(
    node: Node,
    src: &[u8],
    decorators: Vec<String>,
    symbols: &mut FileSymbols,
) {
    let signature = item_signature(node, src);

    let symbol = Symbol {
        signature,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators,
    };

    if is_pub(node) {
        symbols.exports.push(symbol);
    } else {
        symbols.internals.push(symbol);
    }
}

// ── macros ────────────────────────────────────────────────────────────

fn extract_macro_definition(
    node: Node,
    src: &[u8],
    decorators: Vec<String>,
    symbols: &mut FileSymbols,
) {
    let Some(name) = item_name(node, src) else {
        return;
    };

    let symbol = Symbol {
        signature: format!("macro_rules! {name}"),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators,
    };

    // macro_definition has no visibility modifier; always emit as export
    // (macro_rules! macros are available in the module scope and typically public)
    symbols.exports.push(symbol);
}

// ── mod declarations ──────────────────────────────────────────────────

fn extract_mod(
    node: Node,
    src: &[u8],
    attrs: &[String],
    symbols: &mut FileSymbols,
) {
    if has_body(node) {
        // Inline module: check for #[cfg(test)] mod tests { ... }
        if is_cfg_test(attrs) {
            extract_test_module(node, src, symbols);
        }
        return;
    }

    // External mod declaration: `mod foo;` → dependency
    if let Some(name) = item_name(node, src) {
        push_unique(&mut symbols.imports, name);
    }
}

fn extract_test_module(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mod_name = item_name(node, src).unwrap_or_else(|| "tests".to_string());

    let Some(body) = node.child_by_field_name("body") else {
        return;
    };

    let mut children = Vec::new();
    let mut pending = Vec::new();
    let mut cursor = body.walk();
    for child in body.children(&mut cursor) {
        match child.kind() {
            "attribute_item" => {
                if let Some(name) = attribute_name(child, src) {
                    pending.push(name);
                }
            }
            "function_item" => {
                let attrs: Vec<_> = std::mem::take(&mut pending);
                if has_test_attr(&attrs) {
                    let name = item_name(child, src).unwrap_or_default();
                    children.push(TestBlock {
                        kind: "test".to_string(),
                        name,
                        line_start: child.start_position().row + 1,
                        line_end: child.end_position().row + 1,
                        children: Vec::new(),
                    });
                }
            }
            _ => {
                pending.clear();
            }
        }
    }

    symbols.tests.push(TestBlock {
        kind: "mod".to_string(),
        name: mod_name,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        children,
    });
}

// ── calls ─────────────────────────────────────────────────────────────

fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let Some(body) = body else {
        return Vec::new();
    };
    let mut calls = Vec::new();
    collect_calls_recursive(body, src, &mut calls);
    calls
}

fn collect_calls_recursive(node: Node, src: &[u8], calls: &mut Vec<String>) {
    match node.kind() {
        "call_expression" => {
            if let Some(name) = call_name(node, src) {
                push_unique(calls, name);
            }
        }
        "macro_invocation" => {
            if let Some(name) = macro_call_name(node, src) {
                push_unique(calls, name);
            }
        }
        _ => {}
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_calls_recursive(child, src, calls);
    }
}

fn call_name(node: Node, src: &[u8]) -> Option<String> {
    let callee = node.child_by_field_name("function").or_else(|| node.named_child(0))?;
    let raw = match callee.kind() {
        "call_expression" => return call_name(callee, src),
        _ => txt(callee, src).trim().to_string(),
    };
    if raw.is_empty() {
        None
    } else {
        Some(collapse_whitespace(&raw))
    }
}

fn macro_call_name(node: Node, src: &[u8]) -> Option<String> {
    let macro_node = node.child_by_field_name("macro").or_else(|| node.named_child(0))?;
    let name = txt(macro_node, src).trim().to_string();
    if name.is_empty() {
        None
    } else {
        Some(format!("{name}!"))
    }
}

// ── helpers ───────────────────────────────────────────────────────────

fn is_pub(node: Node) -> bool {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "visibility_modifier" {
            return true;
        }
        // Visibility modifier is always first, so stop early
        if child.is_named() {
            break;
        }
    }
    false
}

fn item_name(node: Node, src: &[u8]) -> Option<String> {
    let name_node = node.child_by_field_name("name")?;
    let name = txt(name_node, src).trim().to_string();
    if name.is_empty() { None } else { Some(name) }
}

fn type_name(node: Node, src: &[u8]) -> Option<String> {
    // Structs, enums, traits, type aliases use `type_identifier` for their name
    let name_node = node.child_by_field_name("name")?;
    let name = txt(name_node, src).trim().to_string();
    if name.is_empty() { None } else { Some(name) }
}

fn item_signature(node: Node, src: &[u8]) -> String {
    let body = node.child_by_field_name("body");
    // For const/static, the value comes after `=`, use the whole node minus value
    let end = body.map_or_else(
        || {
            // For const/static: trim the `= value;` part
            let raw = txt(node, src);
            if let Some(eq_pos) = raw.find('=') {
                node.start_byte() + eq_pos
            } else {
                // Trim trailing semicolon
                let trimmed = raw.trim_end().trim_end_matches(';');
                node.start_byte() + trimmed.len()
            }
        },
        |b| b.start_byte(),
    );

    let raw = &src[node.start_byte()..end];
    let sig = collapse_whitespace(&String::from_utf8_lossy(raw));
    strip_visibility(&sig)
        .trim()
        .trim_end_matches('{')
        .trim()
        .to_string()
}

fn strip_visibility(sig: &str) -> String {
    let s = sig.trim_start();
    if let Some(rest) = s.strip_prefix("pub") {
        let rest = rest.trim_start();
        // Handle pub(crate), pub(super), pub(in path)
        if let Some(rest) = rest.strip_prefix('(') {
            if let Some(close) = rest.find(')') {
                return rest[close + 1..].trim_start().to_string();
            }
        }
        return rest.to_string();
    }
    s.to_string()
}

fn has_body(node: Node) -> bool {
    node.child_by_field_name("body").is_some()
}

fn has_test_attr(attrs: &[String]) -> bool {
    attrs.iter().any(|a| a == "test" || a == "tokio::test" || a == "rstest")
}

fn is_cfg_test(attrs: &[String]) -> bool {
    attrs.iter().any(|a| a == "cfg(test)")
}

fn attribute_name(node: Node, src: &[u8]) -> Option<String> {
    // attribute_item has an `attribute` child which contains the path + args
    let attr = node.named_child(0)?;
    let raw = txt(attr, src).trim().to_string();
    if raw.is_empty() {
        return None;
    }

    // For cfg(test), keep as-is
    if raw.starts_with("cfg(") {
        return Some(raw);
    }
    // For derive(Debug, Clone), keep as-is
    if raw.starts_with("derive(") {
        return Some(raw);
    }
    // For simple attributes like `test`, extract just the identifier
    if let Some(paren) = raw.find('(') {
        Some(raw[..paren].trim().to_string())
    } else {
        Some(raw)
    }
}

fn derive_list(attrs: &[String]) -> String {
    attrs
        .iter()
        .filter_map(|a| {
            let inner = a.strip_prefix("derive(")?;
            let inner = inner.strip_suffix(')')?;
            Some(inner.to_string())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn struct_field_names(node: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "field_declaration_list" {
            let mut inner = child.walk();
            for field in child.named_children(&mut inner) {
                if field.kind() == "field_declaration" {
                    if let Some(name) = field.child_by_field_name("name") {
                        let name = txt(name, src).trim().to_string();
                        if !name.is_empty() {
                            out.push(name);
                        }
                    }
                }
            }
        }
    }
    out
}

fn enum_variant_names(node: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "enum_variant_list" {
            let mut inner = child.walk();
            for variant in child.named_children(&mut inner) {
                if variant.kind() == "enum_variant" {
                    if let Some(name) = variant.child_by_field_name("name") {
                        let name = txt(name, src).trim().to_string();
                        if !name.is_empty() {
                            out.push(name);
                        }
                    }
                }
            }
        }
    }
    out
}

fn trait_bounds(node: Node, src: &[u8]) -> String {
    // trait Foo: Bar + Baz { ... }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "trait_bounds" {
            return txt(child, src).trim().to_string();
        }
    }
    String::new()
}

fn declaration_list_method_names(node: Node, src: &[u8]) -> Vec<String> {
    let Some(body) = node.child_by_field_name("body") else {
        return Vec::new();
    };

    let mut out = Vec::new();
    let mut cursor = body.walk();
    for child in body.named_children(&mut cursor) {
        match child.kind() {
            "function_item" | "function_signature_item" => {
                if let Some(name) = item_name(child, src) {
                    push_unique(&mut out, name);
                }
            }
            _ => {}
        }
    }
    out
}

fn impl_names(node: Node, src: &[u8]) -> (String, String) {
    // impl Trait for Type → name="Type", extends="Trait"
    // impl Type            → name="Type", extends=""
    let mut type_ids: Vec<String> = Vec::new();
    let mut has_for = false;
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "for" || txt(child, src).trim() == "for" {
            has_for = true;
        }
        if child.kind() == "type_identifier" || child.kind() == "generic_type"
            || child.kind() == "scoped_type_identifier"
        {
            type_ids.push(txt(child, src).trim().to_string());
        }
    }

    if has_for && type_ids.len() >= 2 {
        // impl Trait for Type
        let extends = type_ids[0].clone();
        let name = type_ids[1].clone();
        (name, extends)
    } else if let Some(name) = type_ids.into_iter().last() {
        (name, String::new())
    } else {
        ("?".to_string(), String::new())
    }
}

fn summarize_names(items: &[String]) -> String {
    if items.is_empty() {
        return String::new();
    }
    if items.len() <= 5 {
        return format!("{{{}}}", items.join(", "));
    }
    let shown = &items[..5];
    format!("{{{}, ...+{}}}", shown.join(", "), items.len() - 5)
}

fn collapse_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn push_unique(list: &mut Vec<String>, value: String) {
    if !list.iter().any(|v| v == &value) {
        list.push(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn parse_rs(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extract_sources_only_reads_use_and_mod() {
        let src = br"
use std::path::Path;
use crate::model::{Symbol, TypeDef};
mod submodule;
mod inline_mod { fn inner() {} }
";
        let tree = parse_rs(src);
        let imports = extract_sources_only(tree.root_node(), src);
        assert_eq!(
            imports,
            vec![
                "std::path::Path".to_string(),
                "crate::model::Symbol".to_string(),
                "crate::model::TypeDef".to_string(),
                "submodule".to_string(),
            ]
        );
    }

    #[test]
    fn extract_use_as_clause() {
        let src = br"use std::io::Result as IoResult;";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);
        assert_eq!(symbols.imports, vec!["std::io::Result".to_string()]);
        assert_eq!(symbols.import_bindings.len(), 1);
        assert_eq!(symbols.import_bindings[0].local_name, "IoResult");
        assert_eq!(symbols.import_bindings[0].source, "std::io::Result");
    }

    #[test]
    fn extract_symbols_classifies_pub_vs_private() {
        let src = br"
pub fn exported(x: i32) -> bool { true }
fn private() {}
pub(crate) fn crate_visible() {}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 2);
        assert!(symbols.exports[0].signature.contains("exported"));
        assert!(symbols.exports[1].signature.contains("crate_visible"));
        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.contains("private"));
    }

    #[test]
    fn extract_pub_fn_signature_strips_visibility() {
        let src = br"pub async fn process(input: &str) -> Result<()> { Ok(()) }";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert_eq!(
            symbols.exports[0].signature,
            "async fn process(input: &str) -> Result<()>"
        );
    }

    #[test]
    fn extract_struct_with_fields_and_derives() {
        let src = br"
#[derive(Debug, Clone)]
pub struct MyStruct {
    pub name: String,
    count: usize,
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        let ty = &symbols.types[0];
        assert_eq!(ty.name, "MyStruct");
        assert_eq!(ty.kind, "struct");
        assert!(ty.exported);
        assert!(ty.summary.contains("derives [Debug, Clone]"));
        assert!(ty.summary.contains("{name, count}"));
    }

    #[test]
    fn extract_enum_with_variants() {
        let src = br"
pub enum Color {
    Red,
    Green,
    Blue,
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        let ty = &symbols.types[0];
        assert_eq!(ty.name, "Color");
        assert_eq!(ty.kind, "enum");
        assert!(ty.exported);
        assert_eq!(ty.summary, "{Red, Green, Blue}");
    }

    #[test]
    fn extract_trait_with_methods() {
        let src = br"
pub trait Handler {
    fn handle(&self);
    fn process(&self) -> bool { true }
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        let ty = &symbols.types[0];
        assert_eq!(ty.name, "Handler");
        assert_eq!(ty.kind, "trait");
        assert!(ty.exported);
        assert_eq!(ty.summary, "{handle, process}");
    }

    #[test]
    fn extract_impl_block() {
        let src = br"
impl MyStruct {
    pub fn new() -> Self { Self {} }
    fn helper(&self) {}
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        let ty = &symbols.types[0];
        assert_eq!(ty.name, "MyStruct");
        assert_eq!(ty.kind, "impl");
        assert_eq!(ty.extends, "");
        assert_eq!(ty.summary, "{new, helper}");
    }

    #[test]
    fn extract_impl_trait_for_type() {
        let src = br"
impl Display for MyStruct {
    fn fmt(&self, f: &mut Formatter) -> Result { Ok(()) }
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        let ty = &symbols.types[0];
        assert_eq!(ty.name, "MyStruct");
        assert_eq!(ty.kind, "impl");
        assert_eq!(ty.extends, "Display");
        assert_eq!(ty.summary, "{fmt}");
    }

    #[test]
    fn extract_const_and_static() {
        let src = br#"
pub const MAX: usize = 100;
pub static GLOBAL: &str = "hello";
const PRIVATE: i32 = 42;
"#;
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 2);
        assert!(symbols.exports[0].signature.contains("const MAX: usize"));
        assert!(symbols.exports[1].signature.contains("static GLOBAL: &str"));
        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.contains("const PRIVATE: i32"));
    }

    #[test]
    fn extract_macro_definition() {
        let src = br"
macro_rules! my_macro {
    () => {};
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert_eq!(symbols.exports[0].signature, "macro_rules! my_macro");
    }

    #[test]
    fn extract_test_functions() {
        let src = br"
#[test]
fn test_basic() {
    assert!(true);
}

pub fn real_fn() {}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests.len(), 1);
        assert_eq!(symbols.tests[0].name, "test_basic");
        assert_eq!(symbols.exports.len(), 1);
        assert!(symbols.exports[0].signature.contains("real_fn"));
    }

    #[test]
    fn extract_cfg_test_module() {
        let src = br"
#[cfg(test)]
mod tests {
    #[test]
    fn inner_test_a() {}

    #[test]
    fn inner_test_b() {}

    fn helper() {}
}
";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests.len(), 1);
        assert_eq!(symbols.tests[0].name, "tests");
        assert_eq!(symbols.tests[0].children.len(), 2);
        assert_eq!(symbols.tests[0].children[0].name, "inner_test_a");
        assert_eq!(symbols.tests[0].children[1].name, "inner_test_b");
    }

    #[test]
    fn extract_calls_from_function_body() {
        let src = br#"
fn example() {
    let x = foo();
    bar::baz();
    obj.method();
    println!("hello");
}
"#;
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        let calls = &symbols.internals[0].calls;
        assert!(calls.contains(&"foo".to_string()));
        assert!(calls.contains(&"bar::baz".to_string()));
        assert!(calls.contains(&"obj.method".to_string()));
        assert!(calls.contains(&"println!".to_string()));
    }

    #[test]
    fn extract_type_alias() {
        let src = br"pub type Alias = Vec<String>;";
        let tree = parse_rs(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        assert_eq!(symbols.types[0].name, "Alias");
        assert_eq!(symbols.types[0].kind, "type alias");
        assert!(symbols.types[0].exported);
    }
}
