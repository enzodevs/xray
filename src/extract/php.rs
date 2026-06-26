use tree_sitter::Node;

use crate::model::{FileSymbols, ImportBinding, Symbol, TypeDef};
use crate::util::{trim_quotes, txt};

pub(super) fn extract_symbols(root: Node, src: &[u8]) -> FileSymbols {
    let mut symbols = super::empty_symbols();
    collect_top_level(root, src, &mut symbols);
    symbols
}

pub(super) fn extract_sources_only(root: Node, src: &[u8]) -> Vec<String> {
    let mut symbols = super::empty_symbols();
    collect_imports_recursive(root, src, &mut symbols);
    symbols.imports
}

fn collect_top_level(root: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "namespace_use_declaration" => extract_namespace_use(node, src, symbols),
            "expression_statement" => collect_imports_recursive(node, src, symbols),
            "function_definition" => {
                if let Some(symbol) = extract_function(node, src) {
                    symbols.exports.push(symbol);
                }
            }
            "class_declaration" | "interface_declaration" | "trait_declaration"
            | "enum_declaration" => {
                if let Some(ty) = extract_type(node, src) {
                    symbols.types.push(ty);
                }
            }
            "const_declaration" => extract_const(node, src, symbols),
            "namespace_definition" => collect_namespace(node, src, symbols),
            _ => {}
        }
    }
}

fn collect_namespace(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "compound_statement" => collect_top_level(child, src, symbols),
            "namespace_use_declaration" => extract_namespace_use(child, src, symbols),
            "function_definition" => {
                if let Some(symbol) = extract_function(child, src) {
                    symbols.exports.push(symbol);
                }
            }
            "class_declaration" | "interface_declaration" | "trait_declaration"
            | "enum_declaration" => {
                if let Some(ty) = extract_type(child, src) {
                    symbols.types.push(ty);
                }
            }
            "const_declaration" => extract_const(child, src, symbols),
            _ => {}
        }
    }
}

fn collect_imports_recursive(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    match node.kind() {
        "namespace_use_declaration" => extract_namespace_use(node, src, symbols),
        "include_expression"
        | "include_once_expression"
        | "require_expression"
        | "require_once_expression" => {
            if let Some(path) = literal_child_text(node, src) {
                push_unique(&mut symbols.imports, path);
            }
        }
        _ => {}
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_imports_recursive(child, src, symbols);
    }
}

fn extract_namespace_use(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mut prefix = String::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "namespace_name" => prefix = normalize_php_name(txt(child, src)),
            "namespace_use_clause" => extract_namespace_use_clause(child, src, &prefix, symbols),
            "namespace_use_group" => {
                let mut group_cursor = child.walk();
                for clause in child.children(&mut group_cursor) {
                    if clause.kind() == "namespace_use_clause" {
                        extract_namespace_use_clause(clause, src, &prefix, symbols);
                    }
                }
            }
            _ => {}
        }
    }
}

fn extract_namespace_use_clause(
    node: Node,
    src: &[u8],
    prefix: &str,
    symbols: &mut FileSymbols,
) {
    let mut source = String::new();
    let mut alias = None;
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "name" | "qualified_name" => {
                let value = normalize_php_name(txt(child, src));
                if source.is_empty() {
                    source = value;
                } else {
                    alias = Some(value);
                }
            }
            _ => {}
        }
    }

    if source.is_empty() {
        return;
    }

    if !prefix.is_empty() && !source.starts_with('\\') {
        source = format!("{prefix}\\{source}");
    }

    let local_name = alias.unwrap_or_else(|| last_php_segment(&source).to_string());
    push_unique(&mut symbols.imports, source.clone());
    symbols.import_bindings.push(ImportBinding {
        local_name,
        source,
        is_default: false,
    });
}

fn extract_function(node: Node, src: &[u8]) -> Option<Symbol> {
    let name = node.child_by_field_name("name")?;
    let name = txt(name, src).trim();
    if name.is_empty() {
        return None;
    }

    let body = node.child_by_field_name("body");
    Some(Symbol {
        signature: signature(node, src),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: extract_calls(body, src),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: extract_attributes(node, src),
    })
}

fn extract_type(node: Node, src: &[u8]) -> Option<TypeDef> {
    let name_node = node.child_by_field_name("name")?;
    let name = txt(name_node, src).trim();
    if name.is_empty() {
        return None;
    }

    let kind = match node.kind() {
        "class_declaration" => "class",
        "interface_declaration" => "interface",
        "trait_declaration" => "trait",
        "enum_declaration" => "enum",
        _ => return None,
    };

    Some(TypeDef {
        name: name.to_string(),
        kind: kind.to_string(),
        extends: type_extends(node, src),
        summary: type_member_summary(node, src),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: true,
    })
}

fn extract_const(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != "const_element" {
            continue;
        }
        let Some(name_node) = first_named_child_of_kind(child, "name") else {
            continue;
        };
        let name = txt(name_node, src).trim();
        if name.is_empty() {
            continue;
        }

        symbols.exports.push(Symbol {
            signature: format!("const {name}"),
            line_start: child.start_position().row + 1,
            line_end: child.end_position().row + 1,
            calls: Vec::new(),
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: extract_attributes(node, src),
        });
    }
}

fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let mut calls = Vec::new();
    if let Some(body) = body {
        collect_calls_recursive(body, src, &mut calls);
    }
    calls
}

fn collect_calls_recursive(node: Node, src: &[u8], calls: &mut Vec<String>) {
    if let Some(name) = call_name(node, src) {
        push_unique(calls, name);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_calls_recursive(child, src, calls);
    }
}

fn call_name(node: Node, src: &[u8]) -> Option<String> {
    match node.kind() {
        "function_call_expression" => node
            .child_by_field_name("function")
            .map(|callee| normalize_php_name(txt(callee, src)))
            .filter(|name| !name.is_empty()),
        "member_call_expression" | "nullsafe_member_call_expression" | "scoped_call_expression" => {
            node.child_by_field_name("name")
                .map(|name| normalize_php_name(txt(name, src)))
                .filter(|name| !name.is_empty())
        }
        "include_expression"
        | "include_once_expression"
        | "require_expression"
        | "require_once_expression" => Some(node.kind().trim_end_matches("_expression").to_string()),
        _ => None,
    }
}

fn signature(node: Node, src: &[u8]) -> String {
    let end = node
        .child_by_field_name("body")
        .map_or_else(|| node.end_byte(), |body| body.start_byte());
    let raw = String::from_utf8_lossy(&src[node.start_byte()..end]);
    collapse_whitespace(raw.trim().trim_end_matches('{').trim())
}

fn type_extends(node: Node, src: &[u8]) -> String {
    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "base_clause" | "class_interface_clause") {
            collect_name_children(child, src, &mut names);
        }
    }
    names.join(", ")
}

fn type_member_summary(node: Node, src: &[u8]) -> String {
    let Some(body) = node.child_by_field_name("body") else {
        return String::new();
    };

    let mut names = Vec::new();
    let mut cursor = body.walk();
    for child in body.children(&mut cursor) {
        match child.kind() {
            "method_declaration" | "const_declaration" | "property_declaration" => {
                collect_member_names(child, src, &mut names);
            }
            _ => {}
        }
    }
    summarize_names(&names)
}

fn collect_member_names(node: Node, src: &[u8], names: &mut Vec<String>) {
    match node.kind() {
        "method_declaration" => {
            if let Some(name) = node.child_by_field_name("name") {
                let name = txt(name, src).trim();
                if !name.is_empty() {
                    push_unique(names, name.to_string());
                }
            }
        }
        "const_declaration" | "property_declaration" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                collect_member_names(child, src, names);
            }
        }
        "const_element" | "property_element" => {
            if let Some(name) = first_named_child_of_kind(node, "name")
                .or_else(|| first_named_child_of_kind(node, "variable_name"))
            {
                let name = txt(name, src).trim().trim_start_matches('$');
                if !name.is_empty() {
                    push_unique(names, name.to_string());
                }
            }
        }
        _ => {}
    }
}

fn collect_name_children(node: Node, src: &[u8], names: &mut Vec<String>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(
            child.kind(),
            "name" | "qualified_name" | "relative_name" | "namespace_name"
        ) {
            let name = normalize_php_name(txt(child, src));
            if !name.is_empty() {
                push_unique(names, name);
            }
        } else {
            collect_name_children(child, src, names);
        }
    }
}

fn extract_attributes(node: Node, src: &[u8]) -> Vec<String> {
    let Some(attrs) = node.child_by_field_name("attributes") else {
        return Vec::new();
    };

    let mut out = Vec::new();
    collect_attributes(attrs, src, &mut out);
    out
}

fn collect_attributes(node: Node, src: &[u8], out: &mut Vec<String>) {
    if matches!(node.kind(), "attribute" | "name" | "qualified_name") {
        let name = normalize_php_name(txt(node, src).trim_start_matches("#["));
        if !name.is_empty() && !name.contains('(') {
            push_unique(out, name);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_attributes(child, src, out);
    }
}

fn literal_child_text(node: Node, src: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "string" | "encapsed_string") {
            let value = trim_quotes(txt(child, src).trim()).to_string();
            if !value.is_empty() {
                return Some(value);
            }
        }
    }
    None
}

fn first_named_child_of_kind<'tree>(node: Node<'tree>, kind: &str) -> Option<Node<'tree>> {
    let mut cursor = node.walk();
    let found = node.children(&mut cursor).find(|child| child.kind() == kind);
    found
}

fn normalize_php_name(raw: &str) -> String {
    raw.trim()
        .trim_start_matches('\\')
        .split_whitespace()
        .collect::<Vec<_>>()
        .join("")
}

fn last_php_segment(path: &str) -> &str {
    path.rsplit('\\').next().unwrap_or(path)
}

fn summarize_names(items: &[String]) -> String {
    if items.is_empty() {
        return String::new();
    }
    if items.len() <= 5 {
        format!("{{{}}}", items.join(", "))
    } else {
        format!("{{{}, ...+{}}}", items[..5].join(", "), items.len() - 5)
    }
}

fn collapse_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn push_unique(list: &mut Vec<String>, value: String) {
    if !list.iter().any(|existing| existing == &value) {
        list.push(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_php(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_php::LANGUAGE_PHP.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extract_sources_only_reads_use_and_require() {
        let src = br#"<?php
            use App\Services\UserService;
            require_once __DIR__ . '/ignored.php';
            require './bootstrap.php';
            include_once "../shared/helpers.php";
        "#;
        let tree = parse_php(src);
        let imports = extract_sources_only(tree.root_node(), src);
        assert_eq!(
            imports,
            vec![
                "App\\Services\\UserService".to_string(),
                "./bootstrap.php".to_string(),
                "../shared/helpers.php".to_string()
            ]
        );
    }

    #[test]
    fn extract_symbols_reads_functions_types_consts_and_calls() {
        let src = br"<?php
            namespace App\Http;
            use App\Services\UserService as Users;

            const LIMIT = 10;

            function handle($id): Response {
                $svc = new Users();
                return response($svc->find($id));
            }

            final class Controller extends BaseController implements Routable {
                public string $name;
                public function index() { return handle(1); }
            }
        ";
        let tree = parse_php(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.imports, vec!["App\\Services\\UserService".to_string()]);
        assert_eq!(symbols.import_bindings[0].local_name, "Users");
        assert_eq!(symbols.exports.len(), 2);
        assert_eq!(symbols.exports[0].signature, "const LIMIT");
        assert!(symbols.exports[1].signature.starts_with("function handle"));
        assert_eq!(
            symbols.exports[1].calls,
            vec!["response".to_string(), "find".to_string()]
        );
        assert_eq!(symbols.types[0].name, "Controller");
        assert_eq!(symbols.types[0].kind, "class");
        assert_eq!(symbols.types[0].extends, "BaseController, Routable");
        assert_eq!(symbols.types[0].summary, "{name, index}");
    }
}
