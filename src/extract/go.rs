use tree_sitter::Node;

use crate::model::{FileSymbols, ImportBinding, Symbol, TestBlock, TypeDef};
use crate::util::{trim_quotes, txt};

pub(super) fn extract_symbols(root: Node, src: &[u8]) -> FileSymbols {
    let mut symbols = super::empty_symbols();

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "import_declaration" => extract_import_declaration(
                node,
                src,
                &mut symbols.imports,
                &mut symbols.import_bindings,
            ),
            "function_declaration" => extract_function(node, src, &mut symbols),
            "method_declaration" => extract_method(node, src, &mut symbols),
            "type_declaration" => extract_type_declaration(node, src, &mut symbols.types),
            "const_declaration" | "var_declaration" => {
                extract_value_declaration(node, src, &mut symbols);
            }
            _ => {}
        }
    }

    symbols
}

pub(super) fn extract_sources_only(root: Node, src: &[u8]) -> Vec<String> {
    let mut imports = Vec::new();
    let mut scratch_bindings = Vec::new();

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        if node.kind() == "import_declaration" {
            extract_import_declaration(node, src, &mut imports, &mut scratch_bindings);
        }
    }

    imports
}

fn extract_import_declaration(
    node: Node,
    src: &[u8],
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "import_spec" {
            extract_import_spec(child, src, imports, bindings);
        } else if child.kind() == "import_spec_list" {
            let mut list_cursor = child.walk();
            for spec in child.children(&mut list_cursor) {
                if spec.kind() == "import_spec" {
                    extract_import_spec(spec, src, imports, bindings);
                }
            }
        }
    }
}

fn extract_import_spec(
    node: Node,
    src: &[u8],
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    let mut alias = None;
    let mut source = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "interpreted_string_literal" | "raw_string_literal" => {
                source = Some(trim_quotes(txt(child, src).trim()).to_string());
            }
            "package_identifier" | "identifier" | "blank_identifier" => {
                alias = Some(txt(child, src).trim().to_string());
            }
            _ => {}
        }
    }

    let Some(source) = source.filter(|s| !s.is_empty()) else {
        return;
    };

    push_unique(imports, source.clone());
    let local_name = alias
        .filter(|a| !a.is_empty())
        .unwrap_or_else(|| package_name_from_import(&source).to_string());
    bindings.push(ImportBinding {
        local_name,
        source,
        is_default: false,
    });
}

fn extract_function(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let Some(name_node) = node.child_by_field_name("name") else {
        return;
    };
    let name = txt(name_node, src).trim();
    if name.is_empty() {
        return;
    }

    if let Some(test) = go_test_block(name, node) {
        symbols.tests.push(test);
        return;
    }

    let symbol = function_symbol(node, src);
    if is_exported_name(name) {
        symbols.exports.push(symbol);
    } else {
        symbols.internals.push(symbol);
    }
}

fn extract_method(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let Some(name_node) = node.child_by_field_name("name") else {
        return;
    };
    let name = txt(name_node, src).trim();
    if name.is_empty() {
        return;
    }

    let symbol = function_symbol(node, src);
    if is_exported_name(name) {
        symbols.exports.push(symbol);
    } else {
        symbols.internals.push(symbol);
    }
}

fn function_symbol(node: Node, src: &[u8]) -> Symbol {
    let body = node.child_by_field_name("body");
    Symbol {
        signature: signature_without_body(node, src),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: extract_calls(body, src),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: Vec::new(),
    }
}

fn extract_type_declaration(node: Node, src: &[u8], types: &mut Vec<TypeDef>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "type_spec" | "type_alias") {
            if let Some(ty) = extract_type_spec(child, src) {
                types.push(ty);
            }
        } else if child.kind() == "type_spec_list" {
            let mut list_cursor = child.walk();
            for spec in child.children(&mut list_cursor) {
                if matches!(spec.kind(), "type_spec" | "type_alias") {
                    if let Some(ty) = extract_type_spec(spec, src) {
                        types.push(ty);
                    }
                }
            }
        }
    }
}

fn extract_type_spec(node: Node, src: &[u8]) -> Option<TypeDef> {
    let name_node = node.child_by_field_name("name").or_else(|| {
        let mut cursor = node.walk();
        let found = node
            .children(&mut cursor)
            .find(|child| child.kind() == "type_identifier");
        found
    })?;
    let name = txt(name_node, src).trim();
    if name.is_empty() {
        return None;
    }

    let type_node = node.child_by_field_name("type").or_else(|| {
        let mut cursor = node.walk();
        let found = node.children(&mut cursor).find(|child| {
            matches!(
                child.kind(),
                "struct_type"
                    | "interface_type"
                    | "function_type"
                    | "type_identifier"
                    | "qualified_type"
                    | "pointer_type"
                    | "slice_type"
                    | "array_type"
                    | "map_type"
                    | "channel_type"
            )
        });
        found
    });
    let kind = type_node.map_or("type", |node| match node.kind() {
        "struct_type" => "struct",
        "interface_type" => "interface",
        "function_type" => "func type",
        _ => "type",
    });

    Some(TypeDef {
        name: name.to_string(),
        kind: kind.to_string(),
        extends: type_node.map_or_else(String::new, |node| compact_node_text(node, src)),
        summary: type_node.map_or_else(String::new, |node| type_member_summary(node, src)),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: is_exported_name(name),
    })
}

fn extract_value_declaration(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let keyword = if node.kind() == "const_declaration" {
        "const"
    } else {
        "var"
    };

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "const_spec" | "var_spec") {
            extract_value_spec(child, src, keyword, symbols);
        } else if matches!(child.kind(), "const_spec_list" | "var_spec_list") {
            let mut list_cursor = child.walk();
            for spec in child.children(&mut list_cursor) {
                if matches!(spec.kind(), "const_spec" | "var_spec") {
                    extract_value_spec(spec, src, keyword, symbols);
                }
            }
        }
    }
}

fn extract_value_spec(node: Node, src: &[u8], keyword: &str, symbols: &mut FileSymbols) {
    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "identifier" {
            let name = txt(child, src).trim();
            if !name.is_empty() {
                names.push((name.to_string(), child));
            }
        }
    }

    for (name, name_node) in names {
        let symbol = Symbol {
            signature: format!("{keyword} {name}"),
            line_start: name_node.start_position().row + 1,
            line_end: node.end_position().row + 1,
            calls: Vec::new(),
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        };
        if is_exported_name(&name) {
            symbols.exports.push(symbol);
        } else {
            symbols.internals.push(symbol);
        }
    }
}

fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let Some(body) = body else {
        return Vec::new();
    };
    let mut calls = Vec::new();
    collect_calls_recursive(body, src, &mut calls);
    calls
}

fn collect_calls_recursive(node: Node, src: &[u8], calls: &mut Vec<String>) {
    if node.kind() == "call_expression" {
        if let Some(name) = call_name(node, src) {
            push_unique(calls, name);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_calls_recursive(child, src, calls);
    }
}

fn call_name(node: Node, src: &[u8]) -> Option<String> {
    let callee = node.child_by_field_name("function")?;
    match callee.kind() {
        "identifier" | "selector_expression" => {
            let name = txt(callee, src).trim();
            (!name.is_empty()).then(|| name.to_string())
        }
        _ => {
            let name = txt(callee, src).trim();
            (!name.is_empty() && !name.contains('\n')).then(|| name.to_string())
        }
    }
}

fn signature_without_body(node: Node, src: &[u8]) -> String {
    let end = node
        .child_by_field_name("body")
        .map_or_else(|| node.end_byte(), |body| body.start_byte());
    let raw = String::from_utf8_lossy(&src[node.start_byte()..end]);
    collapse_whitespace(raw.trim().trim_end_matches('{').trim())
}

fn type_member_summary(node: Node, src: &[u8]) -> String {
    let mut names = Vec::new();
    collect_type_member_names(node, src, &mut names);
    summarize_names(&names)
}

fn collect_type_member_names(node: Node, src: &[u8], names: &mut Vec<String>) {
    if matches!(node.kind(), "field_identifier" | "method_elem") {
        let name = if node.kind() == "method_elem" {
            node.child_by_field_name("name").map_or_else(
                || txt(node, src).split('(').next().unwrap_or("").trim(),
                |name| txt(name, src).trim(),
            )
        } else {
            txt(node, src).trim()
        };
        if !name.is_empty() {
            push_unique(names, name.to_string());
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_type_member_names(child, src, names);
    }
}

fn compact_node_text(node: Node, src: &[u8]) -> String {
    let text = txt(node, src).trim();
    if text.len() > 80 {
        format!("{}...", collapse_whitespace(&text[..80]))
    } else {
        collapse_whitespace(text)
    }
}

fn go_test_block(name: &str, node: Node) -> Option<TestBlock> {
    let kind = if is_go_test_name(name, "Test") {
        "test"
    } else if is_go_test_name(name, "Benchmark") {
        "benchmark"
    } else if is_go_test_name(name, "Fuzz") {
        "fuzz"
    } else {
        return None;
    };

    Some(TestBlock {
        kind: kind.to_string(),
        name: name.to_string(),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        children: Vec::new(),
    })
}

fn is_go_test_name(name: &str, prefix: &str) -> bool {
    name.strip_prefix(prefix)
        .and_then(|rest| rest.chars().next())
        .is_some_and(|ch| ch == '_' || ch.is_ascii_uppercase())
}

fn is_exported_name(name: &str) -> bool {
    name.chars().next().is_some_and(char::is_uppercase)
}

fn package_name_from_import(source: &str) -> &str {
    source.rsplit('/').next().unwrap_or(source)
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

    fn parse_go(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_go::LANGUAGE.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extract_sources_only_reads_go_imports() {
        let src = br#"
package service

import (
    "context"
    db "example.com/app/internal/db"
    "example.com/app/pkg/user"
)
"#;
        let tree = parse_go(src);
        let imports = extract_sources_only(tree.root_node(), src);

        assert_eq!(
            imports,
            vec![
                "context".to_string(),
                "example.com/app/internal/db".to_string(),
                "example.com/app/pkg/user".to_string(),
            ]
        );
    }

    #[test]
    fn extract_symbols_reads_go_functions_types_values_calls_and_tests() {
        let src = br#"
package service

import (
    "context"
    db "example.com/app/internal/db"
)

const PublicLimit = 10
var privateCache = map[string]string{}

type Store struct {
    Name string
    db.Client
}

type Runner interface {
    Run(context.Context) error
}

func NewStore(client db.Client) *Store {
    helper()
    return &Store{}
}

func helper() {}

func (s *Store) Run(ctx context.Context) error {
    return s.Client.Open(ctx)
}

func TestNewStore(t *testing.T) {}
"#;
        let tree = parse_go(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.imports,
            vec![
                "context".to_string(),
                "example.com/app/internal/db".to_string(),
            ]
        );
        assert_eq!(symbols.import_bindings[1].local_name, "db");
        assert!(symbols
            .exports
            .iter()
            .any(|sym| sym.signature.starts_with("const PublicLimit")));
        assert!(symbols
            .internals
            .iter()
            .any(|sym| sym.signature.starts_with("var privateCache")));

        let new_store = symbols
            .exports
            .iter()
            .find(|sym| sym.signature.starts_with("func NewStore"))
            .expect("NewStore export");
        assert_eq!(new_store.calls, vec!["helper".to_string()]);

        let method = symbols
            .exports
            .iter()
            .find(|sym| sym.signature.starts_with("func (s *Store) Run"))
            .expect("Run method export");
        assert_eq!(method.calls, vec!["s.Client.Open".to_string()]);

        assert!(symbols
            .types
            .iter()
            .any(|ty| ty.name == "Store" && ty.kind == "struct" && ty.summary == "{Name}"));
        assert!(symbols
            .types
            .iter()
            .any(|ty| ty.name == "Runner" && ty.kind == "interface"));
        assert_eq!(symbols.tests[0].name, "TestNewStore");
    }
}
