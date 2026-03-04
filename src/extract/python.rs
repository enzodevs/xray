use std::collections::HashSet;

use tree_sitter::Node;

use crate::model::{FileSymbols, ImportBinding, Symbol, TypeDef};
use crate::util::txt;

pub(super) fn extract_symbols(root: Node, src: &[u8]) -> FileSymbols {
    let mut symbols = super::empty_symbols();
    let mut explicit_public: Option<HashSet<String>> = None;

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "import_statement" => {
                extract_import_statement(
                    node,
                    src,
                    &mut symbols.imports,
                    &mut symbols.import_bindings,
                );
            }
            "import_from_statement" => {
                extract_import_from_statement(
                    node,
                    src,
                    &mut symbols.imports,
                    &mut symbols.import_bindings,
                );
            }
            "expression_statement" => {
                if let Some(items) = extract_dunder_all(node, src) {
                    explicit_public = Some(items.into_iter().collect());
                }
            }
            _ => {}
        }
    }

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "function_definition" => {
                if let Some((name, symbol)) = extract_function_symbol(node, src, Vec::new()) {
                    if is_public_symbol(&name, explicit_public.as_ref()) {
                        symbols.exports.push(symbol);
                    } else {
                        symbols.internals.push(symbol);
                    }
                }
            }
            "class_definition" => {
                if let Some((name, ty)) = extract_class_type(node, src, &[]) {
                    let mut ty = ty;
                    ty.exported = is_public_symbol(&name, explicit_public.as_ref());
                    symbols.types.push(ty);
                }
            }
            "decorated_definition" => {
                process_decorated_definition(node, src, explicit_public.as_ref(), &mut symbols);
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
        match node.kind() {
            "import_statement" => {
                extract_import_statement(node, src, &mut imports, &mut scratch_bindings);
            }
            "import_from_statement" => {
                extract_import_from_statement(node, src, &mut imports, &mut scratch_bindings);
            }
            _ => {}
        }
    }

    imports
}

fn process_decorated_definition(
    node: Node,
    src: &[u8],
    explicit_public: Option<&HashSet<String>>,
    symbols: &mut FileSymbols,
) {
    let mut decorators = Vec::new();
    let mut definition = None;
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "decorator" => {
                if let Some(name) = decorator_name(child, src) {
                    decorators.push(name);
                }
            }
            "function_definition" | "class_definition" => definition = Some(child),
            _ => {}
        }
    }

    let Some(definition) = definition else {
        return;
    };

    match definition.kind() {
        "function_definition" => {
            if let Some((name, symbol)) = extract_function_symbol(definition, src, decorators) {
                if is_public_symbol(&name, explicit_public) {
                    symbols.exports.push(symbol);
                } else {
                    symbols.internals.push(symbol);
                }
            }
        }
        "class_definition" => {
            if let Some((name, mut ty)) = extract_class_type(definition, src, &decorators) {
                ty.exported = is_public_symbol(&name, explicit_public);
                symbols.types.push(ty);
            }
        }
        _ => {}
    }
}

fn extract_import_statement(
    node: Node,
    src: &[u8],
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "dotted_name" => {
                let source = txt(child, src).trim().to_string();
                if source.is_empty() {
                    continue;
                }
                push_unique(imports, source.clone());
                bindings.push(ImportBinding {
                    local_name: last_segment(&source).to_string(),
                    source,
                    is_default: false,
                });
            }
            "aliased_import" => {
                if let Some((source, local)) = parse_aliased_import(child, src) {
                    push_unique(imports, source.clone());
                    bindings.push(ImportBinding {
                        local_name: local,
                        source,
                        is_default: false,
                    });
                }
            }
            _ => {}
        }
    }
}

fn extract_import_from_statement(
    node: Node,
    src: &[u8],
    imports: &mut Vec<String>,
    bindings: &mut Vec<ImportBinding>,
) {
    let mut base: Option<String> = None;
    let mut imported_names: Vec<(String, Option<String>)> = Vec::new();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "relative_import" => base = Some(txt(child, src).trim().to_string()),
            "dotted_name" => {
                if base.is_none() {
                    base = Some(txt(child, src).trim().to_string());
                } else {
                    imported_names.push((txt(child, src).trim().to_string(), None));
                }
            }
            "aliased_import" => {
                if let Some((name, alias)) = parse_aliased_import(child, src) {
                    imported_names.push((name, Some(alias)));
                }
            }
            "wildcard_import" => imported_names.push(("*".to_string(), None)),
            _ => {}
        }
    }

    let Some(base) = base else {
        return;
    };
    if base.is_empty() {
        return;
    }

    push_unique(imports, base.clone());

    for (name, alias) in imported_names {
        if name == "*" {
            continue;
        }

        let source = join_module_path(&base, &name);
        push_unique(imports, source.clone());
        bindings.push(ImportBinding {
            local_name: alias.unwrap_or_else(|| last_segment(&name).to_string()),
            source,
            is_default: false,
        });
    }
}

fn extract_function_symbol(
    node: Node,
    src: &[u8],
    decorators: Vec<String>,
) -> Option<(String, Symbol)> {
    let name_node = node.child_by_field_name("name")?;
    let name = txt(name_node, src).to_string();
    if name.is_empty() {
        return None;
    }

    let signature = function_signature(node, src);
    let body = node.child_by_field_name("body");

    let symbol = Symbol {
        signature,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: extract_calls(body, src),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators,
    };
    Some((name, symbol))
}

fn extract_class_type(node: Node, src: &[u8], decorators: &[String]) -> Option<(String, TypeDef)> {
    let name_node = node.child_by_field_name("name")?;
    let name = txt(name_node, src).to_string();
    if name.is_empty() {
        return None;
    }

    let (extends, summary) = class_metadata(node, src);
    let is_dataclass = decorators
        .iter()
        .any(|d| d.split('.').next_back() == Some("dataclass"));

    let type_def = TypeDef {
        name: name.clone(),
        kind: if is_dataclass {
            "dataclass".to_string()
        } else {
            "class".to_string()
        },
        extends,
        summary,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported: false,
    };
    Some((name, type_def))
}

fn function_signature(node: Node, src: &[u8]) -> String {
    let body = node.child_by_field_name("body");
    let end = body.map_or(node.end_byte(), |b| b.start_byte());
    let raw = &src[node.start_byte()..end];
    let collapsed: String = String::from_utf8_lossy(raw)
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    collapsed.trim().trim_end_matches(':').to_string()
}

fn class_metadata(node: Node, src: &[u8]) -> (String, String) {
    let header = class_header(node, src);
    let extends = parse_extends_from_header(&header);
    let methods = class_method_names(node, src);
    let summary = summarize_names(&methods);
    (extends, summary)
}

fn class_header(node: Node, src: &[u8]) -> String {
    let body = node.child_by_field_name("body");
    let end = body.map_or(node.end_byte(), |b| b.start_byte());
    String::from_utf8_lossy(&src[node.start_byte()..end])
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .trim()
        .trim_end_matches(':')
        .to_string()
}

fn parse_extends_from_header(header: &str) -> String {
    let Some(start) = header.find('(') else {
        return String::new();
    };
    let Some(end) = header.rfind(')') else {
        return String::new();
    };
    if end <= start {
        return String::new();
    }
    header[start + 1..end].trim().to_string()
}

fn class_method_names(node: Node, src: &[u8]) -> Vec<String> {
    let Some(body) = node.child_by_field_name("body") else {
        return Vec::new();
    };

    let mut out = Vec::new();
    let mut cursor = body.walk();
    for child in body.children(&mut cursor) {
        match child.kind() {
            "function_definition" => {
                if let Some(name) = child.child_by_field_name("name") {
                    let name = txt(name, src).trim();
                    if !name.is_empty() {
                        push_unique(&mut out, name.to_string());
                    }
                }
            }
            "decorated_definition" => {
                let mut inner = child.walk();
                for grand in child.children(&mut inner) {
                    if grand.kind() == "function_definition" {
                        if let Some(name) = grand.child_by_field_name("name") {
                            let name = txt(name, src).trim();
                            if !name.is_empty() {
                                push_unique(&mut out, name.to_string());
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    out
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

fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let Some(body) = body else {
        return Vec::new();
    };

    let mut calls = Vec::new();
    collect_calls_recursive(body, src, &mut calls);
    calls
}

fn collect_calls_recursive(node: Node, src: &[u8], calls: &mut Vec<String>) {
    if node.kind() == "call" {
        if let Some(name) = call_name(node, src) {
            push_unique(calls, name);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_calls_recursive(child, src, calls);
    }
}

fn call_name(call: Node, src: &[u8]) -> Option<String> {
    let callee = call
        .child_by_field_name("function")
        .or_else(|| call.named_child(0))?;
    let raw = match callee.kind() {
        "call" => return call_name(callee, src),
        _ => txt(callee, src).trim().to_string(),
    };
    if raw.is_empty() {
        None
    } else {
        Some(raw.split_whitespace().collect::<Vec<_>>().join(" "))
    }
}

fn extract_dunder_all(node: Node, src: &[u8]) -> Option<Vec<String>> {
    let assignment = node.named_child(0)?;
    if assignment.kind() != "assignment" {
        return None;
    }

    let left = assignment
        .child_by_field_name("left")
        .or_else(|| assignment.named_child(0))?;
    if txt(left, src).trim() != "__all__" {
        return None;
    }

    let right = assignment
        .child_by_field_name("right")
        .or_else(|| assignment.named_child(1))?;
    let names = parse_string_literals(txt(right, src));
    if names.is_empty() {
        None
    } else {
        Some(names)
    }
}

fn parse_string_literals(raw: &str) -> Vec<String> {
    let mut out = Vec::new();
    let chars: Vec<char> = raw.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let quote = chars[i];
        if quote != '\'' && quote != '"' {
            i += 1;
            continue;
        }

        i += 1;
        let mut value = String::new();
        while i < chars.len() {
            let ch = chars[i];
            if ch == '\\' && i + 1 < chars.len() {
                value.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if ch == quote {
                i += 1;
                break;
            }
            value.push(ch);
            i += 1;
        }

        let value = value.trim();
        if !value.is_empty() {
            out.push(value.to_string());
        }
    }

    out
}

fn decorator_name(node: Node, src: &[u8]) -> Option<String> {
    let mut raw = txt(node, src).trim().trim_start_matches('@').to_string();
    if raw.is_empty() {
        return None;
    }

    if let Some(idx) = raw.find('(') {
        raw.truncate(idx);
    }
    if let Some(idx) = raw.find('[') {
        raw.truncate(idx);
    }

    let name = raw.trim().to_string();
    if name.is_empty() {
        None
    } else {
        Some(name)
    }
}

fn parse_aliased_import(node: Node, src: &[u8]) -> Option<(String, String)> {
    let mut source = None;
    let mut alias = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "dotted_name" => source = Some(txt(child, src).trim().to_string()),
            "identifier" => alias = Some(txt(child, src).trim().to_string()),
            _ => {}
        }
    }

    let source = source?;
    let local = alias.unwrap_or_else(|| last_segment(&source).to_string());
    Some((source, local))
}

fn join_module_path(base: &str, name: &str) -> String {
    if base.is_empty() {
        return name.to_string();
    }
    if name.is_empty() {
        return base.to_string();
    }
    if base.ends_with('.') {
        format!("{base}{name}")
    } else {
        format!("{base}.{name}")
    }
}

fn is_public_symbol(name: &str, explicit_public: Option<&HashSet<String>>) -> bool {
    match explicit_public {
        Some(names) => names.contains(name),
        None => !name.starts_with('_'),
    }
}

fn last_segment(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
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

    fn parse_py(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_python::LANGUAGE.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extract_sources_only_reads_python_imports() {
        let src = br"
import pkg.mod
import pkg.other as alias
from . import local
from ..core import service as svc
from app.tools import helper
";
        let tree = parse_py(src);
        let imports = extract_sources_only(tree.root_node(), src);
        assert_eq!(
            imports,
            vec![
                "pkg.mod",
                "pkg.other",
                ".",
                ".local",
                "..core",
                "..core.service",
                "app.tools",
                "app.tools.helper"
            ]
        );
    }

    #[test]
    fn extract_symbols_classifies_public_by_convention() {
        let src = br"
def foo():
    helper()

def _private():
    pass

class Public:
    def method(self):
        return 1

class _Hidden:
    pass
";
        let tree = parse_py(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(symbols.exports[0].signature.starts_with("def foo("));
        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.starts_with("def _private("));
        assert_eq!(symbols.types.len(), 2);
        assert!(symbols
            .types
            .iter()
            .any(|t| t.name == "Public" && t.exported));
        assert!(symbols
            .types
            .iter()
            .any(|t| t.name == "_Hidden" && !t.exported));
    }

    #[test]
    fn extract_symbols_respects_dunder_all() {
        let src = br#"
__all__ = ["_private", "Public"]

def foo():
    pass

def _private():
    pass

class Public:
    pass
"#;
        let tree = parse_py(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(symbols.exports[0].signature.starts_with("def _private("));
        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.starts_with("def foo("));
        assert!(symbols
            .types
            .iter()
            .any(|t| t.name == "Public" && t.exported));
    }

    #[test]
    fn extract_function_calls_and_decorators() {
        let src = br"
@my.decorator(flag=True)
def run():
    helper()
    obj.method()
";
        let tree = parse_py(src);
        let symbols = extract_symbols(tree.root_node(), src);
        assert_eq!(symbols.exports.len(), 1);
        let sym = &symbols.exports[0];
        assert_eq!(sym.decorators, vec!["my.decorator".to_string()]);
        assert_eq!(
            sym.calls,
            vec!["helper".to_string(), "obj.method".to_string()]
        );
    }

    #[test]
    fn decorated_dataclass_sets_kind() {
        let src = br"
from dataclasses import dataclass

@dataclass
class User:
    id: int
";
        let tree = parse_py(src);
        let symbols = extract_symbols(tree.root_node(), src);
        assert_eq!(symbols.types.len(), 1);
        assert_eq!(symbols.types[0].kind, "dataclass");
        assert_eq!(symbols.types[0].name, "User");
    }
}
