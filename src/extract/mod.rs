mod calls;
mod decorators;
mod hooks;
mod jsx;
mod tests_block;
mod types;

use tree_sitter::Node;

use crate::model::{FileSymbols, Hook, ReExport, Symbol};
use crate::util::{trim_quotes, txt};

/// Walk top-level children of the AST root and extract all symbols.
pub fn extract_symbols(root: Node, src: &[u8]) -> FileSymbols {
    let mut symbols = FileSymbols {
        imports: Vec::new(),
        reexports: Vec::new(),
        exports: Vec::new(),
        internals: Vec::new(),
        types: Vec::new(),
        tests: Vec::new(),
        hooks: Vec::new(),
    };

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        match node.kind() {
            "import_statement" => {
                extract_import(node, src, &mut symbols.imports);
            }
            "export_statement" => {
                process_export(node, src, &mut symbols);
            }
            "function_declaration" => {
                if let Some(sym) = extract_function(node, src) {
                    symbols.internals.push(sym);
                }
            }
            "class_declaration" => {
                extract_class(node, src, &mut symbols.internals);
            }
            "interface_declaration" | "type_alias_declaration" | "enum_declaration" => {
                if let Some(t) = types::extract_type_def(node, src, false) {
                    symbols.types.push(t);
                }
            }
            "lexical_declaration" => {
                process_lexical(
                    node,
                    src,
                    false,
                    &mut symbols.internals,
                    Some(&mut symbols.hooks),
                );
            }
            "expression_statement" => {
                if let Some(tb) = tests_block::extract_test_block(node, src) {
                    symbols.tests.push(tb);
                } else {
                    try_extract_namespace_from(node, src, &mut symbols);
                }
            }
            "ambient_declaration" => {
                try_extract_namespace_from(node, src, &mut symbols);
            }
            _ => {}
        }
    }

    symbols
}

// ── Imports ──

fn extract_import(node: Node, src: &[u8], imports: &mut Vec<String>) {
    if let Some(source_node) = node.child_by_field_name("source") {
        let path = trim_quotes(txt(source_node, src));
        if path.starts_with('.') || path.starts_with("@/") {
            imports.push(path.to_string());
        }
    }
}

// ── Signature extraction ──

fn get_signature(node: Node, src: &[u8]) -> String {
    let body = node.child_by_field_name("body");
    let end = body.map_or(node.end_byte(), |b| b.start_byte());
    let raw = &src[node.start_byte()..end];
    let sig = String::from_utf8_lossy(raw);
    let cleaned: String = sig.split_whitespace().collect::<Vec<_>>().join(" ");
    let cleaned = cleaned.trim_end_matches('{').trim();
    cleaned
        .trim_start_matches("export ")
        .trim_start_matches("default ")
        .to_string()
}

// ── Functions ──

fn extract_function(node: Node, src: &[u8]) -> Option<Symbol> {
    let sig = get_signature(node, src);
    if sig.is_empty() {
        return None;
    }

    let body = node.child_by_field_name("body");
    let is_component = jsx::returns_jsx(body);
    let calls = calls::extract_calls(body, src);
    let renders = if is_component {
        jsx::extract_jsx_components(body, src)
    } else {
        Vec::new()
    };

    let (hooks, handlers) = if is_component {
        body.map_or_else(Default::default, |b| {
            hooks::extract_body_hooks_and_handlers(b, src)
        })
    } else {
        (Vec::new(), Vec::new())
    };
    let calls = hooks::filter_hook_calls(calls, &hooks);

    let decorators = decorators::extract_decorators(node, src);

    Some(Symbol {
        signature: sig,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls,
        is_component,
        renders,
        hooks,
        handlers,
        decorators,
    })
}

fn extract_arrow(declarator: Node, src: &[u8]) -> Option<Symbol> {
    let name_node = declarator.child_by_field_name("name")?;
    let name = txt(name_node, src);
    let value = declarator.child_by_field_name("value")?;

    if value.kind() != "arrow_function" {
        return None;
    }

    let arrow_sig = get_signature(value, src);
    let full_text = txt(value, src);
    let is_async = full_text.trim_start().starts_with("async");
    let prefix = if is_async { "async const " } else { "const " };
    let sig = format!("{prefix}{name} = {arrow_sig}");

    let body = value.child_by_field_name("body");
    let is_component = jsx::returns_jsx(body);
    let calls = calls::extract_calls(body, src);
    let renders = if is_component {
        jsx::extract_jsx_components(body, src)
    } else {
        Vec::new()
    };

    let (hooks, handlers) = if is_component {
        body.map_or_else(Default::default, |b| {
            hooks::extract_body_hooks_and_handlers(b, src)
        })
    } else {
        (Vec::new(), Vec::new())
    };
    let calls = hooks::filter_hook_calls(calls, &hooks);

    Some(Symbol {
        signature: sig,
        line_start: declarator.start_position().row + 1,
        line_end: declarator.end_position().row + 1,
        calls,
        is_component,
        renders,
        hooks,
        handlers,
        decorators: Vec::new(),
    })
}

// ── Exports ──

fn process_export(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    if node.child_by_field_name("source").is_some() {
        extract_reexport(node, src, &mut symbols.reexports);
        return;
    }

    let is_default = txt(node, src).starts_with("export default");

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "function_declaration" => {
                if let Some(sym) = extract_function(child, src) {
                    symbols.exports.push(sym);
                }
            }
            "class_declaration" => {
                extract_class(child, src, &mut symbols.exports);
            }
            "interface_declaration" | "type_alias_declaration" | "enum_declaration" => {
                if let Some(t) = types::extract_type_def(child, src, true) {
                    symbols.types.push(t);
                }
            }
            "lexical_declaration" => {
                process_lexical(child, src, true, &mut symbols.exports, None);
            }
            "internal_module" | "module" => {
                extract_namespace(child, src, symbols);
            }
            // ── Anonymous default exports ──
            "function_expression" | "function" if is_default => {
                if let Some(sym) = extract_default_function(child, node, src) {
                    symbols.exports.push(sym);
                }
            }
            "class" if is_default => {
                extract_default_class(child, node, src, &mut symbols.exports);
            }
            "object" if is_default => {
                symbols.exports.push(Symbol {
                    signature: "default = {...}".to_string(),
                    line_start: node.start_position().row + 1,
                    line_end: node.end_position().row + 1,
                    calls: Vec::new(),
                    is_component: false,
                    renders: Vec::new(),
                    hooks: Vec::new(),
                    handlers: Vec::new(),
                    decorators: Vec::new(),
                });
            }
            "identifier" if is_default => {
                let name = txt(child, src);
                symbols.exports.push(Symbol {
                    signature: format!("default = {name}"),
                    line_start: node.start_position().row + 1,
                    line_end: node.end_position().row + 1,
                    calls: Vec::new(),
                    is_component: false,
                    renders: Vec::new(),
                    hooks: Vec::new(),
                    handlers: Vec::new(),
                    decorators: Vec::new(),
                });
            }
            _ => {}
        }
    }
}

/// Extract a `export default function() {}` or `export default function foo() {}`.
fn extract_default_function(func: Node, export_node: Node, src: &[u8]) -> Option<Symbol> {
    let sig = get_signature(func, src);
    let has_name = func.child_by_field_name("name").is_some();
    let final_sig = if has_name {
        sig
    } else {
        format!("default {sig}")
    };

    if final_sig.is_empty() {
        return None;
    }

    let body = func.child_by_field_name("body");
    let is_component = jsx::returns_jsx(body);
    let calls = calls::extract_calls(body, src);
    let renders = if is_component {
        jsx::extract_jsx_components(body, src)
    } else {
        Vec::new()
    };
    let (hooks, handlers) = if is_component {
        body.map_or_else(Default::default, |b| {
            hooks::extract_body_hooks_and_handlers(b, src)
        })
    } else {
        (Vec::new(), Vec::new())
    };
    let calls = hooks::filter_hook_calls(calls, &hooks);

    Some(Symbol {
        signature: final_sig,
        line_start: export_node.start_position().row + 1,
        line_end: export_node.end_position().row + 1,
        calls,
        is_component,
        renders,
        hooks,
        handlers,
        decorators: Vec::new(),
    })
}

/// Extract a `export default class {}` or `export default class Foo {}`.
fn extract_default_class(
    class_node: Node,
    export_node: Node,
    src: &[u8],
    symbols: &mut Vec<Symbol>,
) {
    let class_sig = get_signature(class_node, src);
    let trimmed = class_sig
        .find("class ")
        .map_or(&*class_sig, |i| &class_sig[i..]);
    let has_name = class_node.child_by_field_name("name").is_some();
    let sig = if has_name {
        format!("class {}", trimmed.trim_start_matches("class "))
    } else {
        "default class".to_string()
    };
    let class_decorators = decorators::extract_decorators(class_node, src);

    symbols.push(Symbol {
        signature: sig,
        line_start: export_node.start_position().row + 1,
        line_end: export_node.end_position().row + 1,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: class_decorators,
    });

    if let Some(body) = class_node.child_by_field_name("body") {
        let mut cursor = body.walk();
        for member in body.children(&mut cursor) {
            if matches!(
                member.kind(),
                "method_definition" | "public_field_definition"
            ) {
                if let Some(sym) = extract_function(member, src) {
                    symbols.push(sym);
                }
            }
        }
    }
}

fn extract_reexport(node: Node, src: &[u8], reexports: &mut Vec<ReExport>) {
    let source = node
        .child_by_field_name("source")
        .map(|s| trim_quotes(txt(s, src)).to_string())
        .unwrap_or_default();

    let full = txt(node, src);
    let is_type = full.starts_with("export type");

    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "export_clause" {
            let mut ecursor = child.walk();
            for spec in child.children(&mut ecursor) {
                if spec.kind() == "export_specifier" {
                    if let Some(name) = spec.child_by_field_name("name") {
                        names.push(txt(name, src).to_string());
                    }
                }
            }
        }
    }

    if names.is_empty() && full.contains('*') {
        names.push("*".to_string());
    }

    if !source.is_empty() && !names.is_empty() {
        reexports.push(ReExport {
            names,
            source,
            is_type,
        });
    }
}

// ── Classes ──

fn extract_class(node: Node, src: &[u8], symbols: &mut Vec<Symbol>) {
    let class_sig = get_signature(node, src);
    // Strip decorator prefix: "@Foo(...) class X" → "class X"
    let trimmed = class_sig
        .find("class ")
        .map_or(&*class_sig, |i| &class_sig[i..]);
    let class_start = node.start_position().row + 1;
    let class_end = node.end_position().row + 1;
    let class_decorators = decorators::extract_decorators(node, src);

    symbols.push(Symbol {
        signature: format!("class {}", trimmed.trim_start_matches("class ")),
        line_start: class_start,
        line_end: class_end,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: class_decorators,
    });

    if let Some(body) = node.child_by_field_name("body") {
        let mut cursor = body.walk();
        for member in body.children(&mut cursor) {
            if matches!(
                member.kind(),
                "method_definition" | "public_field_definition"
            ) {
                if let Some(sym) = extract_function(member, src) {
                    symbols.push(sym);
                }
            }
        }
    }
}

// ── Lexical declarations (const/let) ──

fn process_lexical(
    node: Node,
    src: &[u8],
    exported: bool,
    symbols: &mut Vec<Symbol>,
    mut top_hooks: Option<&mut Vec<Hook>>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != "variable_declarator" {
            continue;
        }

        // Check for React hooks (array destructuring + hook call)
        if let Some(ref mut top_hooks) = top_hooks {
            if let Some(hook) = hooks::try_extract_hook(child, src) {
                top_hooks.push(hook);
                continue;
            }
        }

        if let Some(value) = child.child_by_field_name("value") {
            // Try React.memo / React.forwardRef unwrapping first
            if let Some(sym) = try_unwrap_react_wrapper(child, value, src) {
                symbols.push(sym);
            } else if value.kind() == "arrow_function" {
                if let Some(sym) = extract_arrow(child, src) {
                    symbols.push(sym);
                }
            } else if let Some(sym) = extract_const(child, value, src, exported) {
                symbols.push(sym);
            }
        }
    }
}

// ── React.memo / React.forwardRef unwrapping ──

/// Try to unwrap `React.memo(function ...)` or `React.forwardRef((p, r) => ...)`.
///
/// Returns a `Symbol` with the inner function's signature and `is_component: true`.
fn try_unwrap_react_wrapper(declarator: Node, value: Node, src: &[u8]) -> Option<Symbol> {
    if value.kind() != "call_expression" {
        return None;
    }

    let callee = value.child_by_field_name("function")?;
    let callee_text = txt(callee, src);

    let wrapper = match callee_text {
        "React.memo" | "memo" => "memo",
        "React.forwardRef" | "forwardRef" => "forwardRef",
        _ => return None,
    };

    let name_node = declarator.child_by_field_name("name")?;
    let name = txt(name_node, src);

    let args = value.child_by_field_name("arguments")?;

    // Find the first function/arrow argument
    let mut cursor = args.walk();
    for arg in args.children(&mut cursor) {
        match arg.kind() {
            "function" | "function_declaration" | "function_expression" | "arrow_function" => {
                let inner_sig = get_signature(arg, src);
                let body = arg.child_by_field_name("body");
                let renders = jsx::extract_jsx_components(body, src);
                let calls = calls::extract_calls(body, src);
                let (hooks, handlers) = body.map_or_else(Default::default, |b| {
                    hooks::extract_body_hooks_and_handlers(b, src)
                });
                let calls = hooks::filter_hook_calls(calls, &hooks);
                return Some(Symbol {
                    signature: format!("const {name} = {wrapper}({inner_sig})"),
                    line_start: declarator.start_position().row + 1,
                    line_end: declarator.end_position().row + 1,
                    calls,
                    is_component: true,
                    renders,
                    hooks,
                    handlers,
                    decorators: Vec::new(),
                });
            }
            _ => {}
        }
    }

    None
}

// ── Const declarations ──

/// Extract a non-function const declaration.
///
/// Exported consts are always shown. Internal consts are shown when they
/// carry structural information: type annotations, complex values (call
/// expressions, objects, arrays, `new`, `as` casts), or span multiple lines.
fn extract_const(declarator: Node, value: Node, src: &[u8], exported: bool) -> Option<Symbol> {
    let has_type_ann = declarator.child_by_field_name("type").is_some();
    let interesting_value = is_structural_value(value.kind());
    let multiline = value.end_position().row > value.start_position().row + 2;

    if !exported && !has_type_ann && !interesting_value && !multiline {
        return None;
    }

    let name = declarator
        .child_by_field_name("name")
        .map_or("?", |n| txt(n, src));
    let type_ann = declarator
        .child_by_field_name("type")
        .map_or("", |t| txt(t, src));

    let value_hint = if type_ann.is_empty() {
        value_summary(value, src)
    } else {
        String::new()
    };

    let sig = match (type_ann.is_empty(), value_hint.is_empty()) {
        (false, _) => format!("const {name}{type_ann}"),
        (true, false) => format!("const {name} = {value_hint}"),
        (true, true) => format!("const {name}"),
    };

    Some(Symbol {
        signature: sig,
        line_start: declarator.start_position().row + 1,
        line_end: declarator.end_position().row + 1,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: Vec::new(),
    })
}

fn is_structural_value(kind: &str) -> bool {
    matches!(
        kind,
        "call_expression"
            | "object"
            | "array"
            | "new_expression"
            | "as_expression"
            | "satisfies_expression"
            | "template_string"
            | "class"
    )
}

fn value_summary(value: Node, src: &[u8]) -> String {
    match value.kind() {
        "call_expression" => {
            let callee = value.child_by_field_name("function").map_or("?", |f| {
                let t = txt(f, src);
                if t.len() > 30 {
                    &t[..30]
                } else {
                    t
                }
            });
            format!("{callee}(...)")
        }
        "new_expression" => {
            let ctor = value
                .child_by_field_name("constructor")
                .map_or("?", |c| txt(c, src));
            format!("new {ctor}(...)")
        }
        "object" => "{...}".to_string(),
        "array" => "[...]".to_string(),
        "as_expression" | "satisfies_expression" => {
            let t = txt(value, src);
            if t.len() > 60 {
                format!("{}...", &t[..57])
            } else {
                t.to_string()
            }
        }
        _ => String::new(),
    }
}

// ── Namespaces / Modules ──

/// Search children for an `internal_module` or `module` node and extract it.
///
/// Handles the tree-sitter wrapping:
/// - `namespace Foo {}` → `expression_statement` > `internal_module`
/// - `declare module 'x' {}` → `ambient_declaration` > `module`
fn try_extract_namespace_from(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "internal_module" | "module") {
            extract_namespace(child, src, symbols);
            return;
        }
    }
}

/// Extract a `namespace Foo {}` or `declare module 'name' {}`.
///
/// Follows the same pattern as `extract_class`: creates a Symbol for the
/// namespace itself, then extracts exported members from its body.
fn extract_namespace(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    let name = node
        .child_by_field_name("name")
        .map(|n| {
            let t = txt(n, src);
            trim_quotes(t).to_string()
        })
        .unwrap_or_default();

    let keyword = if node.kind() == "module" {
        "module"
    } else {
        "namespace"
    };

    symbols.internals.push(Symbol {
        signature: format!("{keyword} {name}"),
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: Vec::new(),
    });

    let Some(body) = node.child_by_field_name("body") else {
        return;
    };

    let mut cursor = body.walk();
    for child in body.children(&mut cursor) {
        match child.kind() {
            "export_statement" => {
                process_export(child, src, symbols);
            }
            "function_declaration" => {
                if let Some(sym) = extract_function(child, src) {
                    symbols.internals.push(sym);
                }
            }
            "interface_declaration" | "type_alias_declaration" | "enum_declaration" => {
                if let Some(t) = types::extract_type_def(child, src, false) {
                    symbols.types.push(t);
                }
            }
            _ => {}
        }
    }
}

/// Extract only local import and re-export source specifiers (fast path).
///
/// Skips all function/class/type extraction. Used by reverse mode where only
/// the import graph matters, not the full symbol table.
pub fn extract_sources_only(root: Node, src: &[u8]) -> Vec<String> {
    let mut sources = Vec::new();
    let mut cursor = root.walk();

    for node in root.children(&mut cursor) {
        match node.kind() {
            "import_statement" => {
                if let Some(source_node) = node.child_by_field_name("source") {
                    let path = trim_quotes(txt(source_node, src));
                    if (path.starts_with('.') || path.starts_with("@/"))
                        && !sources.iter().any(|s| s == path)
                    {
                        sources.push(path.to_string());
                    }
                }
            }
            "export_statement" => {
                if let Some(source_node) = node.child_by_field_name("source") {
                    let path = trim_quotes(txt(source_node, src));
                    if (path.starts_with('.') || path.starts_with('@'))
                        && !sources.iter().any(|s| s == path)
                    {
                        sources.push(path.to_string());
                    }
                }
            }
            _ => {}
        }
    }

    sources
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::JsxNode;

    fn parse_ts(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    // ── Basic extraction ──

    #[test]
    fn extract_symbols_finds_exported_function() {
        let src = b"export function greet(name: string): string { return name; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.contains("greet"),
            "signature should contain 'greet': {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn extract_symbols_finds_internal_function() {
        let src = b"function helper() { return 42; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.contains("helper"));
    }

    #[test]
    fn extract_symbols_finds_import() {
        let src = b"import { Foo } from './foo';";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.imports, vec!["./foo"]);
    }

    #[test]
    fn extract_symbols_finds_reexport() {
        let src = b"export { Bar } from './bar';";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.reexports.len(), 1);
        assert_eq!(symbols.reexports[0].source, "./bar");
        assert_eq!(symbols.reexports[0].names, vec!["Bar"]);
    }

    // ── Interface extends ──

    #[test]
    fn extract_interface_with_extends() {
        let src = b"export interface Foo extends Bar, Baz { name: string }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        assert_eq!(symbols.types[0].name, "Foo");
        assert_eq!(
            symbols.types[0].extends, "Bar, Baz",
            "should capture extends clause"
        );
    }

    #[test]
    fn extract_interface_without_extends_has_empty_string() {
        let src = b"export interface Config { host: string; port: number }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        assert!(
            symbols.types[0].extends.is_empty(),
            "interface without extends should have empty string"
        );
    }

    // ── Arrows ──

    #[test]
    fn extract_arrow_async_has_prefix() {
        let src = b"export const fetchData = async (url: string) => { return fetch(url); }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.starts_with("async const "),
            "async arrow should have 'async const' prefix: {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn extract_arrow_sync_has_const_prefix() {
        let src = b"export const add = (a: number, b: number) => { return a + b; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.starts_with("const "),
            "sync arrow should have 'const' prefix: {}",
            symbols.exports[0].signature
        );
        assert!(
            !symbols.exports[0].signature.starts_with("async"),
            "sync arrow should NOT have 'async' prefix: {}",
            symbols.exports[0].signature
        );
    }

    // ── Enums ──

    #[test]
    fn extract_enum_members() {
        let src = b"export enum Color { Red, Green, Blue }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.types.len(), 1);
        assert_eq!(symbols.types[0].kind, "enum");
        assert_eq!(symbols.types[0].summary, "{Red, Green, Blue}");
    }

    // ── Calls ──

    #[test]
    fn calls_exclude_noise() {
        let src = b"function work() { console.log('hi'); fetchData(); }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let calls = &symbols.internals[0].calls;
        assert!(calls.contains(&"fetchData".to_string()));
        assert!(!calls.contains(&"console.log".to_string()));
    }

    // ── Const visibility ──

    #[test]
    fn internal_const_with_call_expression_is_visible() {
        let src = b"const schema = z.object({ name: z.string() });";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals.len(),
            1,
            "internal call-expression const should be visible"
        );
        assert!(
            symbols.internals[0].signature.contains("z.object(...)"),
            "signature should hint at z.object(...): {}",
            symbols.internals[0].signature
        );
    }

    #[test]
    fn internal_const_with_type_annotation_is_visible() {
        let src = b"const config: AppConfig = { host: 'localhost', port: 3000 };";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].signature.contains(": AppConfig"),
            "signature should contain type annotation: {}",
            symbols.internals[0].signature
        );
    }

    #[test]
    fn internal_trivial_const_is_hidden() {
        let src = b"const x = 5;";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(
            symbols.internals.is_empty(),
            "trivial single-line const should be hidden"
        );
    }

    #[test]
    fn internal_const_array_is_visible() {
        let src = b"const fields = ['name', 'email', 'phone'];";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].signature.contains("[...]"),
            "array const should show [...] hint: {}",
            symbols.internals[0].signature
        );
    }

    #[test]
    fn internal_const_new_expression_is_visible() {
        let src = b"const cache = new Map();";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].signature.contains("new Map(...)"),
            "new expression should show hint: {}",
            symbols.internals[0].signature
        );
    }

    // ── Test blocks ──

    #[test]
    fn extract_describe_with_nested_it() {
        let src = br"
describe('UserService', () => {
  it('should create user', () => {});
  it('should delete user', () => {});
});
";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests.len(), 1, "should find 1 top-level describe");
        assert_eq!(symbols.tests[0].kind, "describe");
        assert_eq!(symbols.tests[0].name, "UserService");
        assert_eq!(
            symbols.tests[0].children.len(),
            2,
            "should find 2 nested it blocks"
        );
        assert_eq!(symbols.tests[0].children[0].name, "should create user");
        assert_eq!(symbols.tests[0].children[1].name, "should delete user");
    }

    #[test]
    fn extract_nested_describe() {
        let src = br"
describe('outer', () => {
  describe('inner', () => {
    it('test case', () => {});
  });
});
";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests.len(), 1);
        assert_eq!(symbols.tests[0].children.len(), 1);
        assert_eq!(symbols.tests[0].children[0].kind, "describe");
        assert_eq!(symbols.tests[0].children[0].children.len(), 1);
        assert_eq!(symbols.tests[0].children[0].children[0].name, "test case");
    }

    #[test]
    fn extract_test_function() {
        let src = br"test('should work', () => { expect(1).toBe(1); });";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests.len(), 1);
        assert_eq!(symbols.tests[0].kind, "test");
        assert_eq!(symbols.tests[0].name, "should work");
    }

    #[test]
    fn extract_before_each_in_describe() {
        let src = br"
describe('suite', () => {
  beforeEach(() => { setup(); });
  it('case', () => {});
});
";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.tests[0].children.len(), 2);
        assert_eq!(symbols.tests[0].children[0].kind, "beforeEach");
    }

    // ── React hooks ──

    #[test]
    fn extract_use_state_hook() {
        let src = b"const [count, setCount] = useState(0);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1, "should find 1 hook");
        assert_eq!(symbols.hooks[0].kind, "useState");
        assert_eq!(symbols.hooks[0].bindings, vec!["count", "setCount"]);
        assert_eq!(symbols.hooks[0].deps, None);
    }

    #[test]
    fn extract_react_dot_use_state() {
        let src = b"const [isOpen, setIsOpen] = React.useState(false);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useState");
        assert_eq!(symbols.hooks[0].bindings, vec!["isOpen", "setIsOpen"]);
        assert_eq!(symbols.hooks[0].deps, None);
    }

    #[test]
    fn extract_use_memo_hook() {
        let src = b"const memoized = useMemo(() => compute(data), [data]);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useMemo");
        assert_eq!(symbols.hooks[0].bindings, vec!["memoized"]);
        assert_eq!(symbols.hooks[0].deps, Some(vec!["data".to_string()]));
    }

    #[test]
    fn extract_use_ref_hook() {
        let src = b"const inputRef = useRef<HTMLInputElement>(null);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useRef");
        assert_eq!(symbols.hooks[0].bindings, vec!["inputRef"]);
        assert_eq!(symbols.hooks[0].deps, None);
    }

    #[test]
    fn non_hook_call_is_not_extracted_as_hook() {
        let src = b"const result = fetchData();";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(
            symbols.hooks.is_empty(),
            "fetchData is not a hook, should not appear in hooks"
        );
    }

    // ── JSX / React components ──

    fn parse_tsx(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_typescript::LANGUAGE_TSX.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn returns_jsx_detects_component_with_return_statement() {
        let src = b"export function App() { return <div />; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].is_component,
            "function returning JSX should be marked as component"
        );
    }

    #[test]
    fn returns_jsx_false_for_non_jsx_function() {
        let src = b"export function compute(x: number) { return x * 2; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            !symbols.exports[0].is_component,
            "function without JSX return should not be component"
        );
    }

    #[test]
    fn arrow_expression_body_jsx_is_component() {
        let src = b"const Card = () => <div />;";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].is_component,
            "arrow with JSX expression body should be component"
        );
    }

    #[test]
    fn extract_jsx_components_finds_uppercase_tags() {
        let src = b"function App() { return <div><Header /><UserList users={[]} /><footer /></div>; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].is_component);
        assert_eq!(
            symbols.internals[0].renders,
            vec![
                JsxNode { name: "Header".into(), children: vec![] },
                JsxNode { name: "UserList".into(), children: vec![] },
            ],
            "should only include uppercase component tags, not html elements"
        );
    }

    #[test]
    fn memo_unwraps_inner_function() {
        let src = b"export const MemoCard = React.memo(function Card({ title }: Props) { return <h1>{title}</h1>; });";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].is_component,
            "React.memo wrapper should produce a component"
        );
        assert!(
            symbols.exports[0].signature.contains("memo("),
            "signature should show memo wrapper: {}",
            symbols.exports[0].signature
        );
        assert!(
            symbols.exports[0].signature.contains("Card"),
            "signature should include inner function name: {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn forward_ref_unwraps_inner_arrow() {
        let src = b"const Input = React.forwardRef<HTMLInputElement, Props>((props, ref) => { return <input ref={ref} />; });";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].is_component,
            "React.forwardRef wrapper should produce a component"
        );
        assert!(
            symbols.internals[0].signature.contains("forwardRef("),
            "signature should show forwardRef wrapper: {}",
            symbols.internals[0].signature
        );
    }

    #[test]
    fn jsx_fragment_is_component() {
        let src = b"const List = () => <><span /><span /></>;";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(
            symbols.internals[0].is_component,
            "arrow returning JSX fragment should be component"
        );
    }

    #[test]
    fn parenthesized_jsx_return_is_component() {
        let src = b"function App() { return (\n  <div><Header /></div>\n); }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].is_component);
        assert_eq!(
            symbols.internals[0].renders,
            vec![JsxNode { name: "Header".into(), children: vec![] }],
            "should find components in parenthesized JSX return"
        );
    }

    // ── Deps extraction ──

    #[test]
    fn hook_with_empty_deps() {
        let src = b"const memoized = useMemo(() => compute(), []);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].deps, Some(vec![]));
    }

    #[test]
    fn hook_with_multiple_deps() {
        let src = b"const cb = useCallback(() => save(a, b), [a, b]);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(
            symbols.hooks[0].deps,
            Some(vec!["a".to_string(), "b".to_string()])
        );
    }

    #[test]
    fn hook_without_deps_returns_none() {
        let src = b"const inputRef = useRef(null);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].deps, None);
    }

    // ── Component body hooks ──

    #[test]
    fn component_extracts_body_usestate() {
        let src = br"function App() {
  const [count, setCount] = useState(0);
  return <div>{count}</div>;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].is_component);
        assert_eq!(symbols.internals[0].hooks.len(), 1);
        assert_eq!(symbols.internals[0].hooks[0].kind, "useState");
        assert_eq!(
            symbols.internals[0].hooks[0].bindings,
            vec!["count", "setCount"]
        );
    }

    #[test]
    fn component_extracts_bare_useeffect() {
        let src = br"function App() {
  useEffect(() => { console.log('mounted'); }, []);
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals[0].hooks.len(), 1);
        assert_eq!(symbols.internals[0].hooks[0].kind, "useEffect");
        assert!(symbols.internals[0].hooks[0].bindings.is_empty());
        assert_eq!(symbols.internals[0].hooks[0].deps, Some(vec![]));
    }

    #[test]
    fn component_extracts_useeffect_with_deps() {
        let src = br"function App() {
  const [count, setCount] = useState(0);
  useEffect(() => { document.title = String(count); }, [count]);
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals[0].hooks.len(), 2);
        assert_eq!(symbols.internals[0].hooks[1].kind, "useEffect");
        assert_eq!(
            symbols.internals[0].hooks[1].deps,
            Some(vec!["count".to_string()])
        );
    }

    #[test]
    fn component_extracts_multiple_hooks_in_order() {
        let src = br"function Dashboard() {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const theme = useContext(ThemeCtx);
  useEffect(() => { fetch('/api'); }, []);
  const data = useMemo(() => transform(user), [user]);
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let hooks = &symbols.internals[0].hooks;
        assert_eq!(hooks.len(), 5);
        assert_eq!(hooks[0].kind, "useState");
        assert_eq!(hooks[0].bindings, vec!["user", "setUser"]);
        assert_eq!(hooks[1].kind, "useState");
        assert_eq!(hooks[1].bindings, vec!["loading", "setLoading"]);
        assert_eq!(hooks[2].kind, "useContext");
        assert_eq!(hooks[2].bindings, vec!["theme"]);
        assert_eq!(hooks[3].kind, "useEffect");
        assert_eq!(hooks[3].deps, Some(vec![]));
        assert_eq!(hooks[4].kind, "useMemo");
        assert_eq!(hooks[4].bindings, vec!["data"]);
        assert_eq!(hooks[4].deps, Some(vec!["user".to_string()]));
    }

    #[test]
    fn component_hooks_filtered_from_calls() {
        let src = br"function App() {
  const [x, setX] = useState(0);
  fetchData();
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let calls = &symbols.internals[0].calls;
        assert!(
            !calls.iter().any(|c| c.contains("useState")),
            "hooks should not appear in calls: {calls:?}"
        );
        assert!(
            calls.contains(&"fetchData".to_string()),
            "non-hook calls should remain: {calls:?}"
        );
    }

    // ── Component body handlers ──

    #[test]
    fn component_extracts_function_handler() {
        let src = br"function App() {
  function handleClick() { console.log('click'); }
  return <button onClick={handleClick} />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].handlers,
            vec!["handleClick"],
            "should extract function declaration handler"
        );
    }

    #[test]
    fn component_extracts_arrow_handler() {
        let src = br"function App() {
  const handleHover = () => { console.log('hover'); };
  return <div onMouseOver={handleHover} />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].handlers,
            vec!["handleHover"],
            "should extract arrow function handler"
        );
    }

    #[test]
    fn component_skips_nested_callback_functions() {
        let src = br"function App() {
  useEffect(() => {
    function innerHelper() {}
    const innerArrow = () => {};
  }, []);
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(
            symbols.internals[0].handlers.is_empty(),
            "functions inside useEffect callbacks should not be handlers: {:?}",
            symbols.internals[0].handlers
        );
    }

    // ── Edge cases ──

    #[test]
    fn non_component_has_no_body_hooks() {
        let src = b"function compute() { const x = useMemo(() => 1, []); return x; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(
            symbols.internals[0].hooks.is_empty(),
            "non-component functions should not have body hooks extracted"
        );
    }

    #[test]
    fn class_has_empty_hooks_handlers() {
        let src = b"class Foo { bar() { return 1; } }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(!symbols.internals.is_empty());
        assert!(symbols.internals[0].hooks.is_empty());
        assert!(symbols.internals[0].handlers.is_empty());
    }

    #[test]
    fn memo_component_extracts_hooks() {
        let src = br"const Card = React.memo(function Card() {
  const [open, setOpen] = useState(false);
  return <div />;
});";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].is_component);
        assert_eq!(symbols.internals[0].hooks.len(), 1);
        assert_eq!(symbols.internals[0].hooks[0].kind, "useState");
        assert_eq!(
            symbols.internals[0].hooks[0].bindings,
            vec!["open", "setOpen"]
        );
    }

    #[test]
    fn exported_arrow_component_extracts_hooks() {
        let src = br"export const App = () => {
  const [count, setCount] = useState(0);
  const handleClick = () => { setCount(c => c + 1); };
  return <button onClick={handleClick}>{count}</button>;
};";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(symbols.exports[0].is_component);
        assert_eq!(symbols.exports[0].hooks.len(), 1);
        assert_eq!(symbols.exports[0].hooks[0].kind, "useState");
        assert_eq!(symbols.exports[0].handlers, vec!["handleClick"]);
    }

    #[test]
    fn useeffect_member_expression_deps() {
        let src = br"function App() {
  useEffect(() => {}, [props.id, state]);
  return <div />;
}";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].hooks[0].deps,
            Some(vec!["props.id".to_string(), "state".to_string()])
        );
    }

    // ── JSX hierarchy tree ──

    #[test]
    fn jsx_tree_nested_components() {
        let src = b"function App() { return <Card><Avatar /></Card>; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].renders,
            vec![JsxNode {
                name: "Card".into(),
                children: vec![JsxNode { name: "Avatar".into(), children: vec![] }],
            }],
            "nested components should form a tree: Card > Avatar"
        );
    }

    #[test]
    fn jsx_tree_html_passthrough() {
        let src = b"function App() { return <div><span><Badge /></span></div>; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].renders,
            vec![JsxNode { name: "Badge".into(), children: vec![] }],
            "HTML elements should be transparent, promoting children"
        );
    }

    #[test]
    fn jsx_tree_mixed_depth() {
        let src = b"function App() { return <Layout><Header /><Content><List /></Content></Layout>; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].renders,
            vec![JsxNode {
                name: "Layout".into(),
                children: vec![
                    JsxNode {
                        name: "Content".into(),
                        children: vec![JsxNode { name: "List".into(), children: vec![] }],
                    },
                    JsxNode { name: "Header".into(), children: vec![] },
                ],
            }],
            "should preserve hierarchy with sorted children"
        );
    }

    #[test]
    fn jsx_tree_dedup_siblings() {
        let src = b"function App() { return <div><Icon /><Icon /><Badge /></div>; }";
        let tree = parse_tsx(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].renders,
            vec![
                JsxNode { name: "Badge".into(), children: vec![] },
                JsxNode { name: "Icon".into(), children: vec![] },
            ],
            "duplicate siblings should be merged and sorted"
        );
    }

    // ── Decorators ──

    #[test]
    fn class_with_decorator() {
        let src = b"@Component({ selector: 'app' }) class App {}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(!symbols.internals.is_empty());
        assert_eq!(
            symbols.internals[0].decorators,
            vec!["Component"],
            "should extract decorator name from call expression"
        );
    }

    #[test]
    fn member_with_decorator() {
        let src = b"class Foo { @Input() title: string = ''; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        // internals[0] = class Foo, internals[1] = title member
        assert!(
            symbols.internals.len() >= 2,
            "should extract class and member: {:?}",
            symbols.internals.iter().map(|s| &s.signature).collect::<Vec<_>>()
        );
        assert_eq!(
            symbols.internals[1].decorators,
            vec!["Input"],
            "should extract member decorator"
        );
    }

    #[test]
    fn parameterless_decorator() {
        let src = b"@sealed class Immutable {}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].decorators,
            vec!["sealed"],
            "parameterless decorator should extract identifier"
        );
    }

    #[test]
    fn multiple_decorators() {
        let src = b"@A() @B() class Multi {}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(
            symbols.internals[0].decorators,
            vec!["A", "B"],
            "should extract multiple decorators in order"
        );
    }

    #[test]
    fn no_decorators_empty() {
        let src = b"function plain() { return 1; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(
            symbols.internals[0].decorators.is_empty(),
            "function without decorators should have empty vec"
        );
    }

    // ── Anonymous default exports ──

    #[test]
    fn anonymous_default_function() {
        let src = b"export default function() { return 1; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.contains("default"),
            "anonymous default function should have 'default' in sig: {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn anonymous_default_class() {
        let src = b"export default class { foo() { return 1; } }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(!symbols.exports.is_empty());
        assert!(
            symbols.exports[0].signature.contains("default class"),
            "anonymous default class should have 'default class' sig: {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn default_export_object() {
        let src = b"export default { a: 1, b: 2 };";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.contains("default"),
            "default object export should have 'default' sig: {}",
            symbols.exports[0].signature
        );
    }

    #[test]
    fn default_export_identifier() {
        let src = b"const foo = 42;\nexport default foo;";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let default_export = symbols.exports.iter().find(|s| s.signature.contains("default"));
        assert!(
            default_export.is_some(),
            "should extract default identifier export"
        );
        assert!(
            default_export.unwrap().signature.contains("foo"),
            "should reference the identifier name: {}",
            default_export.unwrap().signature
        );
    }

    #[test]
    fn named_default_function_still_works() {
        let src = b"export default function myFunc() { return 1; }";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.exports.len(), 1);
        assert!(
            symbols.exports[0].signature.contains("myFunc"),
            "named default function should keep its name: {}",
            symbols.exports[0].signature
        );
    }

    // ── Namespaces ──

    #[test]
    fn namespace_basic() {
        let src = br"namespace Validation {
  export function validate(s: string) { return s.length > 0; }
}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let ns = symbols.internals.iter().find(|s| s.signature.contains("namespace"));
        assert!(ns.is_some(), "should extract namespace symbol");
        assert!(
            ns.unwrap().signature.contains("Validation"),
            "namespace sig should contain name: {}",
            ns.unwrap().signature
        );

        let validate = symbols.exports.iter().find(|s| s.signature.contains("validate"));
        assert!(
            validate.is_some(),
            "should extract exported function from namespace"
        );
    }

    #[test]
    fn declare_module() {
        let src = br"declare module 'express' {
  interface Request { user: string; }
}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let m = symbols.internals.iter().find(|s| s.signature.contains("module"));
        assert!(m.is_some(), "should extract module symbol");
        assert!(
            m.unwrap().signature.contains("express"),
            "module sig should contain name: {}",
            m.unwrap().signature
        );
    }

    #[test]
    fn exported_namespace() {
        let src = br"export namespace Utils {
  export function helper() { return true; }
}";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let ns = symbols.internals.iter().find(|s| s.signature.contains("namespace"));
        assert!(
            ns.is_some(),
            "exported namespace should be extracted"
        );
    }

    // ── extract_sources_only ──

    #[test]
    fn extract_sources_only_finds_relative_imports() {
        let src = b"import { X } from './foo';\nimport { Y } from './bar';";
        let tree = parse_ts(src);
        let sources = extract_sources_only(tree.root_node(), src);
        assert_eq!(sources, vec!["./foo", "./bar"]);
    }

    #[test]
    fn extract_sources_only_finds_alias_imports() {
        let src = b"import { X } from '@/hooks/use-chat';";
        let tree = parse_ts(src);
        let sources = extract_sources_only(tree.root_node(), src);
        assert_eq!(sources, vec!["@/hooks/use-chat"]);
    }

    #[test]
    fn extract_sources_only_finds_reexport_sources() {
        let src = b"export { X } from './module';";
        let tree = parse_ts(src);
        let sources = extract_sources_only(tree.root_node(), src);
        assert_eq!(sources, vec!["./module"]);
    }

    #[test]
    fn extract_sources_only_skips_external_imports() {
        let src = b"import React from 'react';\nimport { X } from './local';";
        let tree = parse_ts(src);
        let sources = extract_sources_only(tree.root_node(), src);
        assert_eq!(sources, vec!["./local"]);
    }

    #[test]
    fn extract_sources_only_deduplicates() {
        let src = b"import { X } from './foo';\nimport { Y } from './foo';";
        let tree = parse_ts(src);
        let sources = extract_sources_only(tree.root_node(), src);
        assert_eq!(sources, vec!["./foo"]);
    }
}
