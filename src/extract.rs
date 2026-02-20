use tree_sitter::Node;

use crate::model::{FileSymbols, Hook, ReExport, Symbol, TestBlock, TypeDef};
use crate::util::{compress_members, is_noise, trim_quotes, txt};

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
                if let Some(t) = extract_type_def(node, src, false) {
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
                if let Some(tb) = extract_test_block(node, src) {
                    symbols.tests.push(tb);
                }
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
    let is_component = returns_jsx(body);
    let calls = extract_calls(body, src);
    let renders = if is_component {
        extract_jsx_components(body, src)
    } else {
        Vec::new()
    };

    Some(Symbol {
        signature: sig,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        calls,
        is_component,
        renders,
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
    let is_component = returns_jsx(body);
    let calls = extract_calls(body, src);
    let renders = if is_component {
        extract_jsx_components(body, src)
    } else {
        Vec::new()
    };

    Some(Symbol {
        signature: sig,
        line_start: declarator.start_position().row + 1,
        line_end: declarator.end_position().row + 1,
        calls,
        is_component,
        renders,
    })
}

// ── Exports ──

fn process_export(node: Node, src: &[u8], symbols: &mut FileSymbols) {
    if node.child_by_field_name("source").is_some() {
        extract_reexport(node, src, &mut symbols.reexports);
        return;
    }

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
                if let Some(t) = extract_type_def(child, src, true) {
                    symbols.types.push(t);
                }
            }
            "lexical_declaration" => {
                process_lexical(child, src, true, &mut symbols.exports, None);
            }
            _ => {}
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
    let class_start = node.start_position().row + 1;
    let class_end = node.end_position().row + 1;

    symbols.push(Symbol {
        signature: format!("class {}", class_sig.trim_start_matches("class ")),
        line_start: class_start,
        line_end: class_end,
        calls: Vec::new(),
        is_component: false,
        renders: Vec::new(),
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
    mut hooks: Option<&mut Vec<Hook>>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != "variable_declarator" {
            continue;
        }

        // Check for React hooks (array destructuring + hook call)
        if let Some(ref mut hooks) = hooks {
            if let Some(hook) = try_extract_hook(child, src) {
                hooks.push(hook);
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

// ── Type definitions ──

fn extract_type_def(node: Node, src: &[u8], exported: bool) -> Option<TypeDef> {
    let name = node
        .child_by_field_name("name")
        .map(|n| txt(n, src).to_string())
        .unwrap_or_default();

    if name.is_empty() {
        return None;
    }

    let kind = match node.kind() {
        "interface_declaration" => "interface",
        "type_alias_declaration" => "type",
        "enum_declaration" => "enum",
        _ => return None,
    };

    let extends = extract_extends_clause(node, src);

    let summary = match kind {
        "interface" => node
            .child_by_field_name("body")
            .map(|b| compress_members(b, src, &["property_signature", "method_signature"]))
            .unwrap_or_default(),
        "type" => node
            .child_by_field_name("value")
            .map(|v| {
                let t = txt(v, src);
                if t.len() > 80 {
                    format!("{}...", &t[..77])
                } else {
                    t.to_string()
                }
            })
            .unwrap_or_default(),
        "enum" => node
            .child_by_field_name("body")
            .map(|b| {
                compress_members(
                    b,
                    src,
                    &["enum_member", "enum_assignment", "property_identifier"],
                )
            })
            .unwrap_or_default(),
        _ => String::new(),
    };

    Some(TypeDef {
        name,
        kind: kind.to_string(),
        extends,
        summary,
        line_start: node.start_position().row + 1,
        line_end: node.end_position().row + 1,
        exported,
    })
}

/// Extract the `extends` clause from an interface or class declaration.
///
/// For `interface Foo extends Bar, Baz`, returns `"Bar, Baz"`.
fn extract_extends_clause(node: Node, src: &[u8]) -> String {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "extends_type_clause" || child.kind() == "extends_clause" {
            // Collect type names from the clause (skip the "extends" keyword and commas)
            let mut types = Vec::new();
            let mut inner = child.walk();
            for tc in child.children(&mut inner) {
                if tc.kind() != "extends" && tc.kind() != "," {
                    let t = txt(tc, src);
                    if !t.is_empty() {
                        types.push(t.to_string());
                    }
                }
            }
            if !types.is_empty() {
                return types.join(", ");
            }
        }
    }
    String::new()
}

// ── JSX / React component detection ──

/// Returns `true` if the body node contains a JSX return.
///
/// Checks for:
/// - `return <jsx>` inside a statement block
/// - Expression body that is directly a JSX node (arrow without braces)
fn returns_jsx(body: Option<Node>) -> bool {
    let Some(body) = body else { return false };

    // Arrow expression body (no braces): `() => <div />`
    if is_jsx_node(body.kind()) {
        return true;
    }

    // Statement block: look for `return <jsx>`
    if body.kind() == "statement_block" {
        return has_jsx_return(body);
    }

    // Parenthesized expression: `() => (<div />)`
    if body.kind() == "parenthesized_expression" {
        let mut cursor = body.walk();
        for child in body.children(&mut cursor) {
            if is_jsx_node(child.kind()) {
                return true;
            }
        }
    }

    false
}

fn is_jsx_node(kind: &str) -> bool {
    matches!(
        kind,
        "jsx_element" | "jsx_self_closing_element" | "jsx_fragment"
    )
}

/// Recursively search for a `return_statement` whose child is JSX.
fn has_jsx_return(node: Node) -> bool {
    if node.kind() == "return_statement" {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if is_jsx_node(child.kind()) {
                return true;
            }
            // Handle parenthesized returns: `return (<div />)`
            if child.kind() == "parenthesized_expression" {
                let mut inner = child.walk();
                for grandchild in child.children(&mut inner) {
                    if is_jsx_node(grandchild.kind()) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    // Don't descend into nested functions
    if matches!(
        node.kind(),
        "arrow_function" | "function" | "function_declaration"
    ) {
        return false;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if has_jsx_return(child) {
            return true;
        }
    }
    false
}

/// Extract component names rendered in JSX (uppercase tags only).
///
/// Returns a sorted, deduplicated list of component names found in the tree.
/// HTML elements (`div`, `span`, etc.) are excluded by the uppercase filter.
fn extract_jsx_components(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let mut names = Vec::new();
    if let Some(body) = body {
        walk_jsx_components(body, src, &mut names);
    }
    names.sort();
    names.dedup();
    names.truncate(10);
    names
}

fn walk_jsx_components(node: Node, src: &[u8], names: &mut Vec<String>) {
    match node.kind() {
        "jsx_opening_element" | "jsx_self_closing_element" => {
            // The tag name is the "name" field, or the first identifier/member_expression child
            let tag_name = node
                .child_by_field_name("name")
                .map_or("", |n| txt(n, src));
            if tag_name
                .chars()
                .next()
                .is_some_and(char::is_uppercase)
            {
                names.push(tag_name.to_string());
            }
        }
        // Don't descend into nested function definitions
        "arrow_function" | "function" | "function_declaration" => return,
        _ => {}
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        walk_jsx_components(child, src, names);
    }
}

// ── React.memo / React.forwardRef unwrapping ──

/// Try to unwrap `React.memo(function ...)` or `React.forwardRef((p, r) => ...)`.
///
/// Returns a `Symbol` with the inner function's signature and `is_component: true`.
fn try_unwrap_react_wrapper(
    declarator: Node,
    value: Node,
    src: &[u8],
) -> Option<Symbol> {
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
                let renders = extract_jsx_components(body, src);
                let calls = extract_calls(body, src);
                return Some(Symbol {
                    signature: format!("const {name} = {wrapper}({inner_sig})"),
                    line_start: declarator.start_position().row + 1,
                    line_end: declarator.end_position().row + 1,
                    calls,
                    is_component: true,
                    renders,
                });
            }
            _ => {}
        }
    }

    None
}

// ── Test blocks (describe/it/test) ──

/// Extract a test block from an expression statement.
///
/// Recognizes `describe(name, fn)`, `it(name, fn)`, `test(name, fn)`,
/// `beforeEach(fn)`, `afterEach(fn)`, `beforeAll(fn)`, `afterAll(fn)`.
fn extract_test_block(expr_stmt: Node, src: &[u8]) -> Option<TestBlock> {
    let call = find_child_by_kind(expr_stmt, "call_expression")?;
    extract_test_call(call, src)
}

fn extract_test_call(call: Node, src: &[u8]) -> Option<TestBlock> {
    let func = call.child_by_field_name("function")?;

    let func_name = match func.kind() {
        "identifier" => txt(func, src).to_string(),
        "member_expression" => {
            // Handle describe.each, it.each, test.skip etc.
            func.child_by_field_name("object")
                .map(|o| txt(o, src).to_string())
                .unwrap_or_default()
        }
        _ => return None,
    };

    if !is_test_function(&func_name) {
        return None;
    }

    let args = call.child_by_field_name("arguments")?;

    // Extract the test name (first string argument)
    let name = extract_first_string_arg(args, src);

    // For describe blocks, recurse into the callback body to find nested tests
    let children = if func_name == "describe" {
        extract_nested_tests(args, src)
    } else {
        Vec::new()
    };

    Some(TestBlock {
        kind: func_name,
        name,
        line_start: call.start_position().row + 1,
        line_end: call.end_position().row + 1,
        children,
    })
}

fn is_test_function(name: &str) -> bool {
    matches!(
        name,
        "describe" | "it" | "test" | "beforeEach" | "afterEach" | "beforeAll" | "afterAll"
    )
}

/// Extract the first string literal from an arguments node.
fn extract_first_string_arg(args: Node, src: &[u8]) -> String {
    let mut cursor = args.walk();
    for child in args.children(&mut cursor) {
        if child.kind() == "string" || child.kind() == "template_string" {
            return trim_quotes(txt(child, src)).to_string();
        }
    }
    String::new()
}

/// Recurse into a describe callback to find nested describe/it/test calls.
fn extract_nested_tests(args: Node, src: &[u8]) -> Vec<TestBlock> {
    let mut children = Vec::new();

    let mut cursor = args.walk();
    for child in args.children(&mut cursor) {
        if child.kind() == "arrow_function" || child.kind() == "function" {
            if let Some(body) = child.child_by_field_name("body") {
                let mut body_cursor = body.walk();
                for stmt in body.children(&mut body_cursor) {
                    if stmt.kind() == "expression_statement" {
                        if let Some(call) = find_child_by_kind(stmt, "call_expression") {
                            if let Some(tb) = extract_test_call(call, src) {
                                children.push(tb);
                            }
                        }
                    }
                }
            }
        }
    }

    children
}

fn find_child_by_kind<'a>(node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    let result = node.children(&mut cursor).find(|c| c.kind() == kind);
    result
}

// ── React hooks ──

/// Try to extract a React hook from a variable declarator.
///
/// Detects patterns like:
/// - `const [state, setState] = useState(init)`
/// - `const [state, setState] = React.useState(init)`
/// - `const memoized = useMemo(() => ..., [deps])`
/// - `const ref = useRef(init)`
fn try_extract_hook(declarator: Node, src: &[u8]) -> Option<Hook> {
    let value = declarator.child_by_field_name("value")?;

    if value.kind() != "call_expression" {
        return None;
    }

    let hook_name = extract_hook_name(value, src)?;
    let name_node = declarator.child_by_field_name("name")?;

    let bindings = match name_node.kind() {
        // const [state, setState] = useState(...)
        "array_pattern" => {
            let mut names = Vec::new();
            let mut cursor = name_node.walk();
            for child in name_node.children(&mut cursor) {
                if child.kind() == "identifier" {
                    names.push(txt(child, src).to_string());
                }
            }
            names
        }
        // const memoized = useMemo(...)
        "identifier" => vec![txt(name_node, src).to_string()],
        _ => Vec::new(),
    };

    Some(Hook {
        kind: hook_name,
        bindings,
        line_start: declarator.start_position().row + 1,
        line_end: declarator.end_position().row + 1,
    })
}

/// Extract the hook name from a call expression, returning `None` if not a hook.
///
/// Matches `useState(...)`, `React.useState(...)`, `useEffect(...)`, etc.
fn extract_hook_name(call: Node, src: &[u8]) -> Option<String> {
    let func = call.child_by_field_name("function")?;

    let name = match func.kind() {
        "identifier" => txt(func, src).to_string(),
        "member_expression" => {
            // React.useState → "useState"
            func.child_by_field_name("property")
                .map(|p| txt(p, src).to_string())
                .unwrap_or_default()
        }
        _ => return None,
    };

    if name.starts_with("use") && name.len() > 3 {
        Some(name)
    } else {
        None
    }
}

// ── Call extraction (body hints) ──

fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let mut calls = Vec::new();
    if let Some(body) = body {
        walk_calls(body, src, &mut calls);
    }
    calls.sort();
    calls.dedup();
    calls.truncate(10);
    calls
}

fn walk_calls(node: Node, src: &[u8], calls: &mut Vec<String>) {
    if node.kind() == "call_expression" {
        if let Some(func) = node.child_by_field_name("function") {
            let name = match func.kind() {
                "identifier" => {
                    let n = txt(func, src);
                    if is_noise(n) {
                        String::new()
                    } else {
                        n.to_string()
                    }
                }
                "member_expression" => extract_member_call(func, src),
                _ => String::new(),
            };
            if !name.is_empty() {
                calls.push(name);
            }
        }
    }

    // Don't descend into nested function definitions
    if matches!(
        node.kind(),
        "arrow_function" | "function" | "function_declaration"
    ) {
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        walk_calls(child, src, calls);
    }
}

fn extract_member_call(func: Node, src: &[u8]) -> String {
    let Some(prop) = func.child_by_field_name("property") else {
        return String::new();
    };

    let obj = func.child_by_field_name("object").map_or_else(
        || "?".to_string(),
        |o| match o.kind() {
            "identifier" | "this" => txt(o, src).to_string(),
            "member_expression" => {
                let t = txt(o, src);
                if t.len() > 30 {
                    "\u{2026}".to_string()
                } else {
                    t.to_string()
                }
            }
            _ => "\u{2026}".to_string(),
        },
    );

    let full = format!("{}.{}", obj, txt(prop, src));
    if full.len() > 40 || is_noise(&full) {
        String::new()
    } else {
        full
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }

    #[test]
    fn extract_react_dot_use_state() {
        let src = b"const [isOpen, setIsOpen] = React.useState(false);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useState");
        assert_eq!(symbols.hooks[0].bindings, vec!["isOpen", "setIsOpen"]);
    }

    #[test]
    fn extract_use_memo_hook() {
        let src = b"const memoized = useMemo(() => compute(data), [data]);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useMemo");
        assert_eq!(symbols.hooks[0].bindings, vec!["memoized"]);
    }

    #[test]
    fn extract_use_ref_hook() {
        let src = b"const inputRef = useRef<HTMLInputElement>(null);";
        let tree = parse_ts(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.hooks.len(), 1);
        assert_eq!(symbols.hooks[0].kind, "useRef");
        assert_eq!(symbols.hooks[0].bindings, vec!["inputRef"]);
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
            vec!["Header", "UserList"],
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
            vec!["Header"],
            "should find components in parenthesized JSX return"
        );
    }
}
