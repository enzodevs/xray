use tree_sitter::Node;

use crate::model::Hook;
use crate::util::txt;

/// Try to extract a React hook from a variable declarator.
///
/// Detects patterns like:
/// - `const [state, setState] = useState(init)`
/// - `const [state, setState] = React.useState(init)`
/// - `const memoized = useMemo(() => ..., [deps])`
/// - `const ref = useRef(init)`
pub(super) fn try_extract_hook(declarator: Node, src: &[u8]) -> Option<Hook> {
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

    let deps = extract_deps(value, src);

    Some(Hook {
        kind: hook_name,
        bindings,
        deps,
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

/// Extract the dependency array from a hook call, if present.
///
/// The deps array is the last array argument: `useEffect(fn, [a, b])`.
/// Returns `None` if no array argument exists (different from empty deps `[]`).
pub(super) fn extract_deps(call: Node, src: &[u8]) -> Option<Vec<String>> {
    let args = call.child_by_field_name("arguments")?;
    let mut cursor = args.walk();
    let last_array = args
        .children(&mut cursor)
        .filter(|c| c.kind() == "array")
        .last()?;

    let mut deps = Vec::new();
    let mut inner = last_array.walk();
    for child in last_array.children(&mut inner) {
        match child.kind() {
            "identifier" => deps.push(txt(child, src).to_string()),
            "member_expression" => {
                let t = txt(child, src);
                if t.len() <= 30 {
                    deps.push(t.to_string());
                }
            }
            _ => {}
        }
    }
    Some(deps)
}

/// Extract a bare hook call from an expression statement.
///
/// Handles `useEffect(() => {...}, [dep])` and `useLayoutEffect(...)` which
/// appear as expression statements (no variable binding).
pub(super) fn try_extract_bare_hook(expr_stmt: Node, src: &[u8]) -> Option<Hook> {
    let mut cursor = expr_stmt.walk();
    let call = expr_stmt
        .children(&mut cursor)
        .find(|c| c.kind() == "call_expression")?;
    let hook_name = extract_hook_name(call, src)?;

    Some(Hook {
        kind: hook_name,
        bindings: Vec::new(),
        deps: extract_deps(call, src),
        line_start: expr_stmt.start_position().row + 1,
        line_end: expr_stmt.end_position().row + 1,
    })
}

/// Scan direct children of a function body to extract hooks and local handlers.
///
/// Only scans direct children of the `statement_block` — React's Rules of Hooks
/// guarantee that hooks appear at the top level of a function body.
pub(super) fn extract_body_hooks_and_handlers(body: Node, src: &[u8]) -> (Vec<Hook>, Vec<String>) {
    let mut hooks = Vec::new();
    let mut handlers = Vec::new();

    if body.kind() != "statement_block" {
        return (hooks, handlers);
    }

    let mut cursor = body.walk();
    for child in body.children(&mut cursor) {
        match child.kind() {
            "lexical_declaration" => {
                let mut decl_cursor = child.walk();
                for declarator in child.children(&mut decl_cursor) {
                    if declarator.kind() != "variable_declarator" {
                        continue;
                    }
                    if let Some(hook) = try_extract_hook(declarator, src) {
                        hooks.push(hook);
                        continue;
                    }
                    // Local arrow handler: const handleClick = () => { ... }
                    if let Some(value) = declarator.child_by_field_name("value") {
                        if value.kind() == "arrow_function" {
                            if let Some(name_node) = declarator.child_by_field_name("name") {
                                let name = txt(name_node, src);
                                if !name.is_empty() {
                                    handlers.push(name.to_string());
                                }
                            }
                        }
                    }
                }
            }
            "function_declaration" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = txt(name_node, src);
                    if !name.is_empty() {
                        handlers.push(name.to_string());
                    }
                }
            }
            "expression_statement" => {
                if let Some(hook) = try_extract_bare_hook(child, src) {
                    hooks.push(hook);
                }
            }
            _ => {}
        }
    }

    (hooks, handlers)
}

/// Remove hook call names from the calls list to avoid duplication.
pub(super) fn filter_hook_calls(calls: Vec<String>, hooks: &[Hook]) -> Vec<String> {
    if hooks.is_empty() {
        return calls;
    }
    let hook_names: Vec<&str> = hooks.iter().map(|h| h.kind.as_str()).collect();
    calls
        .into_iter()
        .filter(|c| {
            let base = c.strip_prefix("React.").unwrap_or(c);
            !hook_names.contains(&base)
        })
        .collect()
}
