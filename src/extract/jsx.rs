use tree_sitter::Node;

use crate::model::JsxNode;
use crate::util::txt;

/// Returns `true` if the body node contains a JSX return.
///
/// Checks for:
/// - `return <jsx>` inside a statement block
/// - Expression body that is directly a JSX node (arrow without braces)
pub(super) fn returns_jsx(body: Option<Node>) -> bool {
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

/// Extract the JSX component hierarchy from a function body.
///
/// Returns a tree of `JsxNode` where uppercase JSX tags become nodes and
/// lowercase (HTML) elements are transparent — their children are promoted
/// to the parent level.
pub(super) fn extract_jsx_components(body: Option<Node>, src: &[u8]) -> Vec<JsxNode> {
    let Some(body) = body else {
        return Vec::new();
    };
    let mut nodes = collect_jsx_tree(body, src);
    dedup_jsx_nodes(&mut nodes);
    truncate_siblings(&mut nodes, 8);
    nodes
}

/// Recursive AST walk that builds the JSX hierarchy.
fn collect_jsx_tree(node: Node, src: &[u8]) -> Vec<JsxNode> {
    match node.kind() {
        "jsx_element" => collect_jsx_element(node, src),
        "jsx_self_closing_element" => collect_self_closing(node, src),
        // Stop at nested function boundaries
        "arrow_function" | "function" | "function_declaration" => Vec::new(),
        // Fragments, HTML nodes, and everything else: recurse into children
        _ => collect_children(node, src),
    }
}

/// Collect from a `jsx_element` (has opening + closing tags + body).
fn collect_jsx_element(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let tag_name = node
        .child_by_field_name("open_tag")
        .and_then(|open| open.child_by_field_name("name"))
        .map(|n| txt(n, src));

    let is_component = tag_name
        .and_then(|t| t.chars().next())
        .is_some_and(char::is_uppercase);

    let children = collect_children(node, src);

    if is_component {
        vec![JsxNode {
            name: tag_name.unwrap_or_default().to_string(),
            children,
        }]
    } else {
        // HTML element: pass-through, promote children
        children
    }
}

/// Collect from a `jsx_self_closing_element` (leaf, e.g. `<Foo />`).
fn collect_self_closing(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let tag_name = node.child_by_field_name("name").map(|n| txt(n, src));

    let is_component = tag_name
        .and_then(|t| t.chars().next())
        .is_some_and(char::is_uppercase);

    if is_component {
        vec![JsxNode {
            name: tag_name.unwrap_or_default().to_string(),
            children: Vec::new(),
        }]
    } else {
        Vec::new()
    }
}

/// Recurse into all children of `node` and flatten results.
fn collect_children(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let mut result = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        result.extend(collect_jsx_tree(child, src));
    }
    result
}

/// Sort, deduplicate (merging children), and recurse at each level.
fn dedup_jsx_nodes(nodes: &mut Vec<JsxNode>) {
    nodes.sort_by(|a, b| a.name.cmp(&b.name));

    // Merge nodes with the same name: combine children
    let mut i = 0;
    while i + 1 < nodes.len() {
        if nodes[i].name == nodes[i + 1].name {
            let removed = nodes.remove(i + 1);
            nodes[i].children.extend(removed.children);
        } else {
            i += 1;
        }
    }

    // Recurse into children
    for node in nodes.iter_mut() {
        dedup_jsx_nodes(&mut node.children);
    }
}

/// Limit sibling count at each level (recursively).
fn truncate_siblings(nodes: &mut Vec<JsxNode>, max: usize) {
    nodes.truncate(max);
    for node in nodes.iter_mut() {
        truncate_siblings(&mut node.children, max);
    }
}
