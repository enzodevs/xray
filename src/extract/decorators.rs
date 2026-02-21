use tree_sitter::Node;

use crate::util::txt;

/// Extract decorator names from a node's `decorator` field children.
///
/// Returns names without `@` prefix (e.g., `"Component"`, `"Input"`).
/// Works on `class_declaration`, `public_field_definition`, etc.
/// Returns empty vec if the node has no decorators.
pub(super) fn extract_decorators(node: Node, src: &[u8]) -> Vec<String> {
    let mut names = Vec::new();
    let mut cursor = node.walk();
    for decorator in node.children_by_field_name("decorator", &mut cursor) {
        if let Some(name) = decorator_name(decorator, src) {
            names.push(name);
        }
    }
    names
}

/// Extract the name from a single `decorator` node.
///
/// For `call_expression` (e.g., `@Component({})`), extracts the callee name.
/// For all other expressions, uses the full text.
fn decorator_name(decorator: Node, src: &[u8]) -> Option<String> {
    let expr = decorator.named_child(0)?;
    match expr.kind() {
        "call_expression" => {
            let callee = expr.child_by_field_name("function")?;
            Some(txt(callee, src).to_string())
        }
        _ => Some(txt(expr, src).to_string()),
    }
}
