use tree_sitter::Node;

use crate::util::{is_noise, txt};

pub(super) fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
    let mut calls = Vec::new();
    if let Some(body) = body {
        walk_calls(body, src, &mut calls);
    }
    dedup_preserving_order(calls)
}

fn walk_calls(node: Node, src: &[u8], calls: &mut Vec<String>) {
    if node.kind() == "call_expression" {
        let function = node.child_by_field_name("function");
        if let Some(func) = function {
            // Chained calls evaluate their inner callee first:
            // `implement(route).handler(cb)` is `implement`, then `handler`.
            walk_calls(func, src, calls);

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

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if function != Some(child) {
                walk_calls(child, src, calls);
            }
        }
        return;
    }

    // Inline callbacks are part of the enclosing call's behavior. Locally
    // declared functions are separate trace targets and must not leak calls
    // into their parent symbol.
    if is_function(node) && !is_inline_callback(node) {
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        walk_calls(child, src, calls);
    }
}

fn is_function(node: Node) -> bool {
    matches!(
        node.kind(),
        "arrow_function" | "function" | "function_declaration" | "function_expression"
    )
}

fn is_inline_callback(node: Node) -> bool {
    node.parent()
        .is_some_and(|parent| parent.kind() == "arguments")
}

fn dedup_preserving_order(calls: Vec<String>) -> Vec<String> {
    let mut unique = Vec::with_capacity(calls.len());
    for call in calls {
        if !unique.contains(&call) {
            unique.push(call);
        }
    }
    unique
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

    fn extract(src: &[u8]) -> Vec<String> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        extract_calls(Some(tree.root_node()), src)
    }

    #[test]
    fn includes_calls_inside_inline_callbacks() {
        let calls = extract(
            b"implement(route).handler(async () => {
                currentProfile();
                service.recommendForProfessor();
            });",
        );

        assert_eq!(
            calls,
            [
                "implement",
                "\u{2026}.handler",
                "currentProfile",
                "service.recommendForProfessor"
            ]
        );
    }

    #[test]
    fn skips_calls_inside_locally_declared_functions() {
        let calls = extract(
            b"outer();
              function local() { hidden(); }
              const handler = () => concealed();",
        );

        assert_eq!(calls, ["outer"]);
    }

    #[test]
    fn retains_all_calls_in_source_order() {
        let src = b"a(); b(); c(); d(); e(); f(); g(); h(); i(); j(); k(); l(); a();";
        let calls = extract(src);

        assert_eq!(
            calls,
            ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"]
        );
    }
}
