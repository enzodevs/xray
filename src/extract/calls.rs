use tree_sitter::Node;

use crate::util::{is_noise, txt};

pub(super) fn extract_calls(body: Option<Node>, src: &[u8]) -> Vec<String> {
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
