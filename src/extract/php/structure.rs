use tree_sitter::Node;

use super::{dependencies::Context, txt};
use crate::model::{FileSymbols, TestBlock};

mod routes;

pub(super) fn collect(root: Node, src: &[u8], symbols: &mut FileSymbols) {
    super::dependencies::visit_scopes(root, src, |nodes, context| {
        collect_tests(nodes, src, &mut symbols.tests);
        routes::collect(nodes, src, context, &mut symbols.types);
    });
}

fn collect_tests(nodes: &[Node], src: &[u8], tests: &mut Vec<TestBlock>) {
    for &node in nodes {
        if node.kind() == "expression_statement" {
            if let Some(expr) = node.named_child(0) {
                if let Some(test) = test_block(expr, src) {
                    tests.push(test);
                }
            }
        } else if !is_callable(node) {
            let mut cursor = node.walk();
            let children: Vec<_> = node.named_children(&mut cursor).collect();
            collect_tests(&children, src, tests);
        }
    }
}

fn test_block(expr: Node, src: &[u8]) -> Option<TestBlock> {
    let mut call = expr;
    while call.kind() == "member_call_expression" {
        call = call.child_by_field_name("object")?;
    }
    if call.kind() != "function_call_expression" {
        return None;
    }
    let function = call.child_by_field_name("function")?;
    let kind = txt(function, src).trim_start_matches('\\');
    if !matches!(
        kind,
        "it" | "test" | "describe" | "beforeEach" | "afterEach" | "beforeAll" | "afterAll"
    ) {
        return None;
    }
    let args = arguments(call);
    let name = args
        .first()
        .and_then(|&arg| literal(arg, src))
        .unwrap_or_default();
    let mut children = Vec::new();
    if kind == "describe" {
        for arg in args {
            if is_closure(arg) {
                if let Some(body) = arg.child_by_field_name("body") {
                    if body.kind() == "compound_statement" {
                        let mut cursor = body.walk();
                        let nodes: Vec<_> = body.named_children(&mut cursor).collect();
                        collect_tests(&nodes, src, &mut children);
                    } else if let Some(test) = test_block(body, src) {
                        children.push(test);
                    }
                }
            }
        }
    }
    Some(TestBlock {
        kind: kind.to_string(),
        name,
        line_start: expr.start_position().row + 1,
        // Includes fluent suffixes such as ->with([...]), without expanding datasets.
        line_end: expr.end_position().row + 1,
        children,
    })
}

pub(super) fn is_closure(node: Node) -> bool {
    matches!(node.kind(), "anonymous_function" | "arrow_function")
}

fn is_callable(node: Node) -> bool {
    is_closure(node)
        || matches!(
            node.kind(),
            "function_definition"
                | "method_declaration"
                | "class_declaration"
                | "trait_declaration"
                | "interface_declaration"
                | "enum_declaration"
        )
}

fn arguments(call: Node) -> Vec<Node> {
    let Some(args) = call.child_by_field_name("arguments") else {
        return Vec::new();
    };
    let mut cursor = args.walk();
    args.named_children(&mut cursor)
        .filter(|node| node.kind() == "argument")
        .filter_map(|arg| {
            // Named/variadic arguments are deliberately not guessed by position.
            if arg.child_by_field_name("name").is_some() {
                Some(arg)
            } else {
                arg.named_child(0)
            }
        })
        .collect()
}

pub(super) fn literal(node: Node, src: &[u8]) -> Option<String> {
    if !matches!(node.kind(), "string" | "encapsed_string") {
        return None;
    }
    let mut cursor = node.walk();
    if node
        .named_children(&mut cursor)
        .any(|child| !matches!(child.kind(), "string_content" | "escape_sequence"))
    {
        return None;
    }
    let raw = txt(node, src);
    let value = super::trim_quotes(raw);
    // Keep complex escapes source-faithful rather than interpreting PHP at extraction time.
    Some(if raw.starts_with('\'') {
        value.replace("\\'", "'").replace("\\\\", "\\")
    } else {
        value.to_string()
    })
}
