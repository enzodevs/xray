use tree_sitter::Node;

use crate::model::TestBlock;
use crate::util::{trim_quotes, txt};

/// Extract a test block from an expression statement.
///
/// Recognizes `describe(name, fn)`, `it(name, fn)`, `test(name, fn)`,
/// `beforeEach(fn)`, `afterEach(fn)`, `beforeAll(fn)`, `afterAll(fn)`.
pub(super) fn extract_test_block(expr_stmt: Node, src: &[u8]) -> Option<TestBlock> {
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
