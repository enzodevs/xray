use tree_sitter::Node;

use crate::model::TypeDef;
use crate::util::{compress_members, txt};

pub(super) fn extract_type_def(node: Node, src: &[u8], exported: bool) -> Option<TypeDef> {
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
