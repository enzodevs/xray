use std::path::{Path, PathBuf};
use std::process::Command;

use tree_sitter::Node;

/// Extract UTF-8 text from a tree-sitter node, returning `""` on failure.
pub fn txt<'a>(node: Node, src: &'a [u8]) -> &'a str {
    node.utf8_text(src).unwrap_or("")
}

/// Strip surrounding quotes (`'`, `"`, `` ` ``) from a string literal.
pub fn trim_quotes(s: &str) -> &str {
    s.trim_matches(|c: char| c == '\'' || c == '"' || c == '`')
}

/// Returns `true` for common runtime calls that add noise to the digest.
pub fn is_noise(name: &str) -> bool {
    matches!(
        name,
        "console.log"
            | "console.error"
            | "console.warn"
            | "console.info"
            | "JSON.parse"
            | "JSON.stringify"
            | "Object.keys"
            | "Object.values"
            | "Object.entries"
            | "Object.assign"
            | "Array.isArray"
            | "Array.from"
            | "Promise.all"
            | "Promise.resolve"
            | "Promise.reject"
            | "parseInt"
            | "parseFloat"
            | "String"
            | "Number"
            | "Boolean"
            | "Error"
            | "TypeError"
            | "Date"
            | "Map"
            | "Set"
            | "RegExp"
    )
}

/// Compress child nodes into a `{field1, field2, ...}` summary.
///
/// Collects the `"name"` field of every child whose `kind()` is in `kinds`.
/// If a matching child has no `"name"` field, the child's own text is used
/// (handles tree-sitter-typescript enum bodies where members are bare
/// `property_identifier` nodes).
/// Shows up to 5 names; extras are collapsed as `...+N`.
pub fn compress_members(node: Node, src: &[u8], kinds: &[&str]) -> String {
    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if kinds.contains(&child.kind()) {
            let text = child
                .child_by_field_name("name")
                .map_or_else(|| txt(child, src), |n| txt(n, src));
            if !text.is_empty() {
                names.push(text.to_string());
            }
        }
    }
    if names.is_empty() {
        return String::new();
    }
    if names.len() <= 5 {
        format!("{{{}}}", names.join(", "))
    } else {
        let shown = &names[..5];
        format!("{{{}, ...+{}}}", shown.join(", "), names.len() - 5)
    }
}

/// Resolve a file path to a git-relative path, falling back to the input.
pub fn relative_path(path: &Path) -> String {
    let abs = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    let dir = abs.parent().unwrap_or(Path::new("."));

    if let Some(root) = git_root(dir) {
        if let Ok(rel) = abs.strip_prefix(&root) {
            return rel.to_string_lossy().into_owned();
        }
    }

    path.to_string_lossy().into_owned()
}

/// Find the git repository root for a directory.
fn git_root(dir: &Path) -> Option<PathBuf> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(dir)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let root = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Some(PathBuf::from(root))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_noise_catches_console_log() {
        assert!(is_noise("console.log"));
        assert!(is_noise("JSON.stringify"));
    }

    #[test]
    fn is_noise_passes_user_functions() {
        assert!(!is_noise("fetchUser"));
        assert!(!is_noise("db.query"));
    }

    #[test]
    fn compress_members_returns_empty_for_no_matches() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
            .unwrap();
        let src = b"interface Foo {}";
        let tree = parser.parse(src, None).unwrap();
        let root = tree.root_node();

        // The interface body has no property_signature children
        let iface = root.child(0).unwrap();
        let body = iface.child_by_field_name("body").unwrap();
        let result = compress_members(body, src, &["property_signature"]);
        assert!(result.is_empty());
    }

    #[test]
    fn compress_members_formats_fields() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
            .unwrap();
        let src = b"interface Foo { a: string; b: number; c: boolean }";
        let tree = parser.parse(src, None).unwrap();
        let root = tree.root_node();

        let iface = root.child(0).unwrap();
        let body = iface.child_by_field_name("body").unwrap();
        let result = compress_members(body, src, &["property_signature", "method_signature"]);
        assert_eq!(result, "{a, b, c}");
    }
}
