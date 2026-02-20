use std::path::Path;

use tree_sitter::{Language, Parser, Tree};

use crate::error::XrayError;

/// Detect the tree-sitter language from a file extension.
pub fn detect_language(ext: &str) -> Result<Language, XrayError> {
    match ext {
        "tsx" | "jsx" => Ok(tree_sitter_typescript::LANGUAGE_TSX.into()),
        "ts" | "mts" | "js" | "mjs" | "cjs" => {
            Ok(tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
        }
        _ => Err(XrayError::UnsupportedExtension(ext.to_string())),
    }
}

/// Read and parse a source file, returning the tree-sitter tree and source text.
pub fn parse_file(path: &Path) -> Result<(Tree, String), XrayError> {
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

    let source = std::fs::read_to_string(path).map_err(|e| XrayError::Io {
        path: path.display().to_string(),
        source: e,
    })?;

    let language = detect_language(ext)?;

    let mut parser = Parser::new();
    parser
        .set_language(&language)
        .map_err(|e| XrayError::ParseFailed(e.to_string()))?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| XrayError::ParseFailed(path.display().to_string()))?;

    Ok((tree, source))
}
