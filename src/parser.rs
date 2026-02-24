use std::path::Path;

use tree_sitter::{Language, Parser, Tree};

use crate::error::XrayError;
use crate::lang::LanguageKind;

/// Parsed source file with the detected language ecosystem.
pub struct ParsedFile {
    pub language_kind: LanguageKind,
    pub tree: Tree,
    pub source: String,
}

/// Detect the tree-sitter language from a file extension.
pub fn detect_language(ext: &str) -> Result<Language, XrayError> {
    let kind = LanguageKind::from_extension(ext)?;
    Ok(kind.tree_sitter_language_for_extension(ext))
}

/// Read and parse a source file, returning parsed tree, source, and ecosystem.
pub fn parse_file(path: &Path) -> Result<ParsedFile, XrayError> {
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let source = std::fs::read_to_string(path).map_err(|e| XrayError::Io {
        path: path.display().to_string(),
        source: e,
    })?;

    let language_kind = LanguageKind::from_extension(ext)?;
    let language = detect_language(ext)?;

    let mut parser = Parser::new();
    parser
        .set_language(&language)
        .map_err(|e| XrayError::ParseFailed(e.to_string()))?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| XrayError::ParseFailed(path.display().to_string()))?;

    Ok(ParsedFile {
        language_kind,
        tree,
        source,
    })
}
