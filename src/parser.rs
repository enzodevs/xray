use std::path::Path;
use std::time::Instant;

use markdown::{Constructs, ParseOptions};
use tree_sitter::{Language, Parser, Tree};

use crate::error::XrayError;
use crate::lang::LanguageKind;

/// Parsed backend-specific representation for a source file.
pub enum ParsedContent {
    Code(Tree),
    Markdown(markdown::mdast::Node),
}

/// Parsed source file with the detected language ecosystem.
pub struct ParsedFile {
    pub language_kind: LanguageKind,
    pub content: ParsedContent,
    pub source: String,
}

/// Detect the tree-sitter language from a file extension.
pub fn detect_language(ext: &str) -> Result<Language, XrayError> {
    let kind = LanguageKind::from_extension(ext)?;
    kind.tree_sitter_language_for_extension(ext)
}

/// Read and parse a source file, returning parsed tree, source, and ecosystem.
pub fn parse_file(path: &Path) -> Result<ParsedFile, XrayError> {
    let source = std::fs::read_to_string(path).map_err(|e| XrayError::Io {
        path: path.display().to_string(),
        source: e,
    })?;

    parse_source(path, source, None)
}

/// Parse already bounded/read source without reopening a potentially changing file.
pub(crate) fn parse_source(
    path: &Path,
    source: String,
    deadline: Option<Instant>,
) -> Result<ParsedFile, XrayError> {
    let expired = || deadline.is_some_and(|deadline| Instant::now() >= deadline);
    if expired() {
        return Err(XrayError::ParseFailed("parse deadline exceeded".into()));
    }
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let language_kind = LanguageKind::from_extension(ext)?;
    let content = match language_kind {
        LanguageKind::Md => ParsedContent::Markdown(
            markdown::to_mdast(&source, &markdown_parse_options())
                .map_err(|e| XrayError::ParseFailed(e.to_string()))?,
        ),
        LanguageKind::Ts
        | LanguageKind::Sql
        | LanguageKind::Py
        | LanguageKind::Go
        | LanguageKind::Php
        | LanguageKind::Rs
        | LanguageKind::Svelte
        | LanguageKind::Vue => {
            let language = detect_language(ext)?;
            let mut parser = Parser::new();
            parser
                .set_language(&language)
                .map_err(|e| XrayError::ParseFailed(e.to_string()))?;

            let tree = if deadline.is_some() {
                let mut progress = |_: &tree_sitter::ParseState| {
                    if expired() {
                        std::ops::ControlFlow::Break(())
                    } else {
                        std::ops::ControlFlow::Continue(())
                    }
                };
                let options = tree_sitter::ParseOptions::new().progress_callback(&mut progress);
                parser.parse_with_options(
                    &mut |offset, _| &source.as_bytes()[offset..],
                    None,
                    Some(options),
                )
            } else {
                parser.parse(&source, None)
            }
            .ok_or_else(|| {
                XrayError::ParseFailed(if expired() {
                    "parse deadline exceeded".into()
                } else {
                    path.display().to_string()
                })
            })?;
            ParsedContent::Code(tree)
        }
    };

    if expired() {
        return Err(XrayError::ParseFailed("parse deadline exceeded".into()));
    }
    Ok(ParsedFile {
        language_kind,
        content,
        source,
    })
}

impl ParsedFile {
    /// Recovery is useful for navigation, but must never look like a clean parse.
    pub fn has_syntax_errors(&self) -> bool {
        matches!(&self.content, ParsedContent::Code(tree) if tree.root_node().has_error())
    }

    pub fn extract_content(&self) -> crate::model::FileContent {
        match &self.content {
            ParsedContent::Code(tree) => crate::model::FileContent::Code(
                self.language_kind
                    .extract_symbols(tree.root_node(), self.source.as_bytes()),
            ),
            ParsedContent::Markdown(root) => crate::model::FileContent::Markdown(
                crate::extract::extract_markdown_document(root, &self.source),
            ),
        }
    }

    pub fn diagnostic_specifiers(&self) -> Vec<String> {
        match &self.content {
            ParsedContent::Code(tree) => self
                .language_kind
                .extract_diagnostic_specifiers_from_ast(tree.root_node(), self.source.as_bytes()),
            ParsedContent::Markdown(root) => crate::extract::extract_markdown_sources_only(root),
        }
    }
}

pub(crate) fn markdown_parse_options() -> ParseOptions {
    ParseOptions {
        constructs: Constructs {
            frontmatter: true,
            ..Constructs::gfm()
        },
        ..ParseOptions::gfm()
    }
}
