//! Preserve structural CLI modes on platforms without the Unix LSP transport.
use std::path::{Path, PathBuf};

use crate::{error::XrayError, lang::LanguageKind};

pub struct LspClient;

impl LspClient {
    pub fn start(_root: &Path, _language: LanguageKind) -> Result<Self, XrayError> {
        Err(XrayError::Lsp("LSP transport requires Unix".into()))
    }

    pub fn definition(
        &mut self,
        _file: &Path,
        _line: usize,
        _column: usize,
    ) -> Option<(PathBuf, usize)> {
        None
    }

    pub fn warm_up(&mut self, _file: &Path, _imports: &[PathBuf]) {}

    pub fn shutdown(self) {}
}
