/// Errors produced by xray during file processing.
#[derive(Debug, thiserror::Error)]
pub enum XrayError {
    #[error("{path}: {source}")]
    Io {
        path: String,
        source: std::io::Error,
    },

    #[error("unsupported extension: .{0}")]
    UnsupportedExtension(String),

    #[error("parse failed: {0}")]
    ParseFailed(String),

    #[error("{feature} is not supported for {language} files yet")]
    UnsupportedFeature {
        feature: &'static str,
        language: &'static str,
    },

    #[error("lsp: {0}")]
    Lsp(String),
}
