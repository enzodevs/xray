use std::path::{Path, PathBuf};

use super::shared;

const MARKDOWN_EXTENSIONS: &[&str] = &["md"];

/// Resolve a Markdown link target to a supported local file.
///
/// Supported:
/// - Relative and bare local document paths
/// - Fragment/query stripping (`guide.md#intro`, `guide.md?x=1`)
/// - Explicit links to already-supported file extensions
///
/// Not supported in v1:
/// - External URLs
/// - Root-relative site paths (`/docs/...`)
/// - Anchors-only links (`#intro`)
pub(crate) fn resolve_markdown_link(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let cleaned = clean_markdown_target(specifier)?;
    if cleaned.starts_with('/') {
        return None;
    }

    let parent = from_file.parent()?;
    let candidate = parent.join(cleaned);
    try_supported_target(&candidate)
}

fn clean_markdown_target(specifier: &str) -> Option<&str> {
    let trimmed = specifier.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return None;
    }

    let lower = trimmed.to_ascii_lowercase();
    if lower.starts_with("http://")
        || lower.starts_with("https://")
        || lower.starts_with("mailto:")
        || lower.starts_with("tel:")
        || lower.starts_with("//")
    {
        return None;
    }

    let cutoff = trimmed.find(['#', '?']).unwrap_or(trimmed.len());
    let cleaned = trimmed[..cutoff].trim();
    if cleaned.is_empty() {
        None
    } else {
        Some(cleaned)
    }
}

fn try_supported_target(candidate: &Path) -> Option<PathBuf> {
    if let Some(ext) = candidate.extension().and_then(|e| e.to_str()) {
        if crate::lang::is_supported_extension(ext) && candidate.is_file() {
            return Some(candidate.to_path_buf());
        }
    }

    shared::try_extensions_with(candidate, MARKDOWN_EXTENSIONS)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn resolve_markdown_link_finds_relative_markdown_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("README.md");
        let target = dir.path().join("docs").join("guide.md");
        fs::create_dir(dir.path().join("docs")).unwrap();
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_markdown_link("docs/guide", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_markdown_link_strips_fragment() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("README.md");
        let target = dir.path().join("guide.md");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_markdown_link("./guide.md#intro", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_markdown_link_rejects_external_urls() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("README.md");
        fs::write(&entry, "").unwrap();

        let resolved = resolve_markdown_link("https://example.com/docs", &entry);
        assert!(resolved.is_none());
    }

    #[test]
    fn resolve_markdown_link_allows_explicit_supported_code_targets() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("README.md");
        let target = dir.path().join("src").join("main.ts");
        fs::create_dir(dir.path().join("src")).unwrap();
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_markdown_link("src/main.ts", &entry);
        assert_eq!(resolved, Some(target));
    }
}
