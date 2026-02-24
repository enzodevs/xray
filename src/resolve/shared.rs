use std::path::{Path, PathBuf};

pub(super) fn try_extensions_with(base: &Path, exts: &[&str]) -> Option<PathBuf> {
    // If the path already has a known extension, check it directly first.
    if let Some(ext) = base.extension().and_then(|e| e.to_str()) {
        if exts.contains(&ext) && base.is_file() {
            return Some(base.to_path_buf());
        }
    }

    for ext in exts {
        let candidate = base.with_extension(ext);
        if candidate.is_file() {
            return Some(candidate);
        }
    }

    for ext in exts {
        let candidate = base.join("index").with_extension(ext);
        if candidate.is_file() {
            return Some(candidate);
        }
    }

    None
}
