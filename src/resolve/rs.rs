use std::path::{Path, PathBuf};

/// Resolve a Rust module specifier to a local file.
///
/// Supported:
/// - `mod` declarations: `foo` → `foo.rs` or `foo/mod.rs`
/// - Crate-relative: `crate::path::mod` → `<src>/path/mod.rs`
/// - Super-relative: `super::mod` → `<parent>/mod.rs`
/// - Self-relative: `self::mod` → `<dir>/mod.rs`
///
/// Not supported:
/// - External crates (returns None)
/// - Workspace-level resolution
pub(crate) fn resolve_rs_import(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let from_dir = from_file.parent()?;

    if specifier.starts_with("crate::") {
        return resolve_crate_path(specifier, from_file);
    }

    if specifier.starts_with("super::") {
        return resolve_super_path(specifier, from_dir);
    }

    if let Some(rest) = specifier.strip_prefix("self::") {
        return try_module_file(from_dir, rest);
    }

    // Bare module name (from `mod foo;` declarations)
    if !specifier.contains("::") {
        return try_module_file(from_dir, specifier);
    }

    // External crate path
    None
}

fn resolve_crate_path(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let rest = specifier.strip_prefix("crate::")?;
    let src_dir = find_crate_src(from_file)?;
    try_module_path(&src_dir, rest)
}

fn resolve_super_path(specifier: &str, from_dir: &Path) -> Option<PathBuf> {
    let mut base = from_dir.to_path_buf();
    let mut rest = specifier;

    while let Some(after) = rest.strip_prefix("super::") {
        if !base.pop() {
            return None;
        }
        rest = after;
    }

    if rest == "super" {
        base.pop();
        // Resolve to the parent module itself
        return try_module_file(&base, base.file_name()?.to_string_lossy().as_ref());
    }

    try_module_path(&base, rest)
}

fn try_module_file(dir: &Path, name: &str) -> Option<PathBuf> {
    if name.is_empty() {
        return None;
    }

    // Try name.rs
    let file = dir.join(format!("{name}.rs"));
    if file.is_file() {
        return Some(file);
    }

    // Try name/mod.rs
    let mod_file = dir.join(name).join("mod.rs");
    if mod_file.is_file() {
        return Some(mod_file);
    }

    None
}

fn try_module_path(base: &Path, module: &str) -> Option<PathBuf> {
    if module.is_empty() {
        return None;
    }

    // Convert :: to /
    let segments: Vec<&str> = module.split("::").collect();
    if segments.is_empty() {
        return None;
    }

    let mut dir = base.to_path_buf();
    // Navigate to the parent path
    for seg in &segments[..segments.len() - 1] {
        dir = dir.join(seg);
    }

    let last = segments[segments.len() - 1];
    try_module_file(&dir, last)
}

fn find_crate_src(from_file: &Path) -> Option<PathBuf> {
    let git_root = crate::util::git_root(from_file.parent()?);

    for ancestor in from_file.ancestors().skip(1) {
        let cargo_toml = ancestor.join("Cargo.toml");
        if cargo_toml.is_file() {
            // Standard Rust project layout: src/ under the Cargo.toml directory
            let src_dir = ancestor.join("src");
            if src_dir.is_dir() {
                return Some(src_dir);
            }
            // If no src/ dir, use the Cargo.toml directory itself
            return Some(ancestor.to_path_buf());
        }
        if git_root.as_deref().is_some_and(|root| ancestor == root) {
            break;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn resolve_mod_declaration_to_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("lib.rs");
        let target = dir.path().join("utils.rs");
        fs::write(&entry, "mod utils;").unwrap();
        fs::write(&target, "pub fn helper() {}").unwrap();

        let resolved = resolve_rs_import("utils", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_mod_declaration_to_mod_rs() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("lib.rs");
        let sub = dir.path().join("utils");
        fs::create_dir(&sub).unwrap();
        let target = sub.join("mod.rs");
        fs::write(&entry, "mod utils;").unwrap();
        fs::write(&target, "pub fn helper() {}").unwrap();

        let resolved = resolve_rs_import("utils", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_super_path() {
        let dir = tempfile::tempdir().unwrap();
        let parent = dir.path().join("parent");
        let child = parent.join("child");
        fs::create_dir_all(&child).unwrap();
        let entry = child.join("mod.rs");
        let target = parent.join("sibling.rs");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_rs_import("super::sibling", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_external_crate_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.rs");
        fs::write(&entry, "").unwrap();

        let resolved = resolve_rs_import("serde::Deserialize", &entry);
        assert!(resolved.is_none());
    }

    #[test]
    fn resolve_self_path() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("mod.rs");
        let target = dir.path().join("helper.rs");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_rs_import("self::helper", &entry);
        assert_eq!(resolved, Some(target));
    }
}
