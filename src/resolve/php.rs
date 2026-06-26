use std::path::{Path, PathBuf};

use super::shared;

const PHP_EXTENSIONS: &[&str] = &["php", "inc"];

pub fn resolve_php_import(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let from_dir = from_file.parent()?;
    let specifier = specifier.trim();
    if specifier.is_empty() {
        return None;
    }

    if specifier.starts_with("./")
        || specifier.starts_with("../")
        || specifier.starts_with('/')
        || specifier.contains('.')
    {
        return resolve_file_specifier(specifier, from_dir);
    }

    resolve_namespace_specifier(specifier, from_dir)
}

fn resolve_file_specifier(specifier: &str, from_dir: &Path) -> Option<PathBuf> {
    let base = if let Some(rest) = specifier.strip_prefix('/') {
        PathBuf::from("/").join(rest)
    } else {
        from_dir.join(specifier)
    };
    shared::try_extensions_with(&base, PHP_EXTENSIONS)
}

fn resolve_namespace_specifier(specifier: &str, from_dir: &Path) -> Option<PathBuf> {
    let module = specifier.trim_start_matches('\\').replace('\\', "/");
    if module.is_empty() {
        return None;
    }

    for root in search_roots(from_dir) {
        if let Some(resolved) = shared::try_extensions_with(&root.join(&module), PHP_EXTENSIONS) {
            return Some(resolved);
        }
        if let Some(resolved) =
            shared::try_extensions_with(&root.join("src").join(&module), PHP_EXTENSIONS)
        {
            return Some(resolved);
        }
        if let Some(resolved) =
            shared::try_extensions_with(&root.join("app").join(&module), PHP_EXTENSIONS)
        {
            return Some(resolved);
        }
    }

    None
}

fn search_roots(from_dir: &Path) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(git_root) = crate::util::git_root(from_dir) {
        roots.push(git_root);
    }
    roots.push(from_dir.to_path_buf());
    roots
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_relative_php_file() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("bootstrap.php"), "<?php").unwrap();
        let from = dir.path().join("index.php");

        let resolved = resolve_php_import("./bootstrap.php", &from).unwrap();
        assert_eq!(resolved, dir.path().join("bootstrap.php"));
    }

    #[test]
    fn resolve_relative_without_extension() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("helpers.php"), "<?php").unwrap();
        let from = dir.path().join("index.php");

        let resolved = resolve_php_import("./helpers", &from).unwrap();
        assert_eq!(resolved, dir.path().join("helpers.php"));
    }

    #[test]
    fn resolve_namespace_from_app_root() {
        let dir = tempfile::tempdir().unwrap();
        let target_dir = dir.path().join("app/App/Services");
        std::fs::create_dir_all(&target_dir).unwrap();
        std::fs::write(target_dir.join("UserService.php"), "<?php").unwrap();
        let from = dir.path().join("index.php");

        let resolved = resolve_php_import("App\\Services\\UserService", &from).unwrap();
        assert_eq!(resolved, target_dir.join("UserService.php"));
    }
}
