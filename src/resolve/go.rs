use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn resolve_go_import(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let from_dir = from_file.parent()?;
    let specifier = specifier.trim().trim_matches('"').trim_matches('`');
    if specifier.is_empty() {
        return None;
    }

    if specifier.starts_with("./") || specifier.starts_with("../") {
        return resolve_package_path(&from_dir.join(specifier));
    }
    if specifier.starts_with('/') {
        return resolve_package_path(Path::new(specifier));
    }

    let (module_root, module_path) = find_go_module(from_file)?;
    if specifier == module_path {
        return resolve_package_path(&module_root);
    }
    let rest = specifier.strip_prefix(&format!("{module_path}/"))?;
    resolve_package_path(&module_root.join(rest))
}

fn find_go_module(from_file: &Path) -> Option<(PathBuf, String)> {
    let git_root = from_file.parent().and_then(crate::util::git_root);

    for ancestor in from_file.ancestors().skip(1) {
        let go_mod = ancestor.join("go.mod");
        if go_mod.is_file() {
            let module = read_module_path(&go_mod)?;
            return Some((ancestor.to_path_buf(), module));
        }
        if git_root.as_deref().is_some_and(|root| ancestor == root) {
            break;
        }
    }

    None
}

fn read_module_path(go_mod: &Path) -> Option<String> {
    let content = fs::read_to_string(go_mod).ok()?;
    content.lines().find_map(|line| {
        let line = line.trim();
        if line.starts_with("//") {
            return None;
        }
        line.strip_prefix("module ")
            .and_then(|rest| rest.split_whitespace().next())
            .map(str::to_string)
    })
}

fn resolve_package_path(path: &Path) -> Option<PathBuf> {
    if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("go") {
        return Some(path.to_path_buf());
    }

    let with_go_ext = path.with_extension("go");
    if with_go_ext.is_file() {
        return Some(with_go_ext);
    }

    if !path.is_dir() {
        return None;
    }

    let mut files = fs::read_dir(path)
        .ok()?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("go"))
        .collect::<Vec<_>>();
    files.sort();

    files
        .iter()
        .find(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| !name.ends_with("_test.go"))
        })
        .cloned()
        .or_else(|| files.into_iter().next())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_module_import_to_package_file() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("go.mod"), "module example.com/app\n").unwrap();
        let pkg = dir.path().join("internal/db");
        fs::create_dir_all(&pkg).unwrap();
        let target = pkg.join("db.go");
        fs::write(&target, "package db").unwrap();
        let from = dir.path().join("main.go");
        fs::write(&from, "package main").unwrap();

        let resolved = resolve_go_import("example.com/app/internal/db", &from);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_relative_go_package() {
        let dir = tempfile::tempdir().unwrap();
        let child = dir.path().join("child");
        fs::create_dir(&child).unwrap();
        let target = child.join("child.go");
        fs::write(&target, "package child").unwrap();
        let from = dir.path().join("main.go");
        fs::write(&from, "package main").unwrap();

        let resolved = resolve_go_import("./child", &from);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn external_import_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("go.mod"), "module example.com/app\n").unwrap();
        let from = dir.path().join("main.go");
        fs::write(&from, "package main").unwrap();

        let resolved = resolve_go_import("github.com/acme/pkg", &from);
        assert!(resolved.is_none());
    }
}
