use std::path::{Path, PathBuf};

const PY_EXT: &str = "py";

/// Resolve a Python import specifier to a local file.
///
/// Supported:
/// - Relative modules: `.mod`, `..pkg.mod`
/// - Absolute project modules: `pkg.mod`, `pkg`
///
/// Not supported in v1:
/// - Site-packages / virtualenv resolution
/// - Runtime/dynamic imports
pub(crate) fn resolve_py_import(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let from_dir = from_file.parent()?;

    if specifier.starts_with('.') {
        return resolve_relative_import(specifier, from_dir);
    }

    resolve_absolute_import(specifier, from_dir)
}

fn resolve_relative_import(specifier: &str, from_dir: &Path) -> Option<PathBuf> {
    let dots = specifier.bytes().take_while(|b| *b == b'.').count();
    if dots == 0 {
        return None;
    }

    let mut base = from_dir.to_path_buf();
    for _ in 1..dots {
        if !base.pop() {
            return None;
        }
    }

    let module = specifier[dots..].trim_start_matches('.');
    try_module_path(&base, module)
}

fn resolve_absolute_import(specifier: &str, from_dir: &Path) -> Option<PathBuf> {
    let module = specifier.trim();
    if module.is_empty() {
        return None;
    }

    for root in search_roots(from_dir) {
        if let Some(path) = try_module_path(&root, module) {
            return Some(path);
        }
    }

    None
}

fn search_roots(from_dir: &Path) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = Vec::new();
    let git_root = crate::util::git_root(from_dir);

    for ancestor in from_dir.ancestors() {
        let candidate = ancestor.to_path_buf();
        if !roots.iter().any(|r| r == &candidate) {
            roots.push(candidate);
        }
        if git_root.as_deref().is_some_and(|root| ancestor == root) {
            break;
        }
    }

    roots.reverse();
    roots
}

fn try_module_path(base: &Path, module: &str) -> Option<PathBuf> {
    let module_path = module.replace('.', "/");
    if module_path.is_empty() {
        let init = base.join("__init__.py");
        return init.is_file().then_some(init);
    }

    let raw = base.join(&module_path);

    if raw.extension().and_then(|e| e.to_str()) == Some(PY_EXT) && raw.is_file() {
        return Some(raw);
    }

    let file_candidate = raw.with_extension(PY_EXT);
    if file_candidate.is_file() {
        return Some(file_candidate);
    }

    let package_candidate = raw.join("__init__.py");
    if package_candidate.is_file() {
        return Some(package_candidate);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn resolve_relative_module_from_current_package() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        fs::create_dir(&pkg).unwrap();
        let entry = pkg.join("main.py");
        let target = pkg.join("utils.py");
        fs::write(pkg.join("__init__.py"), "").unwrap();
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_py_import(".utils", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_relative_parent_module() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        let sub = pkg.join("sub");
        fs::create_dir_all(&sub).unwrap();
        let entry = sub.join("main.py");
        let target = pkg.join("common.py");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_py_import("..common", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_relative_empty_module_to_package_init() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        fs::create_dir(&pkg).unwrap();
        let entry = pkg.join("main.py");
        let init = pkg.join("__init__.py");
        fs::write(&entry, "").unwrap();
        fs::write(&init, "").unwrap();

        let resolved = resolve_py_import(".", &entry);
        assert_eq!(resolved, Some(init));
    }

    #[test]
    fn resolve_absolute_module_from_project_root() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        fs::create_dir(&pkg).unwrap();
        let entry = pkg.join("main.py");
        let target = pkg.join("helpers.py");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = resolve_py_import("pkg.helpers", &entry);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn resolve_absolute_package_init() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        fs::create_dir(&pkg).unwrap();
        let entry = pkg.join("main.py");
        let init = pkg.join("__init__.py");
        fs::write(&entry, "").unwrap();
        fs::write(&init, "").unwrap();

        let resolved = resolve_py_import("pkg", &entry);
        assert_eq!(resolved, Some(init));
    }

    #[test]
    fn resolve_external_module_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.py");
        fs::write(&entry, "").unwrap();

        let resolved = resolve_py_import("requests", &entry);
        assert!(resolved.is_none());
    }
}
