use std::path::{Path, PathBuf};

use super::shared;

const PHP_EXTENSIONS: &[&str] = &["php", "inc"];

/// Composer configuration nearest to a source file; never executes Composer/PHP.
#[derive(Default)]
pub(crate) struct PhpPaths {
    mappings: Vec<(String, Vec<PathBuf>)>,
    root: Option<PathBuf>,
}

impl PhpPaths {
    pub(crate) fn load(from_dir: &Path) -> Self {
        for dir in from_dir.ancestors() {
            let file = dir.join("composer.json");
            if file.is_file() {
                return Self::from_manifest(&file).unwrap_or_default();
            }
            if dir.join(".git").exists() {
                break;
            }
        }
        Self::default()
    }

    fn from_manifest(file: &Path) -> Option<Self> {
        let value: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(file).ok()?).ok()?;
        let root = file.parent()?;
        let mut mappings = Vec::new();
        for section in ["autoload", "autoload-dev"] {
            let Some(psr4) = value
                .get(section)
                .and_then(|s| s.get("psr-4"))
                .and_then(serde_json::Value::as_object)
            else {
                continue;
            };
            for (prefix, paths) in psr4 {
                let paths: Vec<_> = match paths {
                    serde_json::Value::String(path) => vec![root.join(path)],
                    serde_json::Value::Array(paths) => paths
                        .iter()
                        .filter_map(serde_json::Value::as_str)
                        .map(|path| root.join(path))
                        .collect(),
                    _ => continue,
                };
                mappings.push((prefix.clone(), paths));
            }
        }
        mappings.sort_by_key(|(prefix, _)| std::cmp::Reverse(prefix.len()));
        Some(Self {
            mappings,
            root: Some(root.to_path_buf()),
        })
    }

    pub(crate) fn matches(&self, specifier: &str) -> bool {
        let name = specifier.trim_start_matches('\\');
        self.mappings
            .iter()
            .any(|(prefix, _)| name.starts_with(prefix))
    }

    /// Static Laravel/Inertia Vue convention, not a JavaScript resolver evaluator.
    /// Both Vite default directory spellings are allowed; duplicate matches are unresolved.
    /// Custom page roots, transforms and non-Vue extensions are not supported.
    fn resolve_inertia_page(&self, page: &str) -> Option<PathBuf> {
        if page.is_empty()
            || page.contains(['\\', ':'])
            || page.chars().any(char::is_control)
            || page.split('/').any(|part| matches!(part, "" | "." | ".."))
        {
            return None;
        }
        let root = self.root.as_ref()?.canonicalize().ok()?;
        let mut resolved = None;
        for directory in ["resources/js/pages", "resources/js/Pages"] {
            let Ok(page_root) = root.join(directory).canonicalize() else {
                continue;
            };
            // Neither a symlinked root nor a nested symlink may escape its boundary.
            if !page_root.starts_with(&root) {
                continue;
            }
            let candidate = page_root.join(format!("{page}.vue"));
            if !candidate.is_file() {
                continue;
            }
            let candidate = candidate.canonicalize().ok()?;
            if !candidate.starts_with(&page_root) || resolved.is_some() {
                return None;
            }
            resolved = Some(candidate);
        }
        resolved
    }

    fn resolve(&self, specifier: &str) -> Option<PathBuf> {
        let name = specifier.trim_start_matches('\\');
        for (prefix, paths) in &self.mappings {
            let Some(suffix) = name.strip_prefix(prefix) else {
                continue;
            };
            for path in paths {
                let candidate = path.join(suffix.replace('\\', "/")).with_extension("php");
                if candidate.is_file() {
                    return Some(candidate);
                }
            }
        }
        None
    }
}

pub fn resolve_php_import(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let paths = PhpPaths::load(from_file.parent()?);
    resolve_php_import_with_paths(specifier, from_file, &paths)
}

pub(crate) fn resolve_php_import_with_paths(
    specifier: &str,
    from_file: &Path,
    paths: &PhpPaths,
) -> Option<PathBuf> {
    let from_dir = from_file.parent()?;
    if let Some(page) = specifier.strip_prefix("inertia:") {
        return paths.resolve_inertia_page(page);
    }
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

    if paths.matches(specifier) {
        return paths.resolve(specifier);
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
    if let Some(root) = from_dir.ancestors().find(|dir| dir.join(".git").exists()) {
        roots.push(root.to_path_buf());
    }
    roots.push(from_dir.to_path_buf());
    roots
}

#[cfg(test)]
mod tests {
    use super::*;

    fn inertia_project() -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("composer.json"), "{}").unwrap();
        std::fs::create_dir_all(dir.path().join("app/Http")).unwrap();
        dir
    }

    fn page(dir: &Path, name: &str) -> PathBuf {
        let path = dir.join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, "<template>Page</template>").unwrap();
        path.canonicalize().unwrap()
    }

    #[test]
    fn inertia_pages_resolve_exact_case_in_either_conventional_root() {
        for directory in ["pages", "Pages"] {
            let dir = inertia_project();
            let expected = page(
                dir.path(),
                &format!("resources/js/{directory}/school/Home.vue"),
            );
            let from = dir.path().join("app/Http/Controller.php");
            assert_eq!(
                resolve_php_import("inertia:school/Home", &from),
                Some(expected)
            );
            for missing in [
                "inertia:school/home",
                "inertia:Missing",
                "inertia:school/Home.vue",
                "inertia:school/Home ",
            ] {
                assert!(resolve_php_import(missing, &from).is_none(), "{missing}");
            }
        }
    }

    #[test]
    fn inertia_ambiguous_roots_and_custom_roots_are_unresolved() {
        let dir = inertia_project();
        page(dir.path(), "resources/js/pages/Home.vue");
        page(dir.path(), "resources/js/Pages/Home.vue");
        page(dir.path(), "resources/js/custom/Custom.vue");
        let from = dir.path().join("app/Http/Controller.php");
        assert!(resolve_php_import("inertia:Home", &from).is_none());
        assert!(resolve_php_import("inertia:Custom", &from).is_none());
    }

    #[test]
    fn inertia_does_not_escape_page_root_or_search_parent_composer_projects() {
        let dir = inertia_project();
        page(dir.path(), "resources/js/pages/Home.vue");
        page(dir.path(), "resources/js/Secret.vue");
        let from = dir.path().join("app/Http/Controller.php");
        for invalid in [
            "",
            "../Secret",
            "./Home",
            "school/../../Secret",
            "/Home",
            "C:/Home",
            "school\\Home",
            "school//Home",
            "Home/",
            "Home\n",
        ] {
            assert!(
                resolve_php_import(&format!("inertia:{invalid}"), &from).is_none(),
                "{invalid:?}"
            );
        }
        let nested = dir.path().join("packages/nested");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(nested.join("composer.json"), "{}").unwrap();
        assert!(resolve_php_import("inertia:Home", &nested.join("Controller.php")).is_none());
    }

    #[cfg(unix)]
    #[test]
    fn inertia_rejects_symlink_escape() {
        let dir = inertia_project();
        let outside = tempfile::tempdir().unwrap();
        let secret = page(outside.path(), "Secret.vue");
        let root = dir.path().join("resources/js/pages");
        std::fs::create_dir_all(&root).unwrap();
        std::os::unix::fs::symlink(&secret, root.join("Leak.vue")).unwrap();
        std::os::unix::fs::symlink(outside.path(), root.join("nested")).unwrap();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("resources/js/Pages")).unwrap();
        let from = dir.path().join("app/Http/Controller.php");
        for invalid in ["inertia:Leak", "inertia:nested/Secret", "inertia:Secret"] {
            assert!(resolve_php_import(invalid, &from).is_none(), "{invalid}");
        }
    }

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

    #[test]
    fn composer_psr4_resolves_laravel_dev_and_longest_prefix_arrays() {
        let dir = tempfile::tempdir().unwrap();
        for path in ["app/Http", "app/Services", "tests", "special"] {
            std::fs::create_dir_all(dir.path().join(path)).unwrap();
        }
        std::fs::write(dir.path().join("composer.json"), r#"{"autoload":{"psr-4":{"App\\":"app/","App\\Services\\":["missing/","special/"]}},"autoload-dev":{"psr-4":{"Tests\\":"tests/"}}}"#).unwrap();
        for path in [
            "app/Services/Worker.php",
            "special/Worker.php",
            "app/School.php",
            "tests/Example.php",
        ] {
            std::fs::write(dir.path().join(path), "<?php").unwrap();
        }
        let from = dir.path().join("app/Http/Controller.php");
        assert_eq!(
            resolve_php_import("App\\School", &from),
            Some(dir.path().join("app/School.php"))
        );
        assert_eq!(
            resolve_php_import("\\App\\Services\\Worker", &from),
            Some(dir.path().join("special/Worker.php"))
        );
        assert_eq!(
            resolve_php_import("Tests\\Example", &from),
            Some(dir.path().join("tests/Example.php"))
        );
        assert!(resolve_php_import("App\\Missing", &from).is_none());
        assert!(!PhpPaths::load(from.parent().unwrap()).matches("Application\\School"));
    }

    #[test]
    fn nearest_composer_manifest_isolates_nested_packages() {
        let dir = tempfile::tempdir().unwrap();
        let nested = dir.path().join("packages/one");
        std::fs::create_dir_all(nested.join("lib")).unwrap();
        std::fs::write(
            dir.path().join("composer.json"),
            r#"{"autoload":{"psr-4":{"App\\":"wrong/"}}}"#,
        )
        .unwrap();
        std::fs::write(
            nested.join("composer.json"),
            r#"{"autoload":{"psr-4":{"App\\":"lib/"}}}"#,
        )
        .unwrap();
        std::fs::write(nested.join("lib/Item.php"), "<?php").unwrap();
        assert_eq!(
            resolve_php_import("App\\Item", &nested.join("lib/Consumer.php")),
            Some(nested.join("lib/Item.php"))
        );
    }
}
