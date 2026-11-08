use std::path::{Path, PathBuf};

use crate::model::ReExport;

use super::{shared, PathConfig};

const EXTENSIONS: &[&str] = &[
    "ts", "tsx", "js", "jsx", "mts", "mjs", "cjs", "svelte", "vue",
];

/// Merge import specifiers and re-export sources into a deduplicated list.
pub(crate) fn collect_sources(imports: &[String], reexports: &[ReExport]) -> Vec<String> {
    let mut seen = Vec::new();
    for spec in imports {
        if !seen.contains(spec) {
            seen.push(spec.clone());
        }
    }
    for re in reexports {
        if !is_local_module_specifier(&re.source) {
            continue;
        }
        if !seen.contains(&re.source) {
            seen.push(re.source.clone());
        }
    }
    seen
}

/// Resolve an import specifier to a file path on disk.
///
/// Returns `None` for external (`node_modules`) imports or unresolvable paths.
pub(crate) fn resolve_import(
    specifier: &str,
    from_file: &Path,
    path_config: Option<&PathConfig>,
) -> Option<PathBuf> {
    let parent = from_file.parent()?;

    if specifier.starts_with("./") || specifier.starts_with("../") {
        let base = parent.join(specifier);
        return try_extensions(&base);
    }

    if let Some(cfg) = path_config {
        for candidate in cfg.config.resolve_path_alias_or_base_url(specifier) {
            if let Some(resolved) = try_extensions(&candidate) {
                return Some(resolved);
            }
        }
        if let Some(base_url) = &cfg.config.compiler_options.base_url {
            if let Some(resolved) = try_extensions(&base_url.join(specifier)) {
                return Some(resolved);
            }
        }
    }

    // Preserve the conventional fallback when the config has no resolution settings.
    let has_paths = path_config.is_some_and(|cfg| {
        cfg.config.compiler_options.base_url.is_some()
            || cfg
                .config
                .compiler_options
                .paths
                .as_ref()
                .is_some_and(|paths| !paths.is_empty())
    });
    if specifier.starts_with("@/") && !has_paths {
        if let Some(root) = crate::util::git_root(parent) {
            let rest = &specifier[2..];
            let base = root.join("src").join(rest);
            return try_extensions(&base);
        }
    }

    if specifier == "$lib" {
        if let Some(resolved) = resolve_sveltekit_lib_alias(parent, "") {
            return Some(resolved);
        }
    } else if let Some(rest) = specifier.strip_prefix("$lib/") {
        if let Some(resolved) = resolve_sveltekit_lib_alias(parent, rest) {
            return Some(resolved);
        }
    }

    None
}

/// Resolve inherited JSONC configuration through Oxc; local-file policy stays in xray.
pub(crate) fn load_path_config(start_dir: &Path) -> Option<PathConfig> {
    let tsconfig_path = find_tsconfig(start_dir)?;
    let config = match oxc_resolver::Resolver::default().resolve_tsconfig(&tsconfig_path) {
        Ok(config) => config,
        Err(error) => {
            crate::presentation::warn(format!(
                "failed to resolve {}: {error}",
                tsconfig_path.display()
            ));
            return None;
        }
    };
    Some(PathConfig { config })
}

/// Try a base path with various extensions: direct, .ts, .tsx, .js, .jsx,
/// then /index.ts, /index.tsx, /index.js, /index.jsx.
fn try_extensions(base: &Path) -> Option<PathBuf> {
    shared::try_extensions_with(base, EXTENSIONS)
}

fn resolve_sveltekit_lib_alias(start_dir: &Path, rest: &str) -> Option<PathBuf> {
    for dir in start_dir.ancestors() {
        let base = dir.join("src").join("lib").join(rest);
        if let Some(resolved) = try_extensions(&base) {
            return Some(resolved);
        }
    }
    None
}

fn is_local_module_specifier(source: &str) -> bool {
    source.starts_with('.')
        || source.starts_with('@')
        || source == "$lib"
        || source.starts_with("$lib/")
}

/// Walk up directories from `start` looking for tsconfig.json.
fn find_tsconfig(start: &Path) -> Option<PathBuf> {
    let mut dir = if start.is_dir() {
        start.to_path_buf()
    } else {
        start.parent()?.to_path_buf()
    };

    loop {
        let candidate = dir.join("tsconfig.json");
        if candidate.is_file() {
            return Some(candidate);
        }
        if !dir.pop() {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    // --- try_extensions ---

    #[test]
    fn try_extensions_finds_ts_file() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("foo.ts");
        fs::write(&file, "export const x = 1;").unwrap();

        let result = try_extensions(&dir.path().join("foo"));
        assert_eq!(result, Some(file));
    }

    #[test]
    fn try_extensions_finds_tsx_file() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("foo.tsx");
        fs::write(&file, "export const x = 1;").unwrap();

        let result = try_extensions(&dir.path().join("foo"));
        assert_eq!(result, Some(file));
    }

    #[test]
    fn try_extensions_finds_extended_ts_ecosystem_files() {
        let dir = tempfile::tempdir().unwrap();

        for ext in ["mts", "mjs", "cjs", "svelte", "vue"] {
            let stem = format!("entry_{ext}");
            let file = dir.path().join(format!("{stem}.{ext}"));
            fs::write(&file, "export const x = 1;").unwrap();

            let result = try_extensions(&dir.path().join(&stem));
            assert_eq!(result, Some(file));
        }
    }

    #[test]
    fn try_extensions_appends_to_dotted_module_names() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("user.service.ts");
        fs::write(&file, "export class UserService {}").unwrap();

        let result = try_extensions(&dir.path().join("user.service"));
        assert_eq!(result, Some(file));
    }

    #[test]
    fn try_extensions_substitutes_explicit_extension_before_appending() {
        let dir = tempfile::tempdir().unwrap();
        let emitted_source = dir.path().join("user.ts");
        fs::write(&emitted_source, "export const source = true;").unwrap();
        fs::write(
            dir.path().join("user.js.ts"),
            "export const unrelated = true;",
        )
        .unwrap();

        let result = try_extensions(&dir.path().join("user.js"));
        assert_eq!(result, Some(emitted_source));
    }

    #[test]
    fn try_extensions_finds_index_file() {
        let dir = tempfile::tempdir().unwrap();
        let sub = dir.path().join("components");
        fs::create_dir(&sub).unwrap();
        let file = sub.join("index.ts");
        fs::write(&file, "export {}").unwrap();

        let result = try_extensions(&sub);
        assert_eq!(result, Some(file));
    }

    #[test]
    fn try_extensions_returns_none_for_missing() {
        let dir = tempfile::tempdir().unwrap();
        let result = try_extensions(&dir.path().join("nonexistent"));
        assert_eq!(result, None);
    }

    #[test]
    fn try_extensions_prefers_ts_over_tsx() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("foo.ts"), "").unwrap();
        fs::write(dir.path().join("foo.tsx"), "").unwrap();

        let result = try_extensions(&dir.path().join("foo"));
        assert_eq!(result, Some(dir.path().join("foo.ts")));
    }

    // --- collect_sources ---

    #[test]
    fn collect_sources_merges_imports_and_reexports() {
        let imports = vec!["./a".to_string(), "./b".to_string()];
        let reexports = vec![ReExport {
            names: vec!["X".to_string()],
            source: "./c".to_string(),
            is_type: false,
        }];
        let result = collect_sources(&imports, &reexports);
        assert_eq!(result, vec!["./a", "./b", "./c"]);
    }

    #[test]
    fn collect_sources_keeps_sveltekit_lib_reexports() {
        let imports = vec!["$lib/a".to_string()];
        let reexports = vec![ReExport {
            names: vec!["X".to_string()],
            source: "$lib/b".to_string(),
            is_type: false,
        }];
        let result = collect_sources(&imports, &reexports);
        assert_eq!(result, vec!["$lib/a", "$lib/b"]);
    }

    #[test]
    fn collect_sources_deduplicates() {
        let imports = vec!["./a".to_string(), "./b".to_string()];
        let reexports = vec![ReExport {
            names: vec!["X".to_string()],
            source: "./a".to_string(),
            is_type: false,
        }];
        let result = collect_sources(&imports, &reexports);
        assert_eq!(result, vec!["./a", "./b"]);
    }

    #[test]
    fn collect_sources_skips_external_reexports() {
        let imports = vec![];
        let reexports = vec![ReExport {
            names: vec!["X".to_string()],
            source: "react".to_string(),
            is_type: false,
        }];
        let result = collect_sources(&imports, &reexports);
        assert!(result.is_empty());
    }

    // --- load_path_config ---

    #[test]
    fn load_path_config_parses_basic_tsconfig() {
        let dir = tempfile::tempdir().unwrap();
        let tsconfig = dir.path().join("tsconfig.json");
        fs::write(
            &tsconfig,
            r#"{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@/*": ["src/*"]
    }
  }
}"#,
        )
        .unwrap();

        let cfg = load_path_config(dir.path()).unwrap();
        assert_eq!(
            cfg.config.compiler_options.base_url.as_deref(),
            Some(dir.path())
        );
        assert!(cfg.matches_alias("@/utils"));
        assert_eq!(
            cfg.config.resolve_path_alias_or_base_url("@/utils"),
            vec![dir.path().join("src/utils")]
        );
    }

    #[test]
    fn load_path_config_returns_none_for_missing() {
        let dir = tempfile::tempdir().unwrap();
        assert!(load_path_config(dir.path()).is_none());
    }

    #[test]
    fn load_path_config_handles_jsonc_comments() {
        let dir = tempfile::tempdir().unwrap();
        let tsconfig = dir.path().join("tsconfig.json");
        fs::write(
            &tsconfig,
            r#"{
  // compiler settings
  "compilerOptions": {
    "baseUrl": ".",
    /* path aliases */
    "paths": {
      "@/*": ["src/*"]
    }
  }
}"#,
        )
        .unwrap();

        let cfg = load_path_config(dir.path()).unwrap();
        assert!(cfg.matches_alias("@/utils"));
    }

    #[test]
    fn inherited_paths_keep_their_origin_and_support_jsonc_and_unicode() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("config/código")).unwrap();
        fs::create_dir_all(dir.path().join("app")).unwrap();
        let target = dir.path().join("config/código/value.ts");
        fs::write(&target, "").unwrap();
        fs::write(dir.path().join("config/base.json"),
            r#"{"compilerOptions": {/* unicode and trailing commas */ "paths":{"@/*":["código/*"],},},}"#).unwrap();
        fs::write(
            dir.path().join("app/tsconfig.json"),
            r#"{"extends":"../config/base"}"#,
        )
        .unwrap();
        let cfg = load_path_config(&dir.path().join("app")).unwrap();
        assert_eq!(
            resolve_import("@/value", &dir.path().join("app/main.ts"), Some(&cfg)),
            Some(target)
        );
    }

    #[test]
    fn extends_array_and_package_config_obey_child_overrides() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("node_modules/@config/base")).unwrap();
        fs::write(
            dir.path().join("node_modules/@config/base/package.json"),
            r#"{"exports":"./base.json"}"#,
        )
        .unwrap();
        fs::write(
            dir.path().join("node_modules/@config/base/base.json"),
            r#"{"compilerOptions":{"baseUrl":"../../../","paths":{"@/*":["old/*"]}}}"#,
        )
        .unwrap();
        fs::write(
            dir.path().join("second.json"),
            r#"{"compilerOptions":{"paths":{"@/*":["second/*"]}}}"#,
        )
        .unwrap();
        fs::write(dir.path().join("tsconfig.json"),
            r#"{"extends":["@config/base","./second.json"],"compilerOptions":{"paths":{"exact":["value"],"@/*/end":["*"]}}}"#).unwrap();
        let target = dir.path().join("value.ts");
        fs::write(&target, "").unwrap();
        let cfg = load_path_config(dir.path()).unwrap();
        assert_eq!(
            resolve_import("exact", &dir.path().join("main.ts"), Some(&cfg)),
            Some(target.clone())
        );
        assert_eq!(
            resolve_import("@/value/end", &dir.path().join("main.ts"), Some(&cfg)),
            Some(target)
        );
        assert!(!cfg.matches_alias("@/value"));
        assert!(cfg.matches_alias("@/missing/end"));
    }

    #[test]
    fn invalid_missing_and_cyclic_extends_emit_warnings() {
        let dir = tempfile::tempdir().unwrap();
        for source in [
            "not json",
            r#"{"extends":"./missing"}"#,
            r#"{"extends":"./tsconfig.json"}"#,
        ] {
            fs::write(dir.path().join("tsconfig.json"), source).unwrap();
            assert!(load_path_config(dir.path()).is_none());
            let mut report = crate::presentation::Report {
                text: String::new(),
                json: serde_json::json!({"status":"complete"}),
            };
            crate::presentation::attach_warnings(&mut report);
            assert_eq!(report.json["status"], "partial");
            assert!(report.text.contains("failed to resolve"));
        }
    }

    // --- resolve_import integration ---

    #[test]
    fn resolve_import_relative_finds_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("app.ts");
        fs::write(&entry, "").unwrap();
        let hooks = dir.path().join("hooks");
        fs::create_dir(&hooks).unwrap();
        let target = hooks.join("use-chat.ts");
        fs::write(&target, "").unwrap();

        let result = resolve_import("./hooks/use-chat", &entry, None);
        assert_eq!(result, Some(target));
    }

    #[test]
    fn resolve_import_alias_with_config() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        fs::create_dir(&src).unwrap();
        let target = src.join("utils.ts");
        fs::write(&target, "").unwrap();

        let entry = src.join("app.ts");
        fs::write(&entry, "").unwrap();

        fs::write(
            dir.path().join("tsconfig.json"),
            r#"{"compilerOptions":{"baseUrl":".","paths":{"@/*":["src/*"]}}}"#,
        )
        .unwrap();
        let cfg = load_path_config(dir.path()).unwrap();

        let result = resolve_import("@/utils", &entry, Some(&cfg));
        assert_eq!(result, Some(target));
    }

    #[test]
    fn config_without_paths_preserves_the_conventional_root_alias() {
        let dir = tempfile::tempdir().unwrap();
        assert!(std::process::Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(dir.path())
            .status()
            .unwrap()
            .success());
        fs::create_dir(dir.path().join("src")).unwrap();
        fs::write(
            dir.path().join("tsconfig.json"),
            r#"{"compilerOptions":{"strict":true}}"#,
        )
        .unwrap();
        let target = dir.path().join("src/utils.ts");
        fs::write(&target, "").unwrap();
        let cfg = load_path_config(dir.path()).unwrap();
        assert_eq!(
            resolve_import("@/utils", &dir.path().join("src/main.ts"), Some(&cfg)),
            Some(target)
        );
    }

    #[test]
    fn resolve_import_skips_node_modules() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("app.ts");
        fs::write(&entry, "").unwrap();

        let result = resolve_import("react", &entry, None);
        assert_eq!(result, None);
    }

    #[test]
    fn resolve_import_parent_directory() {
        let dir = tempfile::tempdir().unwrap();
        let sub = dir.path().join("components");
        fs::create_dir(&sub).unwrap();
        let entry = sub.join("app.ts");
        fs::write(&entry, "").unwrap();
        let target = dir.path().join("utils.ts");
        fs::write(&target, "").unwrap();

        let result = resolve_import("../utils", &entry, None).map(|p| p.canonicalize().unwrap());
        assert_eq!(result, Some(target.canonicalize().unwrap()));
    }

    #[test]
    fn resolve_import_relative_finds_mts_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("app.ts");
        fs::write(&entry, "").unwrap();
        let target = dir.path().join("config.mts");
        fs::write(&target, "export {};").unwrap();

        let result = resolve_import("./config", &entry, None);
        assert_eq!(result, Some(target));
    }

    #[test]
    fn resolve_import_finds_sveltekit_lib_alias() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        let lib = src.join("lib");
        fs::create_dir_all(lib.join("components")).unwrap();
        let entry_dir = src.join("routes");
        fs::create_dir(&entry_dir).unwrap();
        let entry = entry_dir.join("+page.svelte");
        fs::write(&entry, "").unwrap();
        let target = lib.join("components").join("Button.svelte");
        fs::write(&target, "").unwrap();

        let result = resolve_import("$lib/components/Button", &entry, None);
        assert_eq!(result, Some(target));
    }

    #[test]
    fn resolve_import_uses_base_url_without_paths() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        fs::create_dir(&src).unwrap();
        let entry = src.join("app.ts");
        fs::write(&entry, "").unwrap();
        let target = src.join("utils.ts");
        fs::write(&target, "").unwrap();
        fs::write(
            dir.path().join("tsconfig.json"),
            r#"{ "compilerOptions": { "baseUrl": "." } }"#,
        )
        .unwrap();

        let cfg = load_path_config(dir.path()).unwrap();
        assert!(!cfg.matches_alias("src/utils"));
        assert_eq!(
            resolve_import("src/utils", &entry, Some(&cfg)),
            Some(target)
        );
    }

    #[test]
    fn resolve_import_finds_exact_sveltekit_lib_alias() {
        let dir = tempfile::tempdir().unwrap();
        let lib = dir.path().join("src").join("lib");
        fs::create_dir_all(&lib).unwrap();
        let target = lib.join("index.ts");
        fs::write(&target, "").unwrap();
        let routes = dir.path().join("src").join("routes");
        fs::create_dir(&routes).unwrap();
        let entry = routes.join("+page.svelte");
        fs::write(&entry, "").unwrap();

        assert_eq!(resolve_import("$lib", &entry, None), Some(target));
    }
}
