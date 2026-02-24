use std::path::{Path, PathBuf};

use crate::model::ReExport;

use super::{shared, PathConfig};

const EXTENSIONS: &[&str] = &["ts", "tsx", "js", "jsx", "mts", "mjs", "cjs"];

/// Merge import specifiers and re-export sources into a deduplicated list.
pub(crate) fn collect_sources(imports: &[String], reexports: &[ReExport]) -> Vec<String> {
    let mut seen = Vec::new();
    for spec in imports {
        if !seen.contains(spec) {
            seen.push(spec.clone());
        }
    }
    for re in reexports {
        if !re.source.starts_with('.') && !re.source.starts_with('@') {
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
        for (pattern, replacements) in &cfg.aliases {
            if let Some(resolved) = expand_alias(specifier, pattern, replacements, &cfg.base_url) {
                return Some(resolved);
            }
        }
    }

    // Fallback: @/ -> <git_root>/src/ when no path config.
    if specifier.starts_with("@/") && path_config.is_none() {
        if let Some(root) = crate::util::git_root(parent) {
            let rest = &specifier[2..];
            let base = root.join("src").join(rest);
            return try_extensions(&base);
        }
    }

    None
}

/// Search tsconfig.json walking up from `start_dir`, parse `paths` + `baseUrl`.
pub(crate) fn load_path_config(start_dir: &Path) -> Option<PathConfig> {
    let tsconfig_path = find_tsconfig(start_dir)?;
    let content = std::fs::read_to_string(&tsconfig_path).ok()?;
    let stripped = strip_jsonc_comments(&content);

    let val: serde_json::Value = match serde_json::from_str(&stripped) {
        Ok(v) => v,
        Err(e) => {
            eprintln!(
                "xray: warning: failed to parse {}: {e}",
                tsconfig_path.display()
            );
            return None;
        }
    };

    let compiler = val.get("compilerOptions")?;
    let tsconfig_dir = tsconfig_path.parent()?;

    let base_url = compiler
        .get("baseUrl")
        .and_then(serde_json::Value::as_str)
        .map_or_else(|| tsconfig_dir.to_path_buf(), |b| tsconfig_dir.join(b));

    let paths = compiler.get("paths")?.as_object()?;
    let mut aliases = Vec::new();

    for (pattern, targets) in paths {
        let replacements: Vec<String> = targets
            .as_array()?
            .iter()
            .filter_map(|v| v.as_str().map(String::from))
            .collect();
        aliases.push((pattern.clone(), replacements));
    }

    Some(PathConfig { base_url, aliases })
}

/// Try a base path with various extensions: direct, .ts, .tsx, .js, .jsx,
/// then /index.ts, /index.tsx, /index.js, /index.jsx.
fn try_extensions(base: &Path) -> Option<PathBuf> {
    shared::try_extensions_with(base, EXTENSIONS)
}

/// Expand an alias pattern against a specifier.
///
/// E.g. pattern `@/*` with replacement `src/*` and specifier `@/hooks/use-chat`
/// expands to `<base_url>/src/hooks/use-chat`, then resolves extensions.
fn expand_alias(
    specifier: &str,
    pattern: &str,
    replacements: &[String],
    base_url: &Path,
) -> Option<PathBuf> {
    if let Some(prefix) = pattern.strip_suffix('*') {
        if let Some(rest) = specifier.strip_prefix(prefix) {
            for replacement in replacements {
                if let Some(rep_prefix) = replacement.strip_suffix('*') {
                    let base = base_url.join(format!("{rep_prefix}{rest}"));
                    if let Some(resolved) = try_extensions(&base) {
                        return Some(resolved);
                    }
                }
            }
        }
    } else if specifier == pattern {
        // Exact match (no wildcard)
        for replacement in replacements {
            let base = base_url.join(replacement);
            if let Some(resolved) = try_extensions(&base) {
                return Some(resolved);
            }
        }
    }
    None
}

/// Strip JSONC comments (`//` line and `/* */` block) while respecting strings.
fn strip_jsonc_comments(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let bytes = input.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    while i < len {
        let ch = bytes[i];

        // String literal — copy verbatim until closing quote
        if ch == b'"' {
            out.push('"');
            i += 1;
            while i < len {
                let c = bytes[i];
                out.push(c as char);
                i += 1;
                if c == b'\\' && i < len {
                    out.push(bytes[i] as char);
                    i += 1;
                } else if c == b'"' {
                    break;
                }
            }
            continue;
        }

        // Line comment
        if ch == b'/' && i + 1 < len && bytes[i + 1] == b'/' {
            // Skip until end of line
            while i < len && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }

        // Block comment
        if ch == b'/' && i + 1 < len && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < len && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                i += 1;
            }
            if i + 1 < len {
                i += 2; // skip */
            }
            continue;
        }

        out.push(ch as char);
        i += 1;
    }

    out
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

    // --- JSONC stripping ---

    #[test]
    fn strip_jsonc_removes_line_comments() {
        let input = r#"{
  // This is a comment
  \"key\": \"value\"
}"#;
        let result = strip_jsonc_comments(input);
        assert!(!result.contains("//"));
        assert!(result.contains(r#"\"key\": \"value\""#));
    }

    #[test]
    fn strip_jsonc_removes_block_comments() {
        let input = r#"{
  /* block comment */
  \"key\": \"value\"
}"#;
        let result = strip_jsonc_comments(input);
        assert!(!result.contains("/*"));
        assert!(result.contains(r#"\"key\": \"value\""#));
    }

    #[test]
    fn strip_jsonc_preserves_strings_with_slashes() {
        let input = r#"{ \"url\": \"https://example.com/api\" }"#;
        let result = strip_jsonc_comments(input);
        assert_eq!(result, input);
    }

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

        for ext in ["mts", "mjs", "cjs"] {
            let stem = format!("entry_{ext}");
            let file = dir.path().join(format!("{stem}.{ext}"));
            fs::write(&file, "export const x = 1;").unwrap();

            let result = try_extensions(&dir.path().join(&stem));
            assert_eq!(result, Some(file));
        }
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

    // --- expand_alias ---

    #[test]
    fn expand_alias_replaces_wildcard() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        fs::create_dir(&src).unwrap();
        let file = src.join("foo.ts");
        fs::write(&file, "").unwrap();

        let result = expand_alias("@/foo", "@/*", &["src/*".to_string()], dir.path());
        assert_eq!(result, Some(file));
    }

    #[test]
    fn expand_alias_exact_match() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("special.ts");
        fs::write(&file, "").unwrap();

        let result = expand_alias("@config", "@config", &["special".to_string()], dir.path());
        assert_eq!(result, Some(file));
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
        assert_eq!(cfg.base_url, dir.path().join("."));
        assert_eq!(cfg.aliases.len(), 1);
        assert_eq!(cfg.aliases[0].0, "@/*");
        assert_eq!(cfg.aliases[0].1, vec!["src/*"]);
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
        assert_eq!(cfg.aliases.len(), 1);
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

        let cfg = PathConfig {
            base_url: dir.path().to_path_buf(),
            aliases: vec![("@/*".to_string(), vec!["src/*".to_string()])],
        };

        let result = resolve_import("@/utils", &entry, Some(&cfg));
        assert_eq!(result, Some(target));
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
}
