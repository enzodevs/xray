use std::path::{Path, PathBuf};

use tree_sitter::{Language as TsLanguage, Node};

use crate::error::XrayError;
use crate::model::{FileContent, FileSymbols};
use crate::{extract, resolve};

/// Supported language ecosystems in xray.
///
/// This is the central isolation boundary used by parser/extractor/dependency
/// resolution codepaths. New languages should plug in here rather than adding
/// `if ext == ...` checks across the codebase.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageKind {
    Ts,
    Sql,
    Py,
    Md,
}

impl LanguageKind {
    /// Resolve an extension to a language ecosystem.
    pub fn from_extension(ext: &str) -> Result<Self, XrayError> {
        Self::for_extension(ext).ok_or_else(|| XrayError::UnsupportedExtension(ext.to_string()))
    }

    /// Resolve an extension to a language ecosystem (non-error variant).
    pub fn for_extension(ext: &str) -> Option<Self> {
        if is_sql_extension(ext) {
            return Some(Self::Sql);
        }
        if is_ts_ecosystem_extension(ext) {
            return Some(Self::Ts);
        }
        if is_python_extension(ext) {
            return Some(Self::Py);
        }
        if is_markdown_extension(ext) {
            return Some(Self::Md);
        }
        None
    }

    /// tree-sitter parser language for this ecosystem.
    pub fn tree_sitter_language(self) -> Result<TsLanguage, XrayError> {
        match self {
            Self::Sql => Ok(tree_sitter_sequel::LANGUAGE.into()),
            Self::Ts => Ok(tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into()),
            Self::Py => Ok(tree_sitter_python::LANGUAGE.into()),
            Self::Md => Err(XrayError::UnsupportedFeature {
                feature: "tree-sitter parser",
                language: "Markdown",
            }),
        }
    }

    /// tree-sitter parser language for JSX-capable files in the TS ecosystem.
    pub fn tree_sitter_language_for_extension(self, ext: &str) -> Result<TsLanguage, XrayError> {
        match self {
            Self::Ts if matches_tsx_extension(ext) => Ok(tree_sitter_typescript::LANGUAGE_TSX.into()),
            Self::Sql | Self::Ts | Self::Py | Self::Md => self.tree_sitter_language(),
        }
    }

    /// Extract a full symbol table using the ecosystem-specific extractor.
    pub fn extract_symbols(self, root: Node, src: &[u8]) -> FileSymbols {
        match self {
            Self::Ts => extract::extract_ts_symbols(root, src),
            Self::Sql => extract::extract_sql_symbols(root, src),
            Self::Py => extract::extract_py_symbols(root, src),
            Self::Md => FileSymbols {
                imports: Vec::new(),
                import_bindings: Vec::new(),
                reexports: Vec::new(),
                exports: Vec::new(),
                internals: Vec::new(),
                types: Vec::new(),
                tests: Vec::new(),
                hooks: Vec::new(),
            },
        }
    }

    /// Extract only dependency specifiers using the ecosystem-specific rules.
    pub fn extract_dependency_specifiers_from_ast(self, root: Node, src: &[u8]) -> Vec<String> {
        match self {
            Self::Ts => extract::extract_ts_sources_only(root, src),
            Self::Sql => extract::extract_sql_sources_only(src),
            Self::Py => extract::extract_py_sources_only(root, src),
            Self::Md => Vec::new(),
        }
    }

    /// Collect dependency specifiers from an already-extracted symbol table.
    pub fn collect_dependency_specifiers(self, content: &FileContent) -> Vec<String> {
        match (self, content) {
            (Self::Ts, FileContent::Code(symbols)) => {
                resolve::collect_sources(&symbols.imports, &symbols.reexports)
            }
            // SQL includes are stored in `imports`; SQL has no re-export concept.
            (Self::Sql | Self::Py, FileContent::Code(symbols)) => dedupe_strings(&symbols.imports),
            (Self::Md, FileContent::Markdown(document)) => document
                .links
                .iter()
                .filter(|link| link.is_local)
                .map(|link| link.target.clone())
                .filter_map(|target| clean_markdown_target(&target))
                .fold(Vec::new(), |mut out, target| {
                    if !out.contains(&target) {
                        out.push(target);
                    }
                    out
                }),
            _ => Vec::new(),
        }
    }

    /// Resolve a dependency specifier according to this ecosystem's rules.
    pub fn resolve_source_specifier(
        self,
        specifier: &str,
        from_file: &Path,
        path_config: Option<&resolve::PathConfig>,
    ) -> Option<PathBuf> {
        match self {
            Self::Ts => resolve::resolve_import(specifier, from_file, path_config),
            Self::Sql => resolve::resolve_sql_include(specifier, from_file),
            Self::Py => resolve::resolve_py_import(specifier, from_file),
            Self::Md => resolve::resolve_markdown_link(specifier, from_file),
        }
    }

    /// Human-facing label for symbol references in this ecosystem.
    pub fn symbol_ref_label(self) -> &'static str {
        match self {
            Self::Ts | Self::Py => "calls",
            Self::Sql => "refs",
            Self::Md => "links",
        }
    }

    /// Human-facing language/ecosystem name for diagnostics.
    pub fn display_name(self) -> &'static str {
        match self {
            Self::Ts => "TypeScript/JavaScript",
            Self::Sql => "SQL",
            Self::Py => "Python",
            Self::Md => "Markdown",
        }
    }

    /// Whether this ecosystem currently supports trace mode.
    pub fn supports_trace(self) -> bool {
        matches!(self, Self::Ts)
    }

    /// Whether this ecosystem currently supports LSP-assisted trace resolution.
    pub fn supports_lsp(self) -> bool {
        matches!(self, Self::Ts)
    }
}

/// Check whether a file extension belongs to a supported ecosystem.
pub fn is_supported_extension(ext: &str) -> bool {
    LanguageKind::for_extension(ext).is_some()
}

fn is_sql_extension(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("sql")
}

fn is_ts_ecosystem_extension(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("ts")
        || ext.eq_ignore_ascii_case("tsx")
        || ext.eq_ignore_ascii_case("js")
        || ext.eq_ignore_ascii_case("jsx")
        || ext.eq_ignore_ascii_case("mts")
        || ext.eq_ignore_ascii_case("mjs")
        || ext.eq_ignore_ascii_case("cjs")
}

fn matches_tsx_extension(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("tsx") || ext.eq_ignore_ascii_case("jsx")
}

fn is_python_extension(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("py")
}

fn is_markdown_extension(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("md")
}

fn clean_markdown_target(target: &str) -> Option<String> {
    let trimmed = target.trim();
    if trimmed.is_empty() {
        return None;
    }

    let cutoff = trimmed.find(['#', '?']).unwrap_or(trimmed.len());
    let cleaned = trimmed[..cutoff].trim();
    if cleaned.is_empty() {
        None
    } else {
        Some(cleaned.to_string())
    }
}

fn dedupe_strings(items: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    for item in items {
        if !out.iter().any(|existing| existing == item) {
            out.push(item.clone());
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{FileSymbols, ReExport};
    use std::fs;

    fn parse_with(kind: LanguageKind, ext: &str, src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&kind.tree_sitter_language_for_extension(ext).unwrap())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    fn empty_symbols() -> FileSymbols {
        FileSymbols {
            imports: Vec::new(),
            import_bindings: Vec::new(),
            reexports: Vec::new(),
            exports: Vec::new(),
            internals: Vec::new(),
            types: Vec::new(),
            tests: Vec::new(),
            hooks: Vec::new(),
        }
    }

    #[test]
    fn language_kind_maps_supported_extensions() {
        assert_eq!(LanguageKind::for_extension("ts"), Some(LanguageKind::Ts));
        assert_eq!(LanguageKind::for_extension("TSX"), Some(LanguageKind::Ts));
        assert_eq!(LanguageKind::for_extension("sql"), Some(LanguageKind::Sql));
        assert_eq!(LanguageKind::for_extension("SQL"), Some(LanguageKind::Sql));
        assert_eq!(LanguageKind::for_extension("py"), Some(LanguageKind::Py));
        assert_eq!(LanguageKind::for_extension("PY"), Some(LanguageKind::Py));
        assert_eq!(LanguageKind::for_extension("md"), Some(LanguageKind::Md));
        assert_eq!(LanguageKind::for_extension("rs"), None);
    }

    #[test]
    fn ts_backend_extracts_ts_dependencies_only() {
        let src = br#"
            import { foo } from "./a";
            export { bar } from "./b";
        "#;
        let tree = parse_with(LanguageKind::Ts, "ts", src);
        let deps = LanguageKind::Ts.extract_dependency_specifiers_from_ast(tree.root_node(), src);
        assert_eq!(deps, vec!["./a".to_string(), "./b".to_string()]);
    }

    #[test]
    fn sql_backend_extracts_sql_includes_only() {
        let src = br"
            \i ./schema.sql
            SOURCE seed.sql;
            @@deploy.sql
        ";
        let tree = parse_with(LanguageKind::Sql, "sql", src);
        let deps = LanguageKind::Sql.extract_dependency_specifiers_from_ast(tree.root_node(), src);
        assert_eq!(
            deps,
            vec![
                "./schema.sql".to_string(),
                "seed.sql".to_string(),
                "deploy.sql".to_string()
            ]
        );
    }

    #[test]
    fn py_backend_extracts_py_imports_only() {
        let src = br"
            import pkg.mod
            from . import local
        ";
        let tree = parse_with(LanguageKind::Py, "py", src);
        let deps = LanguageKind::Py.extract_dependency_specifiers_from_ast(tree.root_node(), src);
        assert_eq!(
            deps,
            vec!["pkg.mod".to_string(), ".".to_string(), ".local".to_string()]
        );
    }

    #[test]
    fn ts_and_sql_collect_dependency_specifiers_use_isolated_rules() {
        let mut symbols = empty_symbols();
        symbols.imports = vec!["./one".to_string(), "./one".to_string()];
        symbols.reexports = vec![ReExport {
            names: vec!["X".to_string()],
            source: "./two".to_string(),
            is_type: false,
        }];

        let content = FileContent::Code(symbols);
        let ts_sources = LanguageKind::Ts.collect_dependency_specifiers(&content);
        let sql_sources = LanguageKind::Sql.collect_dependency_specifiers(&content);
        let py_sources = LanguageKind::Py.collect_dependency_specifiers(&content);

        assert_eq!(ts_sources, vec!["./one".to_string(), "./two".to_string()]);
        assert_eq!(sql_sources, vec!["./one".to_string()]);
        assert_eq!(py_sources, vec!["./one".to_string()]);
    }

    #[test]
    fn ts_backend_does_not_use_sql_bare_include_resolution() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.ts");
        let child = dir.path().join("child.sql");
        fs::write(&entry, "export {}").unwrap();
        fs::write(&child, "select 1;").unwrap();

        let resolved = LanguageKind::Ts.resolve_source_specifier("child.sql", &entry, None);
        assert!(resolved.is_none());
    }

    #[test]
    fn sql_backend_resolves_bare_include_relative_to_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.sql");
        let child = dir.path().join("child.sql");
        fs::write(&entry, "SOURCE child.sql;").unwrap();
        fs::write(&child, "select 1;").unwrap();

        let resolved = LanguageKind::Sql.resolve_source_specifier("child.sql", &entry, None);
        assert_eq!(resolved, Some(child));
    }

    #[test]
    fn sql_backend_ignores_ts_path_alias_resolution() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir(dir.path().join("src")).unwrap();
        fs::write(dir.path().join("src").join("utils.ts"), "export {}").unwrap();
        fs::write(
            dir.path().join("tsconfig.json"),
            r#"{
              "compilerOptions": {
                "baseUrl": ".",
                "paths": { "@/*": ["src/*"] }
              }
            }"#,
        )
        .unwrap();
        let entry = dir.path().join("main.sql");
        fs::write(&entry, "SOURCE @/utils;").unwrap();

        let cfg = resolve::load_path_config(dir.path()).unwrap();
        let resolved = LanguageKind::Sql.resolve_source_specifier("@/utils", &entry, Some(&cfg));
        assert!(resolved.is_none());
    }

    #[test]
    fn py_backend_resolves_relative_module() {
        let dir = tempfile::tempdir().unwrap();
        let pkg = dir.path().join("pkg");
        fs::create_dir(&pkg).unwrap();
        let entry = pkg.join("main.py");
        let target = pkg.join("utils.py");
        fs::write(&entry, "").unwrap();
        fs::write(&target, "").unwrap();

        let resolved = LanguageKind::Py.resolve_source_specifier(".utils", &entry, None);
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn ts_backend_uses_ts_path_alias_resolution() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir(dir.path().join("src")).unwrap();
        let target = dir.path().join("src").join("utils.ts");
        fs::write(&target, "export {}").unwrap();
        fs::write(
            dir.path().join("tsconfig.json"),
            r#"{
              "compilerOptions": {
                "baseUrl": ".",
                "paths": { "@/*": ["src/*"] }
              }
            }"#,
        )
        .unwrap();
        let entry = dir.path().join("src").join("main.ts");
        fs::write(&entry, "import '@/utils'").unwrap();

        let cfg = resolve::load_path_config(dir.path()).unwrap();
        let resolved = LanguageKind::Ts.resolve_source_specifier("@/utils", &entry, Some(&cfg));
        assert_eq!(resolved, Some(target));
    }

    #[test]
    fn symbol_ref_labels_are_language_specific() {
        assert_eq!(LanguageKind::Ts.symbol_ref_label(), "calls");
        assert_eq!(LanguageKind::Sql.symbol_ref_label(), "refs");
        assert_eq!(LanguageKind::Py.symbol_ref_label(), "calls");
        assert_eq!(LanguageKind::Md.symbol_ref_label(), "links");
    }

    #[test]
    fn language_capabilities_are_explicit() {
        assert!(LanguageKind::Ts.supports_trace());
        assert!(LanguageKind::Ts.supports_lsp());
        assert!(!LanguageKind::Sql.supports_trace());
        assert!(!LanguageKind::Sql.supports_lsp());
        assert!(!LanguageKind::Py.supports_trace());
        assert!(!LanguageKind::Py.supports_lsp());
        assert!(!LanguageKind::Md.supports_trace());
        assert!(!LanguageKind::Md.supports_lsp());
    }
}
