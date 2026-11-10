use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::model::FileSummary;
use crate::output::FileDigest;
use crate::resolve::PathConfig;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ResolutionStatus {
    Resolved,
    External,
    Unresolved,
}

#[derive(Debug, serde::Serialize)]
pub struct ResolutionDiagnostic {
    pub from: PathBuf,
    pub specifier: String,
    pub status: ResolutionStatus,
}

#[derive(Debug, Default, Clone, Copy, serde::Serialize)]
pub struct ResolutionStats {
    pub resolved: usize,
    pub external: usize,
    pub unresolved: usize,
}

/// Per-invocation parse cache and dependency-resolution graph.
///
/// All graph-oriented modes use this boundary so a file is parsed at most once
/// during one command and diagnostics use the same resolution decisions.
pub struct ProjectGraph {
    files: HashMap<PathBuf, FileDigest>,
    diagnostics: Vec<ResolutionDiagnostic>,
}

impl ProjectGraph {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn load(&mut self, path: &Path) -> Result<PathBuf, XrayError> {
        let canonical = std::fs::canonicalize(path).map_err(|source| XrayError::Io {
            path: path.display().to_string(),
            source,
        })?;
        if !self.files.contains_key(&canonical) {
            self.files
                .insert(canonical.clone(), FileDigest::from_path(&canonical)?);
        }
        Ok(canonical)
    }

    pub fn digest(&self, path: &Path) -> Option<&FileDigest> {
        self.files.get(path)
    }

    pub fn summary(&mut self, path: &Path) -> Result<FileSummary, XrayError> {
        let canonical = self.load(path)?;
        self.files
            .get(&canonical)
            .map(FileDigest::summarize)
            .ok_or_else(|| XrayError::ParseFailed("graph cache lost loaded file".to_string()))
    }

    pub fn dependencies(
        &mut self,
        path: &Path,
        path_config: Option<&PathConfig>,
    ) -> Result<Vec<PathBuf>, XrayError> {
        let canonical = self.load(path)?;
        let Some(digest) = self.files.get(&canonical) else {
            return Err(XrayError::ParseFailed(
                "graph cache lost loaded file".to_string(),
            ));
        };
        let language = digest.language_kind;
        let local_config = language
            .uses_ts_path_config()
            .then(|| {
                canonical
                    .parent()
                    .and_then(crate::resolve::load_path_config)
            })
            .flatten();
        let path_config = local_config.as_ref().or(path_config);
        let mut specifiers = digest.dependency_specifiers();
        for specifier in digest.diagnostic_specifiers() {
            if !specifiers.contains(specifier) {
                specifiers.push(specifier.clone());
            }
        }
        let mut resolved = Vec::new();
        let php_paths = (language == crate::lang::LanguageKind::Php)
            .then(|| crate::resolve::PhpPaths::load(canonical.parent().unwrap_or(Path::new("."))));

        for specifier in specifiers {
            let candidate = php_paths.as_ref().map_or_else(
                || language.resolve_source_specifier(&specifier, &canonical, path_config),
                |paths| {
                    crate::resolve::resolve_php_import_with_paths(&specifier, &canonical, paths)
                },
            );
            let status = if let Some(candidate) = candidate {
                if let Ok(candidate) = candidate.canonicalize() {
                    let candidates = if language == crate::lang::LanguageKind::Go {
                        crate::resolve::go_package_files(&candidate)?
                    } else {
                        vec![candidate]
                    };
                    for candidate in candidates {
                        let candidate =
                            candidate.canonicalize().map_err(|source| XrayError::Io {
                                path: candidate.display().to_string(),
                                source,
                            })?;
                        if !resolved.contains(&candidate) {
                            resolved.push(candidate);
                        }
                    }
                    ResolutionStatus::Resolved
                } else {
                    ResolutionStatus::Unresolved
                }
            } else if is_external_specifier(language, &specifier, path_config)
                && !php_paths
                    .as_ref()
                    .is_some_and(|paths| paths.matches(&specifier))
            {
                ResolutionStatus::External
            } else {
                ResolutionStatus::Unresolved
            };
            self.diagnostics.push(ResolutionDiagnostic {
                from: canonical.clone(),
                specifier,
                status,
            });
        }
        Ok(resolved)
    }

    pub fn syntax_recovery_files(&self) -> Vec<&Path> {
        let mut files: Vec<_> = self
            .files
            .iter()
            .filter(|(_, digest)| digest.syntax_recovered)
            .map(|(path, _)| path.as_path())
            .collect();
        files.sort();
        files
    }

    pub fn diagnostics(&self) -> &[ResolutionDiagnostic] {
        &self.diagnostics
    }

    pub fn stats(&self) -> ResolutionStats {
        let mut stats = ResolutionStats::default();
        for diagnostic in &self.diagnostics {
            match diagnostic.status {
                ResolutionStatus::Resolved => stats.resolved += 1,
                ResolutionStatus::External => stats.external += 1,
                ResolutionStatus::Unresolved => stats.unresolved += 1,
            }
        }
        stats
    }
}

fn is_external_specifier(
    language: crate::lang::LanguageKind,
    specifier: &str,
    path_config: Option<&PathConfig>,
) -> bool {
    use crate::lang::LanguageKind;
    match language {
        LanguageKind::Ts | LanguageKind::Svelte | LanguageKind::Vue => {
            !specifier.starts_with('.')
                && !specifier.starts_with('/')
                && specifier != "$lib"
                && !specifier.starts_with("$lib/")
                && !path_config.is_some_and(|config| config.matches_alias(specifier))
        }
        LanguageKind::Go | LanguageKind::Py => !specifier.starts_with('.'),
        LanguageKind::Php if specifier.starts_with("inertia:") => false,
        LanguageKind::Php | LanguageKind::Rs => {
            !specifier.starts_with('.')
                && specifier != "crate"
                && !specifier.starts_with("crate::")
                && specifier != "self"
                && !specifier.starts_with("self::")
                && specifier != "super"
                && !specifier.starts_with("super::")
        }
        LanguageKind::Sql | LanguageKind::Md => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn scoped_npm_packages_and_go_stdlib_are_external() {
        assert!(is_external_specifier(
            crate::lang::LanguageKind::Ts,
            "@scope/package",
            None
        ));
        assert!(is_external_specifier(
            crate::lang::LanguageKind::Go,
            "fmt",
            None
        ));
        assert!(!is_external_specifier(
            crate::lang::LanguageKind::Ts,
            "$lib/missing",
            None
        ));
        assert!(is_external_specifier(
            crate::lang::LanguageKind::Rs,
            "crate_utils",
            None
        ));
    }

    #[test]
    fn graph_diagnostics_include_external_and_unresolved_ts_imports() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.ts");
        fs::write(
            &entry,
            "import React from 'react';\nimport missing from './missing';",
        )
        .unwrap();

        let mut graph = ProjectGraph::new();
        assert!(graph.dependencies(&entry, None).unwrap().is_empty());
        let stats = graph.stats();

        assert_eq!(stats.external, 1);
        assert_eq!(stats.unresolved, 1);
        assert_eq!(stats.resolved, 0);
    }

    #[test]
    fn followed_files_use_their_own_inherited_aliases() {
        let dir = tempfile::tempdir().unwrap();
        for package in ["a", "b"] {
            fs::create_dir(dir.path().join(package)).unwrap();
            fs::write(
                dir.path().join(package).join("base.json"),
                r#"{"compilerOptions":{"paths":{"local":["./value"]}}}"#,
            )
            .unwrap();
            fs::write(
                dir.path().join(package).join("tsconfig.json"),
                r#"{"extends":"./base.json"}"#,
            )
            .unwrap();
            fs::write(
                dir.path().join(package).join("value.ts"),
                "export const value = 1;",
            )
            .unwrap();
        }
        let entry = dir.path().join("b/main.ts");
        fs::write(&entry, "import { value } from 'local';").unwrap();
        let wrong_config = crate::resolve::load_path_config(&dir.path().join("a")).unwrap();
        let mut graph = ProjectGraph::new();
        assert_eq!(
            graph.dependencies(&entry, Some(&wrong_config)).unwrap(),
            vec![dir.path().join("b/value.ts").canonicalize().unwrap()]
        );
    }

    #[test]
    fn inertia_dependencies_keep_missing_and_ambiguous_pages_unresolved() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("composer.json"), "{}").unwrap();
        for name in [
            "pages/Home.vue",
            "pages/Duplicate.vue",
            "Pages/Duplicate.vue",
        ] {
            let path = dir.path().join("resources/js").join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, "<template>Page</template>").unwrap();
        }
        let entry = dir.path().join("Controller.php");
        fs::write(
            &entry,
            r"<?php use Inertia\Inertia;
            Inertia::render('Home'); Inertia::render('Missing');
            Inertia::render('Duplicate'); Inertia::render('../Outside');",
        )
        .unwrap();
        let mut graph = ProjectGraph::new();
        assert_eq!(
            graph.dependencies(&entry, None).unwrap(),
            vec![dir
                .path()
                .join("resources/js/pages/Home.vue")
                .canonicalize()
                .unwrap()]
        );
        assert_eq!(graph.stats().resolved, 1);
        assert_eq!(graph.stats().unresolved, 3);
        assert_eq!(graph.stats().external, 1); // Inertia facade itself is a vendor class.
        for diagnostic in graph
            .diagnostics()
            .iter()
            .filter(|d| d.specifier.starts_with("inertia:"))
        {
            assert_eq!(
                diagnostic.status,
                if diagnostic.specifier == "inertia:Home" {
                    ResolutionStatus::Resolved
                } else {
                    ResolutionStatus::Unresolved
                }
            );
        }
    }

    #[test]
    fn php_diagnostics_distinguish_missing_local_class_from_external_namespace() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("app")).unwrap();
        fs::write(
            dir.path().join("composer.json"),
            r#"{"autoload":{"psr-4":{"App\\":"app/"}}}"#,
        )
        .unwrap();
        fs::write(dir.path().join("app/Present.php"), "<?php class Present {}").unwrap();
        let entry = dir.path().join("app/Entry.php");
        fs::write(
            &entry,
            "<?php use App\\Present; use App\\Missing; use Vendor\\Remote;",
        )
        .unwrap();
        let mut graph = ProjectGraph::new();
        assert_eq!(graph.dependencies(&entry, None).unwrap().len(), 1);
        assert_eq!(graph.stats().resolved, 1);
        assert_eq!(graph.stats().unresolved, 1);
        assert_eq!(graph.stats().external, 1);
    }
}
