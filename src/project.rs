use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::model::FileSummary;
use crate::output::FileDigest;
use crate::resolve::PathConfig;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionStatus {
    Resolved,
    External,
    Unresolved,
}

#[derive(Debug)]
pub struct ResolutionDiagnostic {
    pub from: PathBuf,
    pub specifier: String,
    pub status: ResolutionStatus,
}

#[derive(Debug, Default, Clone, Copy)]
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
        let specifiers = digest.dependency_specifiers();
        let mut resolved = Vec::new();

        for specifier in specifiers {
            let candidate = language.resolve_source_specifier(&specifier, &canonical, path_config);
            let status = if let Some(candidate) = candidate {
                if let Ok(candidate) = candidate.canonicalize() {
                    if !resolved.contains(&candidate) {
                        resolved.push(candidate);
                    }
                    ResolutionStatus::Resolved
                } else {
                    ResolutionStatus::Unresolved
                }
            } else if is_external_specifier(language, &specifier) {
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

fn is_external_specifier(language: crate::lang::LanguageKind, specifier: &str) -> bool {
    use crate::lang::LanguageKind;
    match language {
        LanguageKind::Ts | LanguageKind::Svelte | LanguageKind::Vue => {
            !specifier.starts_with('.') && !specifier.starts_with('/')
        }
        LanguageKind::Go | LanguageKind::Py => !specifier.starts_with('.'),
        LanguageKind::Php | LanguageKind::Rs => {
            !specifier.starts_with('.')
                && !specifier.starts_with("crate")
                && !specifier.starts_with("self")
                && !specifier.starts_with("super")
        }
        LanguageKind::Sql | LanguageKind::Md => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoped_npm_packages_and_go_stdlib_are_external() {
        assert!(is_external_specifier(
            crate::lang::LanguageKind::Ts,
            "@scope/package"
        ));
        assert!(is_external_specifier(crate::lang::LanguageKind::Go, "fmt"));
    }
}
