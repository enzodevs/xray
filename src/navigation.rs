//! Static PHP navigation through a private, reusable Intelephense session.
//! All public positions are one-based lines and UTF-8 byte columns (end exclusive).
#[cfg(unix)]
#[path = "lsp_session.rs"]
mod session;

#[cfg(unix)]
use std::collections::HashMap;
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::time::{Instant, SystemTime};

use serde::{Deserialize, Serialize};

use crate::error::XrayError;
#[cfg(unix)]
use crate::lsp::{self, failure, LspClient};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryKind {
    Definition,
    References,
}

/// Exact server selection range, converted to one-based UTF-8 byte positions.
#[derive(Debug, Serialize, Deserialize)]
pub struct NavigationLocation {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// Server-reported static locations, never a guarantee of runtime completeness.
#[derive(Debug, Serialize, Deserialize)]
pub struct NavigationResult {
    pub locations: Vec<NavigationLocation>,
    pub provenance: String,
    pub readiness: String,
    pub warnings: Vec<String>,
}

#[cfg(unix)]
#[derive(Serialize, Deserialize)]
struct Request {
    file: PathBuf,
    line: usize,
    column: usize,
    kind: QueryKind,
}

/// Query PHP definitions or references. Invalid positions and server failures are errors.
/// `XRAY_INTELEPHENSE` can select an executable path; it is never passed to a shell.
/// Opened documents are read fresh. Other PHP files and Composer manifests are refreshed
/// using size/mtime metadata; preserving both can evade change detection. Hidden directories,
/// symlinks and `node_modules` are excluded from that scan. Detected changes restart the index.
#[cfg(unix)]
pub fn query(
    file: &Path,
    line: usize,
    column: usize,
    kind: QueryKind,
) -> Result<NavigationResult, XrayError> {
    let file = file
        .canonicalize()
        .map_err(|e| failure(format!("{}: {e}", file.display())))?;
    validate(&file, line, column)?;
    let root = workspace_root(&file)?;
    session::query(
        &root,
        &Request {
            file,
            line,
            column,
            kind,
        },
    )
}

/// Internal CLI entry point: serve a single canonical workspace, then exit when idle.
#[cfg(unix)]
pub fn run_worker(root: &Path) -> Result<(), XrayError> {
    session::run(root)
}

#[cfg(unix)]
fn validate(file: &Path, line: usize, column: usize) -> Result<(), XrayError> {
    if !file
        .extension()
        .and_then(|s| s.to_str())
        .is_some_and(|extension| extension.eq_ignore_ascii_case("php"))
    {
        return Err(failure("navigation currently supports PHP only"));
    }
    if line == 0 || column == 0 {
        return Err(failure("line and UTF-8 byte column must be one-based"));
    }
    lsp::byte_to_utf16(&lsp::read_source(file)?, line - 1, column - 1)?;
    Ok(())
}

#[cfg(unix)]
fn workspace_root(file: &Path) -> Result<PathBuf, XrayError> {
    let parent = file
        .parent()
        .ok_or_else(|| failure("file has no parent directory"))?;
    // A vendor declaration belongs to the consuming application, not its nested package.
    if let Some(vendor) = parent.ancestors().find(|path| {
        path.file_name().is_some_and(|name| name == "vendor")
            && path
                .parent()
                .is_some_and(|root| root.join("composer.json").is_file())
    }) {
        return vendor
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| failure("vendor has no parent"));
    }
    // Composer root takes precedence over a containing monorepo's Git root.
    if let Some(root) = parent
        .ancestors()
        .find(|p| p.join("composer.json").is_file())
    {
        return Ok(root.to_path_buf());
    }
    Ok(parent
        .ancestors()
        .find(|p| p.join(".git").exists())
        .unwrap_or(parent)
        .to_path_buf())
}

#[cfg(unix)]
type Snapshot = HashMap<PathBuf, (u64, SystemTime)>;

#[cfg(unix)]
struct Workspace {
    client: LspClient,
    snapshot: Snapshot,
    storage: PathBuf,
}

#[cfg(unix)]
impl Workspace {
    fn start(root: &Path, storage: &Path) -> Result<Self, XrayError> {
        let snapshot = snapshot(root)?;
        Ok(Self {
            client: LspClient::start_php(root, storage)?,
            snapshot,
            storage: storage.to_path_buf(),
        })
    }

    fn query(&mut self, root: &Path, request: &Request) -> Result<NavigationResult, XrayError> {
        let file = request
            .file
            .canonicalize()
            .map_err(|e| failure(e.to_string()))?;
        if !file.starts_with(root) {
            return Err(failure("requested file is outside worker workspace"));
        }
        validate(&file, request.line, request.column)?;
        let current = snapshot(root)?;
        if self.snapshot != current {
            // Intelephense has no watched-files acknowledgement. Restart and await its real
            // index progress rather than declaring queued asynchronous changes ready.
            self.client.restart_php(root, &self.storage)?;
        }
        self.snapshot = current;
        let text = self.client.ensure_open(&file)?;
        self.client.wait_for_initial_index()?;
        let character = lsp::byte_to_utf16(&text, request.line - 1, request.column - 1)?;
        let result = self.client.locations(
            &file,
            request.line - 1,
            character,
            matches!(request.kind, QueryKind::References),
        );
        self.client.close(&file)?;
        let locations = result?
            .into_iter()
            .map(|location| {
                let text = if location.file == file {
                    std::borrow::Cow::Borrowed(text.as_str())
                } else {
                    std::borrow::Cow::Owned(lsp::read_source(&location.file)?)
                };
                Ok(NavigationLocation {
                    line: location.line + 1,
                    column: lsp::utf16_to_byte(&text, location.line, location.character)? + 1,
                    end_line: location.end_line + 1,
                    end_column: lsp::utf16_to_byte(
                        &text,
                        location.end_line,
                        location.end_character,
                    )? + 1,
                    file: location.file,
                })
            })
            .collect::<Result<Vec<_>, XrayError>>()?;
        Ok(NavigationResult {
            locations,
            provenance: "lsp:intelephense".into(),
            readiness: "ready".into(),
            warnings: Vec::new(),
        })
    }
}

/// Scan names and metadata only. Never read .env or execute project code; skip symlinks.
#[cfg(unix)]
fn snapshot(root: &Path) -> Result<Snapshot, XrayError> {
    let mut result = HashMap::new();
    let mut pending = vec![(root.to_path_buf(), 0)];
    let deadline = Instant::now() + std::time::Duration::from_secs(15);
    let mut entries = 0;
    while let Some((directory, depth)) = pending.pop() {
        if depth > 64 {
            return Err(failure("workspace directory nesting exceeds 64"));
        }
        let children = std::fs::read_dir(&directory)
            .map_err(|e| failure(format!("scan {}: {e}", directory.display())))?;
        for child in children {
            entries += 1;
            if entries > 500_000 || Instant::now() >= deadline {
                return Err(failure("workspace scan limit/deadline exceeded"));
            }
            let child = child.map_err(|e| failure(e.to_string()))?;
            let kind = child.file_type().map_err(|e| failure(e.to_string()))?;
            let path = child.path();
            if kind.is_dir() {
                if !child.file_name().to_string_lossy().starts_with('.')
                    && child.file_name() != "node_modules"
                {
                    pending.push((path, depth + 1));
                }
            } else if kind.is_file()
                && (path
                    .extension()
                    .and_then(|s| s.to_str())
                    .is_some_and(|extension| extension.eq_ignore_ascii_case("php"))
                    || matches!(
                        child.file_name().to_str(),
                        Some("composer.json" | "composer.lock")
                    ))
            {
                let metadata = child.metadata().map_err(|e| failure(e.to_string()))?;
                result.insert(
                    path,
                    (
                        metadata.len(),
                        metadata.modified().map_err(|e| failure(e.to_string()))?,
                    ),
                );
            }
        }
    }
    Ok(result)
}

/// Navigation requires the Unix local-session backend on this platform.
#[cfg(not(unix))]
pub fn query(
    _file: &Path,
    _line: usize,
    _column: usize,
    _kind: QueryKind,
) -> Result<NavigationResult, XrayError> {
    Err(XrayError::Lsp("navigation requires Unix".into()))
}

/// The private worker is not available outside Unix.
#[cfg(not(unix))]
pub fn run_worker(_root: &Path) -> Result<(), XrayError> {
    Err(XrayError::Lsp("LSP worker requires Unix".into()))
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn vendor_declaration_uses_consuming_composer_workspace() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("composer.json"), "{}").unwrap();
        let dependency = dir.path().join("vendor/vendor-name/package");
        std::fs::create_dir_all(&dependency).unwrap();
        std::fs::write(dependency.join("composer.json"), "{}").unwrap();
        assert_eq!(
            workspace_root(&dependency.join("Source.php")).unwrap(),
            dir.path()
        );
    }

    #[test]
    fn rejects_zero_and_split_utf8_positions_before_starting_server() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("unicode.php");
        std::fs::write(&file, "<?php // é😀\n").unwrap();
        assert!(validate(&file, 0, 1).is_err());
        assert!(validate(&file, 1, 0).is_err());
        assert!(validate(&file, 1, 11).is_err());
        assert!(validate(&file, 3, 1).is_err());
        assert!(validate(&file, 1, 12).is_ok());
    }

    #[test]
    fn refresh_scan_tracks_php_and_composer_but_not_secret_or_hidden_trees() {
        let dir = tempfile::tempdir().unwrap();
        let php = dir.path().join("source.php");
        std::fs::write(&php, "<?php // old").unwrap();
        std::fs::write(dir.path().join("composer.lock"), "{}").unwrap();
        std::fs::write(dir.path().join(".env"), "not read").unwrap();
        std::fs::create_dir(dir.path().join(".cache")).unwrap();
        std::fs::write(dir.path().join(".cache/ignored.php"), "<?php").unwrap();
        std::os::unix::fs::symlink(&php, dir.path().join("symlink.php")).unwrap();
        let before = snapshot(dir.path()).unwrap();
        assert_eq!(before.len(), 2);
        std::fs::write(&php, "<?php // changed length").unwrap();
        assert_ne!(before, snapshot(dir.path()).unwrap());
        std::fs::remove_file(&php).unwrap();
        assert_eq!(snapshot(dir.path()).unwrap().len(), 1);
    }
}
