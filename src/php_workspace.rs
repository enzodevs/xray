//! One filesystem scope supplies both freshness metadata and Intelephense exclusions.
use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant, SystemTime};

use crate::error::XrayError;
use crate::lsp::failure;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Snapshot {
    pub(crate) files: HashMap<PathBuf, (u64, SystemTime)>,
    pub(crate) exclusions: Vec<String>,
}

pub(crate) fn snapshot(root: &Path) -> Result<Snapshot, XrayError> {
    snapshot_with_limit(root, 500_000)
}

fn snapshot_with_limit(root: &Path, limit: usize) -> Result<Snapshot, XrayError> {
    let deadline = Instant::now() + Duration::from_secs(15);
    let visible = crate::git_files::visible(root)?;
    let directories: BTreeSet<_> = visible
        .iter()
        .flatten()
        .filter(|path| relevant(path))
        .flat_map(|path| {
            path.parent()
                .into_iter()
                .flat_map(|parent| parent.ancestors())
                .take_while(|parent| parent.starts_with(root))
                .map(Path::to_path_buf)
        })
        .collect();
    let mut result = Snapshot {
        files: HashMap::new(),
        exclusions: vec!["**/.*/**".into(), "**/node_modules/**".into()],
    };
    let mut pending = vec![(root.to_path_buf(), 0)];
    let vendor = root.join("vendor");
    let mut entries = 0;
    while let Some((directory, depth)) = pending.pop() {
        if depth > 64 {
            return Err(failure("workspace directory nesting exceeds 64"));
        }
        let children = std::fs::read_dir(&directory)
            .map_err(|e| failure(format!("scan {}: {e}", directory.display())))?;
        for child in children {
            entries += 1;
            if entries > limit || Instant::now() >= deadline {
                return Err(failure("workspace scan limit/deadline exceeded"));
            }
            let child = child.map_err(|e| failure(e.to_string()))?;
            let kind = child.file_type().map_err(|e| failure(e.to_string()))?;
            let path = child.path();
            let permitted = visible.as_ref().is_none_or(|files| {
                path.starts_with(&vendor) || files.contains(&path) || directories.contains(&path)
            });
            if kind.is_dir() {
                if child.file_name().to_string_lossy().starts_with('.')
                    || child.file_name() == "node_modules"
                {
                    continue;
                }
                if permitted {
                    pending.push((path, depth + 1));
                } else {
                    result.exclusions.push(exclusion(root, &path, true)?);
                }
            } else if kind.is_symlink() {
                // Exclude the link and any descendants without following it, even outside root.
                result.exclusions.push(exclusion(root, &path, false)?);
                result.exclusions.push(exclusion(root, &path, true)?);
            } else if kind.is_file() && relevant(&path) {
                if permitted {
                    let metadata = child.metadata().map_err(|e| failure(e.to_string()))?;
                    result.files.insert(
                        path,
                        (
                            metadata.len(),
                            metadata.modified().map_err(|e| failure(e.to_string()))?,
                        ),
                    );
                } else {
                    result.exclusions.push(exclusion(root, &path, false)?);
                }
            }
        }
    }
    result.exclusions.sort();
    Ok(result)
}

fn relevant(path: &Path) -> bool {
    path.extension()
        .and_then(|s| s.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("php"))
        || matches!(
            path.file_name().and_then(|s| s.to_str()),
            Some("composer.json" | "composer.lock")
        )
}

fn exclusion(root: &Path, path: &Path, directory: bool) -> Result<String, XrayError> {
    let relative = path
        .strip_prefix(root)
        .map_err(|e| failure(e.to_string()))?;
    let name = relative
        .to_str()
        .ok_or_else(|| failure("PHP exclusion path is not UTF-8"))?;
    // Literal paths, not Git patterns: preserve anchoring and escape glob metacharacters.
    let mut pattern = String::new();
    for character in name.chars() {
        if "\\*?[]{}()!+@".contains(character) {
            pattern.push('\\');
        }
        pattern.push(character);
    }
    if directory {
        pattern.push_str("/**");
    }
    Ok(pattern)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process::Command;

    fn git(root: &Path, args: &[&str]) {
        assert!(Command::new("git")
            .args(args)
            .current_dir(root)
            .output()
            .unwrap()
            .status
            .success());
    }

    #[test]
    fn ignored_runtime_trees_are_pruned_but_vendor_and_tracked_ignored_files_remain() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        git(root, &["init", "-q"]);
        fs::write(
            root.join(".gitignore"),
            "generated/\nvendor/\n*.cache.php\n",
        )
        .unwrap();
        fs::write(root.join("source.php"), "<?php").unwrap();
        fs::create_dir(root.join("generated")).unwrap();
        fs::create_dir(root.join("vendor")).unwrap();
        fs::write(root.join("vendor/type.php"), "<?php").unwrap();
        fs::write(root.join("generated/compiled.php"), "<?php").unwrap();
        // Laravel keeps tracked .gitignore placeholders inside otherwise ignored caches.
        fs::write(root.join("generated/.gitignore"), "*\n!.gitignore\n").unwrap();
        git(root, &["add", "-f", "generated/.gitignore"]);
        fs::write(root.join("kept.cache.php"), "<?php").unwrap();
        git(root, &["add", "-f", "kept.cache.php"]);
        for i in 0..100 {
            fs::write(root.join(format!("generated/{i}.log")), "runtime").unwrap();
        }
        let before = snapshot_with_limit(root, 12).unwrap();
        assert_eq!(before.files.len(), 3);
        assert!(before.files.contains_key(&root.join("source.php")));
        assert!(before.files.contains_key(&root.join("vendor/type.php")));
        assert!(before.files.contains_key(&root.join("kept.cache.php")));
        assert!(before.exclusions.contains(&"generated/**".into()));
        assert!(!before.exclusions.iter().any(|p| p.contains("vendor")));
        fs::write(root.join("generated/compiled.php"), "<?php // changed").unwrap();
        assert_eq!(before, snapshot_with_limit(root, 12).unwrap());
        assert!(snapshot_with_limit(root, 2)
            .unwrap_err()
            .to_string()
            .contains("limit"));
    }

    #[test]
    fn nested_composer_root_uses_relative_git_names_and_fails_on_broken_index() {
        let dir = tempfile::tempdir().unwrap();
        git(dir.path(), &["init", "-q"]);
        let root = dir.path().join("package");
        fs::create_dir(&root).unwrap();
        fs::write(root.join("source.php"), "<?php").unwrap();
        assert!(snapshot(&root)
            .unwrap()
            .files
            .contains_key(&root.join("source.php")));
        fs::write(dir.path().join(".git/index"), "broken index").unwrap();
        assert!(snapshot(&root)
            .unwrap_err()
            .to_string()
            .contains("refusing an unfiltered"));
    }

    #[test]
    fn nested_ignores_negations_and_symlinks_share_the_index_scope() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        git(root, &["init", "-q"]);
        fs::create_dir(root.join("cache")).unwrap();
        fs::write(root.join("cache/.gitignore"), "*.php\n!keep.php\n").unwrap();
        fs::write(root.join("cache/keep.php"), "<?php").unwrap();
        fs::write(root.join("cache/generated[1].php"), "<?php").unwrap();
        std::os::unix::fs::symlink(root.join("cache"), root.join("linked")).unwrap();
        let before = snapshot(root).unwrap();
        assert_eq!(before.files.len(), 1);
        assert!(before
            .exclusions
            .contains(&"cache/generated\\[1\\].php".into()));
        assert!(before.exclusions.contains(&"linked/**".into()));
        fs::write(root.join("cache/.gitignore"), "").unwrap();
        let after = snapshot(root).unwrap();
        assert_ne!(before, after);
        assert_eq!(after.files.len(), 2);
    }
}
