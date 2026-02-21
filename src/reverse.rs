use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::output::FileDigest;
use crate::{extract, parser, resolve, util};

/// Show who imports the target file by scanning the project.
pub fn run(target_path: &str) -> Result<(), XrayError> {
    let target = std::fs::canonicalize(Path::new(target_path)).map_err(|e| XrayError::Io {
        path: target_path.to_string(),
        source: e,
    })?;

    let digest = FileDigest::from_path(&target)?;
    print!("{digest}");

    let git_root = target
        .parent()
        .and_then(util::git_root)
        .ok_or_else(|| XrayError::ParseFailed("not inside a git repository".to_string()))?;

    let path_config = target.parent().and_then(resolve::load_path_config);

    let mut importers: Vec<(String, usize)> = Vec::new();

    walk_project_files(&git_root, &mut |file_path| {
        let Ok(canonical) = file_path.canonicalize() else {
            return;
        };
        if canonical == target {
            return;
        }

        let Ok((tree, source)) = parser::parse_file(file_path) else {
            return;
        };
        let sources = extract::extract_sources_only(tree.root_node(), source.as_bytes());

        for specifier in &sources {
            let Some(resolved) =
                resolve::resolve_import(specifier, file_path, path_config.as_ref())
            else {
                continue;
            };

            let Ok(resolved_canonical) = resolved.canonicalize() else {
                continue;
            };
            if resolved_canonical == target {
                let rel = util::relative_path(file_path);
                let lines = source.lines().count();
                importers.push((rel, lines));
                break;
            }
        }
    });

    println!("── imported by ──");
    println!();

    if importers.is_empty() {
        println!("  (no importers found)");
    } else {
        importers.sort_by(|a, b| a.0.cmp(&b.0));
        for (path, lines) in &importers {
            println!("  {path}  ({lines} lines)");
        }
    }

    Ok(())
}

/// Directories to skip when walking the project tree.
const SKIP_DIRS: &[&str] = &[
    "node_modules",
    "dist",
    "build",
    ".next",
    ".turbo",
    "coverage",
    ".cache",
    "out",
];

/// Recursively walk all `.ts`/`.tsx`/`.js`/`.jsx` files under a directory.
fn walk_project_files(dir: &Path, callback: &mut dyn FnMut(&Path)) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    let mut dirs: Vec<PathBuf> = Vec::new();
    let mut files: Vec<PathBuf> = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            if !should_skip_dir(&name_str) {
                dirs.push(path);
            }
        } else if path.is_file() {
            if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                if matches!(ext, "ts" | "tsx" | "js" | "jsx") {
                    files.push(path);
                }
            }
        }
    }

    dirs.sort();
    files.sort();

    for file in &files {
        callback(file);
    }
    for dir in &dirs {
        walk_project_files(dir, callback);
    }
}

fn should_skip_dir(name: &str) -> bool {
    name.starts_with('.') || SKIP_DIRS.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_skip_dir_skips_node_modules() {
        assert!(should_skip_dir("node_modules"));
    }

    #[test]
    fn should_skip_dir_skips_hidden_dirs() {
        assert!(should_skip_dir(".cache"));
        assert!(should_skip_dir(".git"));
    }

    #[test]
    fn should_skip_dir_allows_normal_dirs() {
        assert!(!should_skip_dir("src"));
        assert!(!should_skip_dir("components"));
    }

    #[test]
    fn should_skip_dir_skips_build_dirs() {
        assert!(should_skip_dir("dist"));
        assert!(should_skip_dir("build"));
        assert!(should_skip_dir(".next"));
    }
}
