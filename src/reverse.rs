use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::lang::{self, LanguageKind};
use crate::output::FileDigest;
use crate::{parser, resolve, util};

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
    let importers = find_importers(&git_root, &target, path_config.as_ref());

    println!("── imported by ──");
    println!();

    if importers.is_empty() {
        println!("  (no importers found)");
    } else {
        for (path, lines) in &importers {
            println!("  {path}  ({lines} lines)");
        }
    }

    Ok(())
}

fn find_importers(
    project_root: &Path,
    target: &Path,
    path_config: Option<&resolve::PathConfig>,
) -> Vec<(String, usize)> {
    let mut importers: Vec<(String, usize)> = Vec::new();

    walk_project_files(project_root, &mut |file_path| {
        let Ok(canonical) = file_path.canonicalize() else {
            return;
        };
        if canonical == target {
            return;
        }

        let Ok(parsed) = parser::parse_file(file_path) else {
            return;
        };
        let sources = parsed.language_kind.extract_dependency_specifiers_from_ast(
            parsed.tree.root_node(),
            parsed.source.as_bytes(),
        );

        for specifier in &sources {
            let Some(resolved) =
                resolve_source_specifier(parsed.language_kind, specifier, file_path, path_config)
            else {
                continue;
            };

            let Ok(resolved_canonical) = resolved.canonicalize() else {
                continue;
            };
            if resolved_canonical == target {
                let rel = util::relative_path(file_path);
                let lines = parsed.source.lines().count();
                importers.push((rel, lines));
                break;
            }
        }
    });

    importers.sort_by(|a, b| a.0.cmp(&b.0));
    importers
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

/// Recursively walk supported source files under a directory.
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
                if lang::is_supported_extension(ext) {
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

fn resolve_source_specifier(
    language_kind: LanguageKind,
    specifier: &str,
    from_file: &Path,
    path_config: Option<&resolve::PathConfig>,
) -> Option<PathBuf> {
    language_kind.resolve_source_specifier(specifier, from_file, path_config)
}

fn should_skip_dir(name: &str) -> bool {
    name.starts_with('.') || SKIP_DIRS.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

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

    #[test]
    fn walk_project_files_includes_sql_files() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.sql"), "select 1;").unwrap();
        std::fs::write(dir.path().join("main.ts"), "export {};").unwrap();
        std::fs::write(dir.path().join("README.md"), "# nope").unwrap();

        let mut seen = Vec::new();
        walk_project_files(dir.path(), &mut |p| {
            let name = p.file_name().and_then(|s| s.to_str()).unwrap().to_string();
            seen.push(name);
        });

        assert!(seen.contains(&"main.sql".to_string()));
        assert!(seen.contains(&"main.ts".to_string()));
        assert!(!seen.contains(&"README.md".to_string()));
    }

    #[test]
    fn find_importers_detects_sql_source_importer() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("child.sql");
        let importer = dir.path().join("root.sql");

        fs::write(&target, "SELECT 1;").unwrap();
        fs::write(&importer, "SOURCE child.sql;").unwrap();

        let importers = find_importers(dir.path(), &target.canonicalize().unwrap(), None);
        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("root.sql"));
        assert_eq!(importers[0].1, 1);
    }

    #[test]
    fn find_importers_detects_ts_alias_importer_with_path_config() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        fs::create_dir(&src).unwrap();
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

        let target = src.join("target.ts");
        let importer = src.join("main.ts");
        fs::write(&target, "export const x = 1;").unwrap();
        fs::write(
            &importer,
            "import { x } from '@/target';\nexport const y = x;",
        )
        .unwrap();

        let cfg = resolve::load_path_config(dir.path()).unwrap();
        let importers = find_importers(dir.path(), &target.canonicalize().unwrap(), Some(&cfg));

        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("main.ts"));
        assert_eq!(importers[0].1, 2);
    }

    #[test]
    fn find_importers_sql_does_not_use_ts_alias_resolution() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        fs::create_dir(&src).unwrap();
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

        let target = src.join("target.ts");
        let sql_importer = dir.path().join("root.sql");
        fs::write(&target, "export const x = 1;").unwrap();
        fs::write(&sql_importer, "SOURCE @/target;").unwrap();

        let cfg = resolve::load_path_config(dir.path()).unwrap();
        let importers = find_importers(dir.path(), &target.canonicalize().unwrap(), Some(&cfg));

        assert!(importers.is_empty());
    }
}
