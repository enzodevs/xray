use std::collections::HashMap;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::process::Command;

use crate::error::XrayError;
use crate::lang::{self, LanguageKind};
use crate::model::FileSymbols;
use crate::output::FileDigest;
use crate::{parser, resolve, util};

/// Show who imports the target file by scanning the project.
pub fn run(target_path: &str) -> Result<crate::presentation::Report, XrayError> {
    let target = std::fs::canonicalize(Path::new(target_path)).map_err(|e| XrayError::Io {
        path: target_path.to_string(),
        source: e,
    })?;

    let digest = FileDigest::from_path(&target)?;

    let git_root = target
        .parent()
        .and_then(util::git_root)
        .ok_or_else(|| XrayError::ParseFailed("not inside a git repository".to_string()))?;

    let path_config = if digest.language_kind.uses_ts_path_config() {
        target.parent().and_then(resolve::load_path_config)
    } else {
        None
    };
    let target_fn_names = extract_target_function_names(&digest);
    let scan = scan_importers(&git_root, &target, path_config.as_ref(), &target_fn_names)?;
    let status = if scan.skipped.is_empty() && scan.recovered == 0 && !digest.syntax_recovered {
        "complete"
    } else {
        "partial"
    };
    let coverage = if digest.language_kind == LanguageKind::Php {
        "PHP static class/import dependencies; Git-visible PHP files; not runtime callers"
    } else {
        "static imports; Git-visible supported files; not runtime callers"
    };
    let mut text = format!("{} static dependents status={status} scanned={} skipped={} parser_recovery_files={}\ncoverage: {coverage}\ntarget: {}\n", scan.importers.len(), scan.scanned, scan.skipped.len(), scan.recovered + usize::from(digest.syntax_recovered), digest.display_path);
    for (path, lines) in &scan.importers {
        let _ = writeln!(text, "{path} ({lines} lines)");
    }
    for error in &scan.skipped {
        let _ = writeln!(text, "skipped: {error}");
    }
    Ok(crate::presentation::Report {
        text,
        json: serde_json::json!({
            "schema_version":1,"mode":"who","status":status,"coverage":coverage,
            "target":digest.display_path,"count":scan.importers.len(),"scanned":scan.scanned,
            "parser_recovery_files":scan.recovered,"target_syntax_recovered":digest.syntax_recovered,"skipped":scan.skipped,
            "results":scan.importers.iter().map(|(path,lines)| serde_json::json!({"path":path,"lines":lines})).collect::<Vec<_>>()
        }),
    })
}

/// Extract function names defined in a SQL target file.
///
/// Returns empty for non-SQL files. Used by `--who` to also find files that
/// *call* functions defined in the target (not just files that include it).
fn extract_target_function_names(digest: &FileDigest) -> Vec<String> {
    if digest.language_kind != LanguageKind::Sql {
        return Vec::new();
    }

    let Some(symbols) = digest.code_symbols() else {
        return Vec::new();
    };

    symbols
        .internals
        .iter()
        .filter_map(|sym| extract_sql_function_name(&sym.signature))
        .map(str::to_string)
        .collect()
}

/// Extract the function name from a `CREATE [OR REPLACE] FUNCTION` signature.
fn extract_sql_function_name(signature: &str) -> Option<&str> {
    let rest = signature
        .strip_prefix("CREATE OR REPLACE FUNCTION ")
        .or_else(|| signature.strip_prefix("CREATE FUNCTION "))?;
    let name = rest.split_whitespace().next()?;
    if name.is_empty() {
        None
    } else {
        Some(name)
    }
}

/// Check whether any symbol in the file references one of the target functions.
fn symbols_reference_functions(symbols: &FileSymbols, target_fn_names: &[String]) -> bool {
    symbols.internals.iter().any(|sym| {
        sym.calls.iter().any(|call| {
            call.strip_prefix("fn:").is_some_and(|fn_name| {
                target_fn_names
                    .iter()
                    .any(|t| t.eq_ignore_ascii_case(fn_name))
            })
        })
    })
}

#[derive(Default)]
struct ReverseScan {
    importers: Vec<(String, usize)>,
    scanned: usize,
    recovered: usize,
    skipped: Vec<String>,
}

#[cfg(test)]
fn find_importers(
    project_root: &Path,
    target: &Path,
    path_config: Option<&resolve::PathConfig>,
    target_fn_names: &[String],
) -> Result<Vec<(String, usize)>, XrayError> {
    Ok(scan_importers(project_root, target, path_config, target_fn_names)?.importers)
}

fn scan_importers(
    project_root: &Path,
    target: &Path,
    path_config: Option<&resolve::PathConfig>,
    target_fn_names: &[String],
) -> Result<ReverseScan, XrayError> {
    let mut scan = ReverseScan::default();
    let php_only = is_php_file(target);
    let mut configs = HashMap::new();
    let mut php_configs = HashMap::new();

    walk_project_files(project_root, &mut |file_path| {
        if php_only && !is_php_file(file_path) {
            return;
        }
        let canonical = match file_path.canonicalize() {
            Ok(path) => path,
            Err(error) => {
                scan.skipped
                    .push(format!("{}: {error}", file_path.display()));
                return;
            }
        };
        if canonical == target {
            return;
        }

        scan.scanned += 1;
        let parsed = match parser::parse_file(file_path) {
            Ok(parsed) => parsed,
            Err(error) => {
                scan.skipped.push(error.to_string());
                return;
            }
        };
        if parsed.has_syntax_errors() {
            scan.recovered += 1;
        }

        // Strategy 1: file-level include directives (\i, SOURCE, @@)
        // Include bare TS imports because a workspace path alias can look
        // exactly like an external package (for example `@workspace/ui`).
        let sources = parsed.diagnostic_specifiers();
        // Resolve each importer with its own nearest config. Reusing the
        // target package's config breaks aliases across monorepo workspaces.
        let directory = file_path.parent().unwrap_or(project_root);
        let effective_path_config = if parsed.language_kind.uses_ts_path_config() {
            configs
                .entry(directory.to_path_buf())
                .or_insert_with(|| resolve::load_path_config(directory))
                .as_ref()
                .or(path_config)
        } else {
            None
        };
        let php_paths = (parsed.language_kind == LanguageKind::Php).then(|| {
            php_configs
                .entry(directory.to_path_buf())
                .or_insert_with(|| resolve::PhpPaths::load(directory))
        });

        for specifier in &sources {
            let resolved = php_paths.as_ref().map_or_else(
                || {
                    resolve_source_specifier(
                        parsed.language_kind,
                        specifier,
                        file_path,
                        effective_path_config,
                    )
                },
                |paths| resolve::resolve_php_import_with_paths(specifier, file_path, paths),
            );
            let Some(resolved) = resolved else {
                continue;
            };

            let Ok(resolved_canonical) = resolved.canonicalize() else {
                continue;
            };
            if dependency_matches_target(parsed.language_kind, &resolved_canonical, target) {
                let rel = file_path
                    .strip_prefix(project_root)
                    .unwrap_or(file_path)
                    .to_string_lossy()
                    .into_owned();
                let lines = parsed.source.lines().count();
                scan.importers.push((rel, lines));
                return; // already matched, skip strategy 2
            }
        }

        // Strategy 2: semantic function references (SQL only).
        // When the target defines SQL functions, also match files that call them.
        if !target_fn_names.is_empty() && parsed.language_kind == LanguageKind::Sql {
            if let crate::model::FileContent::Code(file_symbols) = parsed.extract_content() {
                if symbols_reference_functions(&file_symbols, target_fn_names) {
                    let rel = file_path
                        .strip_prefix(project_root)
                        .unwrap_or(file_path)
                        .to_string_lossy()
                        .into_owned();
                    let lines = parsed.source.lines().count();
                    scan.importers.push((rel, lines));
                }
            }
        }
    })?;

    scan.importers.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(scan)
}

fn dependency_matches_target(language_kind: LanguageKind, resolved: &Path, target: &Path) -> bool {
    if resolved == target {
        return true;
    }

    language_kind == LanguageKind::Go
        && is_go_file(resolved)
        && is_go_file(target)
        && resolved.parent() == target.parent()
}

/// Directories to skip when walking the project tree.
const SKIP_DIRS: &[&str] = &[
    "node_modules",
    "vendor",
    "dist",
    "build",
    "target",
    ".next",
    ".turbo",
    "coverage",
    ".cache",
    "out",
];

/// Recursively walk supported source files under a directory.
fn walk_project_files(dir: &Path, callback: &mut dyn FnMut(&Path)) -> Result<(), XrayError> {
    if let Some(paths) = crate::git_files::visible(dir)? {
        for path in paths {
            let relative = path.strip_prefix(dir).unwrap_or(&path);
            if relative.parent().is_some_and(|parent| {
                parent
                    .components()
                    .any(|part| should_skip_dir(&part.as_os_str().to_string_lossy()))
            }) {
                continue;
            }
            if path.is_file()
                && !path
                    .ancestors()
                    .take_while(|ancestor| *ancestor != dir)
                    .any(Path::is_symlink)
                && path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .is_some_and(lang::is_supported_extension)
            {
                callback(&path);
            }
        }
        return Ok(());
    }
    // Non-Git directories are used by the standalone scanner's unit tests.
    // Production --who resolves the Git root before reaching this function.
    walk_directory(dir, callback);
    Ok(())
}

fn walk_directory(dir: &Path, callback: &mut dyn FnMut(&Path)) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    let mut dirs: Vec<PathBuf> = Vec::new();
    let mut files: Vec<PathBuf> = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if entry.file_type().is_ok_and(|kind| kind.is_symlink()) {
            continue;
        }
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
        walk_directory(dir, callback);
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

fn is_php_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .and_then(LanguageKind::for_extension)
        == Some(LanguageKind::Php)
}

fn is_go_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("go"))
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
        assert!(should_skip_dir("vendor"));
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
        std::fs::write(dir.path().join("main.py"), "def run():\n    return 1").unwrap();
        std::fs::write(dir.path().join("README.md"), "# nope").unwrap();

        let mut seen = Vec::new();
        walk_project_files(dir.path(), &mut |p| {
            let name = p.file_name().and_then(|s| s.to_str()).unwrap().to_string();
            seen.push(name);
        })
        .unwrap();

        assert!(seen.contains(&"main.sql".to_string()));
        assert!(seen.contains(&"main.ts".to_string()));
        assert!(seen.contains(&"main.py".to_string()));
        assert!(seen.contains(&"README.md".to_string()));
    }

    #[test]
    fn git_scan_includes_tracked_and_untracked_but_not_ignored_vendor_or_symlinks() {
        let dir = tempfile::tempdir().unwrap();
        assert!(Command::new("git")
            .args(["init", "-q"])
            .current_dir(dir.path())
            .status()
            .unwrap()
            .success());
        fs::create_dir(dir.path().join("vendor")).unwrap();
        fs::create_dir(dir.path().join("nested")).unwrap();
        fs::write(dir.path().join(".gitignore"), "ignored.php\ntracked.php\n").unwrap();
        fs::write(dir.path().join("nested/.gitignore"), "hidden.php\n").unwrap();
        for file in [
            "tracked.php",
            "untracked space.php",
            "ignored.php",
            "vendor/noise.php",
            "nested/hidden.php",
        ] {
            fs::write(dir.path().join(file), "<?php").unwrap();
        }
        assert!(Command::new("git")
            .args(["add", "-f", "tracked.php"])
            .current_dir(dir.path())
            .status()
            .unwrap()
            .success());
        #[cfg(unix)]
        std::os::unix::fs::symlink(dir.path(), dir.path().join("loop")).unwrap();
        let mut found = Vec::new();
        walk_project_files(dir.path(), &mut |path| {
            found.push(path.strip_prefix(dir.path()).unwrap().to_path_buf());
        })
        .unwrap();
        assert_eq!(
            found,
            vec![
                PathBuf::from("tracked.php"),
                PathBuf::from("untracked space.php")
            ]
        );
    }

    #[test]
    fn broken_git_index_does_not_fall_back_to_unfiltered_scan() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join(".git"),
            "gitdir: /nonexistent/xray-test-repository",
        )
        .unwrap();
        fs::write(dir.path().join("secret.php"), "<?php").unwrap();
        let mut files = Vec::new();
        assert!(
            walk_project_files(dir.path(), &mut |path| files.push(path.to_path_buf())).is_err()
        );
        assert!(files.is_empty());
    }

    #[test]
    fn reverse_php_resolves_composer_imports() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir(dir.path().join("app")).unwrap();
        fs::write(
            dir.path().join("composer.json"),
            r#"{"autoload":{"psr-4":{"App\\":"app/"}}}"#,
        )
        .unwrap();
        let target = dir.path().join("app/Target.php");
        fs::write(&target, "<?php namespace App; class Target {}").unwrap();
        fs::write(
            dir.path().join("app/Consumer.php"),
            "<?php use App\\Target;",
        )
        .unwrap();
        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), None, &[]).unwrap();
        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("Consumer.php"));
    }

    #[test]
    fn find_importers_detects_sql_source_importer() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("child.sql");
        let importer = dir.path().join("root.sql");

        fs::write(&target, "SELECT 1;").unwrap();
        fs::write(&importer, "SOURCE child.sql;").unwrap();

        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), None, &[]).unwrap();
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
        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), Some(&cfg), &[]).unwrap();

        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("main.ts"));
        assert_eq!(importers[0].1, 2);
    }

    #[test]
    fn find_importers_uses_each_workspaces_nearest_tsconfig() {
        let dir = tempfile::tempdir().unwrap();
        let package_src = dir.path().join("packages/ui/src");
        let app = dir.path().join("apps/web");
        fs::create_dir_all(&package_src).unwrap();
        fs::create_dir_all(&app).unwrap();
        fs::write(
            app.join("tsconfig.json"),
            r#"{
  "compilerOptions": {
    "paths": { "@ui/*": ["../../packages/ui/src/*"] }
  }
}"#,
        )
        .unwrap();

        let target = package_src.join("button.ts");
        fs::write(&target, "export const Button = 1;").unwrap();
        fs::write(
            app.join("page.ts"),
            "import { Button } from '@ui/button';\nexport { Button };",
        )
        .unwrap();

        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), None, &[]).unwrap();
        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("apps/web/page.ts"));
    }

    #[test]
    fn find_importers_detects_go_package_import_for_any_file_in_package() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("go.mod"), "module example.com/app\n").unwrap();
        let pkg = dir.path().join("internal/shell");
        fs::create_dir_all(&pkg).unwrap();
        fs::write(pkg.join("aaa.go"), "package shell\n").unwrap();
        let target = pkg.join("shell.go");
        fs::write(&target, "package shell\ntype Options struct{}\n").unwrap();
        let importer_dir = dir.path().join("internal/tools");
        fs::create_dir_all(&importer_dir).unwrap();
        fs::write(
            importer_dir.join("bash.go"),
            "package tools\nimport \"example.com/app/internal/shell\"\nvar _ = shell.Options{}\n",
        )
        .unwrap();

        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), None, &[]).unwrap();

        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("internal/tools/bash.go"));
        assert_eq!(importers[0].1, 3);
    }

    #[test]
    fn find_importers_detects_markdown_linker() {
        let dir = tempfile::tempdir().unwrap();
        let docs = dir.path().join("docs");
        fs::create_dir(&docs).unwrap();
        let target = docs.join("guide.md");
        let importer = dir.path().join("README.md");

        fs::write(&target, "# Guide").unwrap();
        fs::write(&importer, "[Guide](docs/guide.md#intro)").unwrap();

        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), None, &[]).unwrap();
        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("README.md"));
        assert_eq!(importers[0].1, 1);
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
        let importers =
            find_importers(dir.path(), &target.canonicalize().unwrap(), Some(&cfg), &[]).unwrap();

        assert!(importers.is_empty());
    }

    #[test]
    fn extract_sql_function_name_from_create_or_replace() {
        assert_eq!(
            extract_sql_function_name("CREATE OR REPLACE FUNCTION trigger_handler FROM tenants"),
            Some("trigger_handler")
        );
    }

    #[test]
    fn extract_sql_function_name_from_create() {
        assert_eq!(
            extract_sql_function_name("CREATE FUNCTION get_id FROM users"),
            Some("get_id")
        );
    }

    #[test]
    fn extract_sql_function_name_returns_none_for_non_function() {
        assert!(extract_sql_function_name("CREATE TABLE users").is_none());
        assert!(extract_sql_function_name("SELECT 1").is_none());
    }

    #[test]
    fn extract_target_function_names_returns_empty_for_ts() {
        let digest = FileDigest {
            syntax_recovered: false,
            display_path: "src/main.ts".to_string(),
            language_kind: LanguageKind::Ts,
            ext: "ts".to_string(),
            total_lines: 10,
            content: crate::model::FileContent::Code(crate::model::FileSymbols {
                imports: Vec::new(),
                import_bindings: Vec::new(),
                reexports: Vec::new(),
                exports: Vec::new(),
                internals: Vec::new(),
                types: Vec::new(),
                tests: Vec::new(),
                hooks: Vec::new(),
            }),
            diagnostic_specifiers: Vec::new(),
        };
        assert!(extract_target_function_names(&digest).is_empty());
    }

    #[test]
    fn extract_target_function_names_extracts_from_internals() {
        let digest = FileDigest {
            syntax_recovered: false,
            display_path: "ddl.sql".to_string(),
            language_kind: LanguageKind::Sql,
            ext: "sql".to_string(),
            total_lines: 50,
            content: crate::model::FileContent::Code(crate::model::FileSymbols {
                imports: Vec::new(),
                import_bindings: Vec::new(),
                reexports: Vec::new(),
                exports: Vec::new(),
                internals: vec![
                    crate::model::Symbol {
                        signature: "CREATE OR REPLACE FUNCTION get_tenant_id FROM tenants"
                            .to_string(),
                        line_start: 1,
                        line_end: 20,
                        calls: vec!["target:get_tenant_id".to_string()],
                        is_component: false,
                        renders: Vec::new(),
                        hooks: Vec::new(),
                        handlers: Vec::new(),
                        decorators: Vec::new(),
                    },
                    crate::model::Symbol {
                        signature: "CREATE TABLE objects".to_string(),
                        line_start: 22,
                        line_end: 30,
                        calls: vec!["target:objects".to_string()],
                        is_component: false,
                        renders: Vec::new(),
                        hooks: Vec::new(),
                        handlers: Vec::new(),
                        decorators: Vec::new(),
                    },
                ],
                types: Vec::new(),
                tests: Vec::new(),
                hooks: Vec::new(),
            }),
            diagnostic_specifiers: Vec::new(),
        };

        let names = extract_target_function_names(&digest);
        assert_eq!(names, vec!["get_tenant_id"]);
    }

    #[test]
    fn find_importers_detects_sql_function_caller() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("functions.sql");
        let caller = dir.path().join("queries.sql");

        fs::write(
            &target,
            "CREATE OR REPLACE FUNCTION get_tenant_id()\nRETURNS INT\nLANGUAGE plpgsql AS $$\nBEGIN\n  RETURN 1;\nEND;\n$$;",
        ).unwrap();
        fs::write(&caller, "SELECT get_tenant_id(), name FROM tenants;").unwrap();

        let target_fn_names = vec!["get_tenant_id".to_string()];
        let importers = find_importers(
            dir.path(),
            &target.canonicalize().unwrap(),
            None,
            &target_fn_names,
        )
        .unwrap();

        assert_eq!(importers.len(), 1);
        assert!(importers[0].0.ends_with("queries.sql"));
    }

    #[test]
    fn find_importers_sql_function_does_not_match_unrelated() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("functions.sql");
        let unrelated = dir.path().join("other.sql");

        fs::write(
            &target,
            "CREATE FUNCTION get_id() LANGUAGE plpgsql AS $$ BEGIN RETURN 1; END; $$;",
        )
        .unwrap();
        fs::write(&unrelated, "SELECT name FROM users;").unwrap();

        let target_fn_names = vec!["get_id".to_string()];
        let importers = find_importers(
            dir.path(),
            &target.canonicalize().unwrap(),
            None,
            &target_fn_names,
        )
        .unwrap();

        assert!(importers.is_empty());
    }

    #[test]
    fn find_importers_ts_files_skip_sql_function_matching() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("functions.sql");
        let ts_file = dir.path().join("app.ts");

        fs::write(
            &target,
            "CREATE FUNCTION my_func() LANGUAGE plpgsql AS $$ BEGIN RETURN 1; END; $$;",
        )
        .unwrap();
        // TS file happens to contain the text "my_func" but strategy 2 only runs for SQL files.
        fs::write(&ts_file, "const my_func = () => 1;\nexport { my_func };").unwrap();

        let target_fn_names = vec!["my_func".to_string()];
        let importers = find_importers(
            dir.path(),
            &target.canonicalize().unwrap(),
            None,
            &target_fn_names,
        )
        .unwrap();

        assert!(importers.is_empty());
    }
}
