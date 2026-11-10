//! Bounded, read-only convention candidates kept separate from exact LSP results.
use std::collections::HashSet;
use std::path::Path;
use std::time::{Duration, Instant};

use crate::error::XrayError;
use crate::extract::php_conventions::{self as extract, Target};
use crate::lsp::{failure, read_source};
use crate::navigation::{ConventionCandidate, ConventionResult, NavigationLocation};
use crate::parser::{self, ParsedContent};

pub(crate) fn collect(
    root: &Path,
    file: &Path,
    line: usize,
    column: usize,
    exact: &[NavigationLocation],
) -> Result<Option<ConventionResult>, XrayError> {
    let deadline = Instant::now() + Duration::from_secs(15);
    let parsed = parser::parse_source(file, read_source(file)?, Some(deadline))?;
    let ParsedContent::Code(tree) = &parsed.content else {
        return Ok(None);
    };
    let Some(target) = extract::target(
        tree.root_node(),
        parsed.source.as_bytes(),
        tree_sitter::Point::new(line - 1, column - 1),
    ) else {
        return Ok(None);
    };
    let mut report = ConventionResult {
        kind: target.label().into(),
        target_class: target.owner.clone(),
        status: "complete".into(),
        coverage: "convention candidates only; Git-visible non-vendor PHP; scope receiver types unverified; handler construction/class references are not proof of dispatch; listener event types do not prove registration; not runtime callers".into(),
        count: 0,
        omitted: 0,
        scanned: 0,
        parser_recovery_files: Vec::new(),
        warnings: Vec::new(),
        results: Vec::new(),
    };
    if parsed.has_syntax_errors() {
        report.parser_recovery_files.push(file.to_path_buf());
    }
    if let Err(error) = scan(root, &target, &mut report, 50_000, 10_000) {
        report.status = if report.results.is_empty() {
            "failed"
        } else {
            "partial"
        }
        .into();
        report.warnings.push(error.to_string());
    }
    let exact: HashSet<_> = exact.iter().map(key).collect();
    report
        .results
        .retain(|candidate| !exact.contains(&key(&candidate.location)));
    report.results.sort_by(|a, b| {
        (a.kind != "scope-receiver-match")
            .cmp(&(b.kind != "scope-receiver-match"))
            .then_with(|| key(&a.location).cmp(&key(&b.location)))
            .then_with(|| a.kind.cmp(&b.kind))
    });
    report
        .results
        .dedup_by(|a, b| key(&a.location) == key(&b.location) && a.kind == b.kind);
    report.parser_recovery_files.sort();
    report.parser_recovery_files.dedup();
    report.count = report.results.len();
    if report.status == "complete"
        && (!report.warnings.is_empty() || !report.parser_recovery_files.is_empty())
    {
        report.status = "partial".into();
    }
    Ok(Some(report))
}

fn key(location: &NavigationLocation) -> (&Path, usize, usize, usize, usize) {
    (
        &location.file,
        location.line,
        location.column,
        location.end_line,
        location.end_column,
    )
}

fn scan(
    root: &Path,
    target: &Target,
    report: &mut ConventionResult,
    max_files: usize,
    max_results: usize,
) -> Result<(), XrayError> {
    let deadline = Instant::now() + Duration::from_secs(15);
    let files = crate::git_files::visible(root)?.ok_or_else(|| {
        failure("convention candidates require a Git workspace; exact LSP results are unchanged")
    })?;
    let mut candidate_bytes = 0;
    for file in files {
        if !eligible(root, &file) {
            continue;
        }
        if report.scanned >= max_files
            || Instant::now() >= deadline
            || report.warnings.len() >= 64
            || report.parser_recovery_files.len() >= 256
        {
            return Err(failure("convention scan file/deadline limit exceeded"));
        }
        report.scanned += 1;
        let mut limited = false;
        let result = (|| {
            let source = read_source(&file)?;
            if !target.may_match(&source) {
                return Ok(());
            }
            let parsed = parser::parse_source(&file, source, Some(deadline))?;
            if parsed.has_syntax_errors() {
                report.parser_recovery_files.push(file.clone());
            }
            let ParsedContent::Code(tree) = &parsed.content else {
                return Ok(());
            };
            let complete = extract::candidates(
                tree.root_node(),
                parsed.source.as_bytes(),
                target,
                deadline,
                |candidate| {
                    let candidate = ConventionCandidate {
                        kind: candidate.kind.into(),
                        subject: candidate.subject,
                        provenance: "syntax:laravel-convention-candidate".into(),
                        location: NavigationLocation {
                            file: file.clone(),
                            line: candidate.start.row + 1,
                            column: candidate.start.column + 1,
                            end_line: candidate.end.row + 1,
                            end_column: candidate.end.column + 1,
                        },
                    };
                    let Ok(bytes) = serde_json::to_vec(&candidate) else {
                        limited = true;
                        return false;
                    };
                    if report.results.len() >= max_results
                        || candidate_bytes + bytes.len() > 512 * 1024
                    {
                        limited = true;
                        return false;
                    }
                    candidate_bytes += bytes.len();
                    report.results.push(candidate);
                    true
                },
            );
            if !complete {
                limited = true;
                return Err(failure(
                    "convention scan candidate/byte/deadline limit exceeded",
                ));
            }
            Ok(())
        })();
        if let Err(error) = result {
            report.warnings.push(format!("{}: {error}", file.display()));
            if limited || Instant::now() >= deadline {
                return Err(failure("convention scan candidate limit exceeded"));
            }
        }
    }
    if Instant::now() >= deadline {
        return Err(failure("convention scan deadline exceeded"));
    }
    Ok(())
}

fn eligible(root: &Path, file: &Path) -> bool {
    file.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("php"))
        && file.is_file()
        && !file
            .ancestors()
            .take_while(|ancestor| *ancestor != root)
            .any(Path::is_symlink)
        && file.strip_prefix(root).is_ok_and(|path| {
            !path.parent().is_some_and(|parent| {
                parent.components().any(|part| {
                    let name = part.as_os_str().to_string_lossy();
                    name.starts_with('.') || matches!(name.as_ref(), "vendor" | "node_modules")
                })
            })
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process::Command;

    #[test]
    fn convention_scan_keeps_exact_results_separate_and_omits_ignored_and_vendor_php() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        assert!(Command::new("git")
            .args(["init", "-q"])
            .current_dir(root)
            .status()
            .unwrap()
            .success());
        fs::write(root.join(".gitignore"), "cache/\n").unwrap();
        fs::create_dir(root.join("cache")).unwrap();
        fs::create_dir(root.join("vendor")).unwrap();
        let model = root.join("Model.php");
        let text = "<?php namespace App\\Models; class Model { function scopeActive($q) {} }";
        fs::write(&model, text).unwrap();
        for path in ["use.php", "cache/ignored.php", "vendor/dependency.php"] {
            fs::write(root.join(path), "<?php $q->active();").unwrap();
        }
        let column = text.find("scopeActive").unwrap() + 1;
        let mut result = collect(root, &model, 1, column, &[]).unwrap().unwrap();
        assert_eq!(result.status, "complete");
        assert_eq!(result.count, 1);
        assert_eq!(result.results[0].kind, "scope-name-match");
        let target = Target {
            owner: "App\\Models\\Model".into(),
            kind: extract::Kind::Scope("active".into()),
        };
        assert!(scan(root, &target, &mut result, 0, 10)
            .unwrap_err()
            .to_string()
            .contains("file/deadline"));
        result.scanned = 0;
        result.results.clear();
        assert!(scan(root, &target, &mut result, 10, 0)
            .unwrap_err()
            .to_string()
            .contains("candidate limit"));
        let exact = vec![NavigationLocation {
            file: root.join("use.php"),
            line: 1,
            column: 11,
            end_line: 1,
            end_column: 17,
        }];
        assert_eq!(
            collect(root, &model, 1, column, &exact)
                .unwrap()
                .unwrap()
                .count,
            0
        );
        fs::write(
            root.join("cache/ignored.php"),
            "<?php $q->active(); $q->active();",
        )
        .unwrap();
        assert_eq!(
            collect(root, &model, 1, column, &[])
                .unwrap()
                .unwrap()
                .count,
            1
        );
    }

    #[test]
    fn convention_parse_observes_expired_deadlines() {
        assert!(parser::parse_source(
            Path::new("scope.php"),
            "<?php class X {}".into(),
            Some(Instant::now())
        )
        .err()
        .unwrap()
        .to_string()
        .contains("deadline"));
    }

    #[test]
    fn broken_git_index_is_reported_without_losing_exact_results() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("Job.php");
        let source = "<?php namespace App\\Jobs; class Job { function handle() {} }";
        fs::write(&file, source).unwrap();
        fs::write(
            dir.path().join(".git"),
            "gitdir: /nonexistent/xray-conventions",
        )
        .unwrap();
        let result = collect(
            dir.path(),
            &file,
            1,
            source.find("handle").unwrap() + 1,
            &[],
        )
        .unwrap()
        .unwrap();
        assert_eq!(result.status, "failed");
        assert!(result.warnings[0].contains("refusing an unfiltered"));
    }
}
