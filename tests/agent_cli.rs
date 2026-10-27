//! Agent-interface regressions: valid JSON, explicit states, bounded output, recoverable evidence.
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use serde_json::Value;

fn run(root: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_xray"))
        .current_dir(root)
        .args(args)
        .output()
        .unwrap()
}

fn json(output: &Output) -> Value {
    serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|error| panic!("{error}: {}", String::from_utf8_lossy(&output.stdout)))
}

#[test]
fn argument_and_file_failures_are_machine_readable_and_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    for args in [
        vec!["--format", "json", "missing.php"],
        vec![
            "--definition",
            "--format",
            "json",
            "--at",
            "0:1",
            "file.php",
        ],
    ] {
        let output = run(dir.path(), &args);
        assert!(!output.status.success());
        assert_eq!(json(&output)["status"], "failed");
        assert!(output.stdout.len() <= 16384);
    }
}

#[test]
fn syntax_recovery_is_partial_not_a_successful_completeness_claim() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("broken.php"), "<?php function broken(").unwrap();
    let output = run(dir.path(), &["--format", "json", "broken.php"]);
    assert!(output.status.success());
    let data = json(&output);
    assert_eq!(data["status"], "partial");
    assert_eq!(data["syntax_recovered"], true);
}

#[test]
fn oversized_digest_keeps_valid_json_and_exact_private_overflow() {
    let dir = tempfile::tempdir().unwrap();
    let mut source = "<?php\n".to_string();
    for n in 0..150 {
        let _ = writeln!(source, "function method{n}(): void {{}}");
    }
    fs::write(dir.path().join("large.php"), source).unwrap();
    let output = run(
        dir.path(),
        &["--format", "json", "--max-bytes", "1024", "large.php"],
    );
    assert!(output.status.success());
    assert!(output.stdout.len() <= 1024);
    let data = json(&output);
    assert_eq!(data["status"], "partial");
    let path = Path::new(data["truncation"]["artifact"].as_str().unwrap());
    let full: Value = serde_json::from_slice(&fs::read(path).unwrap()).unwrap();
    assert_eq!(
        full["file"]["content"]["data"]["exports"]
            .as_array()
            .unwrap()
            .len(),
        150
    );
    assert_eq!(full["status"], "complete");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(
            fs::metadata(path).unwrap().permissions().mode() & 0o777,
            0o600
        );
    }
    fs::remove_dir_all(path.parent().unwrap()).unwrap();
}

#[test]
fn zero_depth_follow_does_not_emit_child_files() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("entry.php"), "<?php require './dep.php';").unwrap();
    fs::write(dir.path().join("dep.php"), "<?php function dep() {}").unwrap();
    let output = run(
        dir.path(),
        &["--follow", "--depth", "0", "--format", "json", "entry.php"],
    );
    assert!(output.status.success());
    assert_eq!(json(&output)["results"].as_array().unwrap().len(), 0);
}

#[test]
fn reverse_reports_empty_scope_without_echoing_target_digest() {
    let dir = tempfile::tempdir().unwrap();
    assert!(Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(dir.path())
        .status()
        .unwrap()
        .success());
    fs::write(dir.path().join("entry.php"), "<?php class Alone {}").unwrap();
    let output = run(dir.path(), &["--who", "--format", "json", "entry.php"]);
    assert!(output.status.success());
    let data = json(&output);
    assert_eq!(data["status"], "complete");
    assert_eq!(data["count"], 0);
    assert_eq!(data["results"], serde_json::json!([]));
    assert!(data.get("file").is_none());
}

#[test]
fn php_follow_does_not_consume_unrelated_broken_tsconfig() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("tsconfig.json"), "broken config").unwrap();
    fs::write(dir.path().join("entry.php"), "<?php class Entry {}").unwrap();
    let output = run(dir.path(), &["--follow", "--format", "json", "entry.php"]);
    assert!(output.status.success());
    assert_eq!(json(&output)["status"], "complete");
    assert!(output.stderr.is_empty());
}

#[test]
fn php_reverse_finds_same_namespace_class_consumers() {
    let dir = tempfile::tempdir().unwrap();
    assert!(Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(dir.path())
        .status()
        .unwrap()
        .success());
    fs::create_dir(dir.path().join("app")).unwrap();
    fs::write(
        dir.path().join("composer.json"),
        r#"{"autoload":{"psr-4":{"App\\":"app/"}}}"#,
    )
    .unwrap();
    fs::write(
        dir.path().join("app/Target.php"),
        "<?php namespace App; class Target {}",
    )
    .unwrap();
    fs::write(dir.path().join("app/Consumer.php"), "<?php namespace App; class Consumer { public function relation() { return Target::class; } }").unwrap();
    let output = run(dir.path(), &["--who", "--format", "json", "app/Target.php"]);
    assert!(output.status.success());
    let data = json(&output);
    assert_eq!(data["count"], 1);
    assert_eq!(data["results"][0]["path"], "app/Consumer.php");
}
