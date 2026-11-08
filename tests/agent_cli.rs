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
fn invalid_vue_plugin_override_is_an_explicit_error_before_server_startup() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/vue-lsp/Panel.vue");
    let output = Command::new(env!("CARGO_BIN_EXE_xray"))
        .args(["--trace", "--lsp", "-s", "submit"])
        .arg(fixture)
        .env("XRAY_VUE_LANGUAGE_SERVER", dir.path())
        .output()
        .unwrap();
    assert!(!output.status.success());
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        text.contains("not an @vue/language-server package"),
        "{text}"
    );
}

#[test]
fn trace_header_does_not_hide_entry_parser_recovery() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("broken.go"),
        "package main\nfunc Run() {}\nfunc broken(\n",
    )
    .unwrap();
    let output = run(dir.path(), &["--trace", "-s", "Run", "broken.go"]);
    assert!(output.status.success());
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("status=partial"), "{text}");
    assert!(text.contains("parser recovery"), "{text}");
    assert!(text.contains("trace: Run"), "{text}");
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
fn template_trees_are_complete_in_json_and_recoverable_after_overflow() {
    let dir = tempfile::tempdir().unwrap();
    let mut tags = String::new();
    for n in 0..80 {
        let _ = write!(tags, "<Component{n:03}><Nested{n:03} /></Component{n:03}>");
    }
    for (extension, source) in [
        ("vue", format!("<template><div>{tags}</div></template>")),
        ("svelte", format!("<div>{tags}</div>")),
    ] {
        let mut calls = String::new();
        for n in 0..24 {
            let expression = format!("handler{n:03}()");
            if extension == "vue" {
                let _ = write!(calls, "{{{{ {expression} }}}}");
            } else {
                let _ = write!(calls, "{{ {expression} }}");
            }
        }
        let source = source.replace("</div>", &format!("{calls}</div>"));
        let name = format!("wide.{extension}");
        fs::write(dir.path().join(&name), source).unwrap();
        let full_output = run(
            dir.path(),
            &["--format", "json", "--max-bytes", "65536", &name],
        );
        assert!(full_output.status.success());
        let expected = json(&full_output);
        let renders = &expected["file"]["content"]["data"]["exports"][0]["renders"];
        assert_eq!(renders.as_array().unwrap().len(), 80);
        assert_eq!(renders[79]["name"], "Component079");
        assert_eq!(renders[79]["children"][0]["name"], "Nested079");
        let references = &expected["file"]["content"]["data"]["exports"][0]["references"];
        assert_eq!(references.as_array().unwrap().len(), 24);
        assert_eq!(references[23]["target"], "handler023");

        let output = run(
            dir.path(),
            &["--format", "json", "--max-bytes", "1024", &name],
        );
        assert!(output.status.success());
        assert!(output.stdout.len() <= 1024);
        let bounded = json(&output);
        assert_eq!(bounded["status"], "partial");
        let path = Path::new(bounded["truncation"]["artifact"].as_str().unwrap());
        let recovered: Value = serde_json::from_slice(&fs::read(path).unwrap()).unwrap();
        assert_eq!(recovered, expected);
        fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }
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
fn go_follow_lists_the_whole_package_and_reverse_matches_a_nonfirst_file() {
    let dir = tempfile::tempdir().unwrap();
    assert!(Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(dir.path())
        .status()
        .unwrap()
        .success());
    fs::write(dir.path().join("go.mod"), "module example.org/app\n").unwrap();
    fs::create_dir(dir.path().join("worker")).unwrap();
    fs::write(
        dir.path().join("main.go"),
        "package main\nimport \"example.org/app/worker\"\nfunc main() { worker.Run() }",
    )
    .unwrap();
    for (name, body) in [
        ("a.go", "package worker\nfunc Prepare() {}"),
        ("z.go", "package worker\nfunc Run() {}"),
        ("z_test.go", "package worker\nfunc TestRun() {}"),
        ("_ignored.go", "package worker\nfunc Ignored() {}"),
    ] {
        fs::write(dir.path().join("worker").join(name), body).unwrap();
    }
    let output = run(
        dir.path(),
        &["--follow", "--all", "--format", "json", "main.go"],
    );
    assert!(output.status.success());
    let data = json(&output);
    assert_eq!(data["results"].as_array().unwrap().len(), 2, "{data}");
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("a.go") && text.contains("z.go"));
    assert!(!text.contains("_ignored.go") && !text.contains("z_test.go"));
    let reverse = run(dir.path(), &["--who", "--format", "json", "worker/z.go"]);
    assert!(reverse.status.success());
    assert_eq!(json(&reverse)["results"][0]["path"], "main.go");
}

#[test]
fn sql_source_column_does_not_create_a_follow_dependency() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("schema.sql"),
        "CREATE TABLE records (\n source VARCHAR NOT NULL\n);\nSOURCE seed.sql;",
    )
    .unwrap();
    fs::write(dir.path().join("seed.sql"), "SELECT 1;").unwrap();
    let output = run(dir.path(), &["--follow", "--format", "json", "schema.sql"]);
    assert!(output.status.success());
    let data = json(&output);
    assert_eq!(data["resolution"]["resolved"], 1);
    assert_eq!(data["resolution"]["unresolved"], 0);
    assert_eq!(data["results"].as_array().unwrap().len(), 1);
}

#[test]
fn go_trace_does_not_consume_unrelated_broken_tsconfig() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("tsconfig.json"), "broken config").unwrap();
    fs::write(
        dir.path().join("entry.go"),
        "package main\nfunc Run() { helper() }\nfunc helper() {}\n",
    )
    .unwrap();
    let output = run(dir.path(), &["--trace", "-s", "Run", "entry.go"]);
    assert!(output.status.success());
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("helper  (local)"), "{text}");
    assert!(!text.contains("warning:"), "{text}");
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
