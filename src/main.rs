//! Structural navigation with bounded, evidence-bearing output for agents.
mod error;
mod extract;
mod follow;
mod git_files;
mod lang;
#[cfg_attr(not(unix), path = "lsp_unsupported.rs")]
mod lsp;
mod model;
mod navigation;
mod output;
mod parser;
#[cfg(unix)]
mod php_conventions;
#[cfg(unix)]
mod php_workspace;
mod presentation;
mod project;
mod resolve;
mod reverse;
mod trace;
mod util;

use std::fmt::Write as _;
use std::io::Write;
use std::path::Path;

use error::XrayError;
use output::FileDigest;
use presentation::Report;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    Follow,
    NoFollow,
    Who,
    Trace,
    Definition,
    References,
    Implementations,
    TypeDefinition,
    Declaration,
    Supertypes,
    Subtypes,
}

impl Mode {
    fn navigation(self) -> Option<(navigation::QueryKind, &'static str)> {
        use navigation::QueryKind;
        match self {
            Self::Definition => Some((QueryKind::Definition, "definition")),
            Self::References => Some((QueryKind::References, "references")),
            Self::Implementations => Some((QueryKind::Implementations, "implementations")),
            Self::TypeDefinition => Some((QueryKind::TypeDefinition, "type-definition")),
            Self::Declaration => Some((QueryKind::Declaration, "declaration")),
            Self::Supertypes => Some((QueryKind::Supertypes, "supertypes")),
            Self::Subtypes => Some((QueryKind::Subtypes, "subtypes")),
            Self::Follow | Self::NoFollow | Self::Who | Self::Trace => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OutputFormat {
    Text,
    Json,
}

struct CliArgs {
    mode: Mode,
    target_symbol: Option<String>,
    depth: Option<usize>,
    show_all: bool,
    use_lsp: bool,
    format: OutputFormat,
    explain: bool,
    at: Option<(usize, usize)>,
    max_bytes: usize,
    files: Vec<String>,
}

fn next_value<'a>(args: &'a [String], index: &mut usize) -> Result<&'a str, String> {
    let flag = &args[*index];
    *index += 1;
    args.get(*index)
        .map(String::as_str)
        .ok_or_else(|| format!("{flag} requires a value"))
}

fn parse_position(value: &str) -> Result<(usize, usize), String> {
    let invalid = || "--at requires LINE:COL (both >= 1; UTF-8 byte column)".to_string();
    let (line, column) = value.split_once(':').ok_or_else(invalid)?;
    let line = line.parse::<usize>().map_err(|_| invalid())?;
    let column = column.parse::<usize>().map_err(|_| invalid())?;
    if line == 0 || column == 0 {
        return Err(invalid());
    }
    Ok((line, column))
}

fn parse_args(raw: &[String]) -> Result<CliArgs, String> {
    let mut args = CliArgs {
        mode: Mode::Follow,
        target_symbol: None,
        depth: None,
        show_all: false,
        use_lsp: false,
        format: OutputFormat::Text,
        explain: false,
        at: None,
        max_bytes: presentation::DEFAULT_MAX_BYTES,
        files: Vec::new(),
    };
    let mut modes = Vec::new();
    let mut i = 0;
    while i < raw.len() {
        match raw[i].as_str() {
            "--follow" => modes.push(Mode::Follow),
            "--no-follow" => modes.push(Mode::NoFollow),
            "--who" => modes.push(Mode::Who),
            "--trace" => modes.push(Mode::Trace),
            "--definition" => modes.push(Mode::Definition),
            "--references" => modes.push(Mode::References),
            "--implementations" => modes.push(Mode::Implementations),
            "--type-definition" => modes.push(Mode::TypeDefinition),
            "--declaration" => modes.push(Mode::Declaration),
            "--supertypes" => modes.push(Mode::Supertypes),
            "--subtypes" => modes.push(Mode::Subtypes),
            "--all" => args.show_all = true,
            "--lsp" => args.use_lsp = true,
            "--explain" | "--stats" => args.explain = true,
            "--at" => args.at = Some(parse_position(next_value(raw, &mut i)?)?),
            "--symbol" | "-s" => args.target_symbol = Some(next_value(raw, &mut i)?.to_string()),
            "--depth" | "-d" => {
                let value = next_value(raw, &mut i)?;
                args.depth = Some(
                    value
                        .parse()
                        .map_err(|_| format!("--depth: invalid number '{value}'"))?,
                );
            }
            "--max-bytes" => {
                let value = next_value(raw, &mut i)?;
                args.max_bytes = value
                    .parse()
                    .map_err(|_| "--max-bytes requires an integer".to_string())?;
            }
            "--format" => {
                args.format = match next_value(raw, &mut i)? {
                    "text" => OutputFormat::Text,
                    "json" => OutputFormat::Json,
                    value => return Err(format!("--format: unsupported value '{value}'")),
                };
            }
            "--" => {
                args.files.extend_from_slice(&raw[i + 1..]);
                break;
            }
            flag if flag.starts_with('-') => return Err(format!("unknown option: {flag}")),
            file => args.files.push(file.to_string()),
        }
        i += 1;
    }
    if modes.len() > 1 {
        return Err(
            "choose one mode: --follow, --no-follow, --who, --trace, --definition, --references, \
             --implementations, --type-definition, --declaration, --supertypes, --subtypes"
                .into(),
        );
    }
    args.mode = modes
        .first()
        .copied()
        .unwrap_or(if args.format == OutputFormat::Json {
            Mode::NoFollow
        } else {
            Mode::Follow
        });
    validate_args(&args)?;
    Ok(args)
}

fn validate_args(args: &CliArgs) -> Result<(), String> {
    let navigation = args.mode.navigation();
    if let Some((_, mode)) = navigation {
        if args.at.is_none() || args.files.len() != 1 {
            return Err(format!(
                "--{mode} requires --at LINE:COL and exactly one file"
            ));
        }
    }
    if args.at.is_some() && navigation.is_none() {
        return Err(
            "--at requires a navigation mode: --definition, --references, --implementations, \
                    --type-definition, --declaration, --supertypes, or --subtypes"
                .into(),
        );
    }
    if args.depth.is_some() && !matches!(args.mode, Mode::Follow | Mode::Trace) {
        return Err("--depth requires follow or trace mode".into());
    }
    if args.depth.is_some_and(|depth| depth > 32) {
        return Err("--depth must be between 0 and 32".into());
    }
    if args.target_symbol.is_some() && args.mode != Mode::Trace {
        return Err("--symbol requires --trace".into());
    }
    if args.use_lsp && args.mode != Mode::Trace {
        return Err("--lsp requires --trace; navigation uses LSP automatically".into());
    }
    if args.explain && args.mode != Mode::Follow {
        return Err("--explain requires follow mode".into());
    }
    if args.show_all && !matches!(args.mode, Mode::Follow | Mode::Trace) {
        return Err("--all requires follow or trace mode".into());
    }
    if args.format == OutputFormat::Json && args.files.len() > 1 {
        return Err("--format json requires exactly one file".into());
    }
    if args.format == OutputFormat::Json && args.mode == Mode::Trace {
        return Err(
            "--trace does not support JSON; use a position-based mode for structured navigation"
                .into(),
        );
    }
    if !(presentation::MIN_MAX_BYTES..=presentation::MAX_MAX_BYTES).contains(&args.max_bytes) {
        return Err("--max-bytes must be between 1024 and 1048576".into());
    }
    Ok(())
}

fn main() {
    let raw: Vec<String> = std::env::args().skip(1).collect();
    if raw.first().is_some_and(|arg| arg == "--lsp-worker") {
        let result = match raw.as_slice() {
            [_, root] => navigation::run_worker(Path::new(root)),
            _ => Err(XrayError::Lsp("invalid internal worker invocation".into())),
        };
        if let Err(error) = result {
            eprintln!("{error}");
            std::process::exit(1);
        }
        return;
    }
    if raw.is_empty() || matches!(raw.first().map(String::as_str), Some("-h" | "--help")) {
        print_help();
        return;
    }
    if matches!(raw.first().map(String::as_str), Some("-V" | "--version")) {
        println!("xray {}", env!("CARGO_PKG_VERSION"));
        return;
    }
    let args = match parse_args(&raw) {
        Ok(args) if !args.files.is_empty() => args,
        result => {
            let error = result.err().unwrap_or_else(|| "no files specified".into());
            let json = raw.windows(2).any(|pair| pair == ["--format", "json"]);
            let report = failure_report("arguments", &error);
            let _ = emit(
                &report,
                if json {
                    OutputFormat::Json
                } else {
                    OutputFormat::Text
                },
                presentation::DEFAULT_MAX_BYTES,
            );
            std::process::exit(1);
        }
    };
    let mut failed = false;
    let mut combined = Report {
        text: String::new(),
        json: serde_json::Value::Null,
    };
    for path in &args.files {
        let mut report = match process_file(path, &args) {
            Ok(report) => report,
            Err(error) => {
                failed = true;
                failure_report(path, &error.to_string())
            }
        };
        presentation::attach_warnings(&mut report);
        if !combined.text.is_empty() {
            combined.text.push_str("\n---\n");
        }
        combined.text.push_str(&report.text);
        combined.json = report.json;
    }
    if failed && args.files.len() > 1 {
        combined.text.insert_str(
            0,
            "status=failed; one or more files failed (details may be in overflow)\n",
        );
    }
    if let Err(error) = emit(&combined, args.format, args.max_bytes) {
        eprintln!("xray: {error}");
        failed = true;
    }
    if failed {
        std::process::exit(1);
    }
}

fn failure_report(target: &str, error: &str) -> Report {
    Report {
        text: format!("status=failed target={target:?}\nerror: {error}\n"),
        json: serde_json::json!({"schema_version":1,"status":"failed","target":target,"error":error}),
    }
}

fn emit(report: &Report, format: OutputFormat, budget: usize) -> Result<(), XrayError> {
    let output = match format {
        OutputFormat::Text => presentation::bounded_text(&report.text, budget)?,
        OutputFormat::Json => presentation::bounded_json(report.json.clone(), budget)?,
    };
    match std::io::stdout().lock().write_all(output.as_bytes()) {
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
        result => result.map_err(|error| XrayError::Output(error.to_string())),
    }
}

fn process_file(path: &str, args: &CliArgs) -> Result<Report, XrayError> {
    match args.mode {
        Mode::Definition
        | Mode::References
        | Mode::Implementations
        | Mode::TypeDefinition
        | Mode::Declaration
        | Mode::Supertypes
        | Mode::Subtypes => navigation_report(path, args),
        Mode::NoFollow => {
            let digest = FileDigest::from_path(Path::new(path))?;
            Ok(Report {
                text: digest.to_string(),
                json: digest.to_value(),
            })
        }
        Mode::Who => reverse::run(path),
        Mode::Follow => follow::run(
            path,
            &follow::FollowConfig {
                max_depth: args.depth.unwrap_or(1),
                show_all: args.show_all,
                explain: args.explain,
            },
        ),
        Mode::Trace => {
            let text = trace::render(
                path,
                &trace::TraceConfig {
                    max_depth: args.depth.unwrap_or(3),
                    show_all: args.show_all,
                    target_symbol: args.target_symbol.clone(),
                    use_lsp: args.use_lsp,
                },
            )?;
            Ok(Report {
                text,
                json: serde_json::Value::Null,
            })
        }
    }
}

fn navigation_report(path: &str, args: &CliArgs) -> Result<Report, XrayError> {
    let (line, column) = args
        .at
        .ok_or_else(|| XrayError::Lsp("missing position".into()))?;
    let (kind, mode) = args
        .mode
        .navigation()
        .ok_or_else(|| XrayError::Lsp("not a navigation mode".into()))?;
    let result = navigation::query(Path::new(path), line, column, kind)?;
    render_navigation_report(path, line, column, mode, &result)
}

fn render_navigation_report(
    path: &str,
    line: usize,
    column: usize,
    mode: &str,
    result: &navigation::NavigationResult,
) -> Result<Report, XrayError> {
    let coverage = match mode {
        "supertypes" => "server-static-index; direct parent types (one hop); not transitive or runtime completeness",
        "subtypes" => "server-static-index; direct child types (one hop); not transitive or runtime completeness",
        _ => "server-static-index; not runtime completeness",
    };
    let status = navigation_status(&result.readiness, result.locations.len(), &result.warnings);
    let mut text = format!(
        "{} {mode} results status={status} provenance={} readiness={}\n",
        result.locations.len(),
        result.provenance,
        result.readiness
    );
    let absolute = std::fs::canonicalize(path).map_err(|source| XrayError::Io {
        path: path.into(),
        source,
    })?;
    let root = absolute.parent().and_then(util::git_root);
    for location in &result.locations {
        let display = root
            .as_ref()
            .and_then(|root| location.file.strip_prefix(root).ok())
            .unwrap_or(&location.file);
        let _ = writeln!(
            text,
            "{}:{}:{}-{}:{}",
            display.display(),
            location.line,
            location.column,
            location.end_line,
            location.end_column
        );
    }
    if let Some(candidates) = &result.conventions {
        let _ = writeln!(
            text,
            "convention_candidates={} status={} kind={} target_class={} scanned={}",
            candidates.count,
            candidates.status,
            candidates.kind,
            candidates.target_class,
            candidates.scanned
        );
        let _ = writeln!(text, "candidate_coverage={}", candidates.coverage);
        if candidates.omitted > 0 {
            let _ = writeln!(
                text,
                "candidate_omitted={} (worker payload limit; exact LSP results preserved)",
                candidates.omitted
            );
        }
        for candidate in &candidates.results {
            let location = &candidate.location;
            let display = root
                .as_ref()
                .and_then(|root| location.file.strip_prefix(root).ok())
                .unwrap_or(&location.file);
            let _ = writeln!(
                text,
                "candidate[{}] {}:{}:{}-{}:{} subject={} provenance={}",
                candidate.kind,
                display.display(),
                location.line,
                location.column,
                location.end_line,
                location.end_column,
                candidate.subject,
                candidate.provenance
            );
        }
        for file in &candidates.parser_recovery_files {
            let _ = writeln!(text, "candidate_parser_recovery: {}", file.display());
        }
        for warning in &candidates.warnings {
            let _ = writeln!(text, "candidate_warning: {warning}");
        }
    }
    for warning in &result.warnings {
        let _ = writeln!(text, "warning: {warning}");
    }
    let _ = writeln!(
        text,
        "coverage={coverage}; absence is not proof of no runtime use; columns=UTF-8-bytes-1-based"
    );
    Ok(Report {
        text,
        json: serde_json::json!({"schema_version":1,"mode":mode,"status":status,
        "coverage":coverage, "position_encoding":"utf-8-bytes-1-based",
        "query":{"file":path,"line":line,"column":column},"count":result.locations.len(),
        "provenance":result.provenance,"readiness":result.readiness,"warnings":result.warnings,"results":result.locations,"convention_candidates":result.conventions}),
    })
}

fn navigation_status(readiness: &str, count: usize, warnings: &[String]) -> &'static str {
    if readiness != "ready" || !warnings.is_empty() {
        "partial"
    } else if count == 0 {
        "empty"
    } else {
        "complete"
    }
}

fn print_help() {
    println!(
        "xray — bounded structural navigation for agents\n\
Usage: xray [mode] [options] FILE [FILE ...]\n\n\
Modes (choose one):\n\
  --no-follow           File outline: signatures, ranges, dependencies, tests\n\
  --follow              Local dependency tree (default; depth 1)\n\
  --who                 Static file dependents, not runtime callers\n\
  --definition          PHP LSP definition at --at LINE:COL\n\
  --references          PHP LSP references at --at LINE:COL\n\
  --implementations     PHP LSP implementations at --at LINE:COL (premium)\n\
  --type-definition     PHP LSP type definition at --at LINE:COL (premium)\n\
  --declaration         PHP LSP declaration at --at LINE:COL (premium)\n\
  --supertypes          Direct parent types, one hop, at --at LINE:COL (premium)\n\
  --subtypes            Direct child types, one hop, at --at LINE:COL (premium)\n\
  --trace               Lexical call tree (PHP unsupported)\n\n\
Options:\n\
  --at LINE:COL         1-based line and UTF-8 byte column (navigation only)\n\
  --format text|json    Compact text default; JSON defaults to outline mode\n\
  --max-bytes N         Output budget, 1024..1048576 (default 16384)\n\
  --depth N, -d N       Follow/trace depth, 0..32 (trace default 3)\n\
  --all                Include noise-filtered dependencies\n\
  --explain            Follow resolution details\n\
  --symbol NAME, -s    Select trace root\n\
  --lsp                Assist trace with LSP\n\
  --version, --help\n\n\
Large results preserve exact private overflow artifacts. JSON stays valid.\n\
status=complete describes the declared static scope, not runtime completeness."
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn navigation_distinguishes_ready_empty_results_from_incomplete_indexing() {
        assert_eq!(navigation_status("ready", 0, &[]), "empty");
        assert_eq!(navigation_status("ready", 1, &[]), "complete");
        assert_eq!(navigation_status("indexing", 0, &[]), "partial");
        assert_eq!(
            navigation_status("ready", 1, &["incomplete".into()]),
            "partial"
        );
    }

    #[test]
    fn parse_args_default_is_follow() {
        let args = parse_args(&["file.ts".into()]).unwrap();
        assert!(matches!(args.mode, Mode::Follow));
        assert_eq!(args.files, vec!["file.ts"]);
    }

    #[test]
    fn parse_args_no_follow() {
        let args = parse_args(&["--no-follow".into(), "file.ts".into()]).unwrap();
        assert!(matches!(args.mode, Mode::NoFollow));
    }

    #[test]
    fn parse_args_who() {
        let args = parse_args(&["--who".into(), "file.ts".into()]).unwrap();
        assert!(matches!(args.mode, Mode::Who));
    }

    #[test]
    fn parse_args_depth() {
        let args = parse_args(&["--depth".into(), "3".into(), "file.ts".into()]).unwrap();
        assert_eq!(args.depth, Some(3));
    }

    #[test]
    fn parse_args_no_follow_and_who_exclusive() {
        let result = parse_args(&["--no-follow".into(), "--who".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_no_follow_and_depth_exclusive() {
        let result = parse_args(&[
            "--no-follow".into(),
            "--depth".into(),
            "2".into(),
            "file.ts".into(),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_depth_requires_number() {
        let result = parse_args(&["--depth".into(), "abc".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_unknown_option_errors() {
        let result = parse_args(&["--unknown".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_all_flag() {
        let args = parse_args(&["--all".into(), "file.ts".into()]).unwrap();
        assert!(args.show_all);
    }

    #[test]
    fn parse_args_trace() {
        let args = parse_args(&["--trace".into(), "file.ts".into()]).unwrap();
        assert!(matches!(args.mode, Mode::Trace));
    }

    #[test]
    fn parse_args_trace_with_symbol() {
        let args = parse_args(&[
            "--trace".into(),
            "--symbol".into(),
            "App".into(),
            "file.ts".into(),
        ])
        .unwrap();
        assert!(matches!(args.mode, Mode::Trace));
        assert_eq!(args.target_symbol, Some("App".to_string()));
    }

    #[test]
    fn parse_args_trace_with_short_symbol() {
        let args = parse_args(&[
            "--trace".into(),
            "-s".into(),
            "App".into(),
            "file.ts".into(),
        ])
        .unwrap();
        assert_eq!(args.target_symbol, Some("App".to_string()));
    }

    #[test]
    fn parse_args_symbol_without_trace_errors() {
        let result = parse_args(&["--symbol".into(), "App".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_trace_and_who_exclusive() {
        let result = parse_args(&["--trace".into(), "--who".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_trace_and_no_follow_exclusive() {
        let result = parse_args(&["--trace".into(), "--no-follow".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_trace_with_depth() {
        let args = parse_args(&[
            "--trace".into(),
            "--depth".into(),
            "5".into(),
            "file.ts".into(),
        ])
        .unwrap();
        assert!(matches!(args.mode, Mode::Trace));
        assert_eq!(args.depth, Some(5));
    }

    #[test]
    fn parse_args_trace_with_lsp() {
        let args = parse_args(&["--trace".into(), "--lsp".into(), "file.ts".into()]).unwrap();
        assert!(matches!(args.mode, Mode::Trace));
        assert!(args.use_lsp);
    }

    #[test]
    fn parse_args_lsp_without_trace_errors() {
        let result = parse_args(&["--lsp".into(), "file.ts".into()]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_args_lsp_default_false() {
        let args = parse_args(&["--trace".into(), "file.ts".into()]).unwrap();
        assert!(!args.use_lsp);
    }

    #[test]
    fn navigation_requires_unambiguous_position_and_single_file() {
        for raw in [
            vec!["--definition", "a.php"],
            vec!["--references", "--at", "0:1", "a.php"],
            vec!["--definition", "--at", "1:1", "a.php", "b.php"],
            vec!["--at", "1:1", "a.php"],
        ] {
            assert!(parse_args(&raw.into_iter().map(str::to_string).collect::<Vec<_>>()).is_err());
        }
        let args =
            parse_args(&["--definition", "--at", "126:52", "a.php"].map(str::to_string)).unwrap();
        assert!(args.mode == Mode::Definition);
        assert_eq!(args.at, Some((126, 52)));
    }

    #[test]
    fn convention_candidates_do_not_become_exact_references_and_survive_overflow() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("Model.php");
        std::fs::write(&file, "<?php class Model {}\n").unwrap();
        let result = navigation::NavigationResult {
            locations: Vec::new(),
            provenance: "lsp:intelephense".into(),
            readiness: "ready".into(),
            warnings: Vec::new(),
            conventions: Some(Box::new(navigation::ConventionResult {
                kind: "eloquent-scope".into(),
                target_class: "App\\Models\\Model".into(),
                status: "complete".into(),
                coverage: "name-only candidates; receiver types unverified".into(),
                count: 100,
                omitted: 0,
                scanned: 100,
                parser_recovery_files: Vec::new(),
                warnings: Vec::new(),
                results: (1..=100)
                    .map(|line| navigation::ConventionCandidate {
                        kind: "scope-name-match".into(),
                        subject: "active".into(),
                        provenance: "syntax:laravel-convention-candidate".into(),
                        location: navigation::NavigationLocation {
                            file: file.clone(),
                            line,
                            column: 1,
                            end_line: line,
                            end_column: 7,
                        },
                    })
                    .collect(),
            })),
        };
        let report =
            render_navigation_report(file.to_str().unwrap(), 1, 1, "references", &result).unwrap();
        assert_eq!(report.json["status"], "empty");
        assert_eq!(report.json["count"], 0);
        assert_eq!(report.json["results"], serde_json::json!([]));
        assert_eq!(report.json["convention_candidates"]["count"], 100);
        assert!(report.text.contains("candidate[scope-name-match]"));
        let bounded = presentation::bounded_json(report.json.clone(), 1024).unwrap();
        assert!(bounded.len() <= 1024);
        let preview: serde_json::Value = serde_json::from_str(&bounded).unwrap();
        let artifact = Path::new(preview["truncation"]["artifact"].as_str().unwrap());
        let recovered: serde_json::Value =
            serde_json::from_slice(&std::fs::read(artifact).unwrap()).unwrap();
        assert_eq!(recovered, report.json);
        std::fs::remove_dir_all(artifact.parent().unwrap()).unwrap();
    }

    const PREMIUM_MODES: [(&str, Mode); 5] = [
        ("--implementations", Mode::Implementations),
        ("--type-definition", Mode::TypeDefinition),
        ("--declaration", Mode::Declaration),
        ("--supertypes", Mode::Supertypes),
        ("--subtypes", Mode::Subtypes),
    ];

    #[test]
    fn premium_navigation_modes_select_the_matching_query() {
        for (flag, mode) in PREMIUM_MODES {
            let args = parse_args(
                &[flag, "--at", "12:7", "--format", "json", "a.php"].map(str::to_string),
            )
            .unwrap();
            assert!(args.mode == mode);
            assert_eq!(args.at, Some((12, 7)));
            let (kind, name) = args.mode.navigation().unwrap();
            assert_eq!(name, &flag[2..]);
            let expected_kind = match mode {
                Mode::Implementations => "Implementations",
                Mode::TypeDefinition => "TypeDefinition",
                Mode::Declaration => "Declaration",
                Mode::Supertypes => "Supertypes",
                Mode::Subtypes => "Subtypes",
                _ => unreachable!(),
            };
            assert_eq!(serde_json::to_value(kind).unwrap(), expected_kind);
        }
    }

    #[test]
    fn premium_navigation_rejects_missing_positions_multiple_files_and_irrelevant_options() {
        for (flag, _) in PREMIUM_MODES {
            for raw in [
                vec![flag, "a.php"],
                vec![flag, "--at", "1:1"],
                vec![flag, "--at", "1:1", "a.php", "b.php"],
                vec![flag, "--at", "1:0", "a.php"],
                vec![flag, "--at", "1:1:1", "a.php"],
                vec![flag, "--at", "1:1", "--depth", "1", "a.php"],
                vec![flag, "--at", "1:1", "--all", "a.php"],
                vec![flag, "--at", "1:1", "--lsp", "a.php"],
                vec![flag, "--at", "1:1", "--explain", "a.php"],
                vec![flag, "--at", "1:1", "-s", "Foo", "a.php"],
                vec![flag, "--at", "1:1", "--references", "a.php"],
            ] {
                let raw: Vec<_> = raw.into_iter().map(str::to_string).collect();
                assert!(parse_args(&raw).is_err(), "{raw:?}");
            }
        }
    }

    #[test]
    fn premium_reports_preserve_location_schema_and_bounded_exact_recovery() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("source.php");
        std::fs::write(&file, "<?php class Example {}\n").unwrap();
        let result = navigation::NavigationResult {
            locations: (1..=100)
                .map(|line| navigation::NavigationLocation {
                    file: file.clone(),
                    line,
                    column: 7,
                    end_line: line,
                    end_column: 14,
                })
                .collect(),
            provenance: "lsp:intelephense".into(),
            readiness: "ready".into(),
            warnings: Vec::new(),
            conventions: None,
        };
        for (flag, _) in PREMIUM_MODES {
            let mode = &flag[2..];
            let report =
                render_navigation_report(file.to_str().unwrap(), 1, 13, mode, &result).unwrap();
            assert_eq!(report.json["mode"], mode);
            assert_eq!(report.json["status"], "complete");
            assert_eq!(report.json["count"], 100);
            assert_eq!(report.json["position_encoding"], "utf-8-bytes-1-based");
            assert_eq!(
                report.json["results"][0],
                serde_json::json!({
                    "file":file, "line":1, "column":7, "end_line":1, "end_column":14
                })
            );
            assert!(report.text.contains(&format!("100 {mode} results")));
            let coverage = report.json["coverage"].as_str().unwrap();
            assert!(coverage.contains("server-static-index"));
            assert!(
                coverage.contains("not runtime completeness")
                    || coverage.contains("not transitive or runtime completeness")
            );
            if matches!(mode, "supertypes" | "subtypes") {
                assert!(coverage.contains("one hop"));
                assert!(report.text.contains("not transitive"));
            }
            let bounded = presentation::bounded_json(report.json.clone(), 1024).unwrap();
            assert!(bounded.len() <= 1024);
            let bounded: serde_json::Value = serde_json::from_str(&bounded).unwrap();
            assert_eq!(bounded["mode"], mode);
            assert_eq!(bounded["status"], "partial");
            assert_eq!(bounded["truncation"]["original_status"], "complete");
            let artifact = Path::new(bounded["truncation"]["artifact"].as_str().unwrap());
            let recovered: serde_json::Value =
                serde_json::from_slice(&std::fs::read(artifact).unwrap()).unwrap();
            assert_eq!(recovered, report.json);
            std::fs::remove_dir_all(artifact.parent().unwrap()).unwrap();

            let bounded = presentation::bounded_text(&report.text, 1024).unwrap();
            assert!(bounded.len() <= 1024);
            let artifact = Path::new(bounded.split("artifact=").nth(1).unwrap().trim());
            assert_eq!(std::fs::read_to_string(artifact).unwrap(), report.text);
            std::fs::remove_dir_all(artifact.parent().unwrap()).unwrap();
        }
    }

    #[test]
    fn json_follow_and_reverse_are_supported_without_ignored_flags() {
        for mode in ["--follow", "--who"] {
            assert!(parse_args(&[mode, "--format", "json", "a.php"].map(str::to_string)).is_ok());
        }
        assert!(parse_args(&["--who", "--depth", "2", "a.php"].map(str::to_string)).is_err());
        assert!(
            parse_args(&["--references", "--at", "1:1", "--all", "a.php"].map(str::to_string))
                .is_err()
        );
    }

    #[test]
    fn output_budgets_have_safe_bounds() {
        for budget in ["0", "1023", "1048577", "all"] {
            assert!(parse_args(&["--max-bytes", budget, "a.php"].map(str::to_string)).is_err());
        }
        assert_eq!(
            parse_args(&["a.php"].map(str::to_string))
                .unwrap()
                .max_bytes,
            16384
        );
    }

    #[test]
    fn double_dash_accepts_paths_that_look_like_options() {
        let args = parse_args(&["--no-follow", "--", "--help.php"].map(str::to_string)).unwrap();
        assert_eq!(args.files, vec!["--help.php"]);
    }

    #[test]
    fn parse_args_json_rejects_multiple_files() {
        let result = parse_args(&[
            "--format".into(),
            "json".into(),
            "one.ts".into(),
            "two.ts".into(),
        ]);
        assert_eq!(
            result.err().as_deref(),
            Some("--format json requires exactly one file")
        );
    }
}
