//! `xray` — Structural code digest for LLM context optimization.
//!
//! Compresses TypeScript/JavaScript source files to ~10% of their size,
//! showing only structure: signatures, types, exports, imports, body hints,
//! and line ranges.

mod error;
mod extract;
mod follow;
mod lang;
mod lsp;
mod model;
mod output;
mod parser;
mod resolve;
mod reverse;
mod trace;
mod util;

use std::path::Path;

use error::XrayError;
use output::FileDigest;

/// Which processing mode to use.
enum Mode {
    Follow,
    NoFollow,
    Who,
    Trace,
}

struct CliArgs {
    mode: Mode,
    target_symbol: Option<String>,
    depth: Option<usize>,
    show_all: bool,
    use_lsp: bool,
    files: Vec<String>,
}

fn parse_args(args: &[String]) -> Result<CliArgs, String> {
    let mut mode_flags: Vec<&str> = Vec::new();
    let mut target_symbol: Option<String> = None;
    let mut depth: Option<usize> = None;
    let mut show_all = false;
    let mut use_lsp = false;
    let mut files = Vec::new();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--no-follow" => mode_flags.push("--no-follow"),
            "--who" => mode_flags.push("--who"),
            "--trace" => mode_flags.push("--trace"),
            "--all" => show_all = true,
            "--lsp" => use_lsp = true,
            "--symbol" | "-s" => {
                i += 1;
                if i >= args.len() {
                    return Err("--symbol requires a name argument".to_string());
                }
                target_symbol = Some(args[i].clone());
            }
            "--depth" | "-d" => {
                i += 1;
                if i >= args.len() {
                    return Err("--depth requires a number argument".to_string());
                }
                let n: usize = args[i]
                    .parse()
                    .map_err(|_| format!("--depth: invalid number '{}'", args[i]))?;
                depth = Some(n);
            }
            arg if arg.starts_with('-') => {
                return Err(format!("unknown option: {arg}"));
            }
            _ => files.push(args[i].clone()),
        }
        i += 1;
    }

    if mode_flags.len() > 1 {
        return Err("--no-follow, --who, and --trace are mutually exclusive".to_string());
    }

    let mode = match mode_flags.first().copied() {
        Some("--no-follow") => Mode::NoFollow,
        Some("--who") => Mode::Who,
        Some("--trace") => Mode::Trace,
        _ => Mode::Follow,
    };

    if matches!(mode, Mode::NoFollow) && depth.is_some() {
        return Err("--no-follow and --depth are mutually exclusive".to_string());
    }

    if target_symbol.is_some() && !matches!(mode, Mode::Trace) {
        return Err("--symbol requires --trace".to_string());
    }

    if use_lsp && !matches!(mode, Mode::Trace) {
        return Err("--lsp requires --trace".to_string());
    }

    Ok(CliArgs {
        mode,
        target_symbol,
        depth,
        show_all,
        use_lsp,
        files,
    })
}

fn main() {
    let raw: Vec<String> = std::env::args().skip(1).collect();

    if raw.is_empty() || raw[0] == "-h" || raw[0] == "--help" {
        print_help();
        std::process::exit(0);
    }

    let args = match parse_args(&raw) {
        Ok(a) => a,
        Err(msg) => {
            eprintln!("xray: {msg}");
            std::process::exit(1);
        }
    };

    if args.files.is_empty() {
        eprintln!("xray: no files specified");
        std::process::exit(1);
    }

    let multi = args.files.len() > 1;
    for (i, path_str) in args.files.iter().enumerate() {
        if i > 0 && multi {
            println!("\n---\n");
        }
        let result = match args.mode {
            Mode::Trace => {
                let config = trace::TraceConfig {
                    max_depth: args.depth.unwrap_or(3),
                    show_all: args.show_all,
                    target_symbol: args.target_symbol.clone(),
                    use_lsp: args.use_lsp,
                };
                trace::run(path_str, &config)
            }
            Mode::Who => reverse::run(path_str),
            Mode::NoFollow => process_file(path_str),
            Mode::Follow => {
                let config = follow::FollowConfig {
                    max_depth: args.depth.unwrap_or(1),
                    show_all: args.show_all,
                };
                follow::run(path_str, &config)
            }
        };
        if let Err(e) = result {
            eprintln!("xray: {e}");
        }
    }
}

fn process_file(path_str: &str) -> Result<(), XrayError> {
    let digest = FileDigest::from_path(Path::new(path_str))?;
    print!("{digest}");
    Ok(())
}

fn print_help() {
    eprintln!("xray — Structural code digest for LLM context optimization");
    eprintln!("Usage: xray [options] <file> [file2 ...]");
    eprintln!();
    eprintln!("By default, follows local imports (depth 1) with noise filtering.");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --depth N, -d N    Max follow/trace depth (default: 1 follow, 3 trace)");
    eprintln!("  --all              Show all followed files (disable noise filter)");
    eprintln!("  --who              Show files that import the target");
    eprintln!("  --trace            Trace cross-file call chains from exports");
    eprintln!("  --lsp              Use LSP to resolve member calls (requires --trace)");
    eprintln!("  --symbol N, -s N   Trace a specific symbol (requires --trace)");
    eprintln!("  --no-follow        Disable follow, show only the target file");
    eprintln!("  -h, --help         Show help");
    eprintln!();
    eprintln!("Compresses source files to ~10% showing only structure:");
    eprintln!("  signatures, types, exports, imports, body hints, line ranges");
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
