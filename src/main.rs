//! `xray` — Structural code digest for LLM context optimization.
//!
//! Compresses TypeScript/JavaScript source files to ~10% of their size,
//! showing only structure: signatures, types, exports, imports, body hints,
//! and line ranges.

mod error;
mod extract;
mod follow;
mod model;
mod output;
mod parser;
mod resolve;
mod reverse;
mod util;

use std::path::Path;

use error::XrayError;
use output::FileDigest;

struct CliArgs {
    no_follow: bool,
    who: bool,
    depth: Option<usize>,
    show_all: bool,
    files: Vec<String>,
}

fn parse_args(args: &[String]) -> Result<CliArgs, String> {
    let mut no_follow = false;
    let mut who = false;
    let mut depth: Option<usize> = None;
    let mut show_all = false;
    let mut files = Vec::new();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--no-follow" => no_follow = true,
            "--who" => who = true,
            "--all" => show_all = true,
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

    if no_follow && who {
        return Err("--no-follow and --who are mutually exclusive".to_string());
    }

    if no_follow && depth.is_some() {
        return Err("--no-follow and --depth are mutually exclusive".to_string());
    }

    Ok(CliArgs {
        no_follow,
        who,
        depth,
        show_all,
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
        let result = if args.who {
            reverse::run(path_str)
        } else if args.no_follow {
            process_file(path_str)
        } else {
            let config = follow::FollowConfig {
                max_depth: args.depth.unwrap_or(1),
                show_all: args.show_all,
            };
            follow::run(path_str, &config)
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
    eprintln!("  --depth N, -d N    Max follow depth (default 1)");
    eprintln!("  --all              Show all followed files (disable noise filter)");
    eprintln!("  --who              Show files that import the target");
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
        assert!(!args.no_follow);
        assert!(!args.who);
        assert_eq!(args.files, vec!["file.ts"]);
    }

    #[test]
    fn parse_args_no_follow() {
        let args = parse_args(&["--no-follow".into(), "file.ts".into()]).unwrap();
        assert!(args.no_follow);
    }

    #[test]
    fn parse_args_who() {
        let args = parse_args(&["--who".into(), "file.ts".into()]).unwrap();
        assert!(args.who);
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
        let result = parse_args(&["--no-follow".into(), "--depth".into(), "2".into(), "file.ts".into()]);
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
}
