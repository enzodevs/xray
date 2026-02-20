//! `xray` — Structural code digest for LLM context optimization.
//!
//! Compresses TypeScript/JavaScript source files to ~10% of their size,
//! showing only structure: signatures, types, exports, imports, body hints,
//! and line ranges.

mod error;
mod extract;
mod model;
mod output;
mod parser;
mod util;

use std::path::Path;

use error::XrayError;
use output::FileDigest;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    if args.is_empty() || args[0] == "-h" || args[0] == "--help" {
        eprintln!("xray — Structural code digest for LLM context optimization");
        eprintln!("Usage: xray <file> [file2 ...]");
        eprintln!("\nCompresses source files to ~10% showing only structure:");
        eprintln!("  signatures, types, exports, imports, body hints, line ranges");
        std::process::exit(0);
    }

    let multi = args.len() > 1;
    for (i, path_str) in args.iter().enumerate() {
        if i > 0 && multi {
            println!("\n---\n");
        }
        if let Err(e) = process_file(path_str) {
            eprintln!("xray: {e}");
        }
    }
}

fn process_file(path_str: &str) -> Result<(), XrayError> {
    let path = Path::new(path_str);

    let ext = path
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("")
        .to_string();

    let (tree, source) = parser::parse_file(path)?;
    let symbols = extract::extract_symbols(tree.root_node(), source.as_bytes());
    let total_lines = source.lines().count();
    let display_path = util::relative_path(path);

    let digest = FileDigest {
        display_path,
        ext,
        total_lines,
        symbols,
    };

    print!("{digest}");

    Ok(())
}
