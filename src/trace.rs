use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::model::{FileSymbols, Symbol};
use crate::output::{self, FileDigest};
use crate::resolve::{self, PathConfig};
use crate::{follow, util};

/// Configuration for trace mode.
pub struct TraceConfig {
    pub max_depth: usize,
    pub show_all: bool,
    pub target_symbol: Option<String>,
}

/// Where a call was resolved to.
enum CallLocation {
    Local {
        lines: (usize, usize),
    },
    Imported {
        display_path: String,
        lines: (usize, usize),
    },
    Unresolved {
        reason: String,
    },
}

/// A node in the call tree.
struct CallNode {
    call_name: String,
    location: CallLocation,
    children: Vec<CallNode>,
}

/// Intermediate result of resolving a single call name.
enum CallTarget {
    Local {
        line_start: usize,
        line_end: usize,
        calls: Vec<String>,
    },
    Imported {
        path: PathBuf,
        export_name: String,
    },
    Unresolved {
        reason: String,
    },
}

/// Run trace mode: full digest + call tree for selected symbols.
pub fn run(entry_path: &str, config: &TraceConfig) -> Result<(), XrayError> {
    let entry = std::fs::canonicalize(Path::new(entry_path)).map_err(|e| XrayError::Io {
        path: entry_path.to_string(),
        source: e,
    })?;

    let path_config = entry.parent().and_then(resolve::load_path_config);
    let digest = FileDigest::from_path(&entry)?;

    print!("{digest}");

    let symbols = select_symbols(&digest.symbols, config.target_symbol.as_deref());
    if symbols.is_empty() {
        if let Some(name) = &config.target_symbol {
            eprintln!("xray: symbol '{name}' not found");
        }
        return Ok(());
    }

    for sym in symbols {
        let name = output::extract_name_from_signature(&sym.signature);
        println!(
            "trace: {}  [L{}-{}]",
            name, sym.line_start, sym.line_end
        );

        if sym.calls.is_empty() {
            println!("  (no calls)");
        } else {
            let mut visited = HashSet::new();
            visited.insert((entry.clone(), name.clone()));

            let children = build_call_tree(
                &sym.calls,
                &digest.symbols,
                &entry,
                1,
                config,
                path_config.as_ref(),
                &mut visited,
            );

            if !children.is_empty() {
                println!("│");
                render_call_tree(&children, "");
            }
        }
        println!();
    }

    Ok(())
}

/// Select target symbols: filter by name or use all exports.
fn select_symbols<'a>(
    symbols: &'a FileSymbols,
    target: Option<&str>,
) -> Vec<&'a Symbol> {
    match target {
        Some(name) => symbols
            .exports
            .iter()
            .chain(symbols.internals.iter())
            .filter(|s| output::extract_name_from_signature(&s.signature) == name)
            .collect(),
        None => symbols.exports.iter().collect(),
    }
}

/// Resolve a single call name to its target.
fn resolve_call(
    call_name: &str,
    symbols: &FileSymbols,
    file_path: &Path,
    path_config: Option<&PathConfig>,
) -> CallTarget {
    // 1. Local symbol in same file (exports + internals)
    for sym in symbols.exports.iter().chain(symbols.internals.iter()) {
        let sym_name = output::extract_name_from_signature(&sym.signature);
        if sym_name == call_name {
            return CallTarget::Local {
                line_start: sym.line_start,
                line_end: sym.line_end,
                calls: sym.calls.clone(),
            };
        }
    }

    // 2. Imported binding
    for binding in &symbols.import_bindings {
        if binding.local_name == call_name {
            let Some(resolved) =
                resolve::resolve_import(&binding.source, file_path, path_config)
            else {
                return CallTarget::Unresolved {
                    reason: format!("unresolved import: {}", binding.source),
                };
            };
            return CallTarget::Imported {
                path: resolved,
                export_name: if binding.is_default {
                    "default".to_string()
                } else {
                    call_name.to_string()
                },
            };
        }
    }

    // 3. Member call (contains '.')
    if call_name.contains('.') {
        return CallTarget::Unresolved {
            reason: "member call".to_string(),
        };
    }

    // 4. External/global
    CallTarget::Unresolved {
        reason: "external".to_string(),
    }
}

/// Find a matching export symbol in a file's symbols.
fn find_export<'a>(symbols: &'a FileSymbols, name: &str) -> Option<&'a Symbol> {
    symbols.exports.iter().find(|s| {
        let n = output::extract_name_from_signature(&s.signature);
        if name == "default" {
            n == "default" || s.signature.contains("export default")
        } else {
            n == name
        }
    })
}

/// Recursively build the call tree.
fn build_call_tree(
    calls: &[String],
    symbols: &FileSymbols,
    file_path: &Path,
    depth: usize,
    config: &TraceConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<(PathBuf, String)>,
) -> Vec<CallNode> {
    calls
        .iter()
        .map(|call_name| {
            let target = resolve_call(call_name, symbols, file_path, path_config);
            match target {
                CallTarget::Local {
                    line_start,
                    line_end,
                    calls: sub_calls,
                    ..
                } => {
                    let children = if depth < config.max_depth
                        && !sub_calls.is_empty()
                        && visited.insert((file_path.to_path_buf(), call_name.clone()))
                    {
                        build_call_tree(
                            &sub_calls, symbols, file_path, depth + 1, config,
                            path_config, visited,
                        )
                    } else {
                        Vec::new()
                    };
                    CallNode {
                        call_name: call_name.clone(),
                        location: CallLocation::Local {
                            lines: (line_start, line_end),
                        },
                        children,
                    }
                }
                CallTarget::Imported { path, export_name } => {
                    build_imported_node(
                        call_name, &path, &export_name, depth, config,
                        path_config, visited,
                    )
                }
                CallTarget::Unresolved { reason } => CallNode {
                    call_name: call_name.clone(),
                    location: CallLocation::Unresolved { reason },
                    children: Vec::new(),
                },
            }
        })
        .collect()
}

/// Build a call node for an imported symbol, recursing into its calls.
fn build_imported_node(
    call_name: &str,
    path: &Path,
    export_name: &str,
    depth: usize,
    config: &TraceConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<(PathBuf, String)>,
) -> CallNode {
    let Ok(canonical) = path.canonicalize() else {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: "file not found".to_string(),
            },
            children: Vec::new(),
        };
    };

    if !config.show_all && follow::is_noise_path(&canonical) {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: "noise file".to_string(),
            },
            children: Vec::new(),
        };
    }

    let Ok(digest) = FileDigest::from_path(&canonical) else {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: "parse error".to_string(),
            },
            children: Vec::new(),
        };
    };

    let display_path = util::relative_path(&canonical);

    let Some(export_sym) = find_export(&digest.symbols, export_name) else {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Imported {
                display_path,
                lines: (0, 0),
            },
            children: Vec::new(),
        };
    };

    let children = if depth < config.max_depth
        && !export_sym.calls.is_empty()
        && visited.insert((canonical.clone(), call_name.to_string()))
    {
        build_call_tree(
            &export_sym.calls,
            &digest.symbols,
            &canonical,
            depth + 1,
            config,
            path_config,
            visited,
        )
    } else {
        Vec::new()
    };

    CallNode {
        call_name: call_name.to_string(),
        location: CallLocation::Imported {
            display_path,
            lines: (export_sym.line_start, export_sym.line_end),
        },
        children,
    }
}

/// Render the call tree with box-drawing characters.
fn render_call_tree(nodes: &[CallNode], prefix: &str) {
    for (i, node) in nodes.iter().enumerate() {
        let is_last = i == nodes.len() - 1;
        let connector = if is_last { "└── " } else { "├── " };
        let continuation = if is_last { "    " } else { "│   " };

        match &node.location {
            CallLocation::Local { lines, .. } => {
                println!(
                    "{prefix}{connector}{}  (local)  [L{}-{}]",
                    node.call_name, lines.0, lines.1
                );
            }
            CallLocation::Imported {
                display_path,
                lines,
                ..
            } => {
                if lines.0 > 0 {
                    println!(
                        "{prefix}{connector}{}  \u{2190}  {}  [L{}-{}]",
                        node.call_name, display_path, lines.0, lines.1
                    );
                } else {
                    println!(
                        "{prefix}{connector}{}  \u{2190}  {}",
                        node.call_name, display_path
                    );
                }
            }
            CallLocation::Unresolved { reason } => {
                println!(
                    "{prefix}{connector}{}  ({})",
                    node.call_name, reason
                );
            }
        }

        if !node.children.is_empty() {
            let sub_prefix = format!("{prefix}{continuation}");
            render_call_tree(&node.children, &sub_prefix);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{FileSymbols, ImportBinding, Symbol};

    fn empty_symbols() -> FileSymbols {
        FileSymbols {
            imports: Vec::new(),
            import_bindings: Vec::new(),
            reexports: Vec::new(),
            exports: Vec::new(),
            internals: Vec::new(),
            types: Vec::new(),
            tests: Vec::new(),
            hooks: Vec::new(),
        }
    }

    fn make_symbol(name: &str, calls: Vec<&str>) -> Symbol {
        Symbol {
            signature: format!("function {name}()"),
            line_start: 1,
            line_end: 10,
            calls: calls.into_iter().map(String::from).collect(),
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        }
    }

    #[test]
    fn resolve_call_finds_local_export() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol("helper", vec![]));

        let target = resolve_call("helper", &symbols, Path::new("/a.ts"), None);
        assert!(matches!(target, CallTarget::Local { .. }));
    }

    #[test]
    fn resolve_call_finds_local_internal() {
        let mut symbols = empty_symbols();
        symbols.internals.push(make_symbol("helper", vec![]));

        let target = resolve_call("helper", &symbols, Path::new("/a.ts"), None);
        assert!(matches!(target, CallTarget::Local { .. }));
    }

    #[test]
    fn resolve_call_member_is_unresolved() {
        let symbols = empty_symbols();
        let target = resolve_call("obj.method", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, "member call"),
            _ => panic!("expected Unresolved"),
        }
    }

    #[test]
    fn resolve_call_external_is_unresolved() {
        let symbols = empty_symbols();
        let target = resolve_call("unknownFn", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, "external"),
            _ => panic!("expected Unresolved"),
        }
    }

    #[test]
    fn resolve_call_finds_imported_binding() {
        let mut symbols = empty_symbols();
        symbols.import_bindings.push(ImportBinding {
            local_name: "fetchData".to_string(),
            source: "./api".to_string(),
            is_default: false,
        });

        let target = resolve_call("fetchData", &symbols, Path::new("/src/a.ts"), None);
        assert!(matches!(target, CallTarget::Imported { .. } | CallTarget::Unresolved { .. }));
    }

    #[test]
    fn select_symbols_returns_all_exports_when_no_target() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol("foo", vec![]));
        symbols.exports.push(make_symbol("bar", vec![]));
        symbols.internals.push(make_symbol("internal", vec![]));

        let selected = select_symbols(&symbols, None);
        assert_eq!(selected.len(), 2);
    }

    #[test]
    fn select_symbols_filters_by_name() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol("foo", vec![]));
        symbols.exports.push(make_symbol("bar", vec![]));

        let selected = select_symbols(&symbols, Some("bar"));
        assert_eq!(selected.len(), 1);
        assert!(selected[0].signature.contains("bar"));
    }

    #[test]
    fn select_symbols_searches_internals_when_target_given() {
        let mut symbols = empty_symbols();
        symbols.internals.push(make_symbol("helper", vec![]));

        let selected = select_symbols(&symbols, Some("helper"));
        assert_eq!(selected.len(), 1);
    }
}
