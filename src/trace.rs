use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::lang::LanguageKind;
use crate::lsp::LspClient;
use crate::model::{FileSymbols, Symbol};
use crate::output::{self, FileDigest};
use crate::resolve::{self, PathConfig};
use crate::{follow, util};

/// Configuration for trace mode.
pub struct TraceConfig {
    pub max_depth: usize,
    pub show_all: bool,
    pub target_symbol: Option<String>,
    pub use_lsp: bool,
}

/// Why a call could not be resolved.
#[derive(Debug, PartialEq)]
enum UnresolvedReason {
    MemberCall,
    External,
    UnresolvedImport(String),
    FileNotFound,
    NoiseFile,
    ParseError,
}

impl std::fmt::Display for UnresolvedReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MemberCall => f.write_str("member call"),
            Self::External => f.write_str("external"),
            Self::UnresolvedImport(src) => write!(f, "unresolved import: {src}"),
            Self::FileNotFound => f.write_str("file not found"),
            Self::NoiseFile => f.write_str("noise file"),
            Self::ParseError => f.write_str("parse error"),
        }
    }
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
        reason: UnresolvedReason,
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
        reason: UnresolvedReason,
    },
}

/// Run trace mode: full digest + call tree for selected symbols.
pub fn run(entry_path: &str, config: &TraceConfig) -> Result<(), XrayError> {
    let entry = std::fs::canonicalize(Path::new(entry_path)).map_err(|e| XrayError::Io {
        path: entry_path.to_string(),
        source: e,
    })?;

    let digest = FileDigest::from_path(&entry)?;
    validate_trace_capabilities(digest.language_kind, config)?;
    let path_config = entry.parent().and_then(resolve::load_path_config);

    let mut lsp = if config.use_lsp {
        let fallback_dir = entry.parent().unwrap_or(Path::new("."));
        let root = util::git_root(fallback_dir).unwrap_or_else(|| fallback_dir.to_path_buf());
        match LspClient::start(&root) {
            Ok(mut client) => {
                // Resolve imported file paths so the LSP can eagerly load
                // their type graphs (needed for member call resolution).
                let mut import_paths: Vec<PathBuf> = Vec::new();
                let mut seen_sources = HashSet::new();
                for binding in &digest.symbols.import_bindings {
                    if seen_sources.insert(&binding.source) {
                        if let Some(resolved) =
                            resolve::resolve_import(&binding.source, &entry, path_config.as_ref())
                        {
                            import_paths.push(resolved);
                        }
                    }
                }
                client.warm_up(&entry, &import_paths);
                Some(client)
            }
            Err(e) => {
                eprintln!("xray: LSP init failed: {e}");
                None
            }
        }
    } else {
        None
    };

    print!("{digest}");

    let symbols = select_symbols(&digest.symbols, config.target_symbol.as_deref());
    if symbols.is_empty() {
        if let Some(name) = &config.target_symbol {
            eprintln!("xray: symbol '{name}' not found");
        }
        if let Some(client) = lsp {
            client.shutdown();
        }
        return Ok(());
    }

    for sym in symbols {
        let name = output::extract_name_from_signature(&sym.signature);
        println!("trace: {}  [L{}-{}]", name, sym.line_start, sym.line_end);

        if sym.calls.is_empty() {
            println!("  (no calls)");
        } else {
            let mut visited = HashSet::new();
            visited.insert((entry.clone(), name.clone()));

            let children = build_call_tree(
                &sym.calls,
                &digest.symbols,
                &entry,
                (sym.line_start, sym.line_end),
                1,
                config,
                path_config.as_ref(),
                &mut visited,
                lsp.as_mut(),
            );

            if !children.is_empty() {
                println!("│");
                render_call_tree(&children, "");
            }
        }
        println!();
    }

    if let Some(client) = lsp {
        client.shutdown();
    }

    Ok(())
}

fn validate_trace_capabilities(
    language_kind: LanguageKind,
    config: &TraceConfig,
) -> Result<(), XrayError> {
    if !language_kind.supports_trace() {
        return Err(XrayError::UnsupportedFeature {
            feature: "--trace",
            language: language_kind.display_name(),
        });
    }

    if config.use_lsp && !language_kind.supports_lsp() {
        return Err(XrayError::UnsupportedFeature {
            feature: "--lsp",
            language: language_kind.display_name(),
        });
    }

    Ok(())
}

/// Select target symbols: filter by name or use all exports.
fn select_symbols<'a>(symbols: &'a FileSymbols, target: Option<&str>) -> Vec<&'a Symbol> {
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
            let Some(resolved) = resolve::resolve_import(&binding.source, file_path, path_config)
            else {
                return CallTarget::Unresolved {
                    reason: UnresolvedReason::UnresolvedImport(binding.source.clone()),
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

    // 3. this.method → strip prefix and resolve locally
    if let Some(method) = call_name.strip_prefix("this.") {
        if !method.contains('.') {
            for sym in symbols.exports.iter().chain(symbols.internals.iter()) {
                let sym_name = output::extract_name_from_signature(&sym.signature);
                if sym_name == method {
                    return CallTarget::Local {
                        line_start: sym.line_start,
                        line_end: sym.line_end,
                        calls: sym.calls.clone(),
                    };
                }
            }
        }
    }

    // 4. Member call (contains '.')
    if call_name.contains('.') {
        return CallTarget::Unresolved {
            reason: UnresolvedReason::MemberCall,
        };
    }

    // 5. External/global
    CallTarget::Unresolved {
        reason: UnresolvedReason::External,
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
// Params form a cohesive traversal context (path, depth, config, visited, lsp).
#[expect(clippy::too_many_arguments)]
fn build_call_tree(
    calls: &[String],
    symbols: &FileSymbols,
    file_path: &Path,
    caller_lines: (usize, usize),
    depth: usize,
    config: &TraceConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<(PathBuf, String)>,
    mut lsp: Option<&mut LspClient>,
) -> Vec<CallNode> {
    let mut nodes = Vec::with_capacity(calls.len());
    for call_name in calls {
        let target = resolve_call(call_name, symbols, file_path, path_config);
        let node = match target {
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
                        &sub_calls,
                        symbols,
                        file_path,
                        (line_start, line_end),
                        depth + 1,
                        config,
                        path_config,
                        visited,
                        lsp.as_deref_mut(),
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
            CallTarget::Imported { path, export_name } => build_imported_node(
                call_name,
                &path,
                &export_name,
                depth,
                config,
                path_config,
                visited,
                lsp.as_deref_mut(),
            ),
            CallTarget::Unresolved { reason } => {
                if reason == UnresolvedReason::MemberCall {
                    if let Some(client) = lsp.as_deref_mut() {
                        if let Some(resolved) = try_lsp_resolve(
                            call_name,
                            file_path,
                            caller_lines,
                            client,
                            depth,
                            config,
                            path_config,
                            visited,
                        ) {
                            resolved
                        } else {
                            CallNode {
                                call_name: call_name.clone(),
                                location: CallLocation::Unresolved { reason },
                                children: Vec::new(),
                            }
                        }
                    } else {
                        CallNode {
                            call_name: call_name.clone(),
                            location: CallLocation::Unresolved { reason },
                            children: Vec::new(),
                        }
                    }
                } else {
                    CallNode {
                        call_name: call_name.clone(),
                        location: CallLocation::Unresolved { reason },
                        children: Vec::new(),
                    }
                }
            }
        };
        nodes.push(node);
    }
    nodes
}

/// Build a call node for an imported symbol, recursing into its calls.
// Params mirror build_call_tree for recursive delegation.
#[expect(clippy::too_many_arguments)]
fn build_imported_node(
    call_name: &str,
    path: &Path,
    export_name: &str,
    depth: usize,
    config: &TraceConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<(PathBuf, String)>,
    lsp: Option<&mut LspClient>,
) -> CallNode {
    let Ok(canonical) = path.canonicalize() else {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: UnresolvedReason::FileNotFound,
            },
            children: Vec::new(),
        };
    };

    if !config.show_all && follow::is_noise_path(&canonical) {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: UnresolvedReason::NoiseFile,
            },
            children: Vec::new(),
        };
    }

    let Ok(digest) = FileDigest::from_path(&canonical) else {
        return CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Unresolved {
                reason: UnresolvedReason::ParseError,
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
            (export_sym.line_start, export_sym.line_end),
            depth + 1,
            config,
            path_config,
            visited,
            lsp,
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

/// Try to resolve a member call via LSP go-to-definition.
///
/// Extracts the final method name from the call (e.g. `this.svc.foo` → `foo`),
/// finds its position in the source within `caller_lines`, asks the LSP for the
/// definition, and if found, builds a call node that continues the trace.
// Params carry the full traversal state needed to continue the trace.
#[expect(clippy::too_many_arguments)]
fn try_lsp_resolve(
    call_name: &str,
    file_path: &Path,
    caller_lines: (usize, usize),
    lsp: &mut LspClient,
    depth: usize,
    config: &TraceConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<(PathBuf, String)>,
) -> Option<CallNode> {
    let method = call_name.rsplit('.').next()?;

    let source = std::fs::read_to_string(file_path).ok()?;
    let lines: Vec<&str> = source.lines().collect();

    // Search within the caller's line range (1-based → 0-based index).
    let start = caller_lines.0.saturating_sub(1);
    let end = caller_lines.1.min(lines.len());

    // Search for the full call pattern first (e.g. "emailProvider.send(")
    // to avoid false-matching the function's own signature. Point the
    // cursor at the method name portion for the LSP to resolve.
    let full_needle = format!("{call_name}(");
    let method_offset = call_name.len() - method.len();

    let (line_0, col_0) = lines[start..end]
        .iter()
        .enumerate()
        .find_map(|(offset, line_str)| {
            line_str
                .find(&full_needle)
                .map(|col| (start + offset, col + method_offset))
        })
        .or_else(|| {
            // Fallback: search for just "method(" (handles line-broken chains).
            let short_needle = format!("{method}(");
            lines[start..end]
                .iter()
                .enumerate()
                .find_map(|(offset, line_str)| {
                    line_str
                        .find(&short_needle)
                        .map(|col| (start + offset, col))
                })
        })?;

    // Retry with short delay: tsserver may need time to resolve
    // user-defined types from imported modules (built-ins resolve immediately).
    let (target_path, target_line_0) = lsp.definition(file_path, line_0, col_0).or_else(|| {
        std::thread::sleep(std::time::Duration::from_millis(200));
        lsp.definition(file_path, line_0, col_0)
    })?;

    let canonical_target = std::fs::canonicalize(&target_path).unwrap_or(target_path.clone());
    let canonical_source = std::fs::canonicalize(file_path).unwrap_or(file_path.to_path_buf());
    let target_line_1 = target_line_0 + 1; // LSP 0-based → xray 1-based

    // Same-file result: resolve_call step 3 handles this.method(),
    // but if we get here it means resolve_call missed it (e.g. this.X.Y()
    // that the LSP resolved to the same file). Build a Local node.
    if canonical_target == canonical_source {
        let digest = FileDigest::from_path(&canonical_source).ok()?;
        let sym = find_symbol_at_line(&digest.symbols, target_line_1)?;
        let children = if depth < config.max_depth
            && !sym.calls.is_empty()
            && visited.insert((canonical_source.clone(), call_name.to_string()))
        {
            build_call_tree(
                &sym.calls,
                &digest.symbols,
                &canonical_source,
                (sym.line_start, sym.line_end),
                depth + 1,
                config,
                path_config,
                visited,
                Some(lsp),
            )
        } else {
            Vec::new()
        };
        return Some(CallNode {
            call_name: call_name.to_string(),
            location: CallLocation::Local {
                lines: (sym.line_start, sym.line_end),
            },
            children,
        });
    }

    if !config.show_all && follow::is_noise_path(&canonical_target) {
        return None;
    }

    let digest = FileDigest::from_path(&canonical_target).ok()?;
    let display_path = util::relative_path(&canonical_target);

    let sym = find_symbol_at_line(&digest.symbols, target_line_1);

    let (lines, children) = if let Some(s) = sym {
        let sub = if depth < config.max_depth
            && !s.calls.is_empty()
            && visited.insert((canonical_target.clone(), call_name.to_string()))
        {
            build_call_tree(
                &s.calls,
                &digest.symbols,
                &canonical_target,
                (s.line_start, s.line_end),
                depth + 1,
                config,
                path_config,
                visited,
                Some(lsp),
            )
        } else {
            Vec::new()
        };
        ((s.line_start, s.line_end), sub)
    } else {
        ((target_line_1, target_line_1), Vec::new())
    };

    Some(CallNode {
        call_name: call_name.to_string(),
        location: CallLocation::Imported {
            display_path,
            lines,
        },
        children,
    })
}

/// Find the symbol whose line range contains the given line.
/// Prefers the narrowest (most specific) match.
fn find_symbol_at_line(symbols: &FileSymbols, line: usize) -> Option<&Symbol> {
    symbols
        .exports
        .iter()
        .chain(symbols.internals.iter())
        .filter(|s| s.line_start <= line && line <= s.line_end)
        .min_by_key(|s| s.line_end - s.line_start)
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
                println!("{prefix}{connector}{}  ({})", node.call_name, reason);
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
    use crate::lang::LanguageKind;
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

    fn default_trace_config() -> TraceConfig {
        TraceConfig {
            max_depth: 3,
            show_all: false,
            target_symbol: None,
            use_lsp: false,
        }
    }

    #[test]
    fn validate_trace_capabilities_allows_ts() {
        let cfg = default_trace_config();
        let result = validate_trace_capabilities(LanguageKind::Ts, &cfg);
        assert!(result.is_ok());
    }

    #[test]
    fn validate_trace_capabilities_rejects_sql_trace() {
        let cfg = default_trace_config();
        let err = validate_trace_capabilities(LanguageKind::Sql, &cfg).unwrap_err();
        match err {
            XrayError::UnsupportedFeature { feature, language } => {
                assert_eq!(feature, "--trace");
                assert_eq!(language, "SQL");
            }
            other => panic!("expected UnsupportedFeature, got {other:?}"),
        }
    }

    #[test]
    fn run_rejects_sql_files_in_trace_mode() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("query.sql");
        std::fs::write(&file, "select 1;").unwrap();

        let cfg = default_trace_config();
        let err = run(file.to_str().unwrap(), &cfg).unwrap_err();

        match err {
            XrayError::UnsupportedFeature { feature, language } => {
                assert_eq!(feature, "--trace");
                assert_eq!(language, "SQL");
            }
            other => panic!("expected UnsupportedFeature, got {other:?}"),
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
    fn resolve_call_this_method_resolves_local() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol("doWork", vec![]));

        let target = resolve_call("this.doWork", &symbols, Path::new("/a.ts"), None);
        assert!(matches!(target, CallTarget::Local { .. }));
    }

    #[test]
    fn resolve_call_this_method_not_found_is_member_call() {
        let symbols = empty_symbols();
        let target = resolve_call("this.missing", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, UnresolvedReason::MemberCall),
            _ => panic!("expected Unresolved"),
        }
    }

    #[test]
    fn resolve_call_this_chained_stays_member_call() {
        let symbols = empty_symbols();
        let target = resolve_call("this.service.method", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, UnresolvedReason::MemberCall),
            _ => panic!("expected Unresolved"),
        }
    }

    #[test]
    fn resolve_call_member_is_unresolved() {
        let symbols = empty_symbols();
        let target = resolve_call("obj.method", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, UnresolvedReason::MemberCall),
            _ => panic!("expected Unresolved"),
        }
    }

    #[test]
    fn resolve_call_external_is_unresolved() {
        let symbols = empty_symbols();
        let target = resolve_call("unknownFn", &symbols, Path::new("/a.ts"), None);
        match target {
            CallTarget::Unresolved { reason } => assert_eq!(reason, UnresolvedReason::External),
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
        assert!(matches!(
            target,
            CallTarget::Imported { .. } | CallTarget::Unresolved { .. }
        ));
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

    fn make_symbol_at(name: &str, start: usize, end: usize) -> Symbol {
        Symbol {
            signature: format!("function {name}()"),
            line_start: start,
            line_end: end,
            calls: Vec::new(),
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        }
    }

    #[test]
    fn find_symbol_at_line_finds_export() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol_at("foo", 5, 15));

        let sym = find_symbol_at_line(&symbols, 10);
        assert!(sym.is_some());
        assert!(sym.unwrap().signature.contains("foo"));
    }

    #[test]
    fn find_symbol_at_line_finds_internal() {
        let mut symbols = empty_symbols();
        symbols.internals.push(make_symbol_at("bar", 20, 30));

        let sym = find_symbol_at_line(&symbols, 25);
        assert!(sym.is_some());
        assert!(sym.unwrap().signature.contains("bar"));
    }

    #[test]
    fn find_symbol_at_line_returns_none_outside_range() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol_at("foo", 5, 15));

        assert!(find_symbol_at_line(&symbols, 3).is_none());
        assert!(find_symbol_at_line(&symbols, 20).is_none());
    }

    #[test]
    fn find_symbol_at_line_boundary() {
        let mut symbols = empty_symbols();
        symbols.exports.push(make_symbol_at("foo", 5, 15));

        assert!(find_symbol_at_line(&symbols, 5).is_some());
        assert!(find_symbol_at_line(&symbols, 15).is_some());
    }
}
