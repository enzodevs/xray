use std::fmt;
use std::path::Path;

use crate::error::XrayError;
use crate::lang::LanguageKind;
use crate::model::{
    indented_symbol, write_test_tree, FileSummary, FileSymbols, Indented, SymbolRefsLabel,
};
use crate::{parser, util};

/// Complete digest of a single file, ready for display.
pub struct FileDigest {
    pub display_path: String,
    pub language_kind: LanguageKind,
    pub ext: String,
    pub total_lines: usize,
    pub symbols: FileSymbols,
}

impl FileDigest {
    /// Build a full digest from a file path.
    pub fn from_path(path: &Path) -> Result<Self, XrayError> {
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("")
            .to_string();

        let parsed = parser::parse_file(path)?;
        let symbols = parsed
            .language_kind
            .extract_symbols(parsed.tree.root_node(), parsed.source.as_bytes());
        let total_lines = parsed.source.lines().count();
        let display_path = util::relative_path(path);

        Ok(Self {
            display_path,
            language_kind: parsed.language_kind,
            ext,
            total_lines,
            symbols,
        })
    }

    /// Compress into a compact summary for follow-mode children.
    pub fn summarize(&self) -> FileSummary {
        let export_names = self
            .symbols
            .exports
            .iter()
            .map(|s| extract_name_from_signature(&s.signature))
            .collect();
        let type_names = self.symbols.types.iter().map(|t| t.name.clone()).collect();
        FileSummary {
            display_path: self.display_path.clone(),
            total_lines: self.total_lines,
            export_names,
            type_names,
        }
    }
}

/// Extract the primary identifier from a symbol signature.
///
/// `"function greet(name: string)"` → `"greet"`
/// `"const App = (props)"` → `"App"`
/// `"async function fetch()"` → `"fetch"`
/// `"class MyService extends Base"` → `"MyService"`
/// `"public async send(msg: string)"` → `"send"`
pub(crate) fn extract_name_from_signature(sig: &str) -> String {
    let s = sig
        .trim_start_matches("public ")
        .trim_start_matches("private ")
        .trim_start_matches("protected ")
        .trim_start_matches("static ")
        .trim_start_matches("abstract ")
        .trim_start_matches("override ")
        .trim_start_matches("readonly ")
        .trim_start_matches("async ")
        .trim_start_matches("const ")
        .trim_start_matches("function ")
        .trim_start_matches("class ");
    s.split(['(', '=', '<', ' '])
        .next()
        .unwrap_or("?")
        .trim()
        .to_string()
}

impl fmt::Display for FileSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}  ({} lines)", self.display_path, self.total_lines)?;
        if !self.export_names.is_empty() {
            write!(f, "\n    exports: {}", self.export_names.join(", "))?;
        }
        if !self.type_names.is_empty() {
            write!(f, "\n    types: {}", self.type_names.join(", "))?;
        }
        Ok(())
    }
}

impl fmt::Display for FileDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol_refs_label = match self.language_kind.symbol_ref_label() {
            "refs" => SymbolRefsLabel::Refs,
            _ => SymbolRefsLabel::Calls,
        };

        writeln!(
            f,
            "{}  ({}, {} lines)",
            self.display_path, self.ext, self.total_lines
        )?;
        writeln!(f)?;

        if !self.symbols.imports.is_empty() {
            writeln!(f, "imports: {}", self.symbols.imports.join(", "))?;
            writeln!(f)?;
        }

        if !self.symbols.reexports.is_empty() {
            write_reexports(f, &self.symbols.reexports)?;
            writeln!(f)?;
        }

        if !self.symbols.exports.is_empty() {
            writeln!(f, "exports:")?;
            for sym in &self.symbols.exports {
                writeln!(f, "{}", indented_symbol("  ", sym, symbol_refs_label))?;
            }
            if has_more_sections(&self.symbols, SectionAfter::Exports) {
                writeln!(f)?;
            }
        }

        if !self.symbols.internals.is_empty() {
            writeln!(f, "internal:")?;
            for sym in &self.symbols.internals {
                writeln!(f, "{}", indented_symbol("  ", sym, symbol_refs_label))?;
            }
            if has_more_sections(&self.symbols, SectionAfter::Internals) {
                writeln!(f)?;
            }
        }

        if !self.symbols.hooks.is_empty() {
            writeln!(f, "hooks:")?;
            for h in &self.symbols.hooks {
                writeln!(f, "{}", Indented("  ", h))?;
            }
            if has_more_sections(&self.symbols, SectionAfter::Hooks) {
                writeln!(f)?;
            }
        }

        if !self.symbols.types.is_empty() {
            writeln!(f, "types:")?;
            for t in &self.symbols.types {
                writeln!(f, "{}", Indented("  ", t))?;
            }
            if !self.symbols.tests.is_empty() {
                writeln!(f)?;
            }
        }

        if !self.symbols.tests.is_empty() {
            writeln!(f, "tests:")?;
            write_test_tree(f, &self.symbols.tests, "  ")?;
        }

        Ok(())
    }
}

/// Tracks which section we just printed to decide if a blank separator is needed.
#[derive(Clone, Copy)]
enum SectionAfter {
    Exports,
    Internals,
    Hooks,
}

fn has_more_sections(s: &FileSymbols, after: SectionAfter) -> bool {
    match after {
        SectionAfter::Exports => {
            !s.internals.is_empty()
                || !s.hooks.is_empty()
                || !s.types.is_empty()
                || !s.tests.is_empty()
        }
        SectionAfter::Internals => {
            !s.hooks.is_empty() || !s.types.is_empty() || !s.tests.is_empty()
        }
        SectionAfter::Hooks => !s.types.is_empty() || !s.tests.is_empty(),
    }
}

/// Group re-exports by source module for compact output.
fn write_reexports(
    f: &mut fmt::Formatter<'_>,
    reexports: &[crate::model::ReExport],
) -> fmt::Result {
    let mut by_source: Vec<(&str, Vec<&str>, bool)> = Vec::new();

    for re in reexports {
        if let Some(existing) = by_source
            .iter_mut()
            .find(|(s, _, t)| *s == re.source && *t == re.is_type)
        {
            existing.1.extend(re.names.iter().map(String::as_str));
        } else {
            by_source.push((
                &re.source,
                re.names.iter().map(String::as_str).collect(),
                re.is_type,
            ));
        }
    }

    writeln!(f, "re-exports:")?;
    for (source, names, is_type) in &by_source {
        let prefix = if *is_type { "type " } else { "" };
        if names.len() <= 6 {
            writeln!(f, "  {prefix}{{{}}}", names.join(", "))?;
            writeln!(f, "    from {source}")?;
        } else {
            let shown = &names[..6];
            writeln!(
                f,
                "  {prefix}{{{}, ...+{}}}",
                shown.join(", "),
                names.len() - 6
            )?;
            writeln!(f, "    from {source}")?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::LanguageKind;
    use crate::model::{FileSummary, Symbol};

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

    fn make_symbol(signature: &str, calls: Vec<&str>) -> Symbol {
        Symbol {
            signature: signature.to_string(),
            line_start: 1,
            line_end: 2,
            calls: calls.into_iter().map(String::from).collect(),
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        }
    }

    #[test]
    fn extract_name_from_signature_function() {
        assert_eq!(
            extract_name_from_signature("function greet(name: string)"),
            "greet"
        );
    }

    #[test]
    fn extract_name_from_signature_const() {
        assert_eq!(
            extract_name_from_signature("const App = (props: Props)"),
            "App"
        );
    }

    #[test]
    fn extract_name_from_signature_async() {
        assert_eq!(
            extract_name_from_signature("async function fetchData()"),
            "fetchData"
        );
    }

    #[test]
    fn extract_name_from_signature_class() {
        assert_eq!(
            extract_name_from_signature("class MyService extends Base"),
            "MyService"
        );
    }

    #[test]
    fn extract_name_from_signature_public_async_method() {
        assert_eq!(
            extract_name_from_signature("public async send(msg: string)"),
            "send"
        );
    }

    #[test]
    fn extract_name_from_signature_private_static_method() {
        assert_eq!(
            extract_name_from_signature("private static getInstance()"),
            "getInstance"
        );
    }

    #[test]
    fn extract_name_from_signature_protected_method() {
        assert_eq!(extract_name_from_signature("protected onInit()"), "onInit");
    }

    #[test]
    fn file_summary_display_format() {
        let summary = FileSummary {
            display_path: "src/hooks/use-chat.ts".to_string(),
            total_lines: 360,
            export_names: vec!["useChat".to_string()],
            type_names: vec!["ChatMessage".to_string(), "ChatOptions".to_string()],
        };
        let output = format!("{summary}");
        assert!(output.contains("src/hooks/use-chat.ts  (360 lines)"));
        assert!(output.contains("exports: useChat"));
        assert!(output.contains("types: ChatMessage, ChatOptions"));
    }

    #[test]
    fn file_summary_empty_exports_and_types() {
        let summary = FileSummary {
            display_path: "src/empty.ts".to_string(),
            total_lines: 5,
            export_names: vec![],
            type_names: vec![],
        };
        let output = format!("{summary}");
        assert_eq!(output, "src/empty.ts  (5 lines)");
    }

    #[test]
    fn file_digest_display_uses_ts_calls_label_even_for_structural_like_tokens() {
        let mut symbols = empty_symbols();
        symbols.internals.push(make_symbol(
            "function weird()",
            vec!["source:looks_like_sql_ref"],
        ));

        let digest = FileDigest {
            display_path: "src/weird.ts".to_string(),
            language_kind: LanguageKind::Ts,
            ext: "ts".to_string(),
            total_lines: 2,
            symbols,
        };

        let rendered = format!("{digest}");
        assert!(rendered.contains("calls: source:looks_like_sql_ref"));
        assert!(!rendered.contains("refs: source:looks_like_sql_ref"));
    }

    #[test]
    fn file_digest_display_uses_sql_refs_label_even_for_non_structural_tokens() {
        let mut symbols = empty_symbols();
        symbols
            .internals
            .push(make_symbol("SELECT 1", vec!["count"]));

        let digest = FileDigest {
            display_path: "queries/test.sql".to_string(),
            language_kind: LanguageKind::Sql,
            ext: "sql".to_string(),
            total_lines: 1,
            symbols,
        };

        let rendered = format!("{digest}");
        assert!(rendered.contains("refs: count"));
        assert!(!rendered.contains("calls: count"));
    }
}
