use std::fmt;

use crate::model::{write_test_tree, FileSymbols, Indented};

/// Complete digest of a single file, ready for display.
pub struct FileDigest {
    pub display_path: String,
    pub ext: String,
    pub total_lines: usize,
    pub symbols: FileSymbols,
}

impl fmt::Display for FileDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                writeln!(f, "{}", Indented("  ", sym))?;
            }
            if has_more_sections(&self.symbols, SectionAfter::Exports) {
                writeln!(f)?;
            }
        }

        if !self.symbols.internals.is_empty() {
            writeln!(f, "internal:")?;
            for sym in &self.symbols.internals {
                writeln!(f, "{}", Indented("  ", sym))?;
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
