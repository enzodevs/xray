use std::fmt;

/// A function or method extracted from the AST.
pub struct Symbol {
    pub signature: String,
    pub line_start: usize,
    pub line_end: usize,
    pub calls: Vec<String>,
}

/// Wrapper for indented display of a value.
pub struct Indented<'a, T>(pub &'a str, pub &'a T);

impl fmt::Display for Indented<'_, Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, sym) = self;
        write!(
            f,
            "{}{}  [L{}-{}]",
            indent, sym.signature, sym.line_start, sym.line_end
        )?;
        if !sym.calls.is_empty() {
            write!(f, "\n{}  calls: {}", indent, sym.calls.join(", "))?;
        }
        Ok(())
    }
}

/// A re-export statement (`export { x } from './module'`).
pub struct ReExport {
    pub names: Vec<String>,
    pub source: String,
    pub is_type: bool,
}

/// A type definition (interface, type alias, or enum).
pub struct TypeDef {
    pub name: String,
    pub kind: String,
    pub extends: String,
    pub summary: String,
    pub line_start: usize,
    pub line_end: usize,
    pub exported: bool,
}

impl fmt::Display for Indented<'_, TypeDef> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, td) = self;
        let exp = if td.exported { "export " } else { "" };
        write!(f, "{}{}{} {}", indent, exp, td.kind, td.name)?;
        if !td.extends.is_empty() {
            write!(f, " extends {}", td.extends)?;
        }
        if !td.summary.is_empty() {
            write!(f, " {}", td.summary)?;
        }
        write!(f, "  [L{}-{}]", td.line_start, td.line_end)
    }
}

/// A test block (`describe`, `it`, or `test`) with optional nesting.
pub struct TestBlock {
    pub kind: String,
    pub name: String,
    pub line_start: usize,
    pub line_end: usize,
    pub children: Vec<TestBlock>,
}

/// Write a test tree with recursive indentation.
pub fn write_test_tree(f: &mut fmt::Formatter<'_>, tests: &[TestBlock], indent: &str) -> fmt::Result {
    for t in tests {
        writeln!(f, "{}{} {:?}  [L{}-{}]", indent, t.kind, t.name, t.line_start, t.line_end)?;
        if !t.children.is_empty() {
            let deeper = format!("{indent}  ");
            write_test_tree(f, &t.children, &deeper)?;
        }
    }
    Ok(())
}

/// A React hook call extracted from a component.
pub struct Hook {
    pub kind: String,
    pub bindings: Vec<String>,
    pub line_start: usize,
    pub line_end: usize,
}

impl fmt::Display for Indented<'_, Hook> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, h) = self;
        write!(f, "{}{}", indent, h.kind)?;
        if !h.bindings.is_empty() {
            write!(f, ": {}", h.bindings.join(", "))?;
        }
        write!(f, "  [L{}-{}]", h.line_start, h.line_end)
    }
}

/// All symbols extracted from a single file.
pub struct FileSymbols {
    pub imports: Vec<String>,
    pub reexports: Vec<ReExport>,
    pub exports: Vec<Symbol>,
    pub internals: Vec<Symbol>,
    pub types: Vec<TypeDef>,
    pub tests: Vec<TestBlock>,
    pub hooks: Vec<Hook>,
}
