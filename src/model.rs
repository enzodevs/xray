use std::fmt;

/// A JSX component node in the render tree.
#[derive(Debug, PartialEq)]
pub struct JsxNode {
    pub name: String,
    pub children: Vec<JsxNode>,
}

impl fmt::Display for JsxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        match self.children.len() {
            0 => {}
            1 => write!(f, " > {}", self.children[0])?,
            _ => {
                write!(f, " > [")?;
                for (i, child) in self.children.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{child}")?;
                }
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

/// A function or method extracted from the AST.
pub struct Symbol {
    pub signature: String,
    pub line_start: usize,
    pub line_end: usize,
    pub calls: Vec<String>,
    pub is_component: bool,
    pub renders: Vec<JsxNode>,
    pub hooks: Vec<Hook>,
    pub handlers: Vec<String>,
    pub decorators: Vec<String>,
}

/// Wrapper for indented display of a value.
pub struct Indented<'a, T>(pub &'a str, pub &'a T);

#[derive(Clone, Copy)]
pub(crate) enum SymbolRefsLabel {
    Auto,
    Calls,
    Refs,
}

pub(crate) struct SymbolIndented<'a> {
    indent: &'a str,
    symbol: &'a Symbol,
    refs_label: SymbolRefsLabel,
}

pub(crate) fn indented_symbol<'a>(
    indent: &'a str,
    symbol: &'a Symbol,
    refs_label: SymbolRefsLabel,
) -> SymbolIndented<'a> {
    SymbolIndented {
        indent,
        symbol,
        refs_label,
    }
}

impl fmt::Display for Indented<'_, Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, sym) = self;
        fmt_symbol_with_refs_label(f, indent, sym, SymbolRefsLabel::Auto)
    }
}

impl fmt::Display for SymbolIndented<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_symbol_with_refs_label(f, self.indent, self.symbol, self.refs_label)
    }
}

fn fmt_symbol_with_refs_label(
    f: &mut fmt::Formatter<'_>,
    indent: &str,
    sym: &Symbol,
    refs_label: SymbolRefsLabel,
) -> fmt::Result {
    let label_for_calls = match refs_label {
        SymbolRefsLabel::Calls => "calls",
        SymbolRefsLabel::Refs => "refs",
        SymbolRefsLabel::Auto => {
            if sym.calls.iter().all(|c| is_structural_ref(c)) {
                "refs"
            } else {
                "calls"
            }
        }
    };

    let marker = if sym.is_component { "[component] " } else { "" };
    write!(
        f,
        "{}{marker}{}  [L{}-{}]",
        indent, sym.signature, sym.line_start, sym.line_end
    )?;
    if !sym.hooks.is_empty() {
        write!(f, "\n{indent}  hooks:")?;
        let deeper = format!("{indent}    ");
        for h in &sym.hooks {
            write!(f, "\n{}", Indented(&deeper, h))?;
        }
    }
    if !sym.handlers.is_empty() {
        write!(f, "\n{}  handlers: {}", indent, sym.handlers.join(", "))?;
    }
    if !sym.calls.is_empty() {
        write!(
            f,
            "\n{}  {label_for_calls}: {}",
            indent,
            sym.calls.join(", ")
        )?;
    }
    if !sym.renders.is_empty() {
        write!(f, "\n{indent}  renders: ")?;
        for (i, node) in sym.renders.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{node}")?;
        }
    }
    if !sym.decorators.is_empty() {
        let decorated: Vec<String> = sym.decorators.iter().map(|d| format!("@{d}")).collect();
        write!(f, "\n{}  decorators: {}", indent, decorated.join(", "))?;
    }
    Ok(())
}

fn is_structural_ref(value: &str) -> bool {
    ["target:", "source:", "join:", "cte:", "fn:"]
        .iter()
        .any(|prefix| value.starts_with(prefix))
}

/// A single name imported from another module.
pub struct ImportBinding {
    pub local_name: String,
    pub source: String,
    pub is_default: bool,
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
pub fn write_test_tree(
    f: &mut fmt::Formatter<'_>,
    tests: &[TestBlock],
    indent: &str,
) -> fmt::Result {
    for t in tests {
        writeln!(
            f,
            "{}{} {:?}  [L{}-{}]",
            indent, t.kind, t.name, t.line_start, t.line_end
        )?;
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
    pub deps: Option<Vec<String>>,
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
        if let Some(deps) = &h.deps {
            if deps.is_empty() {
                write!(f, "  deps: []")?;
            } else {
                write!(f, "  deps: [{}]", deps.join(", "))?;
            }
        }
        write!(f, "  [L{}-{}]", h.line_start, h.line_end)
    }
}

/// A heading in a Markdown document.
pub struct MarkdownHeading {
    pub title: String,
    pub depth: u8,
    pub line_start: usize,
    pub line_end: usize,
    pub children: Vec<MarkdownHeading>,
}

impl fmt::Display for Indented<'_, MarkdownHeading> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, heading) = self;
        write!(
            f,
            "{}H{} {}  [L{}-{}]",
            indent, heading.depth, heading.title, heading.line_start, heading.line_end
        )
    }
}

/// Write a Markdown heading tree with recursive indentation.
pub fn write_markdown_heading_tree(
    f: &mut fmt::Formatter<'_>,
    headings: &[MarkdownHeading],
    indent: &str,
) -> fmt::Result {
    for heading in headings {
        writeln!(f, "{}", Indented(indent, heading))?;
        if !heading.children.is_empty() {
            let deeper = format!("{indent}  ");
            write_markdown_heading_tree(f, &heading.children, &deeper)?;
        }
    }
    Ok(())
}

/// A link-like reference extracted from a Markdown document.
pub struct MarkdownLink {
    pub label: String,
    pub target: String,
    pub line_start: usize,
    pub line_end: usize,
    pub is_local: bool,
    pub is_image: bool,
}

impl fmt::Display for Indented<'_, MarkdownLink> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, link) = self;
        let scope = if link.is_local { "local" } else { "external" };
        let kind = if link.is_image { "image" } else { "link" };
        if link.label.is_empty() || link.label == link.target {
            write!(
                f,
                "{}{} {}: {}  [L{}-{}]",
                indent, scope, kind, link.target, link.line_start, link.line_end
            )
        } else {
            write!(
                f,
                "{}{} {}: {}  {:?}  [L{}-{}]",
                indent,
                scope,
                kind,
                link.target,
                link.label,
                link.line_start,
                link.line_end
            )
        }
    }
}

/// A fenced code block extracted from a Markdown document.
pub struct MarkdownCodeBlock {
    pub language: Option<String>,
    pub line_start: usize,
    pub line_end: usize,
}

impl fmt::Display for Indented<'_, MarkdownCodeBlock> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, block) = self;
        let lang = block.language.as_deref().unwrap_or("text");
        write!(
            f,
            "{}{}  [L{}-{}]",
            indent, lang, block.line_start, block.line_end
        )
    }
}

/// Frontmatter block extracted from a Markdown document.
pub struct MarkdownFrontmatter {
    pub kind: String,
    pub line_start: usize,
    pub line_end: usize,
}

impl fmt::Display for Indented<'_, MarkdownFrontmatter> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Indented(indent, frontmatter) = self;
        write!(
            f,
            "{}{}  [L{}-{}]",
            indent, frontmatter.kind, frontmatter.line_start, frontmatter.line_end
        )
    }
}

/// Structured digest data for a Markdown document.
pub struct MarkdownDocument {
    pub frontmatter: Option<MarkdownFrontmatter>,
    pub headings: Vec<MarkdownHeading>,
    pub links: Vec<MarkdownLink>,
    pub code_blocks: Vec<MarkdownCodeBlock>,
}

/// Compact representation of a file for follow-mode child nodes.
pub struct FileSummary {
    pub display_path: String,
    pub total_lines: usize,
    pub kind: FileSummaryKind,
}

/// Compact summary content varies by file kind.
pub enum FileSummaryKind {
    Code {
        export_names: Vec<String>,
        type_names: Vec<String>,
    },
    Markdown {
        heading_titles: Vec<String>,
    },
}

/// All code symbols extracted from a single file.
pub struct FileSymbols {
    pub imports: Vec<String>,
    pub import_bindings: Vec<ImportBinding>,
    pub reexports: Vec<ReExport>,
    pub exports: Vec<Symbol>,
    pub internals: Vec<Symbol>,
    pub types: Vec<TypeDef>,
    pub tests: Vec<TestBlock>,
    pub hooks: Vec<Hook>,
}

/// Structured content extracted from a supported file.
pub enum FileContent {
    Code(FileSymbols),
    Markdown(MarkdownDocument),
}

impl FileContent {
    pub fn as_code(&self) -> Option<&FileSymbols> {
        match self {
            Self::Code(symbols) => Some(symbols),
            Self::Markdown(_) => None,
        }
    }
}
