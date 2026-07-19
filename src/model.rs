use std::fmt;

use serde::Serialize;

/// Stable one-based source range used by machine-readable output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start_line: usize,
    pub end_line: usize,
}

/// Normalized symbol category. Extractors retain richer text in `signature`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SymbolKind {
    Function,
    Method,
    Variable,
    Class,
    Interface,
    Trait,
    Enum,
    SqlStatement,
    Component,
    Unknown,
}

/// Normalized relationship category for calls and structural references.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReferenceKind {
    Call,
    Target,
    Source,
    Join,
    Cte,
    Function,
}

/// Typed view over the compact strings retained by extractors.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Reference {
    pub kind: ReferenceKind,
    pub target: String,
}

/// A JSX component node in the render tree.
#[derive(Debug, PartialEq, Serialize)]
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

impl Symbol {
    pub fn span(&self) -> Span {
        Span {
            start_line: self.line_start,
            end_line: self.line_end,
        }
    }

    pub fn name(&self) -> String {
        symbol_name_from_signature(&self.signature)
    }

    pub fn kind(&self) -> SymbolKind {
        symbol_kind_from_signature(&self.signature, self.is_component)
    }

    pub fn references(&self) -> Vec<Reference> {
        self.calls
            .iter()
            .map(|value| Reference::from(value.as_str()))
            .collect()
    }
}

impl From<&str> for Reference {
    fn from(value: &str) -> Self {
        let prefixes = [
            ("target:", ReferenceKind::Target),
            ("source:", ReferenceKind::Source),
            ("join:", ReferenceKind::Join),
            ("cte:", ReferenceKind::Cte),
            ("fn:", ReferenceKind::Function),
        ];
        for (prefix, kind) in prefixes {
            if let Some(target) = value.strip_prefix(prefix) {
                return Self {
                    kind,
                    target: target.to_string(),
                };
            }
        }
        Self {
            kind: ReferenceKind::Call,
            target: value.to_string(),
        }
    }
}

fn symbol_kind_from_signature(signature: &str, is_component: bool) -> SymbolKind {
    if is_component {
        return SymbolKind::Component;
    }
    let lower = signature.trim_start().to_ascii_lowercase();
    if [
        "create ", "alter ", "select ", "insert ", "update ", "delete ", "merge ",
    ]
    .iter()
    .any(|prefix| lower.starts_with(prefix))
    {
        SymbolKind::SqlStatement
    } else if lower.contains(" class ") || lower.starts_with("class ") {
        SymbolKind::Class
    } else if lower.contains(" interface ") || lower.starts_with("interface ") {
        SymbolKind::Interface
    } else if lower.contains(" trait ") || lower.starts_with("trait ") {
        SymbolKind::Trait
    } else if lower.contains(" enum ") || lower.starts_with("enum ") {
        SymbolKind::Enum
    } else if ["const ", "let ", "var "]
        .iter()
        .any(|prefix| lower.starts_with(prefix))
    {
        SymbolKind::Variable
    } else if lower.starts_with("func (") {
        SymbolKind::Method
    } else if ["fn ", "func ", "function ", "def "]
        .iter()
        .any(|marker| lower.starts_with(marker) || lower.contains(marker))
    {
        SymbolKind::Function
    } else if signature.contains('(') {
        SymbolKind::Method
    } else {
        SymbolKind::Unknown
    }
}

fn symbol_name_from_signature(signature: &str) -> String {
    let mut value = signature.trim();
    let words: Vec<&str> = value.split_whitespace().collect();
    let sql_name_index = match words.as_slice() {
        [first, second, ..]
            if first.eq_ignore_ascii_case("create") && second.eq_ignore_ascii_case("or") =>
        {
            Some(4)
        }
        [first, second, ..]
            if (first.eq_ignore_ascii_case("insert") && second.eq_ignore_ascii_case("into"))
                || (first.eq_ignore_ascii_case("delete")
                    && second.eq_ignore_ascii_case("from"))
                || (first.eq_ignore_ascii_case("merge") && second.eq_ignore_ascii_case("into")) =>
        {
            Some(2)
        }
        [first, ..]
            if first.eq_ignore_ascii_case("create") || first.eq_ignore_ascii_case("alter") =>
        {
            Some(2)
        }
        [first, ..] if first.eq_ignore_ascii_case("update") => Some(1),
        _ => None,
    };
    if let Some(index) = sql_name_index {
        return words.get(index).map_or_else(
            || "?".to_string(),
            |name| {
                name.split('(')
                    .next()
                    .unwrap_or(name)
                    .trim_end_matches(';')
                    .to_string()
            },
        );
    }
    let declaration_prefixes = [
        "pub ",
        "public ",
        "private ",
        "protected ",
        "static ",
        "abstract ",
        "override ",
        "readonly ",
        "async ",
        "unsafe ",
        "def ",
        "const ",
        "let ",
        "var ",
        "function ",
        "class ",
        "interface ",
        "trait ",
        "enum ",
        "func ",
        "fn ",
    ];
    loop {
        let before = value;
        for prefix in declaration_prefixes {
            value = value.trim_start_matches(prefix);
        }
        if value == before {
            break;
        }
    }
    if let Some((_, rest)) = value.strip_prefix('(').and_then(|s| s.split_once(") ")) {
        value = rest;
    }
    value
        .split(['(', '=', '<', ' ', ':'])
        .next()
        .filter(|name| !name.is_empty())
        .unwrap_or("?")
        .to_string()
}

impl Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("Symbol", 10)?;
        state.serialize_field("name", &self.name())?;
        state.serialize_field("kind", &self.kind())?;
        state.serialize_field("signature", &self.signature)?;
        state.serialize_field("span", &self.span())?;
        state.serialize_field("references", &self.references())?;
        state.serialize_field("component", &self.is_component)?;
        state.serialize_field("renders", &self.renders)?;
        state.serialize_field("hooks", &self.hooks)?;
        state.serialize_field("handlers", &self.handlers)?;
        state.serialize_field("decorators", &self.decorators)?;
        state.end()
    }
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
#[derive(Serialize)]
pub struct ImportBinding {
    pub local_name: String,
    pub source: String,
    pub is_default: bool,
}

/// A re-export statement (`export { x } from './module'`).
#[derive(Serialize)]
pub struct ReExport {
    pub names: Vec<String>,
    pub source: String,
    pub is_type: bool,
}

/// A type definition (interface, type alias, or enum).
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
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
                indent, scope, kind, link.target, link.label, link.line_start, link.line_end
            )
        }
    }
}

/// A fenced code block extracted from a Markdown document.
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
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
#[derive(Serialize)]
#[serde(tag = "kind", content = "data", rename_all = "snake_case")]
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

#[cfg(test)]
mod tests {
    use super::{symbol_kind_from_signature, symbol_name_from_signature, SymbolKind};

    #[test]
    fn extracts_names_after_multiple_declaration_modifiers() {
        assert_eq!(
            symbol_name_from_signature("async const load = () => value"),
            "load"
        );
        assert_eq!(
            symbol_name_from_signature("pub unsafe fn process()"),
            "process"
        );
    }

    #[test]
    fn classifies_go_receiver_functions_as_methods() {
        assert_eq!(
            symbol_kind_from_signature("func (s *Server) Start()", false),
            SymbolKind::Method
        );
        assert_eq!(
            symbol_kind_from_signature("func Start()", false),
            SymbolKind::Function
        );
    }

    #[test]
    fn extracts_sql_target_names() {
        assert_eq!(
            symbol_name_from_signature("CREATE TABLE objects"),
            "objects"
        );
        assert_eq!(
            symbol_name_from_signature("CREATE OR REPLACE FUNCTION get_tenant_id()"),
            "get_tenant_id"
        );
        assert_eq!(
            symbol_name_from_signature("INSERT INTO audit_log"),
            "audit_log"
        );
    }
}
