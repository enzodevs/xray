//! Static Inertia facade calls only: no helpers, dynamic names, interpolation or escapes.

use tree_sitter::Node;

use super::dependencies::{visit_scopes, Context};
use super::{push_unique, txt};

pub(super) fn collect(root: Node, src: &[u8], imports: &mut Vec<String>) {
    visit_scopes(root, src, |nodes, context| {
        for &node in nodes {
            visit(node, src, context, imports);
        }
    });
}

fn visit(node: Node, src: &[u8], context: &Context, imports: &mut Vec<String>) {
    if let Some(page) = render_page(node, src, context) {
        // Named provenance prevents these dependencies being treated as PHP classes.
        push_unique(imports, format!("inertia:{page}"));
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        visit(child, src, context, imports);
    }
}

fn render_page<'a>(node: Node, src: &'a [u8], context: &Context) -> Option<&'a str> {
    if node.kind() != "scoped_call_expression" {
        return None;
    }
    let scope = context.resolve(node.child_by_field_name("scope")?, src)?;
    let method = node.child_by_field_name("name")?;
    if !scope.eq_ignore_ascii_case("Inertia\\Inertia")
        || method.kind() != "name"
        || !txt(method, src).eq_ignore_ascii_case("render")
    {
        return None;
    }
    let arguments = node.child_by_field_name("arguments")?;
    let mut cursor = arguments.walk();
    let mut args = arguments
        .named_children(&mut cursor)
        .filter(|arg| arg.kind() == "argument");
    let first = args.next()?;
    let arg = if first.child_by_field_name("name").is_none() {
        first
    } else {
        std::iter::once(first).chain(args).find(|arg| {
            arg.child_by_field_name("name")
                .is_some_and(|name| txt(name, src) == "component")
        })?
    };
    let mut cursor = arg.walk();
    let value = arg.named_children(&mut cursor).last()?;
    if !matches!(value.kind(), "string" | "encapsed_string") {
        return None;
    }
    let raw = txt(value, src);
    // PHP's optional binary-string prefix does not change the literal page name.
    let raw = raw
        .strip_prefix('b')
        .or_else(|| raw.strip_prefix('B'))
        .unwrap_or(raw);
    let page = raw
        .strip_prefix('\'')
        .and_then(|value| value.strip_suffix('\''))
        .or_else(|| {
            raw.strip_prefix('"')
                .and_then(|value| value.strip_suffix('"'))
        })?;
    // No PHP evaluation: even escaped literals are outside this small convention.
    if page.is_empty() || page.contains('\\') || page.chars().any(char::is_control) {
        return None;
    }
    let mut cursor = value.walk();
    let plain = value
        .named_children(&mut cursor)
        .all(|child| child.kind() == "string_content");
    plain.then_some(page)
}

#[cfg(test)]
mod tests {
    use crate::extract::php::{extract_sources_only, extract_symbols, tests::parse_php};
    use crate::lang::LanguageKind;
    use crate::parser::{ParsedContent, ParsedFile};

    fn pages(source: &str) -> Vec<String> {
        let tree = parse_php(source.as_bytes());
        assert!(
            !tree.root_node().has_error(),
            "{}",
            tree.root_node().to_sexp()
        );
        let symbols = extract_symbols(tree.root_node(), source.as_bytes());
        assert_eq!(
            symbols.imports,
            extract_sources_only(tree.root_node(), source.as_bytes())
        );
        let parsed = ParsedFile {
            language_kind: LanguageKind::Php,
            content: ParsedContent::Code(tree),
            source: source.to_string(),
        };
        assert_eq!(symbols.imports, parsed.diagnostic_specifiers());
        symbols
            .imports
            .into_iter()
            .filter(|s| s.starts_with("inertia:"))
            .collect()
    }

    #[test]
    fn literal_facade_calls_preserve_provenance_aliases_and_namespace_scope() {
        assert_eq!(
            pages(
                r#"<?php
            namespace One {
                use Inertia\Inertia as UI;
                UI::render('school/Home');
                ui::RENDER("Dashboard");
                UI::render('school/Home');
                \Inertia\Inertia::render('auth/Login');
                UI::render(props: [], component: 'settings/Profile');
            }
            namespace Two {
                use Other\Renderer as UI;
                UI::render('NotInertia');
                Inertia::render('NotImported');
            }
            namespace Three {
                use Inertia\{Inertia as Page};
                Page::render('Grouped');
            }
        "#
            ),
            [
                "inertia:school/Home",
                "inertia:Dashboard",
                "inertia:auth/Login",
                "inertia:settings/Profile",
                "inertia:Grouped"
            ]
        );
    }

    #[test]
    fn binary_prefixes_and_non_interpolated_dollars_preserve_literal_names() {
        assert_eq!(
            pages(
                r#"<?php use Inertia\Inertia;
                Inertia::render(b'Home');
                Inertia::render(B"Dashboard");
                Inertia::render('Dollar$Literal');
                Inertia::render("Trailing$");
            "#
            ),
            [
                "inertia:Home",
                "inertia:Dashboard",
                "inertia:Dollar$Literal",
                "inertia:Trailing$"
            ]
        );
    }

    #[test]
    fn dynamic_interpolated_escaped_and_unrelated_calls_are_not_dependencies() {
        assert!(pages(
            r#"<?php
            use Inertia\Inertia;
            Inertia::render($page);
            Inertia::render("school/$page");
            Inertia::render("school/{$page}");
            Inertia::render("school/${page}");
            Inertia::render(B"school/$page");
            Inertia::render('school/' . $page);
            Inertia::render("escaped\x2fPage");
            Inertia::render('escaped\'Page');
            Inertia::render(...$pages);
            Inertia::render(props: ['NotAComponent']);
            Inertia::render('');
            Inertia::other('NotRender');
            Inertia::$method('DynamicMethod');
            Renderer::render('Unrelated');
            $renderer->render('Instance');
            inertia('Helper');
        "#
        )
        .is_empty());
    }
}
