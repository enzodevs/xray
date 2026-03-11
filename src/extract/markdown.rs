use std::collections::HashMap;

use markdown::mdast::{Code, Definition, Image, ImageReference, Link, LinkReference, Node};
use markdown::unist::Position;

use crate::model::{
    MarkdownCodeBlock, MarkdownDocument, MarkdownFrontmatter, MarkdownHeading, MarkdownLink,
};

pub(super) fn extract_document(root: &Node, source: &str) -> MarkdownDocument {
    let definitions = collect_definitions(root);
    let mut headings = Vec::new();
    let mut links = Vec::new();
    let mut code_blocks = Vec::new();
    let mut frontmatter = None;

    walk_markdown(root, &mut |node| match node {
        Node::Heading(heading) => headings.push(FlatHeading::from_heading(heading)),
        Node::Link(link) => {
            links.push(make_direct_link(link));
        }
        Node::Image(image) => {
            links.push(make_image_link(image));
        }
        Node::LinkReference(reference) => {
            if let Some(markdown_link) = make_reference_link(reference, &definitions) {
                links.push(markdown_link);
            }
        }
        Node::ImageReference(reference) => {
            if let Some(markdown_link) = make_image_reference(reference, &definitions) {
                links.push(markdown_link);
            }
        }
        Node::Code(block) if is_fenced_code_block(block, source) => {
            let (line_start, line_end) = line_range(block.position.as_ref());
            code_blocks.push(MarkdownCodeBlock {
                language: block
                    .lang
                    .as_deref()
                    .map(str::trim)
                    .filter(|lang| !lang.is_empty())
                    .map(ToOwned::to_owned),
                line_start,
                line_end,
            });
        }
        Node::Yaml(yaml) if frontmatter.is_none() => {
            let (line_start, line_end) = line_range(yaml.position.as_ref());
            frontmatter = Some(MarkdownFrontmatter {
                kind: "yaml frontmatter".to_string(),
                line_start,
                line_end,
            });
        }
        Node::Toml(toml) if frontmatter.is_none() => {
            let (line_start, line_end) = line_range(toml.position.as_ref());
            frontmatter = Some(MarkdownFrontmatter {
                kind: "toml frontmatter".to_string(),
                line_start,
                line_end,
            });
        }
        _ => {}
    });

    MarkdownDocument {
        frontmatter,
        headings: build_heading_tree(headings),
        links,
        code_blocks,
    }
}

pub(super) fn extract_sources_only(root: &Node) -> Vec<String> {
    let definitions = collect_definitions(root);
    let mut sources = Vec::new();

    walk_markdown(root, &mut |node| {
        let target = match node {
            Node::Link(Link { url, .. }) | Node::Image(Image { url, .. }) => Some(url.as_str()),
            Node::LinkReference(LinkReference { identifier, .. })
            | Node::ImageReference(ImageReference { identifier, .. }) => {
                definitions.get(identifier).map(String::as_str)
            }
            _ => None,
        };

        let Some(target) = target else {
            return;
        };
        let Some(cleaned) = cleaned_local_target(target) else {
            return;
        };
        if !sources.contains(&cleaned) {
            sources.push(cleaned);
        }
    });

    sources
}

#[derive(Clone)]
struct FlatHeading {
    title: String,
    depth: u8,
    line_start: usize,
    line_end: usize,
}

impl FlatHeading {
    fn from_heading(heading: &markdown::mdast::Heading) -> Self {
        let (line_start, line_end) = line_range(heading.position.as_ref());
        let title = flatten_children_text(&heading.children);
        Self {
            title: if title.trim().is_empty() {
                "(empty heading)".to_string()
            } else {
                title.trim().to_string()
            },
            depth: heading.depth,
            line_start,
            line_end,
        }
    }
}

fn build_heading_tree(flat: Vec<FlatHeading>) -> Vec<MarkdownHeading> {
    let mut roots: Vec<MarkdownHeading> = Vec::new();
    let mut path: Vec<usize> = Vec::new();

    for heading in flat {
        while let Some(current) = get_heading(&roots, &path) {
            if current.depth < heading.depth {
                break;
            }
            path.pop();
        }

        let siblings = heading_children_mut(&mut roots, &path);
        siblings.push(MarkdownHeading {
            title: heading.title,
            depth: heading.depth,
            line_start: heading.line_start,
            line_end: heading.line_end,
            children: Vec::new(),
        });
        path.push(siblings.len() - 1);
    }

    roots
}

fn get_heading<'a>(roots: &'a [MarkdownHeading], path: &[usize]) -> Option<&'a MarkdownHeading> {
    let mut headings = roots;
    let mut current = None;
    for &index in path {
        let heading = headings.get(index)?;
        current = Some(heading);
        headings = &heading.children;
    }
    current
}

fn heading_children_mut<'a>(
    roots: &'a mut Vec<MarkdownHeading>,
    path: &[usize],
) -> &'a mut Vec<MarkdownHeading> {
    let mut headings = roots;
    for &index in path {
        headings = &mut headings[index].children;
    }
    headings
}

fn collect_definitions(root: &Node) -> HashMap<String, String> {
    let mut definitions = HashMap::new();
    walk_markdown(root, &mut |node| {
        if let Node::Definition(Definition {
            identifier, url, ..
        }) = node
        {
            definitions
                .entry(identifier.clone())
                .or_insert_with(|| url.clone());
        }
    });
    definitions
}

fn make_direct_link(link: &Link) -> MarkdownLink {
    let (line_start, line_end) = line_range(link.position.as_ref());
    MarkdownLink {
        label: flatten_children_text(&link.children),
        target: link.url.clone(),
        line_start,
        line_end,
        is_local: is_local_target(&link.url),
        is_image: false,
    }
}

fn make_image_link(image: &Image) -> MarkdownLink {
    let (line_start, line_end) = line_range(image.position.as_ref());
    MarkdownLink {
        label: image.alt.clone(),
        target: image.url.clone(),
        line_start,
        line_end,
        is_local: is_local_target(&image.url),
        is_image: true,
    }
}

fn make_reference_link(
    reference: &LinkReference,
    definitions: &HashMap<String, String>,
) -> Option<MarkdownLink> {
    let target = definitions.get(&reference.identifier)?;
    let (line_start, line_end) = line_range(reference.position.as_ref());
    Some(MarkdownLink {
        label: flatten_children_text(&reference.children),
        target: target.clone(),
        line_start,
        line_end,
        is_local: is_local_target(target),
        is_image: false,
    })
}

fn make_image_reference(
    reference: &ImageReference,
    definitions: &HashMap<String, String>,
) -> Option<MarkdownLink> {
    let target = definitions.get(&reference.identifier)?;
    let (line_start, line_end) = line_range(reference.position.as_ref());
    Some(MarkdownLink {
        label: reference.alt.clone(),
        target: target.clone(),
        line_start,
        line_end,
        is_local: is_local_target(target),
        is_image: true,
    })
}

fn flatten_children_text(children: &[Node]) -> String {
    children.iter().map(flatten_node_text).collect::<String>()
}

fn flatten_node_text(node: &Node) -> String {
    match node {
        Node::Text(text) => text.value.clone(),
        Node::InlineCode(code) => code.value.clone(),
        Node::InlineMath(math) => math.value.clone(),
        Node::Image(image) => image.alt.clone(),
        Node::ImageReference(image) => image.alt.clone(),
        Node::Break(_) => " ".to_string(),
        Node::Delete(delete) => flatten_children_text(&delete.children),
        Node::Emphasis(emphasis) => flatten_children_text(&emphasis.children),
        Node::FootnoteDefinition(definition) => flatten_children_text(&definition.children),
        Node::Heading(heading) => flatten_children_text(&heading.children),
        Node::Link(link) => flatten_children_text(&link.children),
        Node::LinkReference(reference) => flatten_children_text(&reference.children),
        Node::List(list) => flatten_children_text(&list.children),
        Node::ListItem(item) => flatten_children_text(&item.children),
        Node::MdxJsxFlowElement(element) => flatten_children_text(&element.children),
        Node::MdxJsxTextElement(element) => flatten_children_text(&element.children),
        Node::Paragraph(paragraph) => flatten_children_text(&paragraph.children),
        Node::Root(root) => flatten_children_text(&root.children),
        Node::Strong(strong) => flatten_children_text(&strong.children),
        Node::Table(table) => flatten_children_text(&table.children),
        Node::TableCell(cell) => flatten_children_text(&cell.children),
        Node::TableRow(row) => flatten_children_text(&row.children),
        _ => String::new(),
    }
}

fn walk_markdown(node: &Node, visitor: &mut impl FnMut(&Node)) {
    visitor(node);
    match node {
        Node::Root(root) => walk_children(&root.children, visitor),
        Node::Blockquote(blockquote) => walk_children(&blockquote.children, visitor),
        Node::Delete(delete) => walk_children(&delete.children, visitor),
        Node::Emphasis(emphasis) => walk_children(&emphasis.children, visitor),
        Node::FootnoteDefinition(definition) => walk_children(&definition.children, visitor),
        Node::Heading(heading) => walk_children(&heading.children, visitor),
        Node::Link(link) => walk_children(&link.children, visitor),
        Node::LinkReference(reference) => walk_children(&reference.children, visitor),
        Node::List(list) => walk_children(&list.children, visitor),
        Node::ListItem(item) => walk_children(&item.children, visitor),
        Node::MdxJsxFlowElement(element) => walk_children(&element.children, visitor),
        Node::MdxJsxTextElement(element) => walk_children(&element.children, visitor),
        Node::Paragraph(paragraph) => walk_children(&paragraph.children, visitor),
        Node::Strong(strong) => walk_children(&strong.children, visitor),
        Node::Table(table) => walk_children(&table.children, visitor),
        Node::TableCell(cell) => walk_children(&cell.children, visitor),
        Node::TableRow(row) => walk_children(&row.children, visitor),
        _ => {}
    }
}

fn walk_children(children: &[Node], visitor: &mut impl FnMut(&Node)) {
    for child in children {
        walk_markdown(child, visitor);
    }
}

fn is_local_target(target: &str) -> bool {
    let trimmed = target.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return false;
    }

    let lower = trimmed.to_ascii_lowercase();
    !(lower.starts_with("http://")
        || lower.starts_with("https://")
        || lower.starts_with("mailto:")
        || lower.starts_with("tel:")
        || lower.starts_with("//"))
}

fn cleaned_local_target(target: &str) -> Option<String> {
    if !is_local_target(target) {
        return None;
    }

    let trimmed = target.trim();
    let cutoff = trimmed
        .find(['#', '?'])
        .unwrap_or(trimmed.len());
    let cleaned = trimmed[..cutoff].trim();
    if cleaned.is_empty() {
        None
    } else {
        Some(cleaned.to_string())
    }
}

fn is_fenced_code_block(block: &Code, source: &str) -> bool {
    let Some(position) = &block.position else {
        return false;
    };
    source
        .lines()
        .nth(position.start.line.saturating_sub(1))
        .is_some_and(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("```") || trimmed.starts_with("~~~")
        })
}

fn line_range(position: Option<&Position>) -> (usize, usize) {
    position.map_or((1, 1), |position| {
        let line_start = position.start.line;
        let line_end = if position.end.column == 1 && position.end.line > line_start {
            position.end.line - 1
        } else {
            position.end.line
        };
        (line_start, line_end.max(line_start))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_markdown(src: &str) -> Node {
        markdown::to_mdast(src, &crate::parser::markdown_parse_options()).unwrap()
    }

    #[test]
    fn extract_sources_only_collects_local_links_and_reference_links() {
        let root = parse_markdown(
            r"
[Guide](./guide.md)
[Docs][docs]
[External](https://example.com)

[docs]: ../docs/readme.md
",
        );

        let sources = extract_sources_only(&root);
        assert_eq!(
            sources,
            vec!["./guide.md".to_string(), "../docs/readme.md".to_string()]
        );
    }

    #[test]
    fn extract_document_builds_heading_tree() {
        let root = parse_markdown("# Root\n## Child\n### Grandchild\n## Peer\n");
        let document = extract_document(&root, "# Root\n## Child\n### Grandchild\n## Peer\n");

        assert_eq!(document.headings.len(), 1);
        assert_eq!(document.headings[0].title, "Root");
        assert_eq!(document.headings[0].children.len(), 2);
        assert_eq!(document.headings[0].children[0].title, "Child");
        assert_eq!(document.headings[0].children[0].children[0].title, "Grandchild");
        assert_eq!(document.headings[0].children[1].title, "Peer");
    }

    #[test]
    fn extract_document_marks_frontmatter_and_fenced_code_blocks() {
        let src = "---\ntitle: Test\n---\n\n```rust\nfn main() {}\n```\n";
        let root = parse_markdown(src);
        let document = extract_document(&root, src);

        assert!(document.frontmatter.is_some());
        assert_eq!(document.code_blocks.len(), 1);
        assert_eq!(document.code_blocks[0].language.as_deref(), Some("rust"));
    }
}
