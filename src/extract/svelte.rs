use tree_sitter::{Node, Parser};

use crate::model::{FileSymbols, Hook, JsxNode, Symbol};
use crate::util::txt;

use super::{calls, extract_all_sources, extract_sources_only, extract_symbols};

#[derive(Clone, Copy)]
struct ScriptRange {
    start: usize,
    end: usize,
    is_module: bool,
}

pub(super) fn extract_symbols_from_svelte(root: Node, src: &[u8]) -> FileSymbols {
    let script_ranges = collect_script_ranges(root, src);
    let mut symbols = extract_script_symbols(src, &script_ranges);
    add_instance_props(&mut symbols, src, &script_ranges);

    if let Some(component) = extract_component_symbol(root, src) {
        symbols.exports.insert(0, component);
    }

    symbols
}

pub(super) fn extract_sources_only_from_svelte(root: Node, src: &[u8]) -> Vec<String> {
    let script_ranges = collect_script_ranges(root, src);
    let Some((tree, virtual_src)) = parse_script_tree(src, &script_ranges) else {
        return Vec::new();
    };

    extract_sources_only(tree.root_node(), virtual_src.as_bytes())
}

pub(super) fn extract_all_sources_from_svelte(root: Node, src: &[u8]) -> Vec<String> {
    let script_ranges = collect_script_ranges(root, src);
    let Some((tree, virtual_src)) = parse_script_tree(src, &script_ranges) else {
        return Vec::new();
    };

    extract_all_sources(tree.root_node(), virtual_src.as_bytes())
}

fn extract_script_symbols(src: &[u8], script_ranges: &[ScriptRange]) -> FileSymbols {
    let Some((tree, virtual_src)) = parse_script_tree(src, script_ranges) else {
        return FileSymbols {
            imports: Vec::new(),
            import_bindings: Vec::new(),
            reexports: Vec::new(),
            exports: Vec::new(),
            internals: Vec::new(),
            types: Vec::new(),
            tests: Vec::new(),
            hooks: Vec::new(),
        };
    };

    let mut symbols = extract_symbols(tree.root_node(), virtual_src.as_bytes());
    symbols.hooks.extend(extract_svelte_top_level_hooks(
        tree.root_node(),
        virtual_src.as_bytes(),
    ));
    symbols
}

fn parse_script_tree(
    src: &[u8],
    script_ranges: &[ScriptRange],
) -> Option<(tree_sitter::Tree, String)> {
    if script_ranges.is_empty() {
        return None;
    }

    let virtual_src = virtual_script_source(src, script_ranges);
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
        .ok()?;
    let tree = parser.parse(&virtual_src, None)?;
    Some((tree, virtual_src))
}

fn virtual_script_source(src: &[u8], script_ranges: &[ScriptRange]) -> String {
    let mut out: Vec<u8> = src
        .iter()
        .map(|b| if *b == b'\n' { b'\n' } else { b' ' })
        .collect();

    for range in script_ranges {
        if range.end <= src.len() && range.start <= range.end {
            out[range.start..range.end].copy_from_slice(&src[range.start..range.end]);
        }
    }

    String::from_utf8_lossy(&out).into_owned()
}

fn collect_script_ranges(root: Node, src: &[u8]) -> Vec<ScriptRange> {
    let mut ranges = Vec::new();
    collect_script_ranges_inner(root, src, &mut ranges);
    ranges
}

fn collect_script_ranges_inner(node: Node, src: &[u8], ranges: &mut Vec<ScriptRange>) {
    if node.kind() == "script_element" {
        let is_module = script_is_module(node, src);
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "raw_text" {
                ranges.push(ScriptRange {
                    start: child.start_byte(),
                    end: child.end_byte(),
                    is_module,
                });
            }
        }
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_script_ranges_inner(child, src, ranges);
    }
}

fn script_is_module(script: Node, src: &[u8]) -> bool {
    let mut cursor = script.walk();
    for child in script.children(&mut cursor) {
        if child.kind() != "start_tag" {
            continue;
        }
        let mut tag_cursor = child.walk();
        for attr in child.children(&mut tag_cursor) {
            if attr.kind() != "attribute" {
                continue;
            }
            let attr_text = txt(attr, src).trim();
            if attr_text == "module"
                || attr_text.contains("context=\"module\"")
                || attr_text.contains("context='module'")
            {
                return true;
            }
        }
    }
    false
}

fn add_instance_props(symbols: &mut FileSymbols, src: &[u8], script_ranges: &[ScriptRange]) {
    let Some((tree, virtual_src)) = parse_script_tree(src, script_ranges) else {
        return;
    };

    let mut props = Vec::new();
    collect_instance_props(
        tree.root_node(),
        virtual_src.as_bytes(),
        script_ranges,
        &mut props,
    );

    for prop in props {
        let prop_name = symbol_name(&prop.signature);
        symbols
            .exports
            .retain(|symbol| symbol_name(&symbol.signature) != prop_name);
        symbols.exports.push(prop);
    }
}

fn collect_instance_props(
    node: Node,
    src: &[u8],
    script_ranges: &[ScriptRange],
    props: &mut Vec<Symbol>,
) {
    if node.kind() == "export_statement"
        && is_instance_script_byte(node.start_byte(), script_ranges)
    {
        collect_props_from_export(node, src, props);
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_instance_props(child, src, script_ranges, props);
    }
}

fn collect_props_from_export(node: Node, src: &[u8], props: &mut Vec<Symbol>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != "lexical_declaration" {
            continue;
        }
        let mut decl_cursor = child.walk();
        for declarator in child.children(&mut decl_cursor) {
            if declarator.kind() != "variable_declarator" {
                continue;
            }
            let Some(name_node) = declarator.child_by_field_name("name") else {
                continue;
            };
            if name_node.kind() != "identifier" {
                continue;
            }
            let name = txt(name_node, src);
            if name.is_empty() {
                continue;
            }

            let type_part = declarator
                .child_by_field_name("type")
                .map_or(String::new(), |node| {
                    let annotation = txt(node, src).trim().trim_start_matches(':').trim();
                    if annotation.is_empty() {
                        String::new()
                    } else {
                        format!(": {annotation}")
                    }
                });
            let value_part =
                declarator
                    .child_by_field_name("value")
                    .map_or(String::new(), |node| {
                        let value = txt(node, src).trim();
                        if value.is_empty() {
                            String::new()
                        } else {
                            format!(" = {}", truncate_chars(value, 60))
                        }
                    });

            props.push(Symbol {
                signature: format!("prop {name}{type_part}{value_part}"),
                line_start: declarator.start_position().row + 1,
                line_end: declarator.end_position().row + 1,
                calls: declarator
                    .child_by_field_name("value")
                    .map_or_else(Vec::new, |value| calls::extract_calls(Some(value), src)),
                is_component: false,
                renders: Vec::new(),
                hooks: Vec::new(),
                handlers: Vec::new(),
                decorators: Vec::new(),
            });
        }
    }
}

fn is_instance_script_byte(byte: usize, script_ranges: &[ScriptRange]) -> bool {
    script_ranges
        .iter()
        .any(|range| !range.is_module && range.start <= byte && byte <= range.end)
}

fn extract_component_symbol(root: Node, src: &[u8]) -> Option<Symbol> {
    let mut renders = collect_template_tree(root, src);
    dedup_template_nodes(&mut renders);
    truncate_template_nodes(&mut renders, 8);

    let mut calls = collect_template_calls(root, src);
    calls.sort();
    calls.dedup();
    calls.truncate(10);

    if renders.is_empty() && calls.is_empty() {
        return None;
    }

    Some(Symbol {
        signature: "default component".to_string(),
        line_start: 1,
        line_end: root.end_position().row + 1,
        calls,
        is_component: true,
        renders,
        hooks: Vec::new(),
        handlers: Vec::new(),
        decorators: Vec::new(),
    })
}

fn collect_template_tree(node: Node, src: &[u8]) -> Vec<JsxNode> {
    match node.kind() {
        "script_element" | "style_element" => Vec::new(),
        "element" => collect_element(node, src),
        "self_closing_tag" => collect_self_closing_tag(node, src),
        _ => collect_template_children(node, src),
    }
}

fn collect_element(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let name = element_tag_name(node, src);
    let mut children = Vec::new();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if matches!(child.kind(), "start_tag" | "end_tag") {
            continue;
        }
        children.extend(collect_template_tree(child, src));
    }

    dedup_template_nodes(&mut children);
    truncate_template_nodes(&mut children, 8);

    if name.as_deref().is_some_and(is_component_tag) {
        vec![JsxNode {
            name: name.unwrap_or_default(),
            children,
        }]
    } else {
        children
    }
}

fn collect_self_closing_tag(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let Some(name) = tag_name(node, src) else {
        return Vec::new();
    };
    if is_component_tag(&name) {
        vec![JsxNode {
            name,
            children: Vec::new(),
        }]
    } else {
        Vec::new()
    }
}

fn collect_template_children(node: Node, src: &[u8]) -> Vec<JsxNode> {
    let mut result = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        result.extend(collect_template_tree(child, src));
    }
    result
}

fn element_tag_name(node: Node, src: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "start_tag" {
            return tag_name(child, src);
        }
    }
    None
}

fn tag_name(node: Node, src: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    let name = node
        .children(&mut cursor)
        .find(|child| child.kind() == "tag_name")
        .map(|child| txt(child, src).to_string());
    name
}

fn is_component_tag(name: &str) -> bool {
    name.starts_with("svelte:")
        || name
            .chars()
            .next()
            .is_some_and(|first| first.is_ascii_uppercase())
}

fn dedup_template_nodes(nodes: &mut Vec<JsxNode>) {
    nodes.sort_by(|a, b| a.name.cmp(&b.name));
    let mut i = 0;
    while i + 1 < nodes.len() {
        if nodes[i].name == nodes[i + 1].name {
            let mut extra = nodes.remove(i + 1);
            nodes[i].children.append(&mut extra.children);
        } else {
            i += 1;
        }
    }
    for node in nodes.iter_mut() {
        dedup_template_nodes(&mut node.children);
    }
}

fn truncate_template_nodes(nodes: &mut Vec<JsxNode>, max: usize) {
    if nodes.len() > max {
        nodes.truncate(max);
    }
    for node in nodes.iter_mut() {
        truncate_template_nodes(&mut node.children, max);
    }
}

fn collect_template_calls(root: Node, src: &[u8]) -> Vec<String> {
    let mut calls = Vec::new();
    collect_template_calls_inner(root, src, &mut calls);
    calls
}

fn collect_template_calls_inner(node: Node, src: &[u8], calls_out: &mut Vec<String>) {
    if matches!(node.kind(), "script_element" | "style_element") {
        return;
    }

    if is_svelte_expression_text(node.kind()) {
        calls_out.extend(parse_expression_calls(txt(node, src)));
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_template_calls_inner(child, src, calls_out);
    }
}

fn is_svelte_expression_text(kind: &str) -> bool {
    matches!(
        kind,
        "svelte_raw_text"
            | "svelte_raw_text_each"
            | "svelte_raw_text_snippet_arguments"
            | "raw_text_expr"
            | "raw_text_await"
            | "raw_text_each"
    )
}

fn parse_expression_calls(expr: &str) -> Vec<String> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        return Vec::new();
    }

    let source = format!("{trimmed};");
    let mut parser = Parser::new();
    if parser
        .set_language(&tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
        .is_err()
    {
        return Vec::new();
    }
    let Some(tree) = parser.parse(&source, None) else {
        return Vec::new();
    };

    calls::extract_calls(Some(tree.root_node()), source.as_bytes())
}

fn extract_svelte_top_level_hooks(root: Node, src: &[u8]) -> Vec<Hook> {
    let mut hooks = Vec::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        match child.kind() {
            "lexical_declaration" => {
                let mut decl_cursor = child.walk();
                for declarator in child.children(&mut decl_cursor) {
                    if declarator.kind() != "variable_declarator" {
                        continue;
                    }
                    if let Some(hook) = extract_rune_hook(declarator, src) {
                        hooks.push(hook);
                    }
                }
            }
            "expression_statement" => {
                if let Some(hook) = extract_lifecycle_hook(child, src) {
                    hooks.push(hook);
                }
            }
            _ => {}
        }
    }
    hooks
}

fn extract_rune_hook(declarator: Node, src: &[u8]) -> Option<Hook> {
    let value = declarator.child_by_field_name("value")?;
    if value.kind() != "call_expression" {
        return None;
    }
    let name = call_name(value, src)?;
    if !is_svelte_hook_name(&name) {
        return None;
    }
    let name_node = declarator.child_by_field_name("name")?;
    let bindings = collect_binding_names(name_node, src);

    Some(Hook {
        kind: name,
        bindings,
        deps: None,
        line_start: declarator.start_position().row + 1,
        line_end: declarator.end_position().row + 1,
    })
}

fn extract_lifecycle_hook(expr_stmt: Node, src: &[u8]) -> Option<Hook> {
    let mut cursor = expr_stmt.walk();
    let call = expr_stmt
        .children(&mut cursor)
        .find(|child| child.kind() == "call_expression")?;
    let name = call_name(call, src)?;
    if !is_svelte_hook_name(&name) {
        return None;
    }

    Some(Hook {
        kind: name,
        bindings: Vec::new(),
        deps: None,
        line_start: expr_stmt.start_position().row + 1,
        line_end: expr_stmt.end_position().row + 1,
    })
}

fn call_name(call: Node, src: &[u8]) -> Option<String> {
    let func = call.child_by_field_name("function")?;
    match func.kind() {
        "identifier" => Some(txt(func, src).to_string()),
        "member_expression" => func
            .child_by_field_name("property")
            .map(|node| txt(node, src).to_string()),
        _ => None,
    }
}

fn is_svelte_hook_name(name: &str) -> bool {
    matches!(
        name,
        "onMount" | "beforeUpdate" | "afterUpdate" | "onDestroy" | "tick"
    ) || name.starts_with('$')
}

fn collect_binding_names(node: Node, src: &[u8]) -> Vec<String> {
    match node.kind() {
        "identifier" => vec![txt(node, src).to_string()],
        "array_pattern" | "object_pattern" => {
            let mut names = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "identifier" {
                    names.push(txt(child, src).to_string());
                }
            }
            names
        }
        _ => Vec::new(),
    }
}

fn symbol_name(signature: &str) -> &str {
    signature
        .trim_start_matches("prop ")
        .trim_start_matches("const ")
        .trim_start_matches("let ")
        .split([':', '=', ' ', '('])
        .next()
        .unwrap_or("")
}

fn truncate_chars(value: &str, max_chars: usize) -> String {
    let mut chars = value.chars();
    let truncated: String = chars.by_ref().take(max_chars).collect();
    if chars.next().is_some() {
        format!("{truncated}...")
    } else {
        truncated
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_svelte(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_svelte_ng::LANGUAGE.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extracts_script_imports_props_types_and_template_components() {
        let src = br#"
<script lang="ts">
  import Card from './Card.svelte';
  import { formatName } from './format';

  export let name: string;
  type User = { id: string };
  const label = formatName(name);
</script>

<Card>
  <Avatar src={avatarFor(name)} />
  {#if visible()}
    <svelte:component this={Dynamic} />
  {/if}
</Card>
"#;
        let tree = parse_svelte(src);
        let symbols = extract_symbols_from_svelte(tree.root_node(), src);

        assert_eq!(symbols.imports, vec!["./Card.svelte", "./format"]);
        assert!(symbols
            .exports
            .iter()
            .any(|s| s.signature == "default component"));
        assert!(symbols
            .exports
            .iter()
            .any(|s| s.signature == "prop name: string"));
        assert!(symbols.types.iter().any(|t| t.name == "User"));

        let component = symbols
            .exports
            .iter()
            .find(|s| s.signature == "default component")
            .unwrap();
        assert_eq!(component.renders[0].name, "Card");
        assert_eq!(component.renders[0].children[0].name, "Avatar");
        assert!(component.calls.contains(&"avatarFor".to_string()));
        assert!(component.calls.contains(&"visible".to_string()));
    }

    #[test]
    fn ignores_module_script_exports_as_props() {
        let src = br#"
<script context="module">
  export const prerender = true;
</script>
<script>
  export let title;
</script>
<h1>{title}</h1>
"#;
        let tree = parse_svelte(src);
        let symbols = extract_symbols_from_svelte(tree.root_node(), src);

        assert!(symbols.exports.iter().any(|s| s.signature == "prop title"));
        assert!(!symbols
            .exports
            .iter()
            .any(|s| s.signature.starts_with("prop prerender")));
    }

    #[test]
    fn extracts_svelte_runes_and_lifecycle_hooks() {
        let src = br"
<script>
  import { onMount } from 'svelte';
  let count = $state(0);
  let doubled = $derived(count * 2);
  onMount(() => refresh());
</script>
<button>{doubled}</button>
";
        let tree = parse_svelte(src);
        let symbols = extract_symbols_from_svelte(tree.root_node(), src);
        let hooks: Vec<_> = symbols
            .hooks
            .iter()
            .map(|hook| hook.kind.as_str())
            .collect();

        assert!(hooks.contains(&"$state"));
        assert!(hooks.contains(&"$derived"));
        assert!(hooks.contains(&"onMount"));
    }

    #[test]
    fn extracts_script_sources_only() {
        let src = br"
<script>
  import Child from './Child.svelte';
  export { util } from './util';
</script>
<Child />
";
        let tree = parse_svelte(src);
        let sources = extract_sources_only_from_svelte(tree.root_node(), src);

        assert_eq!(sources, vec!["./Child.svelte", "./util"]);
    }
}
