use tree_sitter::{Node, Parser};

use crate::model::{FileSymbols, Hook, JsxNode, Symbol};
use crate::util::{trim_quotes, txt};

use super::{calls, extract_all_sources, extract_sources_only, extract_symbols};

#[derive(Clone, Copy)]
enum ScriptDialect {
    Ts,
    Tsx,
}

#[derive(Clone, Copy)]
struct ScriptRange {
    start: usize,
    end: usize,
    dialect: ScriptDialect,
}

pub(super) fn extract_symbols_from_vue(root: Node, src: &[u8]) -> FileSymbols {
    let script_ranges = collect_script_ranges(root, src);
    let mut symbols = extract_script_symbols(src, &script_ranges);
    extend_unique(&mut symbols.imports, collect_script_srcs(root, src));
    dedupe_strings(&mut symbols.imports);

    if let Some(component) = extract_component_symbol(root, src) {
        symbols.exports.insert(0, component);
    }

    symbols
}

pub(super) fn extract_sources_only_from_vue(root: Node, src: &[u8]) -> Vec<String> {
    extract_vue_sources(root, src, false)
}

pub(super) fn extract_all_sources_from_vue(root: Node, src: &[u8]) -> Vec<String> {
    extract_vue_sources(root, src, true)
}

fn extract_vue_sources(root: Node, src: &[u8], include_external: bool) -> Vec<String> {
    let script_ranges = collect_script_ranges(root, src);
    let mut sources = collect_script_srcs(root, src);

    if let Some((tree, virtual_src)) = parse_script_tree(src, &script_ranges) {
        let script_sources = if include_external {
            extract_all_sources(tree.root_node(), virtual_src.as_bytes())
        } else {
            extract_sources_only(tree.root_node(), virtual_src.as_bytes())
        };
        extend_unique(&mut sources, script_sources);
    }

    sources
}

fn extract_script_symbols(src: &[u8], script_ranges: &[ScriptRange]) -> FileSymbols {
    let Some((tree, virtual_src)) = parse_script_tree(src, script_ranges) else {
        return empty_symbols();
    };

    let mut symbols = extract_symbols(tree.root_node(), virtual_src.as_bytes());
    extend_unique_hooks(
        &mut symbols.hooks,
        extract_vue_top_level_hooks(tree.root_node(), virtual_src.as_bytes()),
    );
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
    let dialect = script_ranges
        .iter()
        .find(|range| matches!(range.dialect, ScriptDialect::Tsx))
        .map_or(ScriptDialect::Ts, |range| range.dialect);
    let language = match dialect {
        ScriptDialect::Ts => tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
        ScriptDialect::Tsx => tree_sitter_typescript::LANGUAGE_TSX.into(),
    };

    let mut parser = Parser::new();
    parser.set_language(&language).ok()?;
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
        let dialect = script_dialect(node, src);
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "raw_text" {
                ranges.push(ScriptRange {
                    start: child.start_byte(),
                    end: child.end_byte(),
                    dialect,
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

fn script_dialect(script: Node, src: &[u8]) -> ScriptDialect {
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
            if attr_text.contains("lang=\"tsx\"")
                || attr_text.contains("lang='tsx'")
                || attr_text.contains("lang=tsx")
                || attr_text.contains("lang=\"jsx\"")
                || attr_text.contains("lang='jsx'")
                || attr_text.contains("lang=jsx")
            {
                return ScriptDialect::Tsx;
            }
        }
    }

    ScriptDialect::Ts
}

fn collect_script_srcs(root: Node, src: &[u8]) -> Vec<String> {
    let mut sources = Vec::new();
    collect_script_srcs_inner(root, src, &mut sources);
    sources
}

fn collect_script_srcs_inner(node: Node, src: &[u8], sources: &mut Vec<String>) {
    if node.kind() == "script_element" {
        if let Some(source) = element_src_attribute(node, src) {
            push_unique(sources, source);
        }
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_script_srcs_inner(child, src, sources);
    }
}

fn element_src_attribute(element: Node, src: &[u8]) -> Option<String> {
    let mut cursor = element.walk();
    for child in element.children(&mut cursor) {
        if child.kind() != "start_tag" {
            continue;
        }

        let mut tag_cursor = child.walk();
        for attr in child.children(&mut tag_cursor) {
            if attr.kind() != "attribute" {
                continue;
            }
            let attr_text = txt(attr, src).trim();
            if !attr_text.starts_with("src") {
                continue;
            }
            let value = attr_value(attr, src)?;
            if is_local_module_specifier(&value) {
                return Some(value);
            }
        }
    }

    None
}

fn attr_value(attr: Node, src: &[u8]) -> Option<String> {
    let mut cursor = attr.walk();
    for child in attr.children(&mut cursor) {
        match child.kind() {
            "attribute_value" => return Some(trim_quotes(txt(child, src)).to_string()),
            "quoted_attribute_value" => {
                let mut value_cursor = child.walk();
                for value in child.children(&mut value_cursor) {
                    if value.kind() == "attribute_value" {
                        return Some(trim_quotes(txt(value, src)).to_string());
                    }
                }
            }
            _ => {}
        }
    }

    None
}

fn extract_component_symbol(root: Node, src: &[u8]) -> Option<Symbol> {
    let mut renders = collect_template_tree(root, src);
    dedup_template_nodes(&mut renders);
    truncate_template_nodes(&mut renders, 8);

    let mut calls = collect_template_calls(root, src);
    calls.sort();
    calls.dedup();
    calls.truncate(12);

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
    if matches!(
        name,
        "slot" | "template" | "transition" | "Transition" | "Teleport" | "KeepAlive" | "Suspense"
    ) {
        return false;
    }

    name == "component"
        || name.contains('-')
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

    if node.kind() == "interpolation" {
        if let Some(raw) = first_child_kind(node, "raw_text") {
            calls_out.extend(parse_expression_calls(txt(raw, src)));
        }
        return;
    }

    if node.kind() == "directive_attribute" {
        collect_directive_calls(node, src, calls_out);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_template_calls_inner(child, src, calls_out);
    }
}

fn collect_directive_calls(node: Node, src: &[u8], calls_out: &mut Vec<String>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "attribute_value" => calls_out.extend(parse_expression_calls(txt(child, src))),
            "quoted_attribute_value" => {
                if let Some(value) = first_child_kind(child, "attribute_value") {
                    calls_out.extend(parse_expression_calls(txt(value, src)));
                }
            }
            "directive_dynamic_argument" => {
                if let Some(value) = first_child_kind(child, "directive_dynamic_argument_value") {
                    calls_out.extend(parse_expression_calls(txt(value, src)));
                }
            }
            _ => {}
        }
    }
}

fn first_child_kind<'a>(node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    let child = node
        .children(&mut cursor)
        .find(|child| child.kind() == kind);
    child
}

fn parse_expression_calls(expr: &str) -> Vec<String> {
    let trimmed = trim_quotes(expr.trim()).trim();
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

fn extract_vue_top_level_hooks(root: Node, src: &[u8]) -> Vec<Hook> {
    let mut hooks = Vec::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        match child.kind() {
            "lexical_declaration" => collect_lexical_hooks(child, src, &mut hooks),
            "expression_statement" => {
                if let Some(hook) = extract_expression_hook(child, src) {
                    hooks.push(hook);
                }
            }
            _ => {}
        }
    }
    hooks
}

fn collect_lexical_hooks(node: Node, src: &[u8], hooks: &mut Vec<Hook>) {
    let mut cursor = node.walk();
    for declarator in node.children(&mut cursor) {
        if declarator.kind() != "variable_declarator" {
            continue;
        }
        let Some(value) = declarator.child_by_field_name("value") else {
            continue;
        };
        let Some(name) = first_call_name(value, src) else {
            continue;
        };
        if !is_vue_hook_name(&name) {
            continue;
        }
        hooks.push(Hook {
            kind: name,
            bindings: collect_binding_names(declarator, src),
            deps: None,
            line_start: declarator.start_position().row + 1,
            line_end: declarator.end_position().row + 1,
        });
    }
}

fn extract_expression_hook(expr_stmt: Node, src: &[u8]) -> Option<Hook> {
    let name = first_call_name(expr_stmt, src)?;
    if !is_vue_hook_name(&name) {
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

fn first_call_name(node: Node, src: &[u8]) -> Option<String> {
    if node.kind() == "call_expression" {
        return call_name(node, src);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(name) = first_call_name(child, src) {
            return Some(name);
        }
    }

    None
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

fn is_vue_hook_name(name: &str) -> bool {
    matches!(
        name,
        "defineProps"
            | "defineEmits"
            | "defineModel"
            | "defineOptions"
            | "defineSlots"
            | "ref"
            | "shallowRef"
            | "computed"
            | "watch"
            | "watchEffect"
            | "onMounted"
            | "onUpdated"
            | "onUnmounted"
            | "onBeforeMount"
            | "onBeforeUpdate"
            | "onBeforeUnmount"
            | "onActivated"
            | "onDeactivated"
            | "onErrorCaptured"
    ) || name.starts_with("use")
}

fn collect_binding_names(node: Node, src: &[u8]) -> Vec<String> {
    let Some(name) = node.child_by_field_name("name") else {
        return Vec::new();
    };

    match name.kind() {
        "identifier" => vec![txt(name, src).to_string()],
        "object_pattern" | "array_pattern" => {
            let mut names = Vec::new();
            let mut cursor = name.walk();
            for child in name.children(&mut cursor) {
                if child.kind() == "identifier" {
                    names.push(txt(child, src).to_string());
                }
            }
            names
        }
        _ => Vec::new(),
    }
}

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

fn extend_unique(items: &mut Vec<String>, extras: Vec<String>) {
    for item in extras {
        push_unique(items, item);
    }
}

fn dedupe_strings(items: &mut Vec<String>) {
    let mut seen = Vec::new();
    items.retain(|item| {
        if seen.iter().any(|existing| existing == item) {
            false
        } else {
            seen.push(item.clone());
            true
        }
    });
}

fn push_unique(items: &mut Vec<String>, item: String) {
    if !items.iter().any(|existing| existing == &item) {
        items.push(item);
    }
}

fn extend_unique_hooks(hooks: &mut Vec<Hook>, extras: Vec<Hook>) {
    for hook in extras {
        if !hooks.iter().any(|existing| {
            existing.kind == hook.kind
                && existing.bindings == hook.bindings
                && existing.line_start == hook.line_start
                && existing.line_end == hook.line_end
        }) {
            hooks.push(hook);
        }
    }
}

fn is_local_module_specifier(path: &str) -> bool {
    path.starts_with('.') || path.starts_with('@') || path.starts_with("$lib/")
}

#[cfg(test)]
mod tests {
    use tree_sitter::Parser;

    use super::*;

    fn parse_vue(src: &str) -> tree_sitter::Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_vue3::LANGUAGE.into())
            .expect("vue parser should load");
        parser.parse(src, None).expect("vue source should parse")
    }

    fn has_render(nodes: &[JsxNode], name: &str) -> bool {
        nodes
            .iter()
            .any(|node| node.name == name || has_render(&node.children, name))
    }

    #[test]
    fn extracts_script_imports_types_hooks_and_template_components() {
        let src = r#"
<script setup lang="ts">
import { SidebarProvider } from '@/components/ui/sidebar';
import { cn } from '@/lib/utils';
import type { AppVariant } from '@/types';

type Props = { variant?: AppVariant };

const props = defineProps<Props>();
const emit = defineEmits<{ select: [value: string] }>();
const isOpen = usePage().props.sidebarOpen;

function submit() {
    emit('select', props.variant ?? 'sidebar');
}
</script>

<template>
    <SidebarProvider :default-open="isOpen" @close="submit()">
        <component :is="iconForPrompt(prompt)" />
        {{ cn('x', props.variant) }}
    </SidebarProvider>
</template>
"#;
        let tree = parse_vue(src);
        let symbols = extract_symbols_from_vue(tree.root_node(), src.as_bytes());

        assert_eq!(
            symbols.imports,
            vec![
                "@/components/ui/sidebar".to_string(),
                "@/lib/utils".to_string(),
                "@/types".to_string(),
            ]
        );
        assert!(symbols
            .types
            .iter()
            .any(|ty| ty.name == "Props" && ty.kind == "type"));
        assert!(symbols
            .internals
            .iter()
            .any(|sym| sym.signature == "const props = defineProps(...)"));
        assert!(symbols
            .hooks
            .iter()
            .any(|hook| hook.kind == "defineProps" && hook.bindings == ["props"]));

        let component = symbols
            .exports
            .iter()
            .find(|sym| sym.signature == "default component")
            .expect("component symbol");
        assert!(component.is_component);
        assert!(has_render(&component.renders, "SidebarProvider"));
        assert!(has_render(&component.renders, "component"));
        assert!(component.calls.contains(&"submit".to_string()));
        assert!(component.calls.contains(&"iconForPrompt".to_string()));
        assert!(component.calls.contains(&"cn".to_string()));
    }

    #[test]
    fn extracts_script_sources_only_and_script_src() {
        let src = r#"
<script setup lang="ts" src="./setup.ts"></script>
<script setup lang="ts">
import LocalThing from './LocalThing.vue';
export { named } from '@/lib/named';
import External from 'vue';
</script>
"#;
        let tree = parse_vue(src);
        let sources = extract_sources_only_from_vue(tree.root_node(), src.as_bytes());

        assert_eq!(
            sources,
            vec![
                "./setup.ts".to_string(),
                "./LocalThing.vue".to_string(),
                "@/lib/named".to_string(),
            ]
        );
    }

    #[test]
    fn skips_html_tags_slots_and_vue_builtins_in_template_tree() {
        let src = r"
<template>
    <div>
        <slot />
        <Transition><ModalPanel /></Transition>
        <user-card />
    </div>
</template>
";
        let tree = parse_vue(src);
        let symbols = extract_symbols_from_vue(tree.root_node(), src.as_bytes());
        let component = symbols
            .exports
            .iter()
            .find(|sym| sym.signature == "default component")
            .expect("component symbol");

        assert_eq!(component.renders.len(), 2);
        assert!(has_render(&component.renders, "ModalPanel"));
        assert!(has_render(&component.renders, "user-card"));
    }
}
