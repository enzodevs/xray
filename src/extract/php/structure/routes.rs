use tree_sitter::Node;

use super::{Context, arguments, is_callable, is_closure, literal, txt};
use crate::model::TypeDef;

const DYNAMIC: &str = "<dynamic>";

#[derive(Clone, Default)]
struct Group {
    prefix: String,
    name: String,
    middleware: Vec<String>,
    controller: Option<String>,
    has_namespace: bool,
}

pub(super) fn collect(nodes: &[Node], src: &[u8], context: &Context, routes: &mut Vec<TypeDef>) {
    walk(nodes, src, context, &Group::default(), routes);
}

fn walk(nodes: &[Node], src: &[u8], context: &Context, group: &Group, routes: &mut Vec<TypeDef>) {
    for &node in nodes {
        if node.kind() == "expression_statement" {
            if let Some(expr) = node.named_child(0) {
                extract(expr, src, context, group, routes);
            }
        } else if !is_callable(node) {
            let mut cursor = node.walk();
            let children: Vec<_> = node.named_children(&mut cursor).collect();
            walk(&children, src, context, group, routes);
        }
    }
}

fn chain<'tree>(expr: Node<'tree>, src: &[u8], context: &Context) -> Option<Vec<Node<'tree>>> {
    let mut calls = Vec::new();
    let mut node = expr;
    while node.kind() == "member_call_expression" {
        calls.push(node);
        node = node.child_by_field_name("object")?;
    }
    if node.kind() != "scoped_call_expression" {
        return None;
    }
    let scope = context.resolve(node.child_by_field_name("scope")?, src)?;
    if !matches!(
        scope.as_str(),
        "Route" | "Illuminate\\Support\\Facades\\Route"
    ) {
        return None;
    }
    calls.push(node);
    calls.reverse();
    Some(calls)
}

fn method<'a>(call: Node, src: &'a [u8]) -> &'a str {
    call.child_by_field_name("name")
        .filter(|name| name.kind() == "name")
        .map_or("", |name| txt(name, src))
}

fn extract(
    expr: Node,
    src: &[u8],
    context: &Context,
    inherited: &Group,
    routes: &mut Vec<TypeDef>,
) {
    let Some(calls) = chain(expr, src, context) else {
        return;
    };
    let mut group = inherited.clone();
    if let Some(&group_call) = calls.iter().find(|&&call| method(call, src) == "group") {
        for &call in &calls {
            if call == group_call {
                break;
            }
            apply_modifier(call, src, context, &mut group);
        }
        let args = arguments(group_call);
        if let Some(&options) = args.first() {
            if options.kind() == "array_creation_expression" {
                apply_options(options, src, context, &mut group);
            } else if !is_closure(options) {
                unknown_options(&mut group);
            }
        }
        // Only an inline group callback is inspected; external registration is not executed.
        for arg in args {
            if is_closure(arg) {
                if let Some(body) = arg.child_by_field_name("body") {
                    if body.kind() == "compound_statement" {
                        let mut cursor = body.walk();
                        let nodes: Vec<_> = body.named_children(&mut cursor).collect();
                        walk(&nodes, src, context, &group, routes);
                    } else {
                        extract(body, src, context, &group, routes);
                    }
                }
            }
        }
        return;
    }
    let Some((index, &registration)) = calls
        .iter()
        .enumerate()
        .find(|(_, call)| is_registration(method(**call, src)))
    else {
        return;
    };
    for &call in &calls[..index] {
        apply_modifier(call, src, context, &mut group);
    }
    let args = arguments(registration);
    let verb = method(registration, src);
    let path_index = usize::from(verb == "match");
    let path = args
        .get(path_index)
        .and_then(|&arg| literal(arg, src))
        .unwrap_or_else(|| DYNAMIC.to_string());
    let action = args.get(path_index + 1).copied();
    let mut name = if group.name.is_empty() {
        "<none>".to_string()
    } else {
        group.name.clone()
    };
    for &call in &calls[index + 1..] {
        if method(call, src) == "name" {
            let value = arguments(call)
                .first()
                .and_then(|&arg| literal(arg, src))
                .unwrap_or_else(|| DYNAMIC.to_string());
            name = concatenate(&group.name, &value);
        } else {
            apply_modifier(call, src, context, &mut group);
        }
    }
    let verbs = match verb {
        "match" => args.first().map_or_else(
            || DYNAMIC.to_string(),
            |&arg| literal_list(arg, src).join("|"),
        ),
        "any" | "redirect" | "permanentRedirect" => "ANY".to_string(),
        "view" | "inertia" | "get" => "GET|HEAD".to_string(),
        _ => verb.to_ascii_uppercase(),
    };
    let path = join_path(&group.prefix, &path);
    let mut action = action_summary(verb, action, src, context, &group);
    if group.has_namespace && action.starts_with("controller=") {
        // Legacy group namespaces can rewrite even class-string actions. Keep the
        // literal visible, but do not present it as a resolved controller identity.
        action = action.replacen(
            "controller=",
            "controller=<unresolved:group-namespace> controller_literal=",
            1,
        );
    }
    let summary = format!(
        "{action} name={name} middleware=[{}]",
        group.middleware.join(", ")
    );
    routes.push(TypeDef {
        name: format!("{verbs} {path}"),
        kind: "route".to_string(),
        extends: String::new(),
        summary,
        line_start: expr.start_position().row + 1,
        line_end: expr.end_position().row + 1,
        exported: false,
    });
}

fn is_registration(name: &str) -> bool {
    matches!(
        name,
        "get"
            | "post"
            | "put"
            | "patch"
            | "delete"
            | "options"
            | "head"
            | "any"
            | "match"
            | "redirect"
            | "permanentRedirect"
            | "view"
            | "inertia"
    )
}

fn apply_modifier(call: Node, src: &[u8], context: &Context, group: &mut Group) {
    let value = arguments(call).first().copied();
    apply_option(method(call, src), value, src, context, group);
}

fn apply_options(options: Node, src: &[u8], context: &Context, group: &mut Group) {
    let mut cursor = options.walk();
    for entry in options.named_children(&mut cursor) {
        if entry.kind() != "array_element_initializer" {
            continue;
        }
        let Some(key) = entry.named_child(0).and_then(|key| literal(key, src)) else {
            // Unknown group keys may change any inherited routing attribute.
            unknown_options(group);
            continue;
        };
        apply_option(&key, entry.named_child(1), src, context, group);
    }
}

fn unknown_options(group: &mut Group) {
    group.prefix = DYNAMIC.to_string();
    group.name = DYNAMIC.to_string();
    group.middleware.push(DYNAMIC.to_string());
    group.controller = Some(DYNAMIC.to_string());
    group.has_namespace = true;
}

fn apply_option(key: &str, value: Option<Node>, src: &[u8], context: &Context, group: &mut Group) {
    match key {
        "namespace" => group.has_namespace = true,
        "prefix" => {
            let value = value
                .and_then(|value| literal(value, src))
                .unwrap_or_else(|| DYNAMIC.to_string());
            group.prefix = join_path(&group.prefix, &value);
        }
        "name" | "as" => {
            let value = value
                .and_then(|value| literal(value, src))
                .unwrap_or_else(|| DYNAMIC.to_string());
            group.name = concatenate(&group.name, &value);
        }
        "middleware" => group.middleware.extend(value.map_or_else(
            || vec![DYNAMIC.to_string()],
            |value| literal_list(value, src),
        )),
        "controller" => {
            group.controller = Some(
                value
                    .and_then(|value| {
                        class_literal(value, src, context).or_else(|| literal(value, src))
                    })
                    .unwrap_or_else(|| DYNAMIC.to_string()),
            );
        }
        _ => {}
    }
}

fn action_summary(
    verb: &str,
    action: Option<Node>,
    src: &[u8],
    context: &Context,
    group: &Group,
) -> String {
    let Some(action) = action else {
        return format!("action={DYNAMIC}");
    };
    if matches!(verb, "redirect" | "permanentRedirect" | "view" | "inertia") {
        let target = literal(action, src).unwrap_or_else(|| DYNAMIC.to_string());
        return format!("action={verb} target={target}");
    }
    if is_closure(action) {
        return "action=closure".to_string();
    }
    if action.kind() == "array_creation_expression" {
        let mut cursor = action.walk();
        let entries: Vec<_> = action
            .named_children(&mut cursor)
            .filter(|entry| entry.kind() == "array_element_initializer")
            .collect();
        if entries.len() == 2 && entries.iter().all(|entry| entry.named_child_count() == 1) {
            let controller = entries[0]
                .named_child(0)
                .and_then(|node| class_literal(node, src, context).or_else(|| literal(node, src)))
                .unwrap_or_else(|| DYNAMIC.to_string());
            let method = entries[1]
                .named_child(0)
                .and_then(|node| literal(node, src))
                .unwrap_or_else(|| DYNAMIC.to_string());
            return format!("controller={controller} action={method}");
        }
    }
    if let Some(controller) = class_literal(action, src, context) {
        return format!("controller={controller} action=__invoke");
    }
    if let Some(value) = literal(action, src) {
        if let Some((controller, method)) = value.split_once('@') {
            return format!("controller={controller} action={method}");
        }
        if let Some(controller) = &group.controller {
            return format!("controller={controller} action={value}");
        }
        return format!("controller={value} action=__invoke");
    }
    format!("action={DYNAMIC}")
}

fn class_literal(node: Node, src: &[u8], context: &Context) -> Option<String> {
    if node.kind() != "class_constant_access_expression"
        || !txt(node.named_child(1)?, src).eq_ignore_ascii_case("class")
    {
        return None;
    }
    context.resolve(node.named_child(0)?, src)
}

fn literal_list(node: Node, src: &[u8]) -> Vec<String> {
    if let Some(value) = literal(node, src) {
        return vec![value];
    }
    if node.kind() != "array_creation_expression" {
        return vec![DYNAMIC.to_string()];
    }
    let mut cursor = node.walk();
    node.named_children(&mut cursor)
        .filter(|entry| entry.kind() == "array_element_initializer")
        .map(|entry| {
            if entry.named_child_count() == 1 {
                entry
                    .named_child(0)
                    .and_then(|value| literal(value, src))
                    .unwrap_or_else(|| DYNAMIC.to_string())
            } else {
                DYNAMIC.to_string()
            }
        })
        .collect()
}

fn concatenate(prefix: &str, value: &str) -> String {
    if prefix == DYNAMIC || value == DYNAMIC {
        DYNAMIC.to_string()
    } else {
        format!("{prefix}{value}")
    }
}

fn join_path(prefix: &str, path: &str) -> String {
    if prefix == DYNAMIC || path == DYNAMIC {
        return DYNAMIC.to_string();
    }
    let prefix = prefix.trim_matches('/');
    let path = path.trim_matches('/');
    match (prefix.is_empty(), path.is_empty()) {
        (true, _) => format!("/{path}"),
        (_, true) => format!("/{prefix}"),
        _ => format!("/{prefix}/{path}"),
    }
}
