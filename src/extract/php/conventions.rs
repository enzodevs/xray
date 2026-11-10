//! Syntactic Laravel convention hints. These are candidates, never LSP references.
use std::time::Instant;
use tree_sitter::{Node, Point};

use super::dependencies::{self, Context};
use super::txt;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    Scope(String),
    Job,
    Listener(Vec<String>),
}

#[derive(Debug)]
pub(crate) struct Target {
    pub owner: String,
    pub kind: Kind,
}

#[derive(Debug)]
pub(crate) struct Candidate {
    pub kind: &'static str,
    pub subject: String,
    pub start: Point,
    pub end: Point,
}

impl Target {
    pub fn label(&self) -> &'static str {
        match self.kind {
            Kind::Scope(_) => "eloquent-scope",
            Kind::Job => "framework-job-handler",
            Kind::Listener(_) => "framework-listener-handler",
        }
    }

    /// A cheap negative filter only; matching sources must still be parsed.
    pub fn may_match(&self, source: &str) -> bool {
        let source = source.to_ascii_lowercase();
        let contains = |name: &str| {
            source.contains(
                &name
                    .rsplit('\\')
                    .next()
                    .unwrap_or(name)
                    .to_ascii_lowercase(),
            )
        };
        match &self.kind {
            Kind::Scope(name) => contains(name),
            Kind::Job => contains(&self.owner),
            Kind::Listener(events) => {
                contains(&self.owner) || events.iter().any(|name| contains(name))
            }
        }
    }
}

pub(crate) fn target(root: Node, src: &[u8], position: Point) -> Option<Target> {
    let mut found = None;
    dependencies::visit_scopes(root, src, |nodes, context| {
        for &node in nodes {
            if !matches!(node.kind(), "class_declaration" | "trait_declaration") {
                continue;
            }
            let (Some(name), Some(body)) = (
                node.child_by_field_name("name"),
                node.child_by_field_name("body"),
            ) else {
                continue;
            };
            let Some(owner) = context.resolve(name, src) else {
                continue;
            };
            let mut cursor = body.walk();
            for method in body
                .named_children(&mut cursor)
                .filter(|n| n.kind() == "method_declaration")
            {
                let Some(name) = method.child_by_field_name("name") else {
                    continue;
                };
                if position < name.start_position() || position >= name.end_position() {
                    continue;
                }
                if let Some(kind) = classify(node, method, &owner, src, context) {
                    found = Some(Target {
                        owner: owner.clone(),
                        kind,
                    });
                }
            }
        }
    });
    found
}

fn classify(class: Node, method: Node, owner: &str, src: &[u8], context: &Context) -> Option<Kind> {
    let name = txt(method.child_by_field_name("name")?, src);
    let attributes = method.child_by_field_name("attributes");
    let scope_attribute = attributes.is_some_and(|attributes| {
        let mut found = false;
        walk(attributes, &mut |node| {
            if node.kind() == "attribute"
                && node
                    .named_child(0)
                    .and_then(|n| context.resolve(n, src))
                    .is_some_and(|n| {
                        n.eq_ignore_ascii_case("Illuminate\\Database\\Eloquent\\Attributes\\Scope")
                    })
            {
                found = true;
            }
        });
        found
    });
    if scope_attribute {
        return Some(Kind::Scope(name.to_string()));
    }
    let mut model =
        class.kind() == "trait_declaration" || owner.split('\\').any(|part| part == "Models");
    let mut queued = false;
    let mut cursor = class.walk();
    for node in class.named_children(&mut cursor) {
        if matches!(node.kind(), "base_clause" | "class_interface_clause") {
            walk(node, &mut |node| {
                if let Some(name) = context.resolve(node, src) {
                    model |= name.eq_ignore_ascii_case("Illuminate\\Database\\Eloquent\\Model");
                    queued |= name
                        .eq_ignore_ascii_case("Illuminate\\Contracts\\Queue\\ShouldQueue")
                        || name.eq_ignore_ascii_case(
                            "Illuminate\\Contracts\\Queue\\ShouldQueueAfterCommit",
                        );
                }
            });
        }
    }
    if model {
        if let Some(suffix) = name
            .strip_prefix("scope")
            .filter(|s| s.starts_with(|c: char| c.is_ascii_uppercase()))
        {
            let mut call = suffix.to_string();
            call[..1].make_ascii_lowercase();
            return Some(Kind::Scope(call));
        }
    }
    if !name.eq_ignore_ascii_case("handle") {
        return None;
    }
    let mut cursor = method.walk();
    if method
        .named_children(&mut cursor)
        .any(|node| node.kind() == "visibility_modifier" && txt(node, src) != "public")
    {
        return None;
    }
    if owner.split('\\').any(|part| part == "Listeners") {
        let mut events = Vec::new();
        if let Some(parameters) = method.child_by_field_name("parameters") {
            // Only the first parameter is the event; subsequent parameters may be services.
            let mut cursor = parameters.walk();
            let parameter = parameters.named_children(&mut cursor).find(|node| {
                matches!(
                    node.kind(),
                    "simple_parameter" | "variadic_parameter" | "property_promotion_parameter"
                )
            });
            if let Some(parameter) = parameter {
                if let Some(ty) = parameter.child_by_field_name("type") {
                    walk(ty, &mut |node| {
                        if node.kind() == "named_type" {
                            if let Some(name) =
                                node.named_child(0).and_then(|n| context.resolve(n, src))
                            {
                                events.push(name);
                            }
                        }
                    });
                }
            }
        }
        events.sort();
        events.dedup();
        return Some(Kind::Listener(events));
    }
    (queued || owner.split('\\').any(|part| part == "Jobs")).then_some(Kind::Job)
}

/// Stream matches so the caller can enforce aggregate count/byte bounds before allocation.
pub(crate) fn candidates(
    root: Node,
    src: &[u8],
    target: &Target,
    deadline: Instant,
    mut emit: impl FnMut(Candidate) -> bool,
) -> bool {
    let mut complete = true;
    dependencies::visit_scopes(root, src, |nodes, context| {
        if !complete {
            return;
        }
        for &node in nodes {
            complete = walk_while(node, &mut |node| {
                if Instant::now() >= deadline {
                    return false;
                }
                candidate(node, src, target, context).is_none_or(&mut emit)
            });
            if !complete {
                break;
            }
        }
    });
    complete && Instant::now() < deadline
}

fn candidate(node: Node, src: &[u8], target: &Target, context: &Context) -> Option<Candidate> {
    let matched = |node: Node, kind: &'static str, subject: String| Candidate {
        kind,
        subject,
        start: node.start_position(),
        end: node.end_position(),
    };
    if let Kind::Scope(call_name) = &target.kind {
        let name = scope_call(node, src, call_name)?;
        let receiver = scope_receiver(node, src, context);
        let kind = if receiver
            .as_ref()
            .is_some_and(|name| name.eq_ignore_ascii_case(&target.owner))
        {
            "scope-receiver-match"
        } else {
            "scope-name-match"
        };
        return Some(matched(
            name,
            kind,
            receiver.unwrap_or_else(|| call_name.clone()),
        ));
    }
    let name = match node.kind() {
        "scoped_call_expression" => node.child_by_field_name("scope"),
        "object_creation_expression" | "class_constant_access_expression" => node.named_child(0),
        _ => None,
    }?;
    let subject = context.resolve(name, src)?;
    let owner = subject.eq_ignore_ascii_case(&target.owner);
    let event = matches!(&target.kind, Kind::Listener(events) if events.iter().any(|name| name.eq_ignore_ascii_case(&subject)));
    if !owner && !event {
        return None;
    }
    let (location, kind) = match node.kind() {
        "scoped_call_expression" => {
            let method = node
                .child_by_field_name("name")
                .filter(|n| n.kind() == "name")?;
            (method, dispatch_kind(txt(method, src), event)?)
        }
        "object_creation_expression" => (
            name,
            if event {
                "event-construction"
            } else {
                "handler-construction"
            },
        ),
        "class_constant_access_expression"
            if node
                .named_child(1)
                .is_some_and(|n| txt(n, src).eq_ignore_ascii_case("class")) =>
        {
            (
                name,
                if event {
                    "event-class-reference"
                } else {
                    "handler-class-reference"
                },
            )
        }
        _ => return None,
    };
    Some(matched(location, kind, subject))
}

fn dispatch_kind(method: &str, event: bool) -> Option<&'static str> {
    let method = method.to_ascii_lowercase();
    if matches!(
        method.as_str(),
        "dispatch" | "dispatchif" | "dispatchunless"
    ) {
        return Some(if event {
            "event-dispatch"
        } else {
            "handler-dispatch"
        });
    }
    match (event, method.as_str()) {
        (true, "broadcast") => Some("event-dispatch"),
        (false, "dispatchsync" | "dispatchnow" | "dispatchafterresponse") => {
            Some("handler-dispatch")
        }
        (false, "withchain") => Some("handler-chain-setup"),
        _ => None,
    }
}

// A literal static root is useful evidence, not proof of the chain's inferred return type.
fn scope_receiver(mut node: Node, src: &[u8], context: &Context) -> Option<String> {
    // Deep chains still yield name-only candidates; avoid quadratic root walks.
    for _ in 0..64 {
        if node.kind() == "scoped_call_expression" {
            return node
                .child_by_field_name("scope")
                .and_then(|scope| context.resolve(scope, src));
        }
        node = node.child_by_field_name("object")?;
    }
    None
}

fn scope_call<'tree>(node: Node<'tree>, src: &[u8], call_name: &str) -> Option<Node<'tree>> {
    if !matches!(
        node.kind(),
        "member_call_expression" | "nullsafe_member_call_expression" | "scoped_call_expression"
    ) {
        return None;
    }
    node.child_by_field_name("name")
        .filter(|name| name.kind() == "name" && txt(*name, src).eq_ignore_ascii_case(call_name))
}

fn walk(node: Node, visit: &mut impl FnMut(Node)) {
    walk_while(node, &mut |node| {
        visit(node);
        true
    });
}

fn walk_while(node: Node, visit: &mut impl FnMut(Node) -> bool) -> bool {
    let mut cursor = node.walk();
    loop {
        if !visit(cursor.node()) {
            return false;
        }
        if cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                return true;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extract::php::tests::parse_php;

    fn collected(root: Node, source: &[u8], target: &Target) -> Vec<Candidate> {
        let mut matches = Vec::new();
        assert!(candidates(
            root,
            source,
            target,
            Instant::now() + std::time::Duration::from_secs(5),
            |candidate| {
                matches.push(candidate);
                true
            }
        ));
        matches
    }

    #[test]
    fn candidate_stream_stops_at_callers_budget_and_observes_deadline() {
        let target = selected(
            "<?php namespace App\\Models; class Model { function scopeActive($q) {} }",
            "scopeActive",
        )
        .unwrap();
        let source = b"<?php $q->active()->active()->active();";
        let tree = parse_php(source);
        let mut count = 0;
        assert!(!candidates(
            tree.root_node(),
            source,
            &target,
            Instant::now() + std::time::Duration::from_secs(5),
            |_| {
                count += 1;
                count < 2
            }
        ));
        assert_eq!(count, 2);
        assert!(!candidates(
            tree.root_node(),
            source,
            &target,
            Instant::now(),
            |_| panic!("expired")
        ));
    }

    fn selected(source: &str, method: &str) -> Option<Target> {
        let tree = parse_php(source.as_bytes());
        let offset = source.find(&format!("function {method}")).unwrap() + "function ".len();
        let line = source[..offset].bytes().filter(|b| *b == b'\n').count();
        let column = source[..offset].rsplit('\n').next().unwrap().len();
        target(
            tree.root_node(),
            source.as_bytes(),
            Point::new(line, column),
        )
    }

    #[test]
    fn scope_candidates_are_calls_not_comments_strings_or_same_named_functions() {
        let target = selected(
            "<?php namespace App\\Models; class Message { function scopeForSchool($q) {} }",
            "scopeForSchool",
        )
        .unwrap();
        let source = b"<?php // $q->forSchool()\n $s='forSchool()'; forSchool(); $q->forSchool(); $q?->forSchool(); Model::forSchool(); $q->{$name}();";
        let tree = parse_php(source);
        let matches = collected(tree.root_node(), source, &target);
        assert_eq!(matches.len(), 3);
        assert!(matches.iter().all(|m| m.kind == "scope-name-match"));
    }

    #[test]
    fn scope_candidates_expose_matching_static_chain_receiver_without_claiming_types() {
        let target = selected(
            "<?php namespace App\\Models; class Message { function scopeForSchool($q) {} }",
            "scopeForSchool",
        )
        .unwrap();
        let source = br"<?php use App\Models\Message as Msg; Msg::query()->where('id', 1)->forSchool(); Other::forSchool();";
        let tree = parse_php(source);
        let matches = collected(tree.root_node(), source, &target);
        assert_eq!(matches.len(), 2);
        assert!(matches
            .iter()
            .any(|m| m.kind == "scope-receiver-match" && m.subject == "App\\Models\\Message"));
        assert!(matches
            .iter()
            .any(|m| m.kind == "scope-name-match" && m.subject == "Other"));
    }

    #[test]
    fn attributed_scopes_resolve_aliases_and_do_not_require_scope_prefix() {
        let source = "<?php use Illuminate\\Database\\Eloquent\\Attributes\\Scope as LocalScope; class Run { #[LocalScope] protected function pending($q) {} }";
        assert_eq!(
            selected(source, "pending").unwrap().kind,
            Kind::Scope("pending".into())
        );
        assert!(selected(
            "<?php class Utility { function scopeWork() {} }",
            "scopeWork"
        )
        .is_none());
        assert!(selected(
            "<?php namespace App\\Actions; class Work { function handle() {} }",
            "handle"
        )
        .is_none());
    }

    #[test]
    fn jobs_match_resolved_aliases_dispatch_and_construction_not_unrelated_classes() {
        let target = selected(
            "<?php namespace App\\Jobs; class Work { public function handle() {} }",
            "handle",
        )
        .unwrap();
        let source = br"<?php namespace Feature; use App\Jobs\Work as Job; Job::dispatch(); dispatch(new Job()); Other\Work::dispatch(); Job::class; Job::OTHER;";
        let tree = parse_php(source);
        let matches = collected(tree.root_node(), source, &target);
        assert_eq!(matches.len(), 3);
        assert!(matches.iter().any(|m| m.kind == "handler-dispatch"));
        assert!(matches.iter().any(|m| m.kind == "handler-construction"));
        assert!(matches.iter().all(|m| m.subject == "App\\Jobs\\Work"));
    }

    #[test]
    fn listener_candidates_follow_first_parameter_event_types_not_injected_services() {
        let target = selected("<?php namespace App\\Listeners; use App\\Events\\Done as Event; class Notify { public function handle(Event|\\App\\Events\\Failed $event, \\App\\Service $service) {} }", "handle").unwrap();
        assert_eq!(
            target.kind,
            Kind::Listener(vec![
                "App\\Events\\Done".into(),
                "App\\Events\\Failed".into()
            ])
        );
        let source = br"<?php use App\Events\Done; Done::dispatch(); event(new Done()); \App\Service::dispatch(); \App\Listeners\Notify::class;";
        let tree = parse_php(source);
        let matches = collected(tree.root_node(), source, &target);
        assert_eq!(matches.len(), 3);
        assert!(matches.iter().any(|m| m.kind == "event-dispatch"));
        assert!(matches.iter().any(|m| m.kind == "handler-class-reference"));
    }
}
