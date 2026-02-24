use std::path::Path;

use tree_sitter::Node;

use crate::model::Symbol;
use crate::util::txt;

#[derive(Debug, PartialEq, Eq)]
struct SqlStatementInfo {
    kind: String,
    target: Option<String>,
    sources: Vec<String>,
    joins: Vec<String>,
    functions: Vec<String>,
    ctes: Vec<String>,
}

pub(super) fn extract_symbols(root: Node, src: &[u8]) -> crate::model::FileSymbols {
    let mut symbols = super::empty_symbols();
    symbols.imports = extract_sources_only(src);

    let mut cursor = root.walk();
    for node in root.children(&mut cursor) {
        if !node.is_named() || should_skip_node(node) {
            continue;
        }

        let Some((signature, calls)) = statement_signature_and_refs(node, src) else {
            continue;
        };

        symbols.internals.push(Symbol {
            signature,
            line_start: node.start_position().row + 1,
            line_end: node.end_position().row + 1,
            calls,
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        });
    }

    symbols
}

pub(super) fn extract_sources_only(src: &[u8]) -> Vec<String> {
    let text = String::from_utf8_lossy(src);
    let mut out = Vec::new();
    let mut in_block_comment = false;

    for raw_line in text.lines() {
        let line = strip_sql_comments(raw_line, &mut in_block_comment);
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("\\ir") {
            if let Some(path) = parse_include_path(rest) {
                push_unique(&mut out, path);
            }
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("\\i") {
            if let Some(path) = parse_include_path(rest) {
                push_unique(&mut out, path);
            }
            continue;
        }

        if let Some(rest) = strip_keyword_prefix(trimmed, "source") {
            if let Some(path) = parse_include_path(rest) {
                push_unique(&mut out, path);
            }
            continue;
        }

        if let Some(rest) = trimmed
            .strip_prefix("@@")
            .or_else(|| trimmed.strip_prefix('@'))
        {
            if let Some(path) = parse_include_path(rest).filter(|p| {
                Path::new(p)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("sql"))
            }) {
                push_unique(&mut out, path);
            }
        }
    }

    out
}

fn strip_sql_comments(line: &str, in_block_comment: &mut bool) -> String {
    let mut out = String::with_capacity(line.len());
    let bytes = line.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if *in_block_comment {
            if i + 1 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                *in_block_comment = false;
                i += 2;
            } else {
                i += 1;
            }
            continue;
        }

        if i + 1 < bytes.len() && bytes[i] == b'-' && bytes[i + 1] == b'-' {
            break;
        }

        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            *in_block_comment = true;
            i += 2;
            continue;
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    out
}

fn strip_keyword_prefix<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
    if line.len() < keyword.len() || !line.is_char_boundary(keyword.len()) {
        return None;
    }

    let (head, rest) = line.split_at(keyword.len());
    if !head.eq_ignore_ascii_case(keyword) {
        return None;
    }

    match rest.chars().next() {
        None => Some(rest),
        Some(c) if c.is_whitespace() => Some(rest),
        _ => None,
    }
}

fn parse_include_path(rest: &str) -> Option<String> {
    let s = rest.trim_start();
    if s.is_empty() {
        return None;
    }

    let token = if let Some(quote) = s.chars().next().filter(|c| *c == '"' || *c == '\'') {
        let end = s[1..].find(quote)?;
        &s[1..=end]
    } else {
        s.split_whitespace()
            .next()
            .map(|tok| tok.trim_end_matches(';').trim_end_matches(','))?
    };

    let cleaned = token.trim();
    (!cleaned.is_empty()).then(|| cleaned.to_string())
}

fn should_skip_node(node: Node) -> bool {
    matches!(
        node.kind(),
        "comment" | "comment_statement" | "line_comment" | "block_comment"
    )
}

fn statement_signature_and_refs(node: Node, src: &[u8]) -> Option<(String, Vec<String>)> {
    if let Some(info) = extract_statement_info(node, src) {
        let signature = format_statement_signature(&info);
        let refs = format_statement_refs(&info);
        return Some((signature, refs));
    }

    let signature = fallback_statement_signature(node, src)?;
    if should_skip_fallback_signature(&signature) {
        return None;
    }
    Some((signature, Vec::new()))
}

fn fallback_statement_signature(node: Node, src: &[u8]) -> Option<String> {
    let raw = txt(node, src).trim();
    if raw.is_empty() {
        return None;
    }
    let compact = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    if compact.is_empty() {
        return None;
    }
    let max_len = 100;
    if compact.len() <= max_len {
        Some(compact)
    } else {
        let truncate_at = compact
            .char_indices()
            .map(|(i, _)| i)
            .take_while(|&i| i <= max_len - 3)
            .last()
            .unwrap_or(0);
        Some(format!("{}...", &compact[..truncate_at]))
    }
}

fn should_skip_fallback_signature(signature: &str) -> bool {
    let trimmed = signature.trim();
    if trimmed.is_empty() {
        return true;
    }

    if trimmed.starts_with('\\') {
        // psql metacommands are already handled by `extract_sources_only` and are noise in
        // statement output.
        return true;
    }

    let upper = trimmed.to_ascii_uppercase();

    if upper.starts_with("SET SEARCH_PATH") {
        return true;
    }

    if matches!(
        upper.as_str(),
        "IF" | "THEN" | "ELSE" | "ELSIF" | "END" | "END;" | "END $$;" | "LEVEL SECURITY"
    ) {
        return true;
    }

    // Orphan punctuation fragments (e.g. `)`).
    !trimmed.chars().any(char::is_alphanumeric)
}

fn extract_statement_info(node: Node, src: &[u8]) -> Option<SqlStatementInfo> {
    let statement = if node.kind() == "statement" {
        node
    } else {
        return None;
    };
    let (kind, op_node) = detect_statement_kind(statement)?;

    let target = detect_target(statement, op_node, &kind, src);
    let mut sources = collect_from_sources(statement, src);
    let mut joins = collect_join_sources(statement, src);
    let mut functions = collect_invocations(statement, src);
    let ctes = collect_ctes(statement, src);
    let all_relations = collect_relation_sources(statement, src);

    if kind == "DELETE" {
        for using_source in collect_sources_after_keyword(statement, "keyword_using", src) {
            if !sources.iter().any(|s| s == &using_source) {
                sources.push(using_source);
            }
        }

        // tree-sitter-sequel currently parses some `DELETE ... USING` forms with
        // an `ERROR (keyword_using)` node, so the source table may be absent in
        // the AST. Fall back to text parsing for this clause only.
        for using_source in collect_delete_using_sources_from_text(statement, src) {
            if !sources.iter().any(|s| s == &using_source) {
                sources.push(using_source);
            }
        }
    }

    if kind == "MERGE" {
        for using_source in collect_sources_after_keyword(statement, "keyword_using", src) {
            if !sources.iter().any(|s| s == &using_source) {
                sources.push(using_source);
            }
        }
    }

    if matches!(kind.as_str(), "UPDATE" | "DELETE" | "MERGE") {
        for relation in all_relations {
            if target.as_deref() == Some(relation.as_str()) {
                continue;
            }
            if joins.iter().any(|j| j == &relation) || sources.iter().any(|s| s == &relation) {
                continue;
            }
            sources.push(relation);
        }
    }

    // CTE names are local aliases, not source tables.
    sources.retain(|s| !ctes.iter().any(|cte| cte == s));
    joins.retain(|s| !ctes.iter().any(|cte| cte == s));
    if let Some(target) = &target {
        sources.retain(|s| s != target);
        joins.retain(|s| s != target);
    }
    functions.retain(|f| !f.is_empty());

    Some(SqlStatementInfo {
        kind,
        target,
        sources,
        joins,
        functions,
        ctes,
    })
}

fn detect_statement_kind(statement: Node) -> Option<(String, Option<Node>)> {
    const KNOWN: &[&str] = &[
        "create_materialized_view",
        "create_table",
        "create_view",
        "create_index",
        "create_function",
        "create_type",
        "create_sequence",
        "create_schema",
        "create_database",
        "alter_table",
        "insert",
        "update",
        "delete",
        "set_operation",
        "select",
        "select_expression",
    ];

    let mut cursor = statement.walk();
    for child in statement.children(&mut cursor) {
        if !child.is_named() {
            continue;
        }
        if KNOWN.contains(&child.kind()) {
            return Some((normalize_kind(child.kind()), Some(child)));
        }
    }

    if has_named_child(statement, "keyword_merge") {
        return Some(("MERGE".to_string(), None));
    }

    None
}

fn normalize_kind(kind: &str) -> String {
    kind.replace('_', " ").to_ascii_uppercase()
}

fn detect_target(statement: Node, op_node: Option<Node>, kind: &str, src: &[u8]) -> Option<String> {
    match kind {
        "SELECT" | "SELECT EXPRESSION" | "SET OPERATION" => None,
        "DELETE" => find_first_source_after_keyword(statement, "keyword_from", src)
            .or_else(|| find_first_relation_target(statement, src))
            .or_else(|| find_first_object_reference_name(statement, src)),
        "UPDATE" => find_first_relation_target(statement, src)
            .or_else(|| find_first_source_after_keyword(statement, "keyword_update", src)),
        "MERGE" => find_merge_target(statement, src),
        "INSERT" => op_node
            .and_then(|n| n.child_by_field_name("name"))
            .map(|n| txt(n, src).to_string())
            .filter(|s| !s.is_empty())
            .or_else(|| op_node.and_then(|n| find_first_object_reference_name(n, src)))
            .or_else(|| find_first_object_reference_name(statement, src)),
        _ => op_node
            .and_then(|n| find_first_object_reference_name(n, src))
            .or_else(|| op_node.and_then(|n| find_first_identifier_name(n, src)))
            .or_else(|| find_first_identifier_name(statement, src)),
    }
}

fn find_merge_target(statement: Node, src: &[u8]) -> Option<String> {
    find_first_source_after_keyword(statement, "keyword_into", src)
        .or_else(|| find_first_relation_target(statement, src))
}

fn has_named_child(node: Node, kind: &str) -> bool {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.is_named() && child.kind() == kind {
            return true;
        }
    }
    false
}

fn find_first_source_after_keyword(
    statement: Node,
    keyword_kind: &str,
    src: &[u8],
) -> Option<String> {
    collect_sources_after_keyword(statement, keyword_kind, src)
        .into_iter()
        .next()
}

fn collect_sources_after_keyword(statement: Node, keyword_kind: &str, src: &[u8]) -> Vec<String> {
    let mut saw_keyword = false;
    let mut out = Vec::new();
    let mut cursor = statement.walk();

    for child in statement.children(&mut cursor) {
        if !child.is_named() {
            continue;
        }

        if child.kind() == keyword_kind {
            saw_keyword = true;
            continue;
        }

        if !saw_keyword {
            continue;
        }

        match child.kind() {
            "keyword_only" | "keyword_as" => {}
            // Stop scanning once another clause begins; avoids picking unrelated refs.
            kind if kind.starts_with("keyword_") => break,
            _ => collect_source_names_recursive(child, src, &mut out),
        }
    }

    out
}

fn collect_source_names_recursive(node: Node, src: &[u8], out: &mut Vec<String>) {
    if node.kind() == "relation" {
        if let Some(name) = relation_name(node, src) {
            push_unique(out, name);
        }
        return;
    }

    if node.kind() == "object_reference" {
        if let Some(name) = object_reference_name(node, src) {
            push_unique(out, name);
        }
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_source_names_recursive(child, src, out);
    }
}

fn collect_delete_using_sources_from_text(statement: Node, src: &[u8]) -> Vec<String> {
    let raw = txt(statement, src);
    extract_delete_using_sources_from_text(raw)
}

fn extract_delete_using_sources_from_text(raw: &str) -> Vec<String> {
    let lower = raw.to_ascii_lowercase();
    let Some(using_idx) = find_keyword_ascii(&lower, "using") else {
        return Vec::new();
    };

    let clause_start = using_idx + "using".len();
    let rest = &raw[clause_start..];
    let rest_lower = &lower[clause_start..];
    let clause_end =
        find_first_keyword_or_semicolon(rest_lower, &["where", "returning"]).unwrap_or(rest.len());
    let clause = &rest[..clause_end];

    let mut out = Vec::new();
    for segment in clause.split(',') {
        if let Some(name) = parse_object_name_segment(segment) {
            push_unique(&mut out, name);
        }
    }

    out
}

fn find_keyword_ascii(haystack_lower: &str, keyword_lower: &str) -> Option<usize> {
    let bytes = haystack_lower.as_bytes();
    let kw = keyword_lower.as_bytes();
    if kw.is_empty() || kw.len() > bytes.len() {
        return None;
    }

    for i in 0..=bytes.len() - kw.len() {
        if &bytes[i..i + kw.len()] != kw {
            continue;
        }

        let before_ok = i == 0 || !is_identish_byte(bytes[i - 1]);
        let after_pos = i + kw.len();
        let after_ok = after_pos == bytes.len() || !is_identish_byte(bytes[after_pos]);
        if before_ok && after_ok {
            return Some(i);
        }
    }

    None
}

fn find_first_keyword_or_semicolon(rest_lower: &str, keywords: &[&str]) -> Option<usize> {
    let mut best = rest_lower.find(';');
    for keyword in keywords {
        if let Some(pos) = find_keyword_ascii(rest_lower, keyword) {
            best = Some(best.map_or(pos, |current| current.min(pos)));
        }
    }
    best
}

fn parse_object_name_segment(segment: &str) -> Option<String> {
    let trimmed = segment.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut out = String::new();
    for ch in trimmed.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '_' | '.' | '$') {
            out.push(ch);
        } else {
            break;
        }
    }

    (!out.is_empty()).then_some(out)
}

fn is_identish_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'_' | b'$')
}

fn find_first_relation_target(node: Node, src: &[u8]) -> Option<String> {
    find_first_descendant(node, "relation")
        .and_then(|rel| relation_name(rel, src))
        .or_else(|| {
            find_first_descendant(node, "relation")
                .and_then(|rel| find_first_object_reference_name(rel, src))
        })
}

fn relation_name(node: Node, src: &[u8]) -> Option<String> {
    if node.kind() != "relation" {
        return None;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if !child.is_named() {
            continue;
        }
        if child.kind() == "object_reference" {
            return object_reference_name(child, src);
        }
    }

    None
}

fn find_first_identifier_name(node: Node, src: &[u8]) -> Option<String> {
    if node.kind() == "identifier" {
        let name = txt(node, src).trim();
        return (!name.is_empty()).then(|| name.to_string());
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(name) = find_first_identifier_name(child, src) {
            return Some(name);
        }
    }
    None
}

fn find_first_object_reference_name(node: Node, src: &[u8]) -> Option<String> {
    if node.kind() == "object_reference" {
        return object_reference_name(node, src);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(name) = find_first_object_reference_name(child, src) {
            return Some(name);
        }
    }
    None
}

fn collect_from_sources(statement: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    collect_relation_names_under(statement, "from", src, &mut out);
    out
}

fn collect_join_sources(statement: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    collect_relation_names_under(statement, "join", src, &mut out);
    out
}

fn collect_relation_sources(statement: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    collect_relation_names_recursive(statement, src, &mut out);
    out
}

fn collect_relation_names_under(node: Node, under_kind: &str, src: &[u8], out: &mut Vec<String>) {
    if node.kind() == under_kind {
        collect_relation_names_recursive(node, src, out);
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_relation_names_under(child, under_kind, src, out);
    }
}

fn collect_relation_names_recursive(node: Node, src: &[u8], out: &mut Vec<String>) {
    if node.kind() == "relation" {
        if let Some(name) = relation_name(node, src) {
            push_unique(out, name);
        }
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_relation_names_recursive(child, src, out);
    }
}

fn collect_invocations(statement: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    collect_invocations_recursive(statement, src, &mut out);
    out
}

fn collect_invocations_recursive(node: Node, src: &[u8], out: &mut Vec<String>) {
    if node.kind() == "invocation" {
        if let Some(name) = invocation_name(node, src) {
            push_unique(out, name);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_invocations_recursive(child, src, out);
    }
}

fn invocation_name(node: Node, src: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if !child.is_named() {
            continue;
        }
        if child.kind() == "object_reference" {
            return object_reference_name(child, src);
        }
    }
    None
}

fn push_unique(out: &mut Vec<String>, value: String) {
    if !out.iter().any(|existing| existing == &value) {
        out.push(value);
    }
}

fn collect_ctes(statement: Node, src: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    let mut cursor = statement.walk();
    for child in statement.children(&mut cursor) {
        if child.kind() != "cte" {
            continue;
        }

        let mut inner = child.walk();
        for cte_child in child.children(&mut inner) {
            if cte_child.kind() == "identifier" {
                let name = txt(cte_child, src).trim();
                if !name.is_empty() {
                    push_unique(&mut out, name.to_string());
                }
                break;
            }
        }
    }
    out
}

fn object_reference_name(node: Node, src: &[u8]) -> Option<String> {
    let database = node
        .child_by_field_name("database")
        .map(|n| txt(n, src).trim());
    let schema = node
        .child_by_field_name("schema")
        .map(|n| txt(n, src).trim());
    let name = node
        .child_by_field_name("name")
        .map(|n| txt(n, src).trim())?;

    if name.is_empty() {
        return None;
    }

    let mut parts = Vec::new();
    if let Some(db) = database.filter(|s| !s.is_empty()) {
        parts.push(db.to_string());
    }
    if let Some(schema) = schema.filter(|s| !s.is_empty()) {
        parts.push(schema.to_string());
    }
    parts.push(name.to_string());

    Some(parts.join("."))
}

fn find_first_descendant<'tree>(node: Node<'tree>, kind: &str) -> Option<Node<'tree>> {
    if node.kind() == kind {
        return Some(node);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(found) = find_first_descendant(child, kind) {
            return Some(found);
        }
    }
    None
}

fn format_statement_signature(info: &SqlStatementInfo) -> String {
    let mut parts = Vec::new();

    match (info.kind.as_str(), info.target.as_deref()) {
        ("DELETE", Some(target)) => {
            parts.push("DELETE FROM".to_string());
            parts.push(target.to_string());
        }
        ("INSERT", Some(target)) => {
            parts.push("INSERT INTO".to_string());
            parts.push(target.to_string());
        }
        ("MERGE", Some(target)) => {
            parts.push("MERGE INTO".to_string());
            parts.push(target.to_string());
        }
        (_, Some(target)) => {
            parts.push(info.kind.clone());
            parts.push(target.to_string());
        }
        (_, None) => parts.push(info.kind.clone()),
    }

    if !info.sources.is_empty() {
        let keyword = match info.kind.as_str() {
            "DELETE" | "MERGE" => "USING",
            _ => "FROM",
        };
        parts.push(format!("{keyword} {}", info.sources.join(", ")));
    }

    if !info.joins.is_empty() {
        parts.push(format!("JOIN {}", info.joins.join(", ")));
    }

    if !info.ctes.is_empty() {
        parts.push(format!("WITH [{}]", info.ctes.join(", ")));
    }

    if !info.functions.is_empty() {
        parts.push(format!("FN [{}]", info.functions.join(", ")));
    }

    parts.join(" ")
}

fn format_statement_refs(info: &SqlStatementInfo) -> Vec<String> {
    let mut refs = Vec::new();

    if let Some(target) = &info.target {
        refs.push(format!("target:{target}"));
    }

    for source in &info.sources {
        refs.push(format!("source:{source}"));
    }

    for join in &info.joins {
        refs.push(format!("join:{join}"));
    }

    for cte in &info.ctes {
        refs.push(format!("cte:{cte}"));
    }

    for function in &info.functions {
        refs.push(format!("fn:{function}"));
    }

    refs
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_sql(src: &[u8]) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_sequel::LANGUAGE.into())
            .unwrap();
        parser.parse(src, None).unwrap()
    }

    #[test]
    fn extract_sql_select_statement_as_internal_symbol() {
        let src = br"SELECT id, email FROM users WHERE active = true;";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert_eq!(symbols.internals.len(), 1);
        assert!(symbols.internals[0].signature.contains("SELECT"));
        assert!(symbols.internals[0].signature.contains("FROM users"));
        assert!(symbols.internals[0]
            .calls
            .contains(&"source:users".to_string()));
    }

    #[test]
    fn extract_sql_multiple_statements() {
        let src = br"
            CREATE TABLE users (id INT PRIMARY KEY);
            SELECT id FROM users;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(symbols.internals.len() >= 2);
    }

    #[test]
    fn extract_sql_create_table_captures_target_and_source() {
        let src = br"
            CREATE TABLE analytics.daily_users AS
            SELECT id FROM raw.users;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let create_stmt = symbols
            .internals
            .iter()
            .find(|s| s.signature.starts_with("CREATE TABLE"))
            .unwrap();

        assert!(create_stmt.signature.contains("analytics.daily_users"));
        assert!(create_stmt
            .calls
            .contains(&"target:analytics.daily_users".to_string()));
        assert!(create_stmt.calls.contains(&"source:raw.users".to_string()));
    }

    #[test]
    fn extract_sql_with_cte_captures_cte_names() {
        let src = br"
            WITH active_users AS (SELECT id FROM users)
            SELECT id FROM active_users;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let stmt = &symbols.internals[0];
        assert!(stmt.signature.contains("WITH [active_users]"));
        assert!(stmt.calls.contains(&"cte:active_users".to_string()));
        assert!(!stmt.calls.contains(&"source:active_users".to_string()));
    }

    #[test]
    fn extract_sql_join_and_invocations_are_distinguished() {
        let src = br"
            select count(*), coalesce(u.name, 'n/a')
            from users u
            join orders o on o.user_id = u.id;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let stmt = &symbols.internals[0];
        assert!(stmt.signature.contains("FROM users"));
        assert!(stmt.signature.contains("JOIN orders"));
        assert!(stmt.calls.contains(&"source:users".to_string()));
        assert!(stmt.calls.contains(&"join:orders".to_string()));
        assert!(stmt.calls.contains(&"fn:count".to_string()));
        assert!(stmt.calls.contains(&"fn:coalesce".to_string()));
    }

    #[test]
    fn extract_sql_delete_tracks_target_and_using_sources() {
        let src = br"
            DELETE FROM users
            USING sessions
            WHERE sessions.user_id = users.id;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let stmt = &symbols.internals[0];
        assert!(stmt.signature.contains("DELETE FROM users"));
        assert!(stmt.signature.contains("USING sessions"));
        assert!(stmt.calls.contains(&"target:users".to_string()));
        assert!(stmt.calls.contains(&"source:sessions".to_string()));
    }

    #[test]
    fn extract_sql_merge_statement_detects_keyword_target_and_source() {
        let src = br"
            MERGE INTO target_table t
            USING source_table s
            ON t.id = s.id
            WHEN MATCHED THEN UPDATE SET value = s.value;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let stmt = &symbols.internals[0];
        assert!(stmt.signature.starts_with("MERGE"));
        assert!(stmt.calls.contains(&"target:target_table".to_string()));
        assert!(stmt.calls.contains(&"source:source_table".to_string()));
    }

    #[test]
    fn extract_sql_sources_only_parses_common_include_directives() {
        let src = br"
            -- psql include
            \i ./schema/tables.sql
            SOURCE scripts/seed.sql;
            @@deploy.sql
            @not_sql_var
            /* \i ignored.sql */
        ";
        let sources = extract_sources_only(src);

        assert_eq!(
            sources,
            vec![
                "./schema/tables.sql".to_string(),
                "scripts/seed.sql".to_string(),
                "deploy.sql".to_string()
            ]
        );
    }

    #[test]
    fn fallback_filters_known_noise_fragments() {
        assert!(should_skip_fallback_signature(")"));
        assert!(should_skip_fallback_signature("IF"));
        assert!(should_skip_fallback_signature("END $$;"));
        assert!(should_skip_fallback_signature("LEVEL SECURITY"));
        assert!(should_skip_fallback_signature(
            "SET search_path TO app, public"
        ));
        assert!(should_skip_fallback_signature("\\echo 'applying'"));

        assert!(!should_skip_fallback_signature("CREATE POLICY p ON users"));
        assert!(!should_skip_fallback_signature(
            "ALTER TABLE users ENABLE ROW"
        ));
    }

    #[test]
    fn extract_sql_filters_set_search_path_noise_statement() {
        let src = br"
            SET search_path TO app, public;
            SELECT id FROM app.users;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(symbols.internals.iter().all(|s| !s
            .signature
            .to_ascii_uppercase()
            .starts_with("SET SEARCH_PATH")));
        assert!(symbols
            .internals
            .iter()
            .any(|s| s.signature.contains("SELECT") && s.signature.contains("app.users")));
    }

    #[test]
    fn extract_sql_skips_psql_meta_blob_but_keeps_includes_as_imports() {
        let src = br"
            \echo 'Applying schema'
            \i ./child.sql
            SELECT 1;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        assert!(symbols.imports.contains(&"./child.sql".to_string()));
        assert!(symbols
            .internals
            .iter()
            .all(|s| !s.signature.trim_start().starts_with('\\')));
        assert!(symbols
            .internals
            .iter()
            .any(|s| s.signature.contains("SELECT")));
    }

    #[test]
    fn extract_sql_alter_table_captures_target() {
        let src = br"ALTER TABLE users ENABLE ROW LEVEL SECURITY;";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let alter = symbols
            .internals
            .iter()
            .find(|s| s.signature.starts_with("ALTER TABLE"));
        assert!(alter.is_some(), "should extract ALTER TABLE as a symbol");
        assert!(
            alter.unwrap().calls.contains(&"target:users".to_string()),
            "should capture the table as target ref"
        );
    }
}
