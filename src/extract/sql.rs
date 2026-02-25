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

    // Pre-scan: text-based recovery for CREATE FUNCTION statements that
    // tree-sitter-sequel cannot parse (e.g. missing LANGUAGE clause with
    // complex PL/pgSQL bodies). These are injected as symbols and their
    // line ranges are used to suppress fragmented tokens in the main loop.
    let text_recovered = prescan_unrecognized_functions(src);
    let recovered_ranges: Vec<(usize, usize)> = text_recovered
        .iter()
        .map(|s| (s.line_start, s.line_end))
        .collect();
    symbols.internals.extend(text_recovered);
    let recovered_ranges = &recovered_ranges;

    let mut cursor = root.walk();
    let mut suppress_phantom_select = false;

    for node in root.children(&mut cursor) {
        if !node.is_named() || should_skip_node(node) {
            continue;
        }

        // Skip nodes covered by text-recovered functions.
        let node_start_line = node.start_position().row + 1;
        let node_end_line = node.end_position().row + 1;
        if recovered_ranges
            .iter()
            .any(|&(start, end)| node_start_line >= start && node_end_line <= end)
        {
            continue;
        }

        // CREATE POLICY FOR SELECT causes tree-sitter-sequel to emit a phantom
        // SELECT statement as a sibling — suppress it.
        if suppress_phantom_select {
            suppress_phantom_select = false;
            if is_phantom_select_after_policy(node, src) {
                continue;
            }
        }

        let Some((signature, calls)) = statement_signature_and_refs(node, src) else {
            continue;
        };

        if signature.starts_with("CREATE POLICY") {
            suppress_phantom_select = true;
        }

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

/// Text-based pre-scan: find `CREATE [OR REPLACE] FUNCTION ... AS $$ ... $$`
/// patterns that tree-sitter-sequel failed to parse. Only produces symbols
/// for functions that the AST-based path would miss (no `create_function` node).
fn prescan_unrecognized_functions(src: &[u8]) -> Vec<Symbol> {
    let Ok(text) = std::str::from_utf8(src) else {
        return Vec::new();
    };

    let mut results = Vec::new();
    let upper = text.to_ascii_uppercase();
    let mut search_from = 0;

    while search_from < upper.len() {
        // Find the next CREATE FUNCTION or CREATE OR REPLACE FUNCTION
        let (kind, func_keyword_end) =
            if let Some(pos) = upper[search_from..].find("CREATE OR REPLACE FUNCTION") {
                let abs = search_from + pos;
                (
                    "CREATE OR REPLACE FUNCTION",
                    abs + "CREATE OR REPLACE FUNCTION".len(),
                )
            } else if let Some(pos) = upper[search_from..].find("CREATE FUNCTION") {
                let abs = search_from + pos;
                ("CREATE FUNCTION", abs + "CREATE FUNCTION".len())
            } else {
                break;
            };

        // Extract function name
        let rest = text[func_keyword_end..].trim_start();
        let name_end = rest
            .find(|c: char| c == '(' || c.is_ascii_whitespace())
            .unwrap_or(rest.len());
        let func_name = &rest[..name_end];
        if func_name.is_empty() {
            search_from = func_keyword_end;
            continue;
        }

        // Find dollar-quoted body starting from the function keyword
        let Some(body) = extract_dollar_quoted_body_from_text(&text[func_keyword_end..]) else {
            search_from = func_keyword_end;
            continue;
        };

        // Compute byte positions for the closing delimiter
        let body_offset_in_text = func_keyword_end
            + text[func_keyword_end..].find(body).unwrap_or(0);
        let dollar_len = text[func_keyword_end..].find('$').map_or(2, |p| {
            let delim_start = func_keyword_end + p;
            let delim_end = text[delim_start + 1..]
                .find('$')
                .map_or(delim_start + 2, |q| delim_start + 1 + q + 1);
            delim_end - delim_start
        });
        let func_end_byte = body_offset_in_text + body.len() + dollar_len;

        // Compute line numbers
        let func_start_byte = func_keyword_end - kind.len();
        let line_start = text[..func_start_byte].matches('\n').count() + 1;
        let end_pos = func_end_byte.min(text.len());
        let line_end = text[..end_pos].matches('\n').count() + 1;

        // Check if tree-sitter already parsed this function (has a `create_function`
        // node covering this range). If so, skip — the AST path handles it.
        // We detect this by checking if there's already a parsed statement node.
        // Since we can't access the AST here, we use a heuristic: if LANGUAGE
        // keyword is present before AS, tree-sitter likely parsed it.
        let header = upper[func_start_byte..func_keyword_end + rest.len().min(200)].to_string();
        let has_language = header.contains("LANGUAGE ");
        if has_language {
            search_from = func_end_byte;
            continue;
        }

        let mut sources = Vec::new();
        let mut joins = Vec::new();
        let mut functions = Vec::new();
        reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);

        let info = SqlStatementInfo {
            kind: kind.to_string(),
            target: Some(func_name.to_string()),
            sources,
            joins,
            functions,
            ctes: Vec::new(),
        };

        let signature = format_statement_signature(&info);
        let calls = format_statement_refs(&info);

        results.push(Symbol {
            signature,
            line_start,
            line_end,
            calls,
            is_component: false,
            renders: Vec::new(),
            hooks: Vec::new(),
            handlers: Vec::new(),
            decorators: Vec::new(),
        });

        search_from = func_end_byte;
    }

    results
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

    if let Some(info) = recover_error_node(node, src) {
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
        "IF" | "THEN" | "ELSE" | "ELSIF" | "BEGIN" | "END" | "END;" | "END $$;" | "LEVEL SECURITY"
    ) {
        return true;
    }

    if upper.starts_with("BEGIN ")
        && (upper.contains(" SELECT ")
            || upper.contains(" INSERT ")
            || upper.contains(" UPDATE ")
            || upper.contains(" DELETE ")
            || upper.contains(" IF ")
            || upper.contains(" PERFORM ")
            || upper.contains(" RAISE ")
            || upper.contains(" RETURN "))
    {
        // Procedural fragment leaked from a `DO $$ ... $$` block split by the parser.
        return true;
    }

    // Orphan punctuation fragments (e.g. `)`).
    !trimmed.chars().any(char::is_alphanumeric)
}

// ---------------------------------------------------------------------------
// ERROR node recovery — structured extraction for syntax tree-sitter-sequel
// doesn't support (CREATE POLICY, DO $$ blocks).
// ---------------------------------------------------------------------------

fn recover_error_node(node: Node, src: &[u8]) -> Option<SqlStatementInfo> {
    if node.kind() != "ERROR" {
        return None;
    }
    let text = txt(node, src);
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }

    let start = node.start_byte();
    recover_create_policy(trimmed)
        .or_else(|| recover_create_function_from_source(trimmed, start, src))
        .or_else(|| recover_do_block_from_source(trimmed, start, src))
}

/// Parse `CREATE POLICY <name> ON <table> ...` from ERROR node text.
fn recover_create_policy(text: &str) -> Option<SqlStatementInfo> {
    let upper = text.to_ascii_uppercase();
    if !upper.starts_with("CREATE POLICY") {
        return None;
    }

    // Tokenize: CREATE POLICY <name> ON <table> [FOR ...] [USING ...] [WITH ...]
    let tokens: Vec<&str> = text.split_whitespace().collect();
    // Minimum: CREATE POLICY name ON table → 5 tokens
    if tokens.len() < 5 {
        return None;
    }

    // Find ON keyword (case-insensitive) after the policy name
    let on_pos = tokens
        .iter()
        .skip(3) // skip CREATE, POLICY, <name>
        .position(|t| t.eq_ignore_ascii_case("ON"))?;
    let table_idx = 3 + on_pos + 1;
    if table_idx >= tokens.len() {
        return None;
    }

    let table = tokens[table_idx];
    // Strip trailing punctuation (e.g. `;`)
    let table = table.trim_end_matches([';', ',']);
    if table.is_empty() {
        return None;
    }

    Some(SqlStatementInfo {
        kind: "CREATE POLICY".to_string(),
        target: Some(table.to_string()),
        sources: Vec::new(),
        joins: Vec::new(),
        functions: Vec::new(),
        ctes: Vec::new(),
    })
}

/// Parse `DO $$ ... $$` or `DO $tag$ ... $tag$` from ERROR node text.
/// Extracts body refs by reparsing the dollar-quoted content.
fn recover_do_block_from_source(
    error_text: &str,
    error_start: usize,
    src: &[u8],
) -> Option<SqlStatementInfo> {
    let upper = error_text.to_ascii_uppercase();
    if !upper.starts_with("DO") {
        return None;
    }
    let after_do = &error_text[2..];
    if !after_do.starts_with(|c: char| c.is_ascii_whitespace()) {
        return None;
    }
    let trimmed_rest = after_do.trim_start();
    if !trimmed_rest.starts_with('$') {
        return None;
    }

    let mut sources = Vec::new();
    let mut joins = Vec::new();
    let mut functions = Vec::new();

    // Try body from ERROR node text; fall back to scanning full source
    // when tree-sitter splits the body into sibling nodes.
    if let Some(body) = extract_dollar_quoted_body_from_text(trimmed_rest) {
        reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);
    } else if let Some(full_from_do) =
        src.get(error_start..).and_then(|b| std::str::from_utf8(b).ok())
    {
        let after = &full_from_do[full_from_do.find('$')?..];
        if let Some(body) = extract_dollar_quoted_body_from_text(after) {
            reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);
        }
    }

    Some(SqlStatementInfo {
        kind: "DO".to_string(),
        target: None,
        sources,
        joins,
        functions,
        ctes: Vec::new(),
    })
}

/// Recover `CREATE [OR REPLACE] FUNCTION <name>(...) ... AS $$ ... $$` from ERROR node text.
///
/// When tree-sitter-sequel cannot parse the function (e.g., missing LANGUAGE clause),
/// we extract the function name and body text manually, then reparse the body.
fn recover_create_function_from_source(
    error_text: &str,
    error_start: usize,
    src: &[u8],
) -> Option<SqlStatementInfo> {
    let upper = error_text.to_ascii_uppercase();

    let (kind, skip_len) = if upper.starts_with("CREATE OR REPLACE FUNCTION") {
        ("CREATE OR REPLACE FUNCTION", 26)
    } else if upper.starts_with("CREATE FUNCTION") {
        ("CREATE FUNCTION", 15)
    } else {
        return None;
    };

    let rest = error_text.get(skip_len..)?.trim_start();
    let name_end = rest.find(|c: char| c == '(' || c.is_ascii_whitespace())?;
    let func_name = rest[..name_end].trim();
    if func_name.is_empty() {
        return None;
    }

    let mut sources = Vec::new();
    let mut joins = Vec::new();
    let mut functions = Vec::new();

    // Try body from ERROR text first; fall back to full source scan
    if let Some(body) = extract_dollar_quoted_body_from_text(error_text) {
        reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);
    } else if let Some(full_text) =
        src.get(error_start..).and_then(|b| std::str::from_utf8(b).ok())
    {
        if let Some(body) = extract_dollar_quoted_body_from_text(full_text) {
            reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);
        }
    }

    Some(SqlStatementInfo {
        kind: kind.to_string(),
        target: Some(func_name.to_string()),
        sources,
        joins,
        functions,
        ctes: Vec::new(),
    })
}

/// Returns `true` when a `statement` node looks like a phantom SELECT
/// generated by tree-sitter-sequel fragmenting a `CREATE POLICY ... FOR SELECT`.
fn is_phantom_select_after_policy(node: Node, src: &[u8]) -> bool {
    if node.kind() != "statement" {
        return false;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.is_named() && (child.kind() == "select" || child.kind() == "select_expression") {
            // A real SELECT would have a FROM clause or meaningful content.
            // The phantom one typically starts with `USING (...)` or `WITH CHECK`.
            let text_upper = txt(node, src).to_ascii_uppercase();
            if text_upper.starts_with("SELECT USING")
                || text_upper.starts_with("SELECT WITH")
                || text_upper.starts_with("USING")
            {
                return true;
            }
        }
    }
    false
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

    if matches!(
        kind.as_str(),
        "CREATE FUNCTION" | "CREATE OR REPLACE FUNCTION"
    ) {
        if let Some(create_function) = op_node {
            merge_create_function_body_refs(
                create_function,
                src,
                &mut sources,
                &mut joins,
                &mut functions,
            );
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

fn merge_create_function_body_refs(
    create_function: Node,
    src: &[u8],
    sources: &mut Vec<String>,
    joins: &mut Vec<String>,
    functions: &mut Vec<String>,
) {
    if !matches!(
        create_function.kind(),
        "create_function" | "create_or_replace_function"
    ) {
        return;
    }

    let Some(function_body) = find_direct_named_child(create_function, "function_body") else {
        return;
    };

    // AST-based extraction: walk statement descendants found directly by tree-sitter.
    let mut body_statements = Vec::new();
    collect_statement_descendants(function_body, &mut body_statements);

    for child in body_statements {
        let Some(inner_info) = extract_statement_info(child, src) else {
            continue;
        };

        merge_function_body_statement_info(inner_info, sources, joins, functions);
    }

    // Dollar-quote body reparse: handles procedural bodies (plpgsql, sql)
    // that tree-sitter-sequel cannot fully parse via AST alone.
    merge_reparsed_plpgsql_body_refs(function_body, src, sources, joins, functions);
}

fn merge_reparsed_plpgsql_body_refs(
    function_body: Node,
    src: &[u8],
    sources: &mut Vec<String>,
    joins: &mut Vec<String>,
    functions: &mut Vec<String>,
) {
    let Some(body_text) = extract_dollar_quoted_body_text(function_body, src) else {
        return;
    };
    reparse_body_and_collect_refs(&body_text, sources, joins, functions);
}

/// Reparse dollar-quoted body text as standalone SQL and collect refs.
///
/// PL/pgSQL bodies are wrapped in `DECLARE ... BEGIN ... END` which tree-sitter-sequel
/// cannot parse. We strip the wrapper to expose the inner SQL statements.
fn reparse_body_and_collect_refs(
    body_text: &str,
    sources: &mut Vec<String>,
    joins: &mut Vec<String>,
    functions: &mut Vec<String>,
) {
    let stripped = strip_plpgsql_wrapper(body_text);
    let sql_text = stripped.as_deref().unwrap_or(body_text);

    let mut parser = tree_sitter::Parser::new();
    if parser
        .set_language(&tree_sitter_sequel::LANGUAGE.into())
        .is_err()
    {
        return;
    }

    let Some(tree) = parser.parse(sql_text.as_bytes(), None) else {
        return;
    };
    let body_src = sql_text.as_bytes();

    let mut cursor = tree.root_node().walk();
    for node in tree.root_node().children(&mut cursor) {
        if !node.is_named() || should_skip_node(node) {
            continue;
        }

        let Some(inner_info) =
            extract_statement_info(node, body_src).or_else(|| recover_error_node(node, body_src))
        else {
            continue;
        };

        merge_function_body_statement_info(inner_info, sources, joins, functions);
    }
}

/// Strip PL/pgSQL wrapper (`DECLARE ... BEGIN ... END;`) to expose inner SQL.
///
/// Returns `None` if no wrapper is detected, meaning the text can be used as-is.
fn strip_plpgsql_wrapper(body: &str) -> Option<String> {
    let trimmed = body.trim();
    let upper = trimmed.to_ascii_uppercase();

    // Find BEGIN (skip past DECLARE section if present)
    let lower = trimmed.to_ascii_lowercase();
    let begin_idx = if upper.starts_with("DECLARE") {
        // DECLARE ... BEGIN — find the first standalone BEGIN
        find_keyword_ascii(&lower, "begin")?
    } else if upper.starts_with("BEGIN") {
        0
    } else {
        return None;
    };

    let after_begin = begin_idx + "BEGIN".len();
    let inner = &trimmed[after_begin..];

    // Strip trailing END (with optional semicolon/whitespace)
    let inner_upper = inner.trim_end().to_ascii_uppercase();
    let stripped = if inner_upper.ends_with("END;") {
        &inner[..inner.trim_end().len() - 4]
    } else if inner_upper.ends_with("END") {
        &inner[..inner.trim_end().len() - 3]
    } else {
        inner
    };

    Some(stripped.to_string())
}

fn merge_function_body_statement_info(
    inner_info: SqlStatementInfo,
    sources: &mut Vec<String>,
    joins: &mut Vec<String>,
    functions: &mut Vec<String>,
) {
    let SqlStatementInfo {
        target: inner_target,
        sources: inner_sources,
        joins: inner_joins,
        functions: inner_functions,
        ..
    } = inner_info;

    if let Some(target) = inner_target {
        // CREATE FUNCTION already uses `target:` for the function name, so body write
        // targets are folded into dependencies.
        push_unique(sources, target);
    }

    for source in inner_sources {
        push_unique(sources, source);
    }
    for join in inner_joins {
        push_unique(joins, join);
    }
    for function in inner_functions {
        push_unique(functions, function);
    }
}

fn collect_statement_descendants<'tree>(node: Node<'tree>, out: &mut Vec<Node<'tree>>) {
    if node.kind() == "statement" {
        out.push(node);
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if !child.is_named() {
            continue;
        }
        collect_statement_descendants(child, out);
    }
}

fn extract_dollar_quoted_body_text(function_body: Node, src: &[u8]) -> Option<String> {
    let mut first_quote = None;
    let mut last_quote = None;

    let mut cursor = function_body.walk();
    for child in function_body.children(&mut cursor) {
        if !child.is_named() || child.kind() != "dollar_quote" {
            continue;
        }
        if first_quote.is_none() {
            first_quote = Some(child);
        }
        last_quote = Some(child);
    }

    let start = first_quote?.end_byte();
    let end = last_quote?.start_byte();
    if end <= start {
        return None;
    }

    let body = src.get(start..end)?;
    let text = std::str::from_utf8(body).ok()?;
    Some(text.to_string())
}

/// Extract text between the first pair of matching dollar-quote delimiters from raw text.
///
/// Handles both `$$` and `$tag$` delimiters. Unlike `extract_dollar_quoted_body_text` which
/// works on AST nodes, this operates on raw `&str` for error recovery paths.
fn extract_dollar_quoted_body_from_text(text: &str) -> Option<&str> {
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'$' {
            i += 1;
            continue;
        }
        let delim_start = i;
        i += 1;
        while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1;
        }
        if i >= bytes.len() || bytes[i] != b'$' {
            continue;
        }
        i += 1;
        let delimiter = &text[delim_start..i];
        let body_start = i;

        if let Some(pos) = text[body_start..].find(delimiter) {
            return Some(&text[body_start..body_start + pos]);
        }
        break;
    }
    None
}

fn find_direct_named_child<'tree>(node: Node<'tree>, kind: &str) -> Option<Node<'tree>> {
    let mut cursor = node.walk();
    let found = node
        .children(&mut cursor)
        .find(|child| child.is_named() && child.kind() == kind);
    found
}

fn detect_statement_kind(statement: Node) -> Option<(String, Option<Node>)> {
    const KNOWN: &[&str] = &[
        "create_materialized_view",
        "create_table",
        "create_view",
        "create_index",
        "create_function",
        "create_or_replace_function",
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
        ("DO", None) => parts.push("DO $$ [anonymous block]".to_string()),
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

    // -- CREATE POLICY recovery --

    #[test]
    fn recover_create_policy_extracts_target_table() {
        let info =
            recover_create_policy("CREATE POLICY tenant_isolation ON users USING (tenant_id = 1)")
                .unwrap();
        assert_eq!(info.kind, "CREATE POLICY");
        assert_eq!(info.target.as_deref(), Some("users"));
    }

    #[test]
    fn recover_create_policy_schema_qualified_target() {
        let info = recover_create_policy("CREATE POLICY p ON app.orders USING (true)").unwrap();
        assert_eq!(info.target.as_deref(), Some("app.orders"));
    }

    #[test]
    fn recover_create_policy_for_select_variant() {
        let info = recover_create_policy(
            "CREATE POLICY read_only ON users FOR SELECT USING (active = true)",
        )
        .unwrap();
        assert_eq!(info.kind, "CREATE POLICY");
        assert_eq!(info.target.as_deref(), Some("users"));
    }

    #[test]
    fn recover_create_policy_returns_none_for_unrelated() {
        assert!(recover_create_policy("CREATE TABLE users (id INT)").is_none());
        assert!(recover_create_policy("SELECT 1").is_none());
    }

    #[test]
    fn extract_symbols_create_policy_produces_target_ref() {
        let src = br"CREATE POLICY tenant_isolation ON users USING (tenant_id = current_tenant());";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let policy = symbols
            .internals
            .iter()
            .find(|s| s.signature.starts_with("CREATE POLICY"));
        assert!(policy.is_some(), "should extract CREATE POLICY as a symbol");
        assert!(
            policy.unwrap().calls.contains(&"target:users".to_string()),
            "should capture the table as target ref"
        );
    }

    #[test]
    fn extract_symbols_suppresses_phantom_select_after_create_policy() {
        let src = br"
            CREATE POLICY read_only ON users FOR SELECT USING (active = true);
            CREATE TABLE orders (id INT);
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        // Should have CREATE POLICY + CREATE TABLE, but NOT a phantom SELECT
        let signatures: Vec<&str> = symbols
            .internals
            .iter()
            .map(|s| s.signature.as_str())
            .collect();
        assert!(
            signatures.iter().any(|s| s.starts_with("CREATE POLICY")),
            "should have CREATE POLICY: {signatures:?}"
        );
        assert!(
            signatures.iter().any(|s| s.starts_with("CREATE TABLE")),
            "should have CREATE TABLE: {signatures:?}"
        );
        // The phantom SELECT from FOR SELECT fragmentation should be suppressed
        let phantom_selects: Vec<&&str> = signatures
            .iter()
            .filter(|s| {
                s.to_ascii_uppercase().starts_with("SELECT USING")
                    || s.to_ascii_uppercase().starts_with("SELECT WITH")
            })
            .collect();
        assert!(
            phantom_selects.is_empty(),
            "phantom SELECT should be suppressed: {phantom_selects:?}"
        );
    }

    // -- DO $$ block recovery --

    /// Helper: call `recover_do_block_from_source` with text as its own source.
    fn recover_do_block(text: &str) -> Option<SqlStatementInfo> {
        recover_do_block_from_source(text, 0, text.as_bytes())
    }

    #[test]
    fn recover_do_block_simple() {
        let info = recover_do_block("DO $$ BEGIN RAISE NOTICE 'hello'; END $$;").unwrap();
        assert_eq!(info.kind, "DO");
        assert!(info.target.is_none());
    }

    #[test]
    fn recover_do_block_custom_dollar_tag() {
        let info = recover_do_block("DO $body$ BEGIN PERFORM 1; END $body$;").unwrap();
        assert_eq!(info.kind, "DO");
    }

    #[test]
    fn recover_do_block_returns_none_for_non_do() {
        assert!(recover_do_block("CREATE TABLE users (id INT)").is_none());
        assert!(recover_do_block("DO_SOMETHING()").is_none());
        assert!(recover_do_block("DONE").is_none());
    }

    #[test]
    fn recover_do_block_extracts_body_refs() {
        let text = "DO $$ BEGIN INSERT INTO tenants (name) VALUES ('test'); SELECT id FROM objects; END $$;";
        let info = recover_do_block(text).unwrap();
        assert_eq!(info.kind, "DO");
        assert!(
            info.sources.contains(&"tenants".to_string()),
            "should find source:tenants in {:?}",
            info.sources
        );
        assert!(
            info.sources.contains(&"objects".to_string()),
            "should find source:objects in {:?}",
            info.sources
        );
    }

    #[test]
    fn recover_do_block_custom_tag_extracts_refs() {
        let text = "DO $body$ BEGIN UPDATE config SET value = 'x' WHERE key = 'k'; END $body$;";
        let info = recover_do_block(text).unwrap();
        assert!(
            info.sources.contains(&"config".to_string()),
            "should find source:config in {:?}",
            info.sources
        );
    }

    #[test]
    fn extract_symbols_collapses_do_block() {
        let src = br"
            DO $$ BEGIN
                RAISE NOTICE 'migrating';
            END $$;
            SELECT 1;
        ";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let do_sym = symbols
            .internals
            .iter()
            .find(|s| s.signature.contains("DO $$"));
        assert!(
            do_sym.is_some(),
            "should extract DO block: {:?}",
            symbols
                .internals
                .iter()
                .map(|s| &s.signature)
                .collect::<Vec<_>>()
        );
        assert_eq!(do_sym.unwrap().signature, "DO $$ [anonymous block]");
    }

    #[test]
    fn fallback_filters_begin_fragment() {
        assert!(should_skip_fallback_signature("BEGIN"));
    }

    #[test]
    fn fallback_filters_begin_plpgsql_fragment() {
        assert!(should_skip_fallback_signature(
            "BEGIN SELECT id INTO v_object_id FROM platform.fm__object WHERE api_name = 'x'; IF v_object_id IS NULL"
        ));
    }

    #[test]
    fn fallback_filters_begin_insert_fragment() {
        assert!(should_skip_fallback_signature(
            "BEGIN INSERT INTO tenants (name) VALUES ('test') RETURNING id INTO v_tenant_id;"
        ));
    }

    #[test]
    fn fallback_filters_begin_update_fragment() {
        assert!(should_skip_fallback_signature(
            "BEGIN UPDATE config SET value = 'x' WHERE key = 'k'; END"
        ));
    }

    #[test]
    fn fallback_filters_begin_delete_fragment() {
        assert!(should_skip_fallback_signature(
            "BEGIN DELETE FROM old_sessions WHERE expired = true;"
        ));
    }

    #[test]
    fn extract_symbols_do_block_has_refs() {
        let src = br"
DO $$ BEGIN
    INSERT INTO tenants (name) VALUES ('test');
    SELECT id FROM objects;
END $$;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let do_sym = symbols
            .internals
            .iter()
            .find(|s| s.signature.contains("DO $$"))
            .expect("should extract DO block");
        assert!(
            do_sym.calls.contains(&"source:tenants".to_string()),
            "DO block should have source:tenants ref: {:?}",
            do_sym.calls
        );
        assert!(
            do_sym.calls.contains(&"source:objects".to_string()),
            "DO block should have source:objects ref: {:?}",
            do_sym.calls
        );
    }

    #[test]
    fn extract_symbols_do_block_no_leaked_begin_fragment() {
        let src = br"
DO $$ BEGIN
    INSERT INTO tenants (name) VALUES ('test');
END $$;
SELECT 1;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let leaked = symbols.internals.iter().find(|s| {
            s.signature
                .to_ascii_uppercase()
                .starts_with("BEGIN INSERT")
        });
        assert!(
            leaked.is_none(),
            "leaked BEGIN fragment should be suppressed: {:?}",
            symbols
                .internals
                .iter()
                .map(|s| &s.signature)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn create_function_plpgsql_captures_body_refs() {
        let src = br"
CREATE FUNCTION normalize_object(p_input TEXT)
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
    v_result TEXT;
BEGIN
    SELECT coalesce(name, '') INTO v_result FROM objects WHERE id = p_input;
    INSERT INTO audit_log (action, target) VALUES ('normalize', p_input);
    UPDATE object_stats SET last_seen_at = now() WHERE object_id = p_input;
    RETURN v_result;
END;
$$;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let func = symbols
            .internals
            .iter()
            .find(|s| s.signature.starts_with("CREATE FUNCTION normalize_object"))
            .expect("should extract CREATE FUNCTION symbol");

        assert!(func.calls.contains(&"target:normalize_object".to_string()));
        assert!(func.calls.contains(&"source:objects".to_string()));
        assert!(func.calls.contains(&"source:audit_log".to_string()));
        assert!(func.calls.contains(&"source:object_stats".to_string()));
        assert!(func.calls.contains(&"fn:coalesce".to_string()));
        assert!(func.calls.contains(&"fn:now".to_string()));
    }

    #[test]
    fn create_function_sql_no_duplicate_refs() {
        let src = br"
CREATE FUNCTION count_users()
RETURNS INT
LANGUAGE sql
AS $$
    SELECT count(*) FROM users;
$$;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let func = symbols
            .internals
            .iter()
            .find(|s| s.signature.starts_with("CREATE FUNCTION count_users"))
            .expect("should extract CREATE FUNCTION symbol");

        let users_refs = func
            .calls
            .iter()
            .filter(|c| c.as_str() == "source:users")
            .count();
        let count_refs = func
            .calls
            .iter()
            .filter(|c| c.as_str() == "fn:count")
            .count();

        assert_eq!(users_refs, 1, "source:users should not be duplicated");
        assert_eq!(count_refs, 1, "fn:count should not be duplicated");
    }

    #[test]
    fn create_or_replace_function_plpgsql_captures_body_refs() {
        let src = br"
CREATE OR REPLACE FUNCTION emit_metadata_event()
RETURNS INT
LANGUAGE plpgsql
AS $$
BEGIN
    INSERT INTO event_log (kind) VALUES ('metadata');
    RETURN 1;
END;
$$;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);
        let signatures: Vec<&str> = symbols
            .internals
            .iter()
            .map(|s| s.signature.as_str())
            .collect();

        let func = symbols
            .internals
            .iter()
            .find(|s| s.signature.contains("emit_metadata_event"))
            .unwrap_or_else(|| {
                panic!("should extract CREATE OR REPLACE FUNCTION symbol: {signatures:?}")
            });

        assert!(func
            .calls
            .contains(&"target:emit_metadata_event".to_string()));
        assert!(func.calls.contains(&"source:event_log".to_string()));
    }

    // -- dollar-quote text extraction --

    #[test]
    fn extract_dollar_quoted_body_simple_delimiters() {
        assert_eq!(
            extract_dollar_quoted_body_from_text("$$ BEGIN SELECT 1; END $$"),
            Some(" BEGIN SELECT 1; END ")
        );
    }

    #[test]
    fn extract_dollar_quoted_body_custom_tag() {
        assert_eq!(
            extract_dollar_quoted_body_from_text("$body$ INSERT INTO t VALUES (1); $body$"),
            Some(" INSERT INTO t VALUES (1); ")
        );
    }

    #[test]
    fn extract_dollar_quoted_body_no_closing_delimiter() {
        assert!(extract_dollar_quoted_body_from_text("$$ BEGIN SELECT 1;").is_none());
    }

    #[test]
    fn extract_dollar_quoted_body_no_dollar_quote() {
        assert!(extract_dollar_quoted_body_from_text("SELECT 1;").is_none());
    }

    #[test]
    fn extract_dollar_quoted_body_skips_lone_dollar() {
        assert!(extract_dollar_quoted_body_from_text("price = $5 and value = $10").is_none());
    }

    // -- reparse body refs --

    #[test]
    fn reparse_body_collects_insert_target_as_source() {
        let body = "\n    SELECT name FROM objects WHERE id = 1;\n    INSERT INTO audit_log (action) VALUES ('test');\n";
        let mut sources = Vec::new();
        let mut joins = Vec::new();
        let mut functions = Vec::new();
        reparse_body_and_collect_refs(body, &mut sources, &mut joins, &mut functions);
        assert!(
            sources.contains(&"objects".to_string()),
            "should find objects in sources: {sources:?}"
        );
        assert!(
            sources.contains(&"audit_log".to_string()),
            "should find audit_log in sources: {sources:?}"
        );
    }

    // -- CREATE FUNCTION recovery (no LANGUAGE clause) --

    #[test]
    fn recover_create_function_without_language_extracts_target_and_refs() {
        let src = br"
CREATE OR REPLACE FUNCTION trigger_handler()
RETURNS TRIGGER
AS $$
BEGIN
    SELECT name FROM objects WHERE id = NEW.id;
    INSERT INTO audit_log (action) VALUES ('trigger');
    RETURN NEW;
END;
$$;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let func = symbols
            .internals
            .iter()
            .find(|s| s.signature.contains("trigger_handler"))
            .unwrap_or_else(|| {
                panic!(
                    "should recover CREATE OR REPLACE FUNCTION: {:?}",
                    symbols
                        .internals
                        .iter()
                        .map(|s| &s.signature)
                        .collect::<Vec<_>>()
                )
            });

        assert!(
            func.calls
                .contains(&"target:trigger_handler".to_string()),
            "should have target ref: {:?}",
            func.calls
        );
        assert!(
            func.calls.contains(&"source:objects".to_string()),
            "should have body source ref: {:?}",
            func.calls
        );
        assert!(
            func.calls.contains(&"source:audit_log".to_string()),
            "should have body source ref: {:?}",
            func.calls
        );
    }

    #[test]
    fn recover_create_function_suppresses_orphan_tokens() {
        let src = br"
CREATE OR REPLACE FUNCTION trigger_handler()
RETURNS TRIGGER
AS $$
BEGIN
    SELECT 1 FROM test_table;
    RETURN NEW;
END;
$$;
SELECT id FROM users;
";
        let tree = parse_sql(src);
        let symbols = extract_symbols(tree.root_node(), src);

        let orphans: Vec<&str> = symbols
            .internals
            .iter()
            .filter(|s| {
                let upper = s.signature.to_ascii_uppercase();
                matches!(
                    upper.as_str(),
                    "RETURNS" | "TRIGGER" | "DECLARE" | "AS" | "FUNCTION" | "OR" | "REPLACE"
                )
            })
            .map(|s| s.signature.as_str())
            .collect();
        assert!(
            orphans.is_empty(),
            "should suppress orphan tokens: {orphans:?}"
        );

        // The SELECT after the function should still be present
        assert!(
            symbols.internals.iter().any(|s| s.signature.contains("users")),
            "subsequent SELECT should be preserved: {:?}",
            symbols
                .internals
                .iter()
                .map(|s| &s.signature)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn recover_create_function_returns_none_for_unrelated() {
        assert!(
            recover_create_function_from_source("CREATE TABLE users", 0, b"CREATE TABLE users")
                .is_none()
        );
        assert!(
            recover_create_function_from_source("SELECT 1", 0, b"SELECT 1").is_none()
        );
    }

    #[test]
    fn recover_create_function_body_from_full_source() {
        let src = b"\nCREATE OR REPLACE FUNCTION trigger_handler()\nRETURNS TRIGGER\nAS $$\nBEGIN\n    SELECT name FROM objects WHERE id = NEW.id;\n    INSERT INTO audit_log (action) VALUES ('trigger');\n    RETURN NEW;\nEND;\n$$;\n";
        // Simulate: ERROR node text might be just the header
        let error_text = "CREATE OR REPLACE FUNCTION trigger_handler()";
        let info = recover_create_function_from_source(error_text, 1, src).unwrap();
        assert_eq!(info.target.as_deref(), Some("trigger_handler"));
        assert!(
            info.sources.contains(&"objects".to_string()),
            "should find objects via full source scan: {:?}",
            info.sources
        );
        assert!(
            info.sources.contains(&"audit_log".to_string()),
            "should find audit_log via full source scan: {:?}",
            info.sources
        );
    }

    #[test]
    fn recover_create_function_body_from_error_text() {
        let full = "CREATE OR REPLACE FUNCTION my_func() RETURNS INT AS $$ BEGIN SELECT 1 FROM users; INSERT INTO logs (a) VALUES ('x'); END; $$;";
        let info = recover_create_function_from_source(full, 0, full.as_bytes()).unwrap();
        assert_eq!(info.target.as_deref(), Some("my_func"));
        assert!(
            info.sources.contains(&"users".to_string()),
            "should find users: {:?}",
            info.sources
        );
        assert!(
            info.sources.contains(&"logs".to_string()),
            "should find logs: {:?}",
            info.sources
        );
    }
}
