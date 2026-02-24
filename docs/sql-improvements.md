# SQL Extraction — Known Gaps & Improvement Plan

Status after commits up to `81f573c`.

## What works well

- **Statement extraction**: SELECT, INSERT, UPDATE, DELETE, MERGE, CREATE TABLE/VIEW/INDEX/FUNCTION/TYPE/SCHEMA, ALTER TABLE
- **Refs**: `target:`, `source:`, `join:`, `cte:`, `fn:` — correctly extracted
- **Include directives**: `\i`, `\ir`, `SOURCE`, `@@` — captured as imports, follow/who work
- **Noise filtering**: orphan fragments, SET search_path, psql metacommand blobs filtered
- **CREATE FUNCTION (LANGUAGE sql)**: body is parsed by tree-sitter → refs already extracted from inner statements
- **UTF-8 safety**: `is_char_boundary` + `char_indices` guards on all string slicing

## Gaps to address

### 1. CREATE POLICY — not parsed by tree-sitter-sequel

**Problem**: `CREATE POLICY name ON table USING (...)` becomes an ERROR node in the AST. The parser doesn't recognize this syntax, so it falls through to fallback. Worse, it can swallow adjacent statements (like `DO $$` blocks) into the same ERROR node.

**Current behavior**: appears as truncated fallback signature or gets swallowed.

**Fix approach**: detect ERROR nodes that start with `CREATE POLICY`, parse target table from text via regex/manual extraction. Something like:

```
ERROR node text: "CREATE POLICY tenant_isolation ON users USING (...)"
                                                ^^ extract "users" as target
```

**Effort**: Medium. Text parsing of ERROR nodes is fragile but CREATE POLICY has a predictable structure (`CREATE POLICY <name> ON <table>`).

**Files to change**: `src/extract/sql.rs` — add a recovery path in `statement_signature_and_refs` for ERROR nodes whose text starts with `CREATE POLICY`.

---

### 2. DO $$ ... $$ anonymous blocks — fragmented or swallowed

**Problem**: tree-sitter-sequel doesn't parse `DO $$ ... $$` as a standalone statement. It either:
- Gets swallowed into an adjacent ERROR node (when preceded by unsupported syntax like CREATE POLICY)
- Gets split into multiple fallback fragments (`DO $$`, `BEGIN ...`, `END $$;`)

**Current behavior**: fragments are mostly filtered by noise heuristics, but the block is invisible in output.

**Fix approach**: Two options:
1. **Collapse**: detect `DO` keyword at statement level, extract the full text as a single symbol with a compact signature like `DO $$ [anonymous block]`
2. **Omit**: if the block is purely procedural glue (common in migrations), skip it entirely

Option 1 is better for visibility. The challenge is that when DO $$ is inside an ERROR node, recovery requires text parsing.

**Effort**: Medium. Standalone `DO $$` (not swallowed by ERROR) is simpler; ERROR-recovery case is harder.

**Files to change**: `src/extract/sql.rs` — detect `keyword_do` in statement children, or text-match ERROR nodes starting with `DO`.

---

### 3. CREATE FUNCTION body refs (LANGUAGE plpgsql) — opaque body

**Problem**: PL/pgSQL function bodies are stored as dollar-quoted strings (`$$ ... $$`). tree-sitter-sequel parses the `create_function` node correctly (name, args, return type) but the body content inside `$$` is opaque text — no sub-AST for the PL/pgSQL statements inside.

**Current behavior**: `CREATE FUNCTION normalize_object [L14-144]` with no refs, even though the function body contains SELECT/UPDATE statements referencing multiple tables.

**LANGUAGE sql functions already work** — their body is parsed into sub-statements with full AST.

**Fix approach**: For plpgsql bodies, extract the dollar-quoted text and scan it for table references. Options:
1. **Regex scan**: find `FROM <table>`, `JOIN <table>`, `INTO <table>`, `UPDATE <table>` patterns in the body text
2. **Re-parse**: feed the body text into tree-sitter-sequel as if it were standalone SQL (won't work for PL/pgSQL control flow like `IF/THEN/ELSE`, but would catch embedded SQL statements)
3. **Hybrid**: re-parse, fall back to regex for what the parser misses

Option 2 is the most promising — tree-sitter-sequel will parse the SQL parts and produce ERROR nodes for PL/pgSQL control flow, but the SQL statements should be extractable.

**Effort**: High. Requires extracting dollar-quoted body text, re-parsing, merging refs back into the parent symbol. Edge cases around nested `$$`, `$body$` delimiters.

**Files to change**: `src/extract/sql.rs` — after `create_function` is detected, check if `function_body` contains a `dollar_quote` with opaque text; if so, attempt re-parse of inner content.

---

## Suggested implementation order

1. **CREATE POLICY** (medium) — highest visibility in DDL-heavy codebases, predictable text structure
2. **DO $$ collapse** (medium) — reduces noise in migrations, can share ERROR-recovery logic with CREATE POLICY
3. **CREATE FUNCTION plpgsql refs** (high) — biggest real-world impact but most complex

## tree-sitter-sequel reference

AST shapes observed for each case (from dump on real files):

```
-- ALTER TABLE (works, added to KNOWN)
statement > alter_table > object_reference

-- CREATE POLICY (ERROR node)
ERROR > keyword_create, keyword_on, keyword_using

-- DO $$ (ERROR node or fragments)
ERROR > keyword_do, dollar_quote, keyword_begin, keyword_end, dollar_quote

-- CREATE FUNCTION (LANGUAGE sql — body parsed)
statement > create_function > function_body > statement > select > from > relation

-- CREATE FUNCTION (LANGUAGE plpgsql — body opaque)
statement > create_function > function_body > dollar_quote, [opaque text], dollar_quote
```
