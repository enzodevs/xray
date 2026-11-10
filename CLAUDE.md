# xray

Bounded structural navigation for TS/JS, Go, Vue, Svelte, Python, PHP, Rust, SQL, and Markdown. Compression varies; extraction is not runtime completeness. See `docs/agent-navigation.md` for the AXI/output contract.

## Build & Verify

```sh
cargo clippy --all-targets -- -D warnings   # lint (MUST pass before commit)
cargo test                                   # all tests
cargo build --release                        # optimized binary
```

## Usage

```
xray <file> [file2 ...]          # follow mode (default): tree with depth 1 + noise filter
xray --depth 2 <file>            # follow with deeper recursion
xray --all <file>                # follow without noise filter
xray --who <file>                # reverse: show who imports this file
xray --trace <file>              # trace cross-file call chains (depth 3)
xray --trace --lsp <file>        # trace with LSP-resolved member calls
xray --trace <file> -s <name>    # trace a specific symbol
xray --no-follow <file>          # single-file structural digest
xray --definition --at L:C <file> # exact LSP definition; 1-based UTF-8 byte column
xray --references --at L:C <file> # static symbol references (including PHP)
xray --implementations --at L:C <file> # PHP implementations (Intelephense premium)
xray --type-definition --at L:C <file> # PHP type definition (premium)
xray --declaration --at L:C <file> # PHP declaration (premium)
xray --supertypes --at L:C <file> # direct parent types, one hop (premium)
xray --subtypes --at L:C <file>   # direct child types, one hop (premium)
xray --follow --format json <file> # structured dependency tree
xray --max-bytes 4096 <file>      # bounded output + private exact overflow
```

Position-based navigation requires exactly one PHP file and uses LSP automatically. All navigation modes support bounded text/JSON and exact overflow recovery. Hierarchy queries are one-hop static relationships, not transitive or runtime completeness; `--depth` does not apply. These modes never edit source files.

## Architecture

| File | Role |
|------|------|
| `main.rs` | CLI arg parsing (Mode enum) + dispatch |
| `lang.rs` | `LanguageKind` enum (Ts/Sql/Py/Go/Php/Rs/Svelte/Vue/Md): central dispatch for extraction, resolution, capabilities |
| `follow.rs` | `--follow` (default): tree building, noise detection, rendering |
| `trace.rs` | `--trace`: cross-file call graph traversal + rendering + LSP fallback |
| `lsp.rs` | Deadline-bounded LSP transport, positions/URIs, server capabilities and definitions/references |
| `navigation.rs` | Exact position queries and reusable workspace LSP sessions; separate convention result types |
| `php_conventions.rs` | Bounded Git-visible PHP convention scan; candidates never become exact LSP references |
| `extract/php/conventions.rs` | Syntactic scopes, Job handlers and listener event candidates |
| `extract/php/inertia.rs` | Literal namespace-resolved Inertia render dependencies |
| `presentation.rs` | AXI byte caps, valid compact JSON, private exact overflow artifacts |
| `reverse.rs` | `--who`: Git-aware enumeration, fresh directory-scoped resolution, named recovery evidence, importers including conventional Inertia pages |
| `reverse_cache.rs` | Private bounded parse-fact cache; resolved edges and Git visibility are not cached |
| `resolve.rs` | Facade module re-exporting `resolve::ts`, `resolve::go`, `resolve::py`, `resolve::php`, `resolve::rs`, `resolve::sql`, and `resolve::markdown` |
| `resolve/ts.rs` | Local TS/JS import resolution; Oxc-backed inherited JSONC/tsconfig paths via `load_path_config` |
| `resolve/go.rs` | Go import resolution: `go.mod` module paths + relative packages; enumerate all non-test package sources for follow |
| `resolve/py.rs` | Python import resolution: relative (`.`/`..`) + local project modules/packages |
| `resolve/php.rs` | PHP import resolution: nearest Composer PSR-4 mappings (including dev/array paths), relative includes, and legacy namespace fallback |
| `resolve/rs.rs` | Rust module resolution: `mod`, `crate::`, `super::`, `self::` paths |
| `resolve/sql.rs` | SQL include resolution: `resolve_sql_include` (bare + relative paths) |
| `resolve/markdown.rs` | Markdown link resolution: local relative links + fragment stripping |
| `resolve/shared.rs` | Shared `try_extensions_with()` utility |
| `output.rs` | `FileDigest::from_path`, `summarize()`, `FileSummary`, Display impls |
| `model.rs` | Symbol, JsxNode, TypeDef, Hook, MarkdownDocument, FileSummary, FileContent, FileSymbols, SymbolRefsLabel |
| `extract/mod.rs` | Backend entrypoints: TS/Python/Rust/SQL/Markdown extraction + integration tests |
| `extract/markdown.rs` | Markdown extraction (headings, links, frontmatter, fenced code blocks) |
| `extract/go.rs` | Go extraction (imports, funcs/methods, vars/consts, structs/interfaces/types, tests, calls) |
| `extract/python.rs` | Python extraction (imports, public/internal funcs, classes/dataclasses, calls, `__all__`) |
| `extract/php.rs` | PHP extraction (`use`, include/require, funcs, classes/interfaces/traits/enums, owner-qualified methods with visibility/ranges/calls) |
| `extract/rust.rs` | Rust extraction (use/mod, pub/private fns, structs/enums/traits/impls, derives, tests, calls) |
| `extract/svelte.rs` | Svelte extraction (template components + `<script>` imports/exports/types/calls via TS backend) |
| `extract/vue.rs` | Vue extraction (template components/directive calls + `<script>` imports/exports/types/calls via TS backend) |
| `extract/sql.rs` | SQL statement extraction (SELECT, CREATE, INSERT, UPDATE, DELETE, MERGE) |
| `extract/sql_includes.rs` | SQL client includes at statement boundaries, excluding comments and quoted bodies |
| `extract/jsx.rs` | JSX detection + hierarchy tree (`returns_jsx`, `extract_jsx_components`) |
| `extract/hooks.rs` | React hooks extraction |
| `extract/calls.rs` | Call extraction |
| `extract/decorators.rs` | Decorator extraction (`@Component`, `@Input`, etc.) |
| `extract/types.rs` | Type defs (interface, type alias, enum) |
| `extract/tests_block.rs` | Test block extraction (describe/it/test) |
| `parser.rs` | tree-sitter code parsing + Markdown mdast parsing via `ParsedFile` |
| `error.rs` | XrayError enum (thiserror) |
| `util.rs` | txt(), trim_quotes(), compress_members(), is_noise(), git_root() |

## Conventions

- **Rust practices**: See [docs/rust-practices.md](docs/rust-practices.md) — MANDATORY
- **Lints**: `clippy::all = deny`, `clippy::pedantic = warn` (see Cargo.toml)
- **Errors**: `thiserror` only. No `unwrap()`/`expect()` outside tests
- **Agent output**: route results through `presentation`; never print unbounded per-file output. Preserve errors, declared coverage and exact overflow. New output shapes need context-cost and recovery tests.
- **Tests**: Descriptive names, one behavior per test. Use `parse_ts()` / `parse_tsx()` / `parse_rs()` helpers
- **tree-sitter-typescript v0.23 gotchas** (runtime: tree-sitter v0.27):
  - Enum members are `property_identifier`, not `enum_member`
  - JSX tag name: `child_by_field_name("name")`, NOT `child(0)`
  - Function expressions in args: `function_expression` node kind
  - `namespace` → wrapped in `expression_statement` > `internal_module`
  - `declare module` → wrapped in `ambient_declaration` > `module`
  - Decorators: accessed via `children_by_field_name("decorator")`; `named_child(0)` gives the expression after `@`
- **tree-sitter-rust v0.24 gotchas**:
  - Attributes (`#[...]`) are sibling `attribute_item` nodes, not children of the decorated item
  - `visibility_modifier` is a child node (first named child), not a keyword token
  - `impl Trait for Type` has two `type_identifier` children: trait first, type second
  - `macro_definition` has `name` field; macro calls are `macro_invocation` (separate node kind)
  - `function_signature_item` for trait method signatures without body (vs `function_item` with body)

## Smoke Testing

After `cargo test` passes, validate changes against a real codebase:

**Test project**: `/home/rrghost/SoftEng/fluxomind/fluxomind-platform` (Next.js, ~large)

Do NOT create temporary test files for smoke testing — run xray directly against real files.
See `AGENTS.md` for detailed agent workflow and example targets.
