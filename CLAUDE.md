# xray

Structural code digest for TS/JS and SQL files using tree-sitter. ~10x compression.

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
xray --no-follow <file>          # plain: single file digest only
```

## Architecture

| File | Role |
|------|------|
| `main.rs` | CLI arg parsing (Mode enum) + dispatch |
| `lang.rs` | `LanguageKind` enum (Ts/Sql): central dispatch for extraction, resolution, capabilities |
| `follow.rs` | `--follow` (default): tree building, noise detection, rendering |
| `trace.rs` | `--trace`: cross-file call graph traversal + rendering + LSP fallback |
| `lsp.rs` | `LspClient`: JSON-RPC 2.0 over stdio, `typescript-language-server` integration |
| `reverse.rs` | `--who`: project scanning, find importers (TS + SQL) |
| `resolve.rs` | Facade module re-exporting `resolve::ts` and `resolve::sql` |
| `resolve/ts.rs` | TS/JS import resolution: `PathConfig`, `resolve_import`, `load_path_config` |
| `resolve/sql.rs` | SQL include resolution: `resolve_sql_include` (bare + relative paths) |
| `resolve/shared.rs` | Shared `try_extensions_with()` utility |
| `output.rs` | `FileDigest::from_path`, `summarize()`, `FileSummary`, Display impls |
| `model.rs` | Symbol, JsxNode, TypeDef, Hook, ImportBinding, TestBlock, FileSummary, FileSymbols, SymbolRefsLabel |
| `extract/mod.rs` | Backend entrypoints: `extract_ts_symbols`, `extract_sql_symbols` + integration tests |
| `extract/sql.rs` | SQL statement extraction (SELECT, CREATE, INSERT, UPDATE, DELETE, MERGE) + include directives |
| `extract/jsx.rs` | JSX detection + hierarchy tree (`returns_jsx`, `extract_jsx_components`) |
| `extract/hooks.rs` | React hooks extraction |
| `extract/calls.rs` | Call extraction |
| `extract/decorators.rs` | Decorator extraction (`@Component`, `@Input`, etc.) |
| `extract/types.rs` | Type defs (interface, type alias, enum) |
| `extract/tests_block.rs` | Test block extraction (describe/it/test) |
| `parser.rs` | tree-sitter language detection + `ParsedFile` (tree + source + `LanguageKind`) |
| `error.rs` | XrayError enum (thiserror) |
| `util.rs` | txt(), trim_quotes(), compress_members(), is_noise(), git_root() |

## Conventions

- **Rust practices**: See [docs/rust-practices.md](docs/rust-practices.md) — MANDATORY
- **Lints**: `clippy::all = deny`, `clippy::pedantic = warn` (see Cargo.toml)
- **Errors**: `thiserror` only. No `unwrap()`/`expect()` outside tests
- **Tests**: Descriptive names, one behavior per test. Use `parse_ts()` / `parse_tsx()` helpers
- **tree-sitter v0.23 gotchas**:
  - Enum members are `property_identifier`, not `enum_member`
  - JSX tag name: `child_by_field_name("name")`, NOT `child(0)`
  - Function expressions in args: `function_expression` node kind
  - `namespace` → wrapped in `expression_statement` > `internal_module`
  - `declare module` → wrapped in `ambient_declaration` > `module`
  - Decorators: accessed via `children_by_field_name("decorator")`; `named_child(0)` gives the expression after `@`

## Smoke Testing

After `cargo test` passes, validate changes against a real codebase:

**Test project**: `/home/rrghost/SoftEng/fluxomind/fluxomind-platform` (Next.js, ~large)

Do NOT create temporary test files for smoke testing — run xray directly against real files.
See `AGENTS.md` for detailed agent workflow and example targets.
