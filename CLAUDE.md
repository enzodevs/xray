# xray

Structural code digest for TS/JS files using tree-sitter. ~10x compression.

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
xray --all <file>                # follow without noise filtering
xray --who <file>                # reverse: show who imports this file
xray --no-follow <file>          # plain: single file digest only
```

## Architecture

| File | Role |
|------|------|
| `main.rs` | CLI arg parsing + dispatch |
| `follow.rs` | `--follow` (default): tree building, noise detection, rendering |
| `reverse.rs` | `--who`: project scanning, find importers |
| `resolve.rs` | Import resolution: `PathConfig`, `resolve_import`, `load_path_config` |
| `output.rs` | `FileDigest::from_path`, `summarize()`, `FileSummary`, Display impls |
| `model.rs` | Symbol, JsxNode, TypeDef, Hook, TestBlock, FileSummary, FileSymbols |
| `extract/mod.rs` | `extract_symbols` + `extract_sources_only` + integration tests |
| `extract/jsx.rs` | JSX detection + hierarchy tree (`returns_jsx`, `extract_jsx_components`) |
| `extract/hooks.rs` | React hooks extraction |
| `extract/calls.rs` | Call extraction |
| `extract/decorators.rs` | Decorator extraction (`@Component`, `@Input`, etc.) |
| `extract/types.rs` | Type defs (interface, type alias, enum) |
| `extract/tests_block.rs` | Test block extraction (describe/it/test) |
| `parser.rs` | tree-sitter language detection + parse |
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
