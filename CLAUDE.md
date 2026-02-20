# xray

Structural code digest for TS/JS files using tree-sitter. ~10x compression.

## Build & Verify

```sh
cargo clippy --all-targets -- -D warnings   # lint (MUST pass before commit)
cargo test                                   # all tests
cargo build --release                        # optimized binary
```

## Architecture

| File | Role |
|------|------|
| `main.rs` | Entry point, arg parsing |
| `extract/mod.rs` | `extract_symbols` + symbol builders + integration tests |
| `extract/jsx.rs` | JSX detection + hierarchy tree (`returns_jsx`, `extract_jsx_components`) |
| `extract/hooks.rs` | React hooks extraction |
| `extract/calls.rs` | Call extraction |
| `extract/types.rs` | Type defs (interface, type alias, enum) |
| `extract/tests_block.rs` | Test block extraction (describe/it/test) |
| `model.rs` | Symbol, JsxNode, TypeDef, Hook, TestBlock, FileSymbols |
| `parser.rs` | tree-sitter language detection + parse |
| `output.rs` | FileDigest + Display formatting |
| `error.rs` | XrayError enum (thiserror) |
| `util.rs` | txt(), trim_quotes(), compress_members(), is_noise() |

## Conventions

- **Rust practices**: See [docs/rust-practices.md](docs/rust-practices.md) — MANDATORY
- **Lints**: `clippy::all = deny`, `clippy::pedantic = warn` (see Cargo.toml)
- **Errors**: `thiserror` only. No `unwrap()`/`expect()` outside tests
- **Tests**: Descriptive names, one behavior per test. Use `parse_ts()` / `parse_tsx()` helpers
- **tree-sitter v0.23 gotchas**:
  - Enum members are `property_identifier`, not `enum_member`
  - JSX tag name: `child_by_field_name("name")`, NOT `child(0)`
  - Function expressions in args: `function_expression` node kind
