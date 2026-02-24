# xray — Agent Guide

See `CLAUDE.md` for architecture, conventions, and tree-sitter gotchas.

## Build & Verify

```sh
cargo clippy --all-targets -- -D warnings   # lint (MUST pass before commit)
cargo test                                   # unit tests (MUST pass)
cargo build --release                        # optimized binary
```

## Smoke Testing — Real Project

**NEVER create temporary .ts/.tsx files for smoke testing.** Test directly against a real codebase.

**Test project**: `/home/rrghost/SoftEng/fluxomind/fluxomind-platform`

Alias for commands below:
```sh
FP="/home/rrghost/SoftEng/fluxomind/fluxomind-platform"
```

### Finding test targets

```sh
# TSX components (JSX, hooks, calls)
fd -e tsx -t f -E node_modules $FP/src/components | head -5

# Engine/service files (exports, calls, imports)
fd -e ts -t f -E node_modules $FP/src/engine | head -5

# Type definition files (interfaces, types, enums)
fd -e ts -t f -E node_modules $FP/src/types | head -5

# Test files (describe/it blocks)
fd -e ts -t f -E node_modules $FP/tests | head -5

# Core library files (mixed: types + logic)
fd -e ts -t f -E node_modules $FP/src/lib | head -5

# SQL files (if sql extraction is relevant)
fd -e sql -t f -E node_modules $FP/database | head -5
```

### Test matrix by mode

After building (`cargo build --release`), test each mode that your change could affect:

| Mode | Command | Validates |
|------|---------|-----------|
| `--no-follow` | `xray --no-follow $FP/src/components/SomeComponent.tsx` | Symbol extraction, JSX, hooks, types |
| default (follow) | `xray $FP/src/engine/SomeService.ts` | Import resolution, tree building, noise filtering |
| `--trace` | `xray --trace $FP/src/engine/SomeService.ts` | Cross-file call graph, call resolution |
| `--trace -s NAME` | `xray --trace -s functionName $FP/src/engine/SomeService.ts` | Targeted symbol tracing |
| `--who` | `xray --who $FP/src/lib/SomeUtil.ts` | Reverse dependency scanning |
| `--trace --lsp` | `xray --trace --lsp $FP/src/engine/SomeService.ts` | LSP integration (requires typescript-language-server) |
| multi-file | `xray $FP/src/engine/A.ts $FP/src/engine/B.ts` | Concatenated output |

### What to verify in output

1. **No panics or crashes** — exit code 0
2. **Symbols present** — exported functions/classes/types from the file appear in output
3. **Imports resolve** — follow tree shows resolved dependencies (not all `[unresolved]`)
4. **JSX detected** — TSX files show component hierarchy under `jsx:`
5. **Hooks listed** — TSX files with hooks show them under `hooks:`
6. **Types extracted** — files with interfaces/types show them in output
7. **Noise filtering** — `ui/` and `components/ui/` paths collapsed (unless `--all`)
8. **SQL refs** — SQL files show `refs:` (not `calls:`) with `target:`, `source:`, `join:`, `cte:`, `fn:` prefixes
9. **SQL follow** — SQL include chains (`\i`, `SOURCE`, `@@`) resolve and build tree

### Workflow

1. `cargo test` — must pass
2. `cargo clippy --all-targets -- -D warnings` — must pass
3. `cargo build --release` — build
4. Pick 2-3 diverse files from fluxomind-platform (component + service + types)
5. Run the relevant modes from the test matrix above
6. Verify output makes sense (no regressions, new feature visible)
