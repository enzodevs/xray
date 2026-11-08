# Navigation fidelity follow-up — 2026-11-07

This change completes the Vue-LSP/trace-output WIP and addresses concrete
structural-navigation gaps. It does not claim complete runtime call graphs or
add trace support for Python, PHP, or Rust.

## Changes and regression evidence

- **Vue/Svelte evidence retention:** remove extraction-time cuts of render trees
  and template calls. The existing presentation budget owns truncation. CLI tests
  preserve 80 root components, their nested components, and 24 calls in both
  ecosystems' underlying model; JSON overflow recovers the exact full digest.
- **TS/Vue semantic startup:** `typescript-language-server` normally permits
  syntax-only definition replies while its semantic project loads. Batch queries
  now select `tsserver.useSyntaxServer=never`; the existing transport deadlines
  still apply. The Vue plugin is discovered through the installed language-server
  package, with a validated directory override.
- **Real LSP regression:** an opt-in test exercises `.vue -> .ts` and `.ts -> .vue`
  definitions on cold servers, then checks a member-call trace into the TS method.
  Tested with typescript-language-server 6.0.2 and Vue language-server/plugin
  3.3.13. No guessed indexing sleeps are used.
- **Definition is not implementation:** a binding/type definition without a
  matching callable is labeled `definition only; callable target unresolved`.
  Keep its file/line without classifying it as external or traversing an enclosing
  function. A real destructured composable callback still returns only its local
  binding from the server; an implementation request returns no locations. This
  remains unresolved, explicitly, rather than becoming a fabricated call edge.
- **Inherited TS paths:** replace the handwritten JSONC/alias machinery with a
  thin adapter over Oxc Resolver 11.24.3. Tests cover origin-relative paths,
  Unicode, comments/trailing commas, relative and package `extends`, arrays,
  overrides, wildcard suffixes, missing/cyclic configurations, and each followed
  file's nearest config. External npm packages remain outside ordinary follow.
  The conventional `@/ -> src/` fallback remains when no paths/base URL are set.
  This dependency raises the minimum Rust version to 1.95.
- **Go packages:** follow expands every non-test source file in an imported
  package, excluding Go-ignored dot/underscore names. It includes platform/build
  variants rather than claiming to evaluate Go's active build graph. Reverse
  lookup already matched package directories; a CLI regression protects lookup
  of a nonfirst package file. A real bbolt follow returned 43 dependency files,
  including 20 from `internal/common`.
- **SQL includes:** a small client-command scanner tracks statement boundaries,
  nested comments, strings and dollar-quoted bodies. Columns named `source` no
  longer become includes. Regressions also retain Unicode/quoted include paths
  and trailing multiline comments. This is not a full SQL client interpreter.
- **Trace output:** emit the entry header and requested tree without formatting
  an unused outline. Preserve parser-recovery warnings and guard lexical call
  position ranges. Non-TS traces do not read unrelated tsconfig files.

## Verification

- `cargo test`: 414 unit tests and 13 CLI integration tests passed. One opt-in
  server test is ignored by default and passed when explicitly selected.
- `cargo clippy --all-targets -- -D warnings`: passed.
- `cargo fmt --check`, `git diff --check`: passed.
- `cargo build --release`: passed.
- Python stress-harness tests: 7 passed.
- Real-project sampling: 134 files, 331 commands (134 JSON digests, 134 text
  digests, 63 depth-2 follows), zero command/time-budget/invariant failures.
  Corpus: a local Vue/PHP/TS application, bbolt, and SvelteKit snapshots.
  Overflow artifacts are read and validated by the harness, not ignored.
- Focused release smoke checks: Vue digest/LSP trace, TS follow/trace/reverse,
  types, Go package follow, Svelte digest, SQL digest, and multi-file output.
  Reverse scanning correctly reported partial coverage for parser recovery in
  existing application sources; successful execution does not erase that status.

The configured Fluxomind smoke checkout was unavailable; the above real projects
were used instead. Svelte LSP was not exercised because its optional server is
not installed. No application code was executed or changed. Checked-in minimal
Vue/TS fixtures are protocol regressions, separate from real-project smokes.

Reproduce the server check:

```sh
cargo test live_vue_lsp_resolves_both_directions_on_cold_start -- --ignored
```

Reproduce sampling with an empty/private output directory:

```sh
python3 scripts/stress_xray.py --binary target/release/xray \
  --output /tmp/xray-gap-verification --per-language 12 \
  /path/to/application /path/to/bbolt /path/to/sveltekit
```

Passing this sample is execution/recovery evidence, not a semantic-accuracy
percentage. Returned callables, framework magic, package/workspace resolution
outside each backend's declared scope, and unsupported language modes remain
limits; see the README and agent-navigation contract.
