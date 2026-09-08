# Real-project stress test — 2026-09-08

## Verdict

**xray is useful as a fast structural navigation and local-dependency tool. It is not an authoritative call graph, public-API inventory, or completeness proof.** Use a digest to choose source ranges, follow to discover nearby dependencies, and Git-scoped reverse lookup to find static importers. Verify consequential conclusions against source.

This campaign reviewed and committed the existing PHP/Composer/reverse-scan WIP, found and fixed two additional defects, upgraded dependencies, and repeated the corpus after each change. It did not modify or execute application code in the tested projects. No extra language-server integrations were added: correctness and bounded execution are more valuable next steps than advertising additional installed servers as supported.

## Revisions and verification

- `3d41add`: reviewed WIP — PHP methods, Composer PSR-4 resolution, Git-aware reverse scans. 350 Rust tests, strict Clippy, release build, and real Laravel smoke tests passed.
- `0f214ed`: corrected Vue/Svelte component ranges and wide-tree nested-symbol traversal. 357 Rust tests and strict Clippy passed.
- `8420cc1`: dependency upgrades, including tree-sitter 0.27.0, Python grammar 0.25.0, Rust grammar 0.24.2. All 357 tests, strict Clippy, and release build passed again.
- Five Python unit tests cover the new runner's validation, error propagation, and deadline behavior.
- All 2,274 stdout artifacts were byte-identical before and after dependency upgrades on this corpus. This is strong sampled compatibility evidence, not proof for all grammar inputs.

The agent guide's Fluxomind path is absent on this machine; the requested projects replaced it for smoke testing.

## Corpus and method

Nine local project collections:

- `redapro/redapro-whitelabel`, `bun-projects/courtex`
- OSS snapshots: `microsoft/TypeScript`, `tokio-rs/tokio`, `encode/httpx`, `laravel/reverb`, `etcd-io/bbolt`, `sveltejs/kit`, `vueuse/vueuse`

**1,057 distinct file paths; 11 extensions spanning all nine supported ecosystems.** Collection directories can contain multiple downloaded versions; this is not nine uniform Git checkouts. Source snapshots include TypeScript 5.9.3/v6.0.3, HTTPX 0.28.1, Reverb 1.10.2, Kit 2.61.0, VueUse 12.8.2, and bbolt's cached `main` snapshot. These snapshots are not claimed to be current upstream releases.

The reproducible runner (`scripts/stress_xray.py`) selects up to 40 files per extension per collection: the three largest, then stable SHA-256-ranked paths. It uses Git tracked files where available and a filtered filesystem walk for snapshots. Dependency/build/hidden directories and symlinks are excluded. Compiler fixtures and generated files outside those directories deliberately remain eligible as adversarial inputs; the bulk corpus is **not exclusively handwritten production code**.

Every selected file gets text and JSON digests. Up to five per extension/collection also get depth-2 follow with resolution diagnostics. Each process has a 20-second process-group deadline. The runner records full stdout/stderr, command, cwd, elapsed time, source/output bytes, exit code, JSON errors, inventory, and binary hash. Checks include JSON schema version, nonempty symbol names, and inclusive export/internal spans within file bounds. It does not independently validate every symbol/type/reference or detect all parser recovery errors.

Separate source-led checks covered 24 first-party Redapro/Courtex files and 37 OSS files, including local imports, PHP visibility/method identity, JSX/hooks, returned handlers, Rust re-exports, and selected traces. Final workflow checks repeated TS static/LSP trace, Go LSP trace, PHP/TS reverse edges, `--all`, multi-file mode, and the expected non-Git reverse error.

## Final bulk results

**2,274/2,274 commands passed execution and basic JSON invariants: no timeouts, command errors, or invalid checked spans.** This is not a 100% semantic-accuracy score.

| Mode | Commands | Median | p95 | Maximum |
|---|---:|---:|---:|---:|
| JSON digest | 1,057 | 2.48 ms | 21.28 ms | 1.83 s |
| Text digest | 1,057 | 2.43 ms | 20.79 ms | 1.74 s |
| Follow, depth 2 | 160 | 9.63 ms | 329.79 ms | 1.72 s |

These are local release-build wall times across different files, including process startup, not repeated cold-cache benchmark distributions. Concurrent activity and caches can affect results.

Text digests reduced 70,089,944 source bytes to 7,409,407 output bytes (**9.46x aggregate byte compression**). JSON emitted 38,931,930 bytes (**1.80x**). These are not token measurements. Large comment/compiler fixtures heavily inflate aggregate compression; the source-reviewed private-project digest cohort was about **4.1x**, and the OSS cohort about **5.6x**. Follow output includes additional files, so source-only compression comparisons are misleading for that mode.

| Extension | Files | Text source/output ratio |
|---|---:|---:|
| Go | 40 | 5.61x |
| JS | 90 | 4.56x |
| Markdown | 261 | 26.76x |
| PHP | 80 | 6.00x |
| Python | 80 | 4.46x |
| Rust | 50 | 9.68x |
| SQL | 16 | 2.44x |
| Svelte | 80 | 3.43x |
| TS | 200 | 54.41x (strong fixture bias) |
| TSX | 80 | 3.20x |
| Vue | 80 | 6.55x |

## Defects fixed during testing

### 1. Wide AST traversal could take minutes

Reproduction: `xray --no-follow .../microsoft/TypeScript/5.9.3/tests/cases/fourslash/reallyLargeFile.ts` — 3,502,391 bytes, 583,711 lines, largely compiler-fixture comments.

The initial digest, JSON, and follow runs each exceeded 20 seconds. A separate digest run still had not completed after **90 seconds**, with roughly 147 MiB peak RSS. `extract_nested_functions` queried each node's function ancestors using `Node::parent()`, repeatedly paying tree-sitter parent-discovery costs on an extremely wide tree.

Carrying the existing boolean function-ancestor state during downward traversal preserves extraction policy without repeated parent lookup. The same real file then completed in **0.33 seconds**, around the same peak RSS: an observed lower-bound improvement above **270x**. A 10,000-comment unit regression also checks that sibling/nested function classification remains correct. No input-size refusal or partial-result shortcut was introduced.

### 2. Vue/Svelte components extended past EOF

The initial JSON checks found **104 invalid spans: 55 Vue and 49 Svelte**. Synthetic component end lines used the tree root's final row plus one, counting a phantom line after a trailing newline. For example, Redapro `resources/js/pages/auth/ForgotPassword.vue` has 68 logical lines but reported an end line of 69.

Synthetic component spans now use the same logical-line convention as file totals. Regression coverage includes trailing newline, unterminated final line, and empty input. All 104 cases pass on the repeated corpus.

## Remaining accuracy and reliability limits

1. **Confirmed false LSP call edge — high priority.** In Courtex `apps/desktop/ui/src/App.tsx`, `setShowWelcome` is a React state setter (line 52; calls around 90–92). Static trace treats it as external. `--trace --lsp -s App` maps its definition inside `App` to the enclosing component range, fabricating a local recursive call tree. `src/trace.rs::find_symbol_at_line` uses containment rather than establishing that the definition denotes that callable. Fix by preserving definition identity/position and distinguishing local bindings from enclosing callables, not by blindly accepting the enclosing range. This remains reproducible after dependency upgrades.
2. **Returned anonymous callables are not trace roots.** Redapro `ai-worker/src/processors/correctionProcessor.ts::createCorrectionProcessor` returns an async closure containing provider/adapter calls. Its factory trace says `(no calls)`. This limits navigation into the returned handler; it does not mean invoking the factory immediately executes those deferred calls. A future change should model the returned callable explicitly rather than attribute all deferred work to the factory.
3. **Rust API re-export incompleteness.** Tokio `tokio/src/fs/mod.rs` exposes functions through `pub use`; the digest represents these as imports rather than a complete exported API. Do not use digest exports alone to judge crate/module public reachability.
4. **TS-family digests are not complete dependency inventories.** External/builtin import filtering is intentional in the extractor. VueUse's workspace import `@vueuse/core` can be absent from the digest's imports even while follow resolves it. Separate value/type imports can also duplicate displayed paths. Treat this as compact-navigation policy with limitations, not proof of an absent dependency.
5. **Confirmed SQL false dependency.** Redapro `lead-intelligence/src/redapro_leads/storage/schema.sql:42` declares a column as `source VARCHAR NOT NULL,`; the digest incorrectly emits `imports: VARCHAR`. `src/extract/sql.rs::extract_sources_only` interprets a trimmed line beginning with `source` as an include command without enough statement context. Table names and `refs: target:...` were correctly retained in this example, but SQL include discovery needs statement-aware discrimination. Final artifact: `00268.stdout`.
6. **LSP has no internal request/shutdown deadlines.** Source inspection also found byte offsets sent as LSP character positions without UTF-16 conversion, and manually constructed file URIs without percent encoding. These are reliability risks, not all experimentally reproduced in this campaign. Add bounded I/O, proper position/URI handling, and explicit fallback provenance before relying on unattended LSP traces.
7. **Reverse lookup requires Git metadata.** Actual `--who` on the HTTPX snapshot exits 1 with `not inside a git repository`. It does not silently succeed with no importers. An initial auxiliary test accidentally omitted `--who`; its alleged JS/Python reverse failures were discarded after command-log review. Confirmed final reverse checks in real Git projects correctly found Courtex `App -> DailyView` and Redapro `BroadcastCommunicationMessage -> BuildCommunicationInbox`.

These remaining items are documented rather than hidden behind successful exit codes. Broad trace-model/LSP redesign was not bundled into the dependency or extraction fixes.

## Language servers and dependencies

- Installed and exercised: `typescript-language-server` 5.1.3 and `gopls` 0.22.0. Final targeted TS and Go LSP checks completed in approximately 3.05 s and 6.13 s respectively; successful completion does not invalidate the TS false-edge finding.
- Svelte LSP is integrated but `svelte-language-server` is absent locally, so no genuine Svelte LSP success is claimed.
- Rust analyzer, Pyright, Intelephense, and Vue language server executables are present, but **xray does not integrate them**. Python/PHP/Rust cross-file trace is unsupported; Vue uses static trace resolution.
- All **14 direct plus dev dependency entries** match crates.io's latest stable releases at final verification (`https://crates.io/api/v1/crates/<name>`). Besides parser/grammar upgrades, serde is 1.0.229, serde_json 1.0.151, thiserror 2.0.20, and tempfile 3.27.0. Cargo also refreshed compatible transitive dependencies. This is a version check, not a security audit or claim that every transitive crate is globally newest.

## Reproduce and inspect evidence

```sh
cargo test
cargo clippy --all-targets -- -D warnings
cargo build --release
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s scripts -p 'test_*.py'
python3 scripts/stress_xray.py --binary target/release/xray \
  --output /tmp/xray-stress-new --per-language 40 /path/to/project /path/to/snapshot
```

Use a new/empty output directory. The tool is read-only toward input repositories but its output may contain private source structure; retain logs locally, not in public commits.

Local campaign artifacts (ephemeral, not checked in):

- `/tmp/xray-stress-final/`: final metadata, inventory, 2,274-command results, summary, full outputs/errors.
- `/tmp/xray-stress-bulk-v2/`: initial 107 failures (104 span violations and three timeouts).
- `/tmp/xray-stress-fixed/` and `/tmp/xray-stress-latest/`: before/after dependency comparison.
- `/tmp/xray-final-workflows/`: final reverse/static/LSP/multi-file/`--all` checks.
- `/tmp/xray-stress-private/`, `/tmp/xray-stress-oss/`: auxiliary source-review evidence. Their initial findings are superseded by this adjudicated report, especially the invalid reverse test and compression comparisons mixing follow/digest modes.
- `/tmp/xray-final-dependencies.json`: final primary-registry version comparison.

**Recommended next investment:** trustworthy and bounded LSP edges plus SQL include discrimination first; then returned-callable and Rust re-export modeling. More installed servers alone will not address those limitations.
