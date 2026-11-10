# Agent-navigation verification (0.3.0)

Baseline: `fc30354ca1ea39c998afa37e2f33144fa0e96370` (0.2.0).
Platform: Linux. PHP server: Intelephense **1.18.5** (free definition/reference features).
The configured Fluxomind smoke directory was unavailable; existing Redapro files
were used instead, plus an existing Courtex TSX component. No application,
Artisan, migrations or production tests were executed; application files were
not edited. No temporary TS/TSX smoke files were created.

## Checks

- `cargo test`: **403 unit + 7 CLI integration tests passed**.
- `cargo clippy --all-targets -- -D warnings`: passed.
- `cargo build --release`: passed.
- Python stress-harness tests: **7 passed**.
- Real corpus: **410 commands, zero failures**, over 190 existing files;
  190 text outlines, 190 JSON outlines, 30 follow queries. PHP coverage included
  40 files. Validators recover overflow artifacts before checking schema and
  source ranges, rather than mistaking a short preview for the full result.
- Focused real-project checks: **19 passed**, including PHP definitions,
  references, vendor declarations in the consuming workspace, same-namespace
  dependencies, route/Pest extraction, unsupported PHP tracing, multi-file
  output, bounded JSON and TS/TSX tracing with and without LSP.
- A frozen-snapshot independent review covered 24/29 changed files, with 24
  focused tests and an independent worker reuse/edit/deletion probe. No material
  findings in that scope. Five PHP-extraction files were inspected by the parent
  and covered by regressions. Later LSP refinements were parent-tested; this is
  not an exhaustive independent review or a cross-platform certification.
- `git diff --check`: passed. Touched source files were formatted; repository-wide
  formatting still has pre-existing differences in untouched Svelte/Vue tests.

## Observed outcomes

| Case | Result |
|---|---|
| `routes/settings.php` | 13 route declarations, including inherited middleware |
| `tests/Feature/CommunicationEligibilityTest.php` | 11 Pest blocks |
| `--who` on `CommunicationMessage.php` | 43 static dependent files; all six previously missing same-namespace model consumers present |
| `$buildInbox->totalUnreadCount` | Correct definition at `BuildCommunicationInbox.php:66` |
| References to that method | 6 server-reported uses; declaration excluded |
| References to `CommunicationMessage` | 191 locations, including the missing model consumers |
| Vendor interface definition, then references from its declaration | Resolved into Laravel vendor source and back to the consuming application's job |
| Eloquent `forSchool` scope | `status=empty`, zero locations; remains unsupported by this server inference |
| TSX `setShowWelcome` | Not falsely linked to the enclosing `App` component |
| Ambiguous TS builtin overload | Explicit partial-trace warning, rather than silently choosing one definition |

The 31,733-byte class-reference JSON result was capped to **1,015 bytes** with
`--max-bytes 1024`; the private artifact retained all 191 locations and was
successfully recovered and validated.

## Measurements (local observations, not service guarantees)

- Cold PHP definition: **13.14 s** after excluding hidden directories. The same
  implementation before that exclusion took **59.15 s**; the workspace contained
  43,169 PHP files under `.cache`, unrelated to the intended source index.
  Vendor declarations remain available.
- Three subsequent definition queries: **0.292–0.294 s**. Other queries vary:
  method references took **4.52 s** in the final run (0.31 s in the preceding
  run), class references **0.55 s**, vendor queries **0.29–0.35 s**. Metadata
  changes can trigger cached server restarts; this is not an incremental watcher.
- PHP outline sample: text median **2.87 ms**, p95 **24.55 ms**; JSON median
  **2.76 ms**, p95 **24.69 ms**. Follow sample (five PHP files): median
  **146 ms**, maximum **721 ms**.
- `--who` on the message model: **3.45 s**, versus **5.09 s** in the initial
  baseline measurement. It now restricts PHP-target scanning to PHP files and
  omits the redundant target digest; it is still a project scan, not an index.
- Complete JSON across the 190-file sample: **835,794 bytes**; displayed JSON:
  **688,345 bytes** with seven overflow artifacts. Complete text: **330,145
  bytes**; displayed text: **307,032 bytes** with two artifacts. Compression
  statistics must use complete output, not count omitted bytes as compression.
- PHP source-to-complete-outline ratio: **6.59× text / 2.47× JSON**. Extraction
  now contains additional real dependencies, routes and tests; greater semantic
  coverage can legitimately make the full outline larger than before.

## Evidence and reproduction

Release binary SHA-256:
`81417d775cca22a5796f1548e3e9510ba9adfe91af68ab421c0d86e334533b7c`.

Local, temporary evidence (not repository fixtures or durable APIs):

- `/tmp/xray-axi-{tests,clippy,build,python}-final.log`
- `/tmp/xray-axi-smoke-final.log`, `/tmp/xray-axi-smoke-8ca2jw4t/`
- `/tmp/xray-axi-corpus-JAqfK6/`
- `/tmp/evidence-review-wi3ee2jf/`

```sh
cargo test
cargo clippy --all-targets -- -D warnings
cargo build --release
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s scripts -p 'test_*.py'
python3 scripts/stress_xray.py --binary target/release/xray \
  --output /tmp/xray-corpus-review --per-language 40 /path/to/real/project
```

See [the navigation contract](agent-navigation.md) for position units, freshness,
output recovery and limitations. PHP tracing, Eloquent magic, macros, runtime
container bindings and resource-route expansion are not claimed complete.

## PHP workspace scan fix: redapro-whitelabel

Validated the fix described in [php-workspace-scan-gap.md](php-workspace-scan-gap.md)
on `/home/rrghost/SoftEng/redapro/redapro-whitelabel`, using the release build.
No application files were changed and no application/Artisan code was executed.
The configured Fluxomind directory remains unavailable on this machine.

- `cargo test`: **417 passed, 1 ignored** unit tests; **13 passed** CLI tests.
- `cargo clippy --all-targets -- -D warnings`: passed after fixing a doc-comment
  backtick warning in the first run.
- `cargo build --release` and `git diff --check`: passed.
- Regression tests use a 12-entry cap with 100 ignored runtime files, ignored
  generated PHP, a tracked `.gitignore` placeholder and retained vendor PHP.
  Editing generated PHP leaves the snapshot unchanged; exceeding a smaller cap
  fails explicitly. Nested ignores, tracked ignored files, negations, symlinks,
  glob escaping, nested Composer roots and broken indexes are also covered.
- The existing non-Git freshness and vendor-workspace-root tests still pass.

| Release query | Wall time | Observed result |
|---|---:|---|
| `--references --at 100:21 app/Actions/InstitutionalAgents/SendInstitutionalAgentMessage.php`, cold worker/index | **20.550 s** | 32 references, complete, ready |
| Same query, warm session | **1.991 s** | Same 32 references, complete, ready |
| `--definition --at 35:34 app/Http/Controllers/School/AgentMessageController.php` | **0.135 s** | `SendInstitutionalAgentMessage.php:100:21-100:27` |
| Same definition, repeated | **0.126 s** | Same exact location |
| `--definition --at 10:47 app/Jobs/RecoverTrailChapterJob.php` | **0.122 s** | Vendor `ShouldQueue.php:5:11-5:22` |
| `--references --at 5:11 vendor/laravel/framework/src/Illuminate/Contracts/Queue/ShouldQueue.php` | **0.216 s** | 106 references, including the consuming application's jobs |
| `--no-follow` on the original target | **0.025 s** | PHP outline, 1,673 lines |
| Default follow on the original target | **0.166 s** | Complete; 48 resolved, 20 external, 0 unresolved |
| `--who` on the original target | **3.842 s** | 14 dependents; explicitly partial, 3,070 scanned, 2 parser-recovered files |

All navigation locations above were checked against the actual source byte
ranges: method ranges contain `handle`; vendor definition/reference ranges end
in `ShouldQueue`. The first vendor validation assertion had an off-by-one expected
column (46 instead of 47); inspection and the corrected range check passed,
without a product-code change. No reference output was truncated.
The 90-second LSP and 210-second queue deadlines were **not increased**.
These measurements are local observations, not runtime-completeness guarantees.

The release binary was installed atomically at `/home/rrghost/.local/bin/xray`,
the executable resolved by `command -v xray`. Its SHA-256 matches the build:
`c051ad35db722e74bc539f5f6befe611bc47e35e7dd097f9b1fc9edc2241afa0`.
A definition query through that global executable also passed (**13.19 s** with
its separate cold worker/index), returning the exact method location above.

Local temporary evidence: `/tmp/xray-php-{tests,clippy,build}.log` and
`/tmp/xray-php-validation/` (query output, timings and measurements JSON).
