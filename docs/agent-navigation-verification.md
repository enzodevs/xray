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
