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

## Licensed PHP navigation and direct type hierarchy

The npm registry's `latest` tag and the installed Intelephense package both
reported **1.18.5**. No downgrade to an older beta/next tag or unnecessary
reinstallation was performed. The licence was configured in the documented
home-directory file with directory/file permissions **0700/0600**, without
placing key contents in source, CLI arguments, documentation or session identity.
A real initialization advertised implementation, type-definition, declaration
and type-hierarchy capabilities. Xray supplies the licence **path**, not its
contents, and fingerprints only its path/metadata for worker reuse.

The first licensed query uncovered a server/client lifecycle gap: Intelephense
sent `window/showMessageRequest` after activation; the old unsupported-method
response caused it to exit, surfacing as `Broken pipe`. Xray now dismisses that
headless prompt with the protocol-defined null response, never selects an action
or logs its potentially sensitive message. A mock-server regression covers it.

Verification on the same Redapro checkout, without changing application files:

- `cargo test`: **428 unit tests passed, 1 ignored; 15 CLI tests passed**.
- `cargo clippy --all-targets -- -D warnings` and `cargo build --release`: passed.
  An intermediate identical-match-arms lint was fixed by merging null responses.
- Tests cover premium capability negotiation and explicit missing-capability
  failures, optional path-only licensing, session identity after activation and
  replacement, LSP location links, hierarchy prepare/resolve with all roots and
  opaque data preserved, deduplication, null versus malformed replies, invalid
  ranges, the 64-root bound, CLI validation and exact bounded-output recovery.
- Every observed navigation range was checked against real source bytes. The
  validation harness initially assumed all ranges were single-line identifiers;
  Intelephense returns a whole class range for the type-definition case. The
  harness was corrected to validate that declaration range, not narrow or alter
  the product's exact server output.

| Query | Wall time | Result |
|---|---:|---|
| `--implementations --at 14:11 app/Contracts/Payments/PaymentGateway.php`, cold | **16.452 s** | 3 classes: Disabled, Fake and Pagarme payment gateways |
| Same query, warm | **0.127 s** | Same 3 exact locations |
| `--implementations --at 16:21` on the contract's `create` method | **0.135 s** | 3 concrete `create` methods |
| `--subtypes --at 14:11` on the contract | **0.121 s** | Same 3 direct implementing types |
| `--supertypes --at 27:7 app/Models/User.php` | **0.160 s** | 7 direct relations, including parent, interface and used traits |
| `--type-definition --at 35:19 app/Http/Controllers/School/AgentMessageController.php` | **0.125 s** | `SendInstitutionalAgentMessage.php:67:1-1673:2`, the whole inferred-type declaration |
| `--declaration --at 35:33` on that controller | **0.124 s** | `SendInstitutionalAgentMessage.php:100:21-100:27` |
| Existing `--definition` at the same position | **0.123 s** | Same method location |
| Existing `--references --at 100:21` on that method | **2.197 s** | 32 uses |
| `--implementations --at 5:11 vendor/laravel/framework/src/Illuminate/Contracts/Queue/ShouldQueue.php --format json --max-bytes 1024` | **0.124 s** | 48 implementations; bounded JSON and complete private artifact validated |

All these navigation results were `complete`/`ready` in their declared static
scope. Hierarchy modes are one hop, not transitive traversal. Traits reported as
supertypes are server semantics, not invented inheritance edges. A paid licence
does not resolve dynamic container selection or guarantee Laravel magic.

A separate real-server test with an isolated empty `HOME` (no licence, without
moving or editing the actual key) verified both paths: implementations failed
explicitly with a premium-licence/capability error (**14.427 s**), while the free
definition query still returned the exact method (**12.747 s**).

The final release was installed atomically at `/home/rrghost/.local/bin/xray`.
The PATH-resolved executable returned all three payment gateway implementations
with exact ranges in **13.08 s** using its separate cold worker. Build and installed
SHA-256 match: `6449b0b29a8563f7ddc5443902d8b1478b62c4cdf974dde18a89f978924b7d0c`.

Local temporary evidence: `/tmp/xray-premium-{tests,clippy,build}.log`,
`/tmp/xray-premium-validation/`, and `/tmp/xray-premium-verify.py`.

## Laravel conventions, Inertia links and warm reverse scans

Baseline: `850aece`. All five additions were validated read-only on
`/home/rrghost/SoftEng/redapro/redapro-whitelabel`. Existing application changes
were left untouched; no PHP/Artisan/Composer or application JavaScript was
executed. The configured Fluxomind directory was unavailable. Temporary sources
were used only by unit tests, not by real-project smoke checks.

- `cargo test`: **457 passed, 1 ignored** unit tests; **15 passed** CLI tests.
- `cargo clippy --all-targets -- -D warnings`, release build and diff checks passed.
- New tests cover Inertia facade aliases/literals, namespace scope, dynamic
  noncoverage, ambiguity, missing pages and path/symlink escape rejection.
- Reverse-cache tests cover cold/warm facts, source edits/addition/removal,
  ignore changes, Composer/TS config changes, Inertia target ambiguity, parser
  recovery, read errors, incompatible/corrupt snapshots and unsafe cache paths.
- Convention tests cover scopes and attributed scopes, aliases, comments and
  strings, static receiver hints, handler construction/dispatch, listener event
  unions and commented parameters. Exact and supplementary result counts remain
  distinct, including through bounded JSON artifact recovery.
- Focused review identified per-file extraction limits, oversized supplemental
  transport data and comments before listener parameters as gaps. Fixes stream
  candidates with count/byte/deadline checks, use parser cancellation, bound
  receiver-chain inspection, preserve exact results if supplements exceed the
  worker envelope, and skip comments when identifying the event parameter.
  Regression tests cover the fixes; intermediate compile/lint/format issues were
  resolved before the final successful checks.

### Real outcomes and measurements

Cold/warm `--who` comparisons used isolated disposable cache directories rather
than deleting the user's cache. Complete JSON (recovering overflow when needed)
was **identical** across each cold/warm pair, including partial/recovery evidence.

| Case | First query | Warm query | Result |
|---|---:|---:|---|
| `--who app/Models/User.php` | **3.921 s** | **0.300 s** | 507 dependents; 3,071 files scanned; partial with 2 named recovery paths |
| `--who resources/js/pages/Dashboard.vue` | **6.788 s** | **0.631 s** | Controller now found; 5,160 files scanned; partial with 6 named recovery paths |
| Dashboard controller outline | **0.004 s** | — | Includes `inertia:Dashboard` |
| Dashboard controller follow | **0.033 s** | — | Conventional Vue page resolves; complete |
| Profile controller follow | **0.025 s** | — | `inertia:settings/Profile` resolves; complete |

The six whole-workspace recovery paths are now present in text and JSON rather
than only counted. They include two PHP inputs, two SQL schema files and two Vue
components. The paths were checked locally; parser recovery is not a claim that
those files fail their native language/compiler. This change names the remaining
coverage limitation rather than hiding it or changing partial to complete.

| Reference query on a declaration | Time | Exact LSP references | Separate convention candidates |
|---|---:|---:|---|
| Legacy `scopeForSchool` on the message model; cold LSP worker | **15.341 s** | 0 | 254: **6 same-class literal receiver hints**, 248 unverified name matches |
| Attributed `#[Scope]` method | **0.200 s** | 0 | 2 name matches |
| Communication Job `handle` | **1.975 s** | 4 | 4 constructions, explicitly not claimed as dispatch |
| Listener `handle` | **1.989 s** | 0 | 3 event-dispatch candidates |
| Trail-generation Job `handle` | **2.326 s** | 0 | 7 candidates, including **2 static dispatch helpers** |

All candidate ranges were checked against the actual source bytes. The first
four reference queries also exercised a **1,024-byte JSON budget** and exact
private artifact recovery. The premium implementation query still returned the
three payment gateway classes (**0.118 s**); ordinary non-conventional queries
have no candidate supplement. These observations do not validate receiver types,
listener registration, dynamic bindings or runtime dispatch.

The global binary at `/home/rrghost/.local/bin/xray` was atomically replaced with
the verified release. SHA-256 matches:
`0e5f169007889ae43a83c0c1fd42555858d347cc4b9fd2a6dcc5307c00c60f48`.
A global-PATH `--who` smoke check found the Dashboard controller and retained all
six recovery paths (**6.92 s** on its first reverse-cache pass, **0.651 s** warm,
identical JSON). A global-PATH Job-reference query also passed with 0 exact
references and 7 separately labelled candidates (**15.119 s**, cold LSP worker).

Local temporary evidence: `/tmp/xray-laravel-{tests,clippy,build,format}.log`,
`/tmp/xray-laravel-validation/`, and `/tmp/xray-laravel-verify.py`. The full result
artifacts and private application source are not repository fixtures. See
[the Laravel navigation contract](laravel-navigation.md) for precise coverage.
