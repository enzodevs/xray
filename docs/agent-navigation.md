# Agent navigation and context-cost contract

Xray is a structural navigation layer, not a proof of runtime reachability.
Use semantic retrieval to find a responsibility, Xray to inspect structure and
resolve static locations, and source/tests to verify behavior.

## Queries

```sh
xray --no-follow app/Models/User.php
xray --follow --format json app/Jobs/SomeJob.php
xray --who --format json app/Models/User.php
xray --definition --at 42:19 app/Jobs/SomeJob.php
xray --references --at 16:7 --format json app/Models/User.php
```

Positions are **one-based lines and UTF-8 byte columns** (as in `rg --column`),
not visual columns or LSP's zero-based UTF-16 coordinates. Place the position
on the exact identifier: a named argument and a method on the same line may
share a name but identify different symbols. Xray converts at the LSP boundary.
Returned end positions are exclusive. Reference queries ask the server to exclude
declarations; exact duplicate locations are removed and results are sorted.

Definition/references currently support **PHP only**, using an installed
Intelephense server. These queries are independent of legacy `--trace`; PHP recursive
call tracing is still unsupported. The AST digest's `calls` are lexical hints,
not resolved graph edges. Never interpret them as an execution sequence.

## Language-server lifecycle and limits

On Unix, navigation automatically reuses a private worker for the selected
Composer workspace (or nearest Git root). Querying a declaration under `vendor`
retains the consuming application's workspace. Session identity includes the
Xray executable and server installation, so upgrading either does not reuse an
old process. `XRAY_INTELEPHENSE` is an optional operator-level executable override,
never a shell command.

Workers/index caches live under `/tmp/xray-lsp-UID` in owned 0700 directories;
sockets and lock files are 0600. Workers exit after 120 seconds idle. Startup
uses a pipe acknowledgment, and indexing uses observed server progress, not a
fixed sleep or caller polling. A query can take about a minute on a cold large
project. `readiness=ready` means initial server-reported indexing ended and the
query responded, not that the server understands every Laravel convention.

Intelephense keeps its documented default exclusions and additionally excludes
hidden directories: generated PHP caches must not dominate indexing or introduce
stale duplicate symbols. Vendor types remain available. Between queries, a
bounded metadata scan checks PHP files and Composer manifests (including vendor);
hidden directories, `node_modules` and symlinks are excluded.
Changes restart the server against its cache and await indexing again. This is
**size/mtime freshness**, not a content-hash guarantee or continuous watcher;
changes preserving both metadata values can be missed until reindexing. Edits
can therefore cost a cached restart rather than an incremental query.

Client queue/query I/O has a 210-second deadline, individual LSP requests have
90-second deadlines, shutdown has a two-second deadline, documents are limited
to 8 MiB and protocol messages to 16 MiB. Server edits are refused. Failures are
not empty results; broken transports are not reused. Legacy trace exposes LSP
fallback errors as bounded partial-result warnings and stops retrying a broken
transport for each call.

Non-Unix builds retain structural modes and report LSP/navigation as unsupported;
Windows has not been exercised in this change. PHP scopes, macros, dynamic
bindings and resource-route expansion are not promised. This is not a replacement
for Larastan/PHPStan or application tests.

## Output and recovery

- Default output is compact text; `--format json` provides compact structured
  JSON, not pretty-printed JSON. Without an explicit mode JSON selects digest.
- The **whole command's stdout is capped at 16,384 bytes** by default. Use
  `--max-bytes N` (1,024–1,048,576) only when the available context justifies it.
- Text retains a leading slice and an explicit omitted-byte count. JSON retains
  complete leading records, with omitted counts by JSON pointer. A single
  oversized value can require an artifact-only JSON response.
- Truncation always saves the exact full result to a private local artifact;
  its path is included. Unix directories/files use 0700/0600. If the artifact
  cannot be saved, the command fails instead of silently discarding evidence.
- Read/filter the artifact locally for missing evidence; do not paste it all
  back into model context. Artifacts may contain private source structure and
  are not automatically deleted while a caller may still need them. Remove
  them when the investigation is over; do not publish them.
- `status=complete` means the operation completed within its **declared static
  scope**, not that all runtime relationships were discovered. `partial`
  identifies parser recovery, incomplete scan, or bounded JSON output. Text
  overflow says `status=truncated`. JSON preserves the pre-truncation status.
- Failures are explicit and exit nonzero. An empty result has count zero and
  remains distinct from failure. Output-pipe closure does not cause a panic.

Digest JSON retains schema version 1 and the existing `file.content` structure;
new status/coverage fields are additive. Follow and reverse now also support
JSON. Trace remains text-only and rejects JSON explicitly.

## PHP coverage

PHP's `dependencies` label (the legacy JSON field is `imports`) includes static
class references as well as imports/includes. Same-namespace references do not
require an explicit `use`. Composer PSR-4 resolution is still structural:
Xray does not execute Composer, PHP, Artisan, or application bootstrap code.

Pest tests use the existing test-tree representation. Recognized Laravel
routes use `types` entries with `kind=route`, source spans and structural
metadata; they are not callable functions or runtime call edges. Middleware
lists are declared registrations, with `excluded_middleware` retained when
present, not a proof of the effective runtime middleware stack. Dynamic
registrations, conditional bindings, scopes, macros and implicit framework
behavior can remain unknown. Check source before making negative claims.

For a PHP target, `--who` scans Git-visible PHP source, not every language in
the repository. It returns **static dependents**, not callers, without repeating
the target's full digest. Counts include scanned files, failures, and parser
recovery. Dependencies/build directories and ignored files remain excluded.

## AXI/context-cost review

This interface follows the principles in Pi's `CONTEXT-HYGIENE.md`:

| Principle | Decision |
|---|---|
| Minimal interface | One CLI; two distinct navigation queries, one position and one output budget. No MCP schema catalog or separate tool per language. |
| Work outside context | Parsing, resolution, deduplication and output reduction happen locally. |
| Content first | Locations/counts precede caveats; reverse lookup no longer echoes an unrelated digest. |
| Deliberate bounds | Valid bounded JSON or text plus exact private overflow, never a silently clipped transcript. |
| Useful aggregates | Result/scan counts, readiness/provenance and explicit coverage avoid follow-up status calls. |
| Definitive states | Zero, partial, failed and complete-in-scope are distinguishable. |
| On-demand guidance | Details live here, not in every result or always-active instruction. |
| No polling ceremony | Query execution owns language-server readiness and timeout handling. |
| No clever encoding by default | Compact JSON and concise text; no TOON dependency without measured benefit. |

Regression coverage lives in `tests/agent_cli.rs` and `src/presentation.rs`.
The real-project stress runner now validates the **full overflow artifact**,
not just the visible subset, and reports displayed and full bytes separately.
Compression after truncation must never be presented as semantic efficiency.
