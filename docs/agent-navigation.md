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
# Intelephense premium (positions below are illustrative):
xray --implementations --at 14:11 app/Contracts/Payments/PaymentGateway.php
xray --type-definition --at 35:20 app/Http/Controllers/School/AgentMessageController.php
xray --declaration --at 35:34 app/Http/Controllers/School/AgentMessageController.php
xray --supertypes --at 27:7 app/Models/User.php
xray --subtypes --at 14:11 --format json app/Contracts/Payments/PaymentGateway.php
```

Positions are **one-based lines and UTF-8 byte columns** (as in `rg --column`),
not visual columns or LSP's zero-based UTF-16 coordinates. Place the position
on the exact identifier: a named argument and a method on the same line may
share a name but identify different symbols. Xray converts at the LSP boundary.
Returned end positions are exclusive. Links and hierarchy items use the server's
selection range; plain LSP locations preserve the server's range, which may span
a whole declaration (observed for Intelephense type definitions). Xray does not
invent a narrower identifier range. Reference queries ask the server to exclude
declarations; exact duplicate locations are removed and results are sorted.

Position-based navigation currently supports **PHP only**, using an installed
Intelephense server. Definition and references are free features. Premium adds:

| Mode | Static question |
|---|---|
| `--implementations` | Which concrete implementations does the server resolve for this contract/method? |
| `--type-definition` | Where is the inferred type of this value declared? |
| `--declaration` | Where is this symbol declared (as distinguished by the server from its definition)? |
| `--supertypes` | Which immediate parent types does this type have? |
| `--subtypes` | Which immediate child types extend/implement this type? |

Hierarchy queries perform one LSP prepare step followed by one relation request
for **each** prepared type, preserving opaque server data. They return the union
of direct related-type selection locations, sorted and deduplicated, not a
recursive graph or proof of transitive coverage. Intelephense may include used
traits among a PHP class's direct supertypes, alongside its parent and interfaces.
A union may prepare several
roots; xray never silently chooses only the first. If the symbol cannot be
resolved, a ready empty result remains distinct from a capability/transport error.

All modes retain exact byte ranges, text/JSON output, private overflow recovery
and server-static provenance. They are independent of legacy `--trace`; PHP
recursive call tracing remains unsupported. The AST digest's `calls` are lexical
hints, not resolved graph edges. Never interpret them as an execution sequence.
Premium does not prove which implementation Laravel's container selects at runtime.

### Persistent Intelephense licence

Install the current stable server with your package manager, for example
`npm install -g intelephense@latest` (or `bun add -g intelephense@latest`). Keep the
licence in the server's documented default file, `$HOME/intelephense/licence.txt`,
with directory mode 0700 and file mode 0600. Enter it using a local secure input
method; do not put the key in repository files, shell arguments, logs or prompts.

Xray detects that file and supplies **only its absolute path** via the documented
`initializationOptions.licenceKey`. It does not read or serialize the key itself.
Other clients can use the same documented file, or explicitly configure its path
if they override the server's defaults. The file is optional: free navigation
continues to work without it. Premium queries require the server's advertised
capability and fail explicitly with a licence hint if it is unavailable; they
never masquerade as an empty result or a structural approximation. Activation
may require the server to contact its licensing service. Headless activation
messages are dismissed with the LSP-defined null response, without selecting an
action, opening a browser or logging potentially sensitive message contents.

Xray remains read-only even with a premium licence: rename, code actions and
`workspace/applyEdit` are not enabled. See the [official initialization options](https://github.com/bmewburn/intelephense-docs/blob/master/installation.md#initialisation-options)
and the [LSP type hierarchy protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareTypeHierarchy).

## Language-server lifecycle and limits

On Unix, navigation automatically reuses a private worker for the selected
Composer workspace (or nearest Git root). Querying a declaration under `vendor`
retains the consuming application's workspace. Session identity includes the
Xray executable, server installation and licence-file metadata, so upgrading the
binary/server or activating, replacing or removing the licence does not reuse an
old capability negotiation. Only path/metadata enter the session fingerprint,
never key contents. `XRAY_INTELEPHENSE` is an optional operator-level executable override,
never a shell command.

Workers/index caches live under `/tmp/xray-lsp-UID` in owned 0700 directories;
sockets and lock files are 0600. Workers exit after 120 seconds idle. Startup
uses a pipe acknowledgment, and indexing uses observed server progress, not a
fixed sleep or caller polling. A query can take about a minute on a cold large
project. `readiness=ready` means initial server-reported indexing ended and the
query responded, not that the server understands every Laravel convention.

Between queries, a bounded metadata scan checks Git-visible PHP files and
Composer manifests (tracked plus non-ignored untracked files), plus the workspace's
`vendor` tree even when Git ignores it. Hidden directories, `node_modules` and
symlinks remain excluded. Directories without visible PHP/Composer files are
pruned before descent; tracked `.gitignore` placeholders do not force a walk of
runtime trees. Nested Git ignores, negations and tracked-file overrides follow
Git's own rules, including for Composer roots nested in a larger repository.
A broken Git index fails explicitly, never falling back to an unfiltered scan.
Without a Git repository, the bounded filesystem walk remains available.

The same scan generates literal Intelephense exclusions, replacing its default
exclusion list; vendor tests and nested vendor declarations are no longer indexed
and refreshed under different policies. Associations are restricted to PHP
(case-insensitive), rather than also indexing untracked-for-freshness `.phtml`
files. Ignored generated PHP is neither refreshed nor indexed. Composer manifests
are freshness inputs, not PHP source. Changes to included files or the computed
exclusions restart the server against its cache and await indexing again.
Editing files inside pruned runtime trees does not cause a restart. This is
**size/mtime freshness**, not a content-hash guarantee or continuous watcher;
changes preserving both metadata values can be missed until reindexing. Edits
can therefore cost a cached restart rather than an incremental query.

Client queue/query I/O has a 210-second deadline, individual LSP requests have
90-second deadlines. A whole hierarchy query (prepare, all direct relation
requests and any retry after observed indexing) shares one 90-second deadline,
and more than 64 prepared roots fails explicitly. Workspace scans retain a
15-second deadline, 500,000 visited
entries and 64 directory levels; pruned descendants consume none of that budget.
On Unix, shared Git enumeration also has a 15-second/16-MiB output bound.
Exhaustion is an explicit failure, never a complete-looking partial index.
Shutdown has a two-second deadline, documents are limited
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
