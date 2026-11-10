# Laravel navigation: exact locations, conventions and reverse-cache scope

Xray does not execute Laravel, PHP, Composer, Artisan or JavaScript configuration.
These additions make static navigation more useful without turning conventional
relationships into exact LSP references or claiming runtime completeness.

## Inertia controller → Vue page

A literal, namespace-resolved `Inertia\Inertia::render('school/Home')` contributes
an `inertia:school/Home` dependency in the outline, follow diagnostics and reverse
lookup. Namespace-scoped aliases, fully qualified facade names, and named
`component:` arguments are supported.

Resolution uses the nearest Composer workspace and exactly one matching Vue file
under `resources/js/pages` or `resources/js/Pages`. A missing page, two matching
roots, or an unsafe path remains **unresolved**, not an external dependency.
Traversal and symlink escapes outside the page-root/workspace boundary are refused.

```sh
xray --no-follow app/Http/Controllers/DashboardController.php
xray --follow --format json app/Http/Controllers/DashboardController.php
xray --who resources/js/pages/Dashboard.vue
```

This is **conventional-path inference**, not custom-resolver evaluation. Custom
page roots/transforms, non-Vue pages, helpers such as `inertia()`, dynamic names,
concatenation, interpolation and escaped literals are not inferred. The follow
and reverse coverage labels make the convention explicit. A custom resolver can
make the conventional mapping inapplicable even when a conventional file exists;
verify that assumption in the application's frontend setup. The White project
uses the Inertia Vite plugin's conventional page roots without overriding them.

## References with separate Laravel candidates

Use `--references --at LINE:COL` **on the method identifier in its declaration**.
The normal `results`, `count`, `status`, readiness and `lsp:intelephense` provenance
still describe the exact server query. A new `convention_candidates` object has
its own `results`, `count`, `status`, coverage, scanned-file count, recovery paths
and warnings. It is null when the declaration does not match a supported
convention. Candidates never increase the exact-reference count, and exact
locations duplicated by a candidate are omitted from the supplementary list.

### Eloquent scopes

- Recognizes `scopeFoo` in model-like classes (direct Eloquent Model base or a
  `Models` namespace segment) and traits, plus methods carrying the resolved
  `Illuminate\Database\Eloquent\Attributes\Scope` attribute, including aliases.
- Looks for syntactic `->foo()`, `?->foo()` and `::foo()` calls. Comments, strings,
  free functions and dynamic method-name expressions are not matches.
- `scope-receiver-match` means a literal static chain root names the same class;
  these candidates are listed first. This is still not inferred return-type proof.
  Deep chains beyond 64 receiver hops retain name-only matching.
- `scope-name-match` may refer to another model or an unrelated method with the
  same name. Its receiver is unverified; inspect the source before using it as
  impact evidence. Literal receiver class names are included when available.

This is a naming/attribute convention, not complete Eloquent type analysis.
Inherited models outside the recognized contexts and calls through dynamic names
may be missed. Querying an arbitrary call site does not silently redirect the
query to a guessed scope declaration.

### Job and Listener `handle`

A public `handle` declaration in a `Jobs` namespace or a class directly declaring
`ShouldQueue`/`ShouldQueueAfterCommit` can expose job-convention candidates.
A `Listeners` namespace supplies listener conventions; the first parameter's
resolved event types (including unions) guide event candidates. Subsequent
injected-service parameters are not treated as events.

Candidates distinguish static dispatch helpers, chain setup, object construction
and `::class` references. Namespace aliases and fully qualified names are resolved
syntactically. For listeners, event dispatch/construction and event/listener class
references are useful leads, **not proof of registration or event discovery**.
Construction and class references may be factories, tests or assertions rather
than dispatch. Dynamic queue/event/container wiring is outside this scope.

All candidates use `syntax:laravel-convention-candidate` provenance and exact
source byte ranges. `0` exact references with positive candidate counts is an
intentional, useful result—not a claim that those candidates are exact callers.

### Bounds and failure isolation

Candidate scanning uses Git-visible, non-hidden PHP outside `vendor` and
`node_modules`, skips symlinks and reads at most 8 MiB per source. The scan has a
15-second deadline, 50,000-file/10,000-candidate bounds, a 512-KiB serialized
candidate budget and bounded diagnostic lists. Parsing supports cancellation;
AST traversal and candidate emission check budgets before accumulating more
results. Recovery, scan limits and errors remain explicit in the supplementary
status/warnings. Non-Git workspaces keep exact LSP navigation but cannot provide
this Git-scoped supplementary scan.

If supplementary data would exceed the worker's 16-MiB response envelope, it is
omitted with `status=partial`, an `omitted` count and a warning; exact LSP locations
are retained. An oversized exact-only result fails explicitly. Ordinary CLI
text/JSON output limits still preserve their complete delivered payload in a
private overflow artifact. No licence is needed for the convention layer itself;
the exact query still uses the installed Intelephense server.

## `--who`: named parser recovery

Text lists `parser_recovery: PATH` for each affected source, including the target
when appropriate. JSON adds `parser_recovery_paths`. For compatibility,
`parser_recovery_files` remains the count excluding the target and
`target_syntax_recovered` remains separate; the text header counts both.

A recovered file is named even if it did not become a dependent. Recovery may
hide an edge, so it keeps the scan partial rather than being ignored as unrelated.
These paths identify navigation-parser recovery, not necessarily invalid PHP,
TypeScript or Vue according to their own compilers.

## `--who`: disposable parse-fact cache

Warm queries reuse per-file diagnostic specifiers, SQL calls, line counts and
recovery flags. They **do not cache the resolved dependency graph**. Every query
reenumerates Git-visible files and resolves edges with fresh per-directory
Composer/TypeScript configuration. Additions, removals, Git ignore changes,
configuration changes and Inertia page availability therefore take effect without
manually rebuilding a graph.

Source size/mtime—and on Unix device/inode/ctime—validate each entry. A source
changing during parsing is not persisted. Executable identity and a format
version invalidate incompatible caches. This remains metadata-based freshness,
not a content-hash guarantee or an atomic snapshot of a concurrently edited tree.

The Unix cache is stored under `$XDG_CACHE_HOME/xray-who`, or
`$HOME/.cache/xray-who`, with owned 0700 directories and 0600 files. It retains
only the latest query's facts, with at most 20,000 entries and a 32-MiB serialized
snapshot. Atomic replacement and nonblocking writer locks prevent partial writes.
Malformed/incompatible, unavailable or unsafe cache storage simply recomputes
normally. Structurally valid JSON corruption is not checksummed. Unsupported
platforms run without persistence. Cache misses, eviction or corruption never
change result completeness, and recovery/error evidence survives cache hits.
