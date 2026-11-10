# PHP navigation fails on real Laravel workspaces — 2026-11-10

Status: implemented and validated on `redapro-whitelabel`. Found with xray 0.3.0
on a production Laravel application (3,042 Git-visible PHP files, about 21,000
PHP files under `vendor`). See the measurements in
[agent-navigation-verification.md](agent-navigation-verification.md#php-workspace-scan-fix-redapro-whitelabel).

## Implemented patch

- `src/git_files.rs` shares Git-visible enumeration between reverse navigation
  and PHP freshness. It handles nested Composer roots and rejects a broken Git
  index without unfiltered fallback. Unix subprocess output is deadline/size bounded.
- `src/php_workspace.rs` uses those paths to prune directories before descent,
  retaining the root `vendor` tree. Only visible PHP/Composer paths keep a
  directory traversable: tracked `.gitignore` placeholders in Laravel storage
  do not pull runtime contents into the walk.
- The same scan supplies Intelephense exclusions, including ignored files in
  partially visible directories and symlinks. Literal paths escape glob syntax.
  Git ignore rules are interpreted by Git, not translated into approximate globs.
- PHP associations now match the scanner's case-insensitive `.php` scope. The
  old independent default exclusions (such as vendor test folders) are replaced.
- Non-Git Composer projects keep a bounded filesystem walk. The original scan
  caps remain; the fix prunes unrelated work instead of increasing limits.
- Regression tests cover ignored runtime volume with an injected small cap,
  tracked ignore placeholders, ignored-PHP edits, vendor retention, nested Git
  ignores/negations, symlinks, nested Composer roots and broken indexes.

The sections below retain the original diagnosis and acceptance criteria.

## Symptom

Every `--definition` and `--references` query fails in about 0.5 seconds, before
Intelephense is started:

```text
$ xray --references --at 100:21 app/Actions/InstitutionalAgents/SendInstitutionalAgentMessage.php
status=failed target="app/Actions/InstitutionalAgents/SendInstitutionalAgentMessage.php"
error: lsp: lsp: workspace scan limit/deadline exceeded
```

The structural modes are unaffected on the same repository: `--no-follow`,
`--follow`, `--who` and `--trace` all return correct results in under 7 seconds.

## Root cause

`snapshot()` in `src/navigation.rs` walks the whole workspace and counts **every
directory entry** against a 500,000 cap, although it only records `*.php`,
`composer.json` and `composer.lock`. It prunes only hidden directories and
`node_modules`.

A Laravel application keeps large Git-ignored runtime trees inside the workspace.
Measured on the failing project:

| Tree | Files |
| --- | ---: |
| `storage/app/private/operations` | 247,461 |
| `storage/framework` | 182,696 |
| `storage/logs` | 48,001 |
| `vendor` | 27,186 |
| Entries the scan visits in total | 540,385 |

540,385 exceeds the cap, so the query is refused. Nearly all of those entries
are non-PHP files that the scan would discard anyway.

## Second problem behind the first

Raising the cap alone is not a fix. `storage` on that project also holds 1,435
PHP files: compiled Blade views, parallel-test caches and archived end-to-end
run artifacts. They are not hidden, so today they would be:

- tracked by the freshness snapshot, so compiling a view or running the test
  suite changes the snapshot and restarts Intelephense on the next query;
- indexed by Intelephense (`php_configuration()` in `src/lsp.rs` does not
  exclude them), which is exactly the "generated PHP caches must not dominate
  indexing or introduce stale duplicate symbols" case that
  `docs/agent-navigation.md` already rules out for hidden directories.

## Required outcome

1. `--definition` and `--references` work on a Laravel workspace whose ignored
   runtime trees hold hundreds of thousands of files.
2. Git-ignored generated PHP is neither tracked for freshness nor indexed.
   `vendor` stays tracked and indexed even though it is Git-ignored: Laravel
   navigation needs dependency declarations.
3. The freshness scan and the Intelephense exclusions describe the same set of
   files, so a file cannot trigger a restart without being indexed, or be
   indexed without being tracked.
4. Bounds remain. A workspace that is too large still fails with an explicit
   error, never an empty or partial result presented as complete.

## Suggested approach

Decide after reading the code; this is the direction that looked cheapest.

- Build the snapshot from Git-visible files plus a bounded walk of `vendor`,
  instead of an unfiltered walk. `src/reverse.rs` already enumerates Git-visible
  files with `git ls-files` for `--who`, including the rule that a broken index
  must not fall back to an unfiltered scan. Reuse that rather than writing a
  second enumerator.
- Keep the current walk as the fallback for a Composer workspace with no Git
  repository, and make its cap count recorded files, or prune directories
  earlier, so discarded entries do not exhaust the budget.
- Derive the Intelephense `files.exclude` list from the same decision. A fixed
  Laravel list (`storage/**`, `bootstrap/cache/**`) is a weaker alternative: it
  fixes this project but not other frameworks' ignored caches.

## Acceptance

- Unit test: a workspace with many ignored non-PHP entries and some ignored PHP
  files under a non-hidden directory. The snapshot succeeds, contains the
  tracked PHP and `vendor` PHP, and omits the ignored PHP. Make the cap
  injectable so the test does not need 500,000 real files.
- Unit test: touching an ignored generated PHP file does not change the
  snapshot.
- Existing navigation tests keep passing, including
  `refresh_scan_tracks_php_and_composer_but_not_secret_or_hidden_trees` and the
  vendor workspace-root test.
- Real check on a large Laravel application: the command in "Symptom" returns a
  non-failed status with exact locations. Record cold and warm timings in
  `docs/agent-navigation-verification.md`.
- Update the lifecycle section of `docs/agent-navigation.md` to describe the new
  scan scope.

## Remaining limits

Cold indexing and warm queries now complete within the existing 90-second
request and 210-second queue deadlines; neither deadline was increased.

- Result quality on Laravel conventions (facades, container bindings, macros,
  Eloquent magic). Those remain static-analysis limits and are out of scope.

## Licensing

No Intelephense licence is needed. Definition and references are free features,
and `docs/agent-navigation-verification.md` records them working on
Intelephense 1.18.5 without a key. A paid licence adds rename, go to
implementation, go to type definition, code actions and folding; xray exposes
none of them. Go to implementation would only become relevant if xray later
adds an "implementations of this interface" query.
