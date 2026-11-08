<h1 align="center">xray</h1>

<p align="center"><strong>See the shape of a codebase before you read the code.</strong></p>

<p align="center">
  xray turns source files into compact, syntax-aware maps of symbols, dependencies, call paths, hooks, render trees, types, and line ranges.
</p>

<p align="center">
  <a href="https://github.com/enzodevs/xray/blob/main/LICENSE"><img alt="License: MIT" src="https://img.shields.io/badge/license-MIT-blue.svg"></a>
  <img alt="Version 0.3.0" src="https://img.shields.io/badge/version-0.3.0-5c6ac4.svg">
  <img alt="Built with Rust" src="https://img.shields.io/badge/built_with-Rust-b7410e.svg">
</p>

---

Reading a large file just to find its structure wastes context. xray reports symbols, source ranges and static relationships so an agent can choose what to read next. Compression varies by language and source; a small digest is not proof of complete analysis.

Output is designed for agents: compact text or JSON, explicit scope/status, and a 16 KiB default byte budget with exact private overflow artifacts. See the [agent navigation and AXI contract](docs/agent-navigation.md) and [verified results and limits](docs/agent-navigation-verification.md).

## What xray gives you

- **A file map:** imports, exports, internal symbols, types, hooks, calls, tests, and line ranges.
- **Local dependency trees:** follow imports without opening every file.
- **Cross-file call traces:** start from one handler or method and follow the path through the project.
- **Reverse dependencies:** find static file dependents (including same-namespace PHP class references), not runtime callers.
- **Exact PHP navigation:** query Intelephense definitions and references at a source position.
- **Frontend structure:** React hooks and JSX trees, plus Vue and Svelte component structure.
- **Machine-readable output:** compact JSON for digests, dependency trees, reverse lookup, definitions and references; oversized results remain valid JSON with recoverable evidence.

xray parses syntax rather than matching text. It understands TypeScript/JavaScript, Go, Vue, Svelte, Python, PHP, Rust, SQL, and Markdown.

## See it in action

Trace one method from a 47-line NestJS controller into its services, evidence loaders, and prompt builders:

```sh
xray --trace --lsp -s getProfessorRecommendations \
  apps/api/src/uniai/uniai.controller.ts
```

```text
trace: getProfessorRecommendations  [L17-24]
│
├── this.currentProfile  (local)  [L36-46]
│   ├── extractAuthIdentity  ←  apps/api/src/shared/auth-context.ts  [L46-82]
│   ├── unauthenticated  ←  apps/api/src/shared/errors.ts  [L213-215]
│   └── this.profilesService.syncAuthProfile
│       ←  apps/api/src/profiles/profiles.service.ts  [L86-117]
└── this.uniaiService.recommendForProfessor
    ←  apps/api/src/uniai/uniai.service.ts  [L20-30]
    ├── this.evidenceTools.loadProfessorEvidence
    │   ←  apps/api/src/uniai/uniai-evidence-tools.ts  [L78-103]
    └── composeProfessorRecommendation
        ←  apps/api/src/uniai/uniai-prompts.ts  [L302-328]
```

Legacy trace is a heuristic static navigation aid, not an authoritative call graph. Prefer exact `--definition` / `--references` queries when symbol identity matters; unsupported or dynamic relationships are not proof of absence.

## Install

xray currently installs from source:

```sh
git clone https://github.com/enzodevs/xray.git
cd xray
cargo install --path .
```

Make sure Cargo's bin directory is on your `PATH`:

```sh
export PATH="$HOME/.cargo/bin:$PATH"
```

To build without installing:

```sh
cargo build --release
./target/release/xray --help
```

Rust 1.95 or newer is required (including the Oxc resolver dependency).

## Quick start

```sh
# Map one file without following imports
xray --no-follow src/app.ts

# Map the file and its direct local dependencies
xray src/app.ts

# Follow two levels deep
xray --depth 2 src/app.ts

# Trace all exported entry points
xray --trace src/app.ts

# Trace one symbol and resolve member calls through the language server
xray --trace --lsp -s createUser src/routes/users.ts

# Find project files that import this file
xray --who src/lib/auth.ts

# Show resolution counts and unresolved imports
xray --explain src/app.ts

# Produce a bounded JSON digest for another tool
xray --no-follow --format json src/app.ts

# Resolve the identifier at a 1-based line / UTF-8 byte column
xray --definition --at 42:19 app/Jobs/SomeJob.php

# Find static symbol references, not just importing files
xray --references --at 16:7 --format json app/Models/User.php

# Structured dependency tree, with a smaller context budget
xray --follow --format json --max-bytes 4096 app/Models/User.php
```

Run xray from the project or workspace root when you want import resolution, reverse lookup, or LSP tracing. This gives it the right `tsconfig.json`, `go.mod`, and workspace context.

### PHP structure and Composer resolution

PHP classes, interfaces, traits, and enums retain their compact type summaries, and their methods also appear as individual symbols with owner-qualified names (`Class::method`), signatures, one-based line ranges, and extracted calls. Public methods appear under exports; protected/private methods appear under internal. JSON uses the same method names and normalized `method` kind. These are syntax-level visibility groups, not a guarantee that a method is a public package API.

Local namespace resolution reads the nearest `composer.json` without executing PHP or Composer. It supports `autoload` and `autoload-dev` PSR-4 mappings, multiple directories, and longest-prefix precedence. Missing classes under a mapped namespace are unresolved rather than external. The structural resolver does not implement Composer classmaps, PSR-0 or dependency autoloaders. PHP recursive call tracing is not implemented; separate LSP definition/reference queries can resolve typed symbols and inherited methods when supported by the server. Relative includes and the legacy namespace-path fallback remain available when no PSR-4 mapping matches.

PHP also extracts Pest test blocks and recognized Laravel route registrations. Routes are structural records (`kind=route`), not immediate calls. Static class references augment PHP dependencies even without explicit imports. Dynamic framework behavior remains outside completeness guarantees.

### Reverse lookup scope

`--who` enumerates tracked and untracked non-ignored files with Git, respecting nested `.gitignore` rules. Tracked files remain eligible even if an ignore rule matches them. Dependencies/build directories such as `vendor`, `node_modules`, and `target`, hidden directories, and symlinked paths are excluded. The scan stays within the selected Git repository: run against a file inside a submodule to inspect that submodule, rather than assuming the parent scan includes it. Ignore-file scope is independent of follow-mode `--all`.

TypeScript and Composer configurations are reused per source directory during reverse lookup. Failure to enumerate an established Git root is an error, not permission to perform an unfiltered scan. For PHP targets, reverse lookup scans PHP only and includes named class references. Results report scan/recovery counts without echoing the target digest. Reverse lookup remains static dependency discovery, not a complete runtime call graph; large first-party repositories can still take time to parse.

## Modes

| Goal | Command | What it does |
|---|---|---|
| Understand one file | `xray --no-follow FILE` | Prints one structural digest with no dependency traversal |
| Explore nearby code | `xray FILE` | Follows local imports to depth 1 and filters low-signal files |
| Explore deeper | `xray --depth N FILE` | Sets the dependency or trace depth |
| Keep every dependency | `xray --all FILE` | Disables the default noise filter |
| Check impact | `xray --who FILE` | Finds project files that import the target |
| Follow behavior | `xray --trace FILE` | Traces calls from exported symbols, to depth 3 by default |
| Follow one entry point | `xray --trace -s NAME FILE` | Traces only the named symbol |
| Resolve member calls | `xray --trace --lsp FILE` | Asks a supported language server for definitions |
| Audit resolution | `xray --explain FILE` | Reports resolved, external, and unresolved dependencies |
| Resolve a symbol | `xray --definition --at LINE:COL FILE` | Returns exact LSP definition locations |
| Find symbol uses | `xray --references --at LINE:COL FILE` | Returns server-reported static references |
| Feed another tool | `xray --format json FILE` | Emits a bounded single-file JSON digest |
| Bound context | `xray --max-bytes 4096 FILE` | Caps stdout; saves exact overflow privately |

You can pass more than one file to the regular text modes:

```sh
xray src/api.ts src/worker.ts
```

## What it extracts

| Language | File structure | Calls | Follow / who | Trace | LSP trace |
|---|:---:|:---:|:---:|:---:|:---:|
| TypeScript / JavaScript / JSX / TSX | ✓ | ✓ | ✓ | ✓ | `typescript-language-server` |
| Go | ✓ | ✓ | ✓ | ✓ | `gopls` |
| Vue | ✓ | ✓ | ✓ | ✓ | `typescript-language-server` + `@vue/language-server` |
| Svelte | ✓ | ✓ | ✓ | ✓ | `svelte-language-server` |
| Python | ✓ | ✓ | ✓ | — | — |
| PHP | ✓ | ✓ | ✓ | — | — |
| Rust | ✓ | ✓ | ✓ | — | — |
| SQL | ✓ | references | includes | — | — |
| Markdown | ✓ | — | local links | — | — |

Supported extensions:

```text
.ts .tsx .js .jsx .mjs .mts .cjs .go .vue .svelte
.py .php .rs .sql .md
```

### TypeScript and frontend files

xray extracts imports and re-exports, functions, classes, types, React hooks, nested handlers, significant calls, tests, and JSX component trees. Vue and Svelte files include script symbols, framework hooks or runes, props, types, and template composition.

A digest of a large TSX component looks like this:

```text
apps/web/features/unialgo/components/Sidebar.tsx  (tsx, 968 lines)

imports: @/i18n/routing, ../api/professor, ../types

exports:
  [component] const Sidebar = (...) =>  [L408-968]
    hooks:
      useRouter: router  [L431-431]
      useTranslations: t  [L432-432]
      useState: locale, setLocale  [L433-435]
      useEffect  deps: [isAccountMenuOpen]  [L465-490]
    handlers: handleLocaleSelect, handleCollapsedModeToggle
    renders: PanelLeftOpen, PanelLeftClose, SageChatHistoryView, SidebarItem, Button

internal:
  [component] const SidebarItem = (...) =>  [L82-141]
  [component] function SageChatHistoryView(...)  [L223-406]
  const handlePointerDown = (...) =>  [L268-272]
  const handlePointerDown = (...) =>  [L470-475]

types:
  interface SidebarItemProps {href, icon, label, isActive, onClick, ...+1}  [L51-58]
  type SidebarMode "menu" | "chat"  [L143-143]
```

Nested symbols keep their lexical scope during tracing, even when separate components reuse the same handler name. Local aliases use Oxc's inherited `tsconfig` resolution, including JSONC, relative/package `extends`, arrays of base configurations, and origin-relative paths. Each followed file uses its own nearest configuration; this does not enable traversal of external npm packages.

### Go, Python, PHP, and Rust

For backend files, xray reports imports, functions and methods, classes or structs, interfaces and aliases, significant calls, tests where applicable, and exact source ranges. Go module paths resolve through `go.mod`; follow includes every non-test source file in an imported package instead of choosing one representative. This is structural enumeration, including platform/build-tag variants, not the active Go build graph. `--who` matches package imports for any source file in that package. Go LSP tracing uses `gopls`.

### SQL and Markdown

SQL digests retain statement structure and relationships such as `target:`, `source:`, `join:`, `cte:`, and `fn:`. Markdown digests retain frontmatter, the heading tree, local and external links, and fenced-code languages. Follow mode traverses SQL includes and local Markdown links. SQL client includes are recognized at statement boundaries outside comments and quoted bodies, so a column such as `source VARCHAR NOT NULL` is not an include.

## Optional language servers

Basic extraction, dependency following, and ordinary traces do not require a language server. Definition/reference queries and `--trace --lsp` need a supported server:

```sh
# PHP definition/reference navigation (freemium; definitions/references are free)
npm install -g intelephense

# TypeScript and JavaScript (typescript-language-server 6 requires Node.js >= 22.22.2)
npm install -g typescript typescript-language-server

# Vue (also needs typescript-language-server)
npm install -g @vue/language-server

# Go
go install golang.org/x/tools/gopls@latest

# Svelte
npm install -g svelte-language-server
```

Plain Vue traces use xray's own static resolution. `--trace --lsp` on Vue uses the TypeScript side of Vue's hybrid setup: `typescript-language-server` loads `@vue/typescript-plugin` from the `@vue/language-server` package found via `vue-language-server` on `PATH` (override with `XRAY_VUE_LANGUAGE_SERVER=/path/to/@vue/language-server`). TypeScript traces load the same plugin when it is available, so definitions crossing into `.vue` files resolve.

TS/Vue batch queries use the semantic server from the first request, avoiding syntax-only answers while a project loads. If a definition identifies a binding or type rather than a callable body, trace labels it `definition only; callable target unresolved` and does not invent a call edge.

Text traces print the entry file header before the requested tree; use `--no-follow` for the outline. Parser-recovery warnings remain visible. Vue/Svelte render trees and template calls are retained in full; the common byte-budget layer caps output and saves exact overflow artifacts instead of discarding components during extraction.

The opt-in integration check exercises both Vue-to-TS and TS-to-Vue definitions on cold servers:

```sh
cargo test live_vue_lsp_resolves_both_directions_on_cold_start -- --ignored
```

## How it works

xray parses each file into a syntax tree, extracts a compact intermediate model, and resolves local project references only when the selected mode needs them. It uses tree-sitter grammars for code and a dedicated Markdown parser. Noise filtering keeps generated files, declarations, fixtures, and other low-signal dependencies out of the default tree; `--all` turns that filter off.

The tool does not execute code, infer runtime values, or replace a type checker. Dynamic imports, dependency injection, framework magic, and heavily computed call targets can remain unresolved. Use `--explain` to see resolution gaps, and treat a trace as a focused navigation map rather than a runtime profile.

## Why use it with coding agents?

Agents usually need a map before they need full implementations. Giving an agent xray output first helps it choose the few source ranges worth reading instead of loading entire files and their imports into context.

A practical sequence is:

```sh
xray --no-follow src/feature.ts      # understand the target
xray --who src/feature.ts            # check its impact radius
xray --trace --lsp -s run src/feature.ts  # follow the behavior
```

Then open the exact ranges reported by xray.

## Stress testing and known limits

The [real-project stress report](docs/stress-test-2026-09-08.md) covers 1,057 files across nine project collections, including large compiler fixtures. The [navigation fidelity follow-up](docs/navigation-gap-verification.md) records the newer TS/Vue, Go, SQL, and overflow fixes with their verification and remaining limits. A repeatable runner is available:

```sh
python3 scripts/stress_xray.py --binary target/release/xray \
  --output /tmp/xray-stress-new /path/to/project /path/to/oss-snapshot
```

Compression depends on the input and output mode; JSON and followed dependencies can be much larger than a plain digest. Passing the stress runner checks execution and basic JSON invariants, not semantic completeness. TS-family digests filter many external imports, Rust public re-exports are not a complete API inventory, and returned anonymous handlers are not modeled as separate trace roots. LSP trace only traverses a matching callable declaration, not an enclosing component or class; a binding/type definition alone does not establish the callable implementation. Verify important edges in source. LSP requests and shutdown are deadline-bounded; transport failures are reported rather than silently treated as empty results. `--who` requires Git metadata; downloaded snapshots without it are unsupported.

## License

[MIT](LICENSE)
