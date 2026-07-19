<h1 align="center">xray</h1>

<p align="center"><strong>See the shape of a codebase before you read the code.</strong></p>

<p align="center">
  xray turns source files into compact, syntax-aware maps of symbols, dependencies, call paths, hooks, render trees, types, and line ranges.
</p>

<p align="center">
  <a href="https://github.com/enzodevs/xray/blob/main/LICENSE"><img alt="License: MIT" src="https://img.shields.io/badge/license-MIT-blue.svg"></a>
  <img alt="Version 0.2.0" src="https://img.shields.io/badge/version-0.2.0-5c6ac4.svg">
  <img alt="Built with Rust" src="https://img.shields.io/badge/built_with-Rust-b7410e.svg">
</p>

---

Reading a large file just to find its exports, hooks, or callers wastes context. xray keeps the useful structure and leaves function bodies behind. Its output is often around 10% of the original source, with line numbers so you can jump straight to the code that matters.

It is built for coding agents, but the output is plain text and works just as well in a terminal.

## What xray gives you

- **A file map:** imports, exports, internal symbols, types, hooks, calls, tests, and line ranges.
- **Local dependency trees:** follow imports without opening every file.
- **Cross-file call traces:** start from one handler or method and follow the path through the project.
- **Reverse dependencies:** find files that import the target before changing it.
- **Frontend structure:** React hooks and JSX trees, plus Vue and Svelte component structure.
- **Machine-readable output:** emit a single-file digest as JSON for scripts and agent tooling.

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

The trace is static and bounded. Local calls resolve directly; `--lsp` adds language-server definitions for member calls such as `this.service.run()`.

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

A recent stable Rust toolchain is required.

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

# Produce a JSON digest for another tool
xray --no-follow --format json src/app.ts
```

Run xray from the project or workspace root when you want import resolution, reverse lookup, or LSP tracing. This gives it the right `tsconfig.json`, `go.mod`, and workspace context.

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
| Feed another tool | `xray --no-follow --format json FILE` | Emits a single-file JSON digest |

You can pass more than one file to the regular text modes:

```sh
xray src/api.ts src/worker.ts
```

## What it extracts

| Language | File structure | Calls | Follow / who | Trace | LSP trace |
|---|:---:|:---:|:---:|:---:|:---:|
| TypeScript / JavaScript / JSX / TSX | ✓ | ✓ | ✓ | ✓ | `typescript-language-server` |
| Go | ✓ | ✓ | ✓ | ✓ | `gopls` |
| Vue | ✓ | ✓ | ✓ | ✓ | not required |
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

Nested symbols keep their lexical scope during tracing, even when separate components reuse the same handler name.

### Go, Python, PHP, and Rust

For backend files, xray reports imports, functions and methods, classes or structs, interfaces and aliases, significant calls, tests where applicable, and exact source ranges. Go module paths resolve through `go.mod`; Go LSP tracing uses `gopls`.

### SQL and Markdown

SQL digests retain statement structure and relationships such as `target:`, `source:`, `join:`, `cte:`, and `fn:`. Markdown digests retain frontmatter, the heading tree, local and external links, and fenced-code languages. Follow mode traverses SQL includes and local Markdown links.

## Optional language servers

Basic extraction, dependency following, and ordinary traces do not require a language server. Install one only when you need `--trace --lsp`:

```sh
# TypeScript and JavaScript
npm install -g typescript typescript-language-server

# Go
go install golang.org/x/tools/gopls@latest

# Svelte
npm install -g svelte-language-server
```

Vue traces use xray's own static resolution and do not require a Vue language server.

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

## License

[MIT](LICENSE)
