# xray

Structural code digest for TypeScript/JavaScript files. Compresses source to ~10% of its size, showing only what matters: **signatures, types, exports, imports, hooks, render trees, and line ranges**.

Built for LLM context windows — feed xray output instead of raw source to save tokens while preserving structural understanding.

## Install

```sh
# From source
cargo install --path .

# Or build manually
cargo build --release
cp target/release/xray ~/.local/bin/
```

Requires Rust 1.70+.

## Usage

```sh
xray <file> [file2 ...]
```

### React component (TSX)

Given a 80-line React component file:

```sh
$ xray components/UserList.tsx
```

```
components/UserList.tsx  (tsx, 80 lines)

imports: @/lib/api, @/utils/date

exports:
  [component] const UserList = ({ initialUsers, onSelect }) =>  [L20-55]
    hooks:
      useState: users, setUsers  [L21-21]
      useState: search, setSearch  [L22-22]
      useState: sortOrder, setSortOrder  [L23-23]
      useEffect  deps: [search]  [L25-27]
      useCallback: handleSort  deps: []  [L29-31]
    calls: filteredUsers.map, users.filter, ….sort
    renders: SearchBar, SortButton
  function useUserSearch(initialQuery = '')  [L67-80]
    calls: useEffect, useState

internal:
  [component] function UserCard({ user, onClick })  [L57-65]
    calls: formatDate
    renders: Avatar

types:
  interface UserProfile {id, name, email, avatar, createdAt}  [L5-11]
  type SortOrder 'asc' | 'desc'  [L13-13]
  interface UserListProps {initialUsers, onSelect}  [L15-18]
```

### Plain TypeScript

```sh
$ xray lib/server.ts
```

```
lib/server.ts  (ts, 36 lines)

imports: ./database, ./logger

exports:
  async function startServer(config: Config)  [L12-18]
    calls: app.listen, app.use, authMiddleware, corsMiddleware, createApp, logger.info
  function createRouter(prefix: string)  [L20-25]
    calls: router.get, router.post

internal:
  async function healthCheck(req: Request, res: Response)  [L27-30]
    calls: db.ping, res.json
  async function createUser(req: Request, res: Response)  [L32-36]
    calls: db.users.create, logger.info, res.json

types:
  export interface Config {port, host, debug}  [L4-8]
  export type Handler (req: Request, res: Response) => Promise<void>  [L10-10]
```

## What it extracts

| Section | Content |
|---------|---------|
| **imports** | Module sources (deduped, path aliases preserved) |
| **re-exports** | `export { x } from './mod'` grouped by source |
| **exports** | Exported functions/consts with signatures |
| **internal** | Non-exported functions |
| **hooks** | React hooks with bindings, deps arrays |
| **types** | Interfaces, type aliases, enums with member summary |
| **tests** | `describe`/`it`/`test` blocks with nesting |

Each function also shows:
- **`[component]`** tag when it returns JSX
- **hooks** — `useState: x, setX  deps: [y]`
- **handlers** — local functions inside components
- **calls** — significant function calls (noise filtered)
- **renders** — JSX component tree: `Layout > [Sidebar, Content > List]`
- **line ranges** — `[L10-25]` for jumping to source

## Supported files

`.ts`, `.tsx`, `.js`, `.jsx`, `.mjs`, `.mts`, `.cjs`, `.cts`

## How it works

xray uses [tree-sitter](https://tree-sitter.github.io/) to parse source into a concrete syntax tree, then walks the AST to extract structural information. No regex, no string matching — just syntax-aware extraction.

## License

[MIT](LICENSE)
