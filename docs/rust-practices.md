# Rust Practices (Mandatory)

Based on [Apollo GraphQL's Rust Best Practices](https://github.com/apollographql/rust-best-practices).

## 1. Ownership & Borrowing

- Prefer `&str` over `String`, `&[T]` over `Vec<T>` in function params
- Only `.clone()` when you truly need a new owned copy
- Small `Copy` types (<=24 bytes): pass by value
- Use `Cow<'_, T>` when ownership is ambiguous
- Never clone inside loops — use `.copied()` / `.cloned()` on iterators

## 2. Error Handling

- Return `Result<T, E>` for fallible operations
- `thiserror` for all error types — no `anyhow` in this crate
- Propagate with `?` — avoid verbose `match` chains
- **No `unwrap()`/`expect()` outside `#[cfg(test)]`**
- Use `let Ok(x) = expr else { return ... }` for early returns
- Test error paths: assert on `.unwrap_err().to_string()`

## 3. Iterators

- Prefer iterators over manual `for` loops for transforms
- Avoid intermediate `.collect()` — pass iterators when possible
- Use `.sum()` over `.fold()` for sums
- Prefer `.iter()` over `.into_iter()` unless ownership transfer is needed
- Use `for` when you need `break`/`continue`/`return` or side-effects

## 4. Linting

- Run before every commit: `cargo clippy --all-targets -- -D warnings`
- Fix warnings, don't silence them
- Use `#[expect(clippy::lint)]` (not `#[allow]`) with justification comment
- Key lints: `redundant_clone`, `large_enum_variant`, `needless_collect`

## 5. Option/Result Patterns

- `match` when pattern matching inner variants
- `let Some(x) = expr else { ... }` for early-return on None
- `?` when you don't care about the Err value
- `.ok()`, `.ok_or()`, `.ok_or_else()` for Result<->Option conversion
- Use `_else` variants (`unwrap_or_else`, `ok_or_else`) when the fallback allocates

## 6. Testing

- Name tests descriptively: `fn extracts_arrow_function_with_return_type()`
- One behavior per test function
- Use `#[cfg(test)] mod tests` with `use super::*`
- Group related tests under sub-modules
- `unwrap()` and `expect()` are fine inside tests

## 7. Comments & Docs

- Comments explain **why**, not what — clear code replaces commentary
- Prefix with context: `// SAFETY:`, `// PERF:`, `// HACK:`
- `// TODO` must link to an issue: `// TODO(#42): ...`
- Break long functions into named helpers instead of commenting steps
- `///` doc comments for public API: what it does, examples, errors

## 8. Performance

- Always benchmark with `--release`
- Don't optimize without evidence — measure first
- Avoid cloning in hot paths; prefer references
- Prefer stack for small types, heap (`Box`) for recursive/large types
- `#[inline]` only when benchmarks prove it helps
