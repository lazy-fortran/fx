# fx roadmap

fx is the user-facing diagnostics and language-service client. It consumes the
stable fo/FortFront diagnostic contract; it does not define compiler lowering.

## Current handoff (2026-08-03)

- `main` is `7681949` (implementation baseline `9e16f11`).
- [#36](https://github.com/lazy-fortran/fx/issues/36) is open: concurrent
  cache digest reallocation can segfault. Treat it as a correctness blocker
  for concurrent editor use, not as an ffc conformance issue.
- fo [#103](https://github.com/lazy-fortran/fo/issues/103) must stabilize
  structured frontend diagnostic mapping before fx grows another translation
  layer; fo [#56](https://github.com/lazy-fortran/fo/issues/56) owns the
  debounced didChange path.

## Delivery gate

Reproduce concurrency defects with a bounded stress test and an independent
process-survival oracle. Preserve diagnostic locations, severity, and source
revision across the fo/FortFront boundary.
