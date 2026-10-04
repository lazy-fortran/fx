# fx

## Goal-oriented implementation

Follow the repository plan and [shared development principles](https://github.com/lazy-fortran/fo/blob/main/doc/GOAL_DRIVEN_DEVELOPMENT.md).
Issues define observable goals and independent correctness evidence. Choose the
smallest adequate implementation; make architectural decisions as soon as
required and as late as possible. Internal layouts, module lists and proposed
mechanisms are changeable, not delivery requirements. Existing accepted public
and semantic contracts remain binding. Reduce maintained code substantially
through [Fo #205](https://github.com/lazy-fortran/fo/issues/205), counting the whole
affected stack and retaining useful independent failure detection.

Use resident Fo Gremlin for development with focused local gates. Only actual
consumer blockers delay the next task; no full architecture queue, benchmark or
GitHub CI wait. Existing controller, escalation, ownership and host rules apply.

Shared Fortran infrastructure library for fo (build tool) and tx (LaTeX build tool).

## Build and Test

```bash
fo build
fo test
```

No external dependencies except the C standard library via `src/proc/fx_sys.c`.
OpenMP parallel directives appear as comments in routines where parallelism applies.

## Modules

- `fx_string`: allocatable string type, builder with geometric growth, case conversion, split/join, search/replace, UTF-8 length.
- `fx_hash`: FNV-1a, xxHash64, incremental hashing, file hashing, hex conversion.
- `fx_path`: path join, dirname, basename, extension, stem, normalize, existence checks.
- `fx_log`: structured stderr logging with level filtering (DEBUG, INFO, WARN, ERROR).
- `fx_json_build`: streaming JSON builder with automatic escaping and nesting.
- `fx_json_parse`: SAX-style event-based JSON parser, path-based extraction.
- `fx_diag`: diagnostic type for compiler/tool output, JSON serialization.
- `fx_proc`: Fortran interface to fx_sys.c for process execution, directory scan, file I/O.
- `fx_dag`: generic directed acyclic graph with topological sort, reverse-dep closure, level grouping.
- `fx_cache`: content-addressed artifact cache with LRU eviction.
- `fx_watch`: inotify-based file watcher with self-write loop prevention.
- `fx_cli`: command-line argument parser with flag/value/positional access.
- `fx_test`: minimal test framework with assert helpers and JSON summary.
- `fx_mcp`: MCP JSON-RPC server framework with framing auto-detect.
- `fx_lsp`: LSP protocol handler for diagnostics-on-save.

## Rules

- Fortran implementation with small C OS interfaces; organize them as needed.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Keep responsibilities cohesive and code readable; reduce duplication and maintained volume without arbitrary file/procedure limits.
- fprettify: 88 cols, 4-space indent.
