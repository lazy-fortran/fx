# fx

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

- Pure Fortran + one C file for OS interfaces.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Modules under 500 lines, functions under 50 lines.
- fprettify: 88 cols, 4-space indent.
