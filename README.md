# fx

Shared Fortran infrastructure for the lazy-fortran toolchain. `fx` has no
dependencies of its own, which is deliberate: it sits at the bottom of the
stack so the tools above it stay cheap to build.

`fo`, the build driver, consumes eight of these modules.

## What it provides

| Module | Purpose |
|---|---|
| `fx_string` | String handling helpers |
| `fx_path` | Path normalization and manipulation |
| `fx_hash` | FNV-1a and SHA-256 content hashing |
| `fx_cache`, `fx_cache_fs`, `fx_cache_key` | Content-addressed artifact store |
| `fx_action_cache` | Action-level caching keyed by inputs and tool identity |
| `fx_dag` | Directed acyclic graph with topological sort and reverse-dependency closure |
| `fx_json_build`, `fx_json_parse` | JSON emission and parsing |
| `fx_lsp` | Language Server Protocol framing and message handling |
| `fx_mcp` | Model Context Protocol server over JSON-RPC/stdio |
| `fx_cli` | Command-line argument parsing |
| `fx_proc` | Process execution |
| `fx_watch` | inotify-based file watching |
| `fx_diag` | Diagnostic records |
| `fx_test` | Minimal test-suite framework |
| `fx_log` | Logging |

## Build

```bash
fo build
fo test
```

`fo` is the build driver for this ecosystem. `fpm` works too and is what CI
uses, since bootstrapping `fo` inside `fx`'s own CI would be circular.

## Executables

`fx-mcp-server` exposes fx functionality over JSON-RPC on stdio.

## License

MIT. See [LICENSE](LICENSE).
