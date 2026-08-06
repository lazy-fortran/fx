# fx roadmap

Snapshot: 2026-08-06. fx supplies shared cache, JSON, DAG, MCP, and LSP
infrastructure. In the compiler path it consumes the structured
fo/FortFront diagnostic contract. It does not define parsing or lowering.

## Current truth and priority

The audited baseline is `5dba93c`. The only open fx issue is
[#36](https://github.com/lazy-fortran/fx/issues/36): concurrent digest-part
reallocation can segfault. It blocks reliable concurrent editor/build use,
but is not an ffc semantic-conformance failure.

Fix #36 before adding concurrency to another cache consumer:

1. reduce the race to a bounded multi-process or multi-thread stress test.
2. establish a process-survival and digest-correctness oracle that fails on
   the parent revision.
3. make cache entry construction immutable or synchronize ownership at the
   narrowest shared boundary.
4. run sanitizer/race tooling where supported and repeat enough iterations to
   put a stated upper bound on recurrence.
5. verify fo and editor consumers preserve result, source revision, diagnostic
   location, and exit status under concurrent load.

## Compiler integration boundary

fo [#103](https://github.com/lazy-fortran/fo/issues/103) stabilizes structured
FortFront diagnostic mapping before fx adds another translation layer. fo
[#56](https://github.com/lazy-fortran/fo/issues/56) owns debounced `didChange`
publication. fx passes records through without reclassifying or suppressing compiler
failures.

Cache keys identify all declared inputs, exact tool revisions, flags, target,
environment, and schema. Partial, interrupted, timed-out, OOM, and failed
actions are never reusable. Writes publish atomically. Readers never observe a
partially allocated digest or artifact.

## Delivery gate

Cache changes need a behavioral producer/consumer oracle, forced misses for
each changed input, safe hits for unrelated changes, crash/race stress, and a
full `fo` pipeline. Diagnostic/LSP
changes need an end-to-end FortFront-to-client test with success, error,
revision change, and forced child failure. Contract documentation lands with
the change.
