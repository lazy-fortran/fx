# fx roadmap

Snapshot: 2026-10-03. fx supplies shared cache, JSON, DAG, MCP, watch, and LSP
infrastructure. In the compiler path it consumes the structured
fo/FortFront diagnostic contract. It does not define parsing or lowering.

## Current truth and priority

Current `main` is `0d13bf2`; `c1da80d` is the watcher-lifecycle implementation
checkpoint. The digest-part concurrency issue #36 and watcher
issues #39/#40 are closed with behavioral evidence. The active provider work is
one immutable filesystem store shared by ordinary fo and Gremlin:

1. [#42](https://github.com/lazy-fortran/fx/issues/42) publishes verified raw
   blobs and canonical trees without rewriting existing content.
2. [#43](https://github.com/lazy-fortran/fx/issues/43) publishes complete action
   results and reports conflicting outputs for one action instead of overwriting.
3. [#44](https://github.com/lazy-fortran/fx/issues/44) collects only unreachable
   store objects from explicit leased roots with bounded, crash-safe work.

fo #165--#168 consumes those primitives as compact input generations,
lane-private transactional build sessions, one ordinary/Gremlin build engine and
low-churn semantic retention. fx does not acquire Gremlin scheduling policy.

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

The new store uses raw content IDs for bytes and canonical manifests for roles,
modes, paths, trees and action results. Existing verified blobs are reused
without a rewrite. Equal action/result publication is idempotent; different
result IDs for one action are nondeterminism or an incomplete-key failure and
remain visible. Such an action is quarantined from reuse while both result IDs
and conflict evidence are retained; a corrected key/schema produces a distinct
action. Materialization may use reflink/clone with byte-copy fallback.
No database, cache daemon, bulk RAM cache or global store lock is added.

Keep legacy `fx_cache` replacement semantics isolated. New store writes use a
versioned namespace; eligible store/v1 payloads may be lazily imported only
after independent hashing/validation. `nopayload` link records never become
executable hits. Collection begins only after fo provides owner-specific roots;
the current one-bit Gremlin pin is not sufficient retention authority.
Root/lease acquisition, graph publication and collection snapshot/deletion use
one explicit synchronization protocol so a newly live graph cannot be swept
between reachability discovery and unlink.

## Delivery gate

Cache changes need a behavioral producer/consumer oracle, forced misses for
each changed input, safe hits for unrelated changes, concurrent identical and
conflicting publication, crash/race stress, and focused fo consumer evidence.
GitHub CI is asynchronous post-submit evidence. Diagnostic/LSP
changes need an end-to-end FortFront-to-client test with success, error,
revision change, and forced child failure. Contract documentation lands with
the change.
