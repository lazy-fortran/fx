# fx roadmap

Snapshot: 2026-10-04. fx supplies shared cache, JSON, DAG, MCP, watch, and LSP
infrastructure. In the compiler path it consumes the structured
fo/FortFront diagnostic contract. It does not define parsing or lowering.

## Current truth and priority

Current integration head is `ecca77c`; `6beeec4` is the immutable-store
implementation checkpoint and `c1da80d` is the watcher-lifecycle checkpoint. The digest-part concurrency issue #36 and watcher
issues #39/#40 are closed with behavioral evidence. The active provider work is
one immutable filesystem store shared by ordinary fo and Gremlin:

1. **Complete:** [#42](https://github.com/lazy-fortran/fx/issues/42) publishes
   verified raw blobs and canonical trees without rewriting existing content.
2. **Complete through `ecca77c`:** [#43](https://github.com/lazy-fortran/fx/issues/43)
   publishes complete action results, materializes complete executable/archive/
   shared/runtime graphs and durably quarantines conflicting outputs for one
   action instead of overwriting.
3. **Active:** [#44](https://github.com/lazy-fortran/fx/issues/44) first lands owner/reason
   roots plus read/publication leases and a monotonic epoch with collection
   disabled. After fo exposes complete semantic roots, its second phase collects
   only unreachable objects with bounded, crash-safe work.
4. **Complete through `084bac3`:** [#45](https://github.com/lazy-fortran/fx/issues/45)
   replaces the remaining JavaScript MCP system fixture with an independent
   Fortran process oracle. Its narrow C process shim lives in a static test-only
   development dependency: cold FPM, four protocol mutants and the exact fo
   consumer pipeline pass while production fx contains no test symbols.

fo #165--#168 consumes those primitives as compact input generations,
lane-private transactional build sessions, one ordinary/Gremlin build engine and
low-churn semantic retention. #44 Phase A is an explicit prerequisite of fo
#166; collection stays disabled while fo #168 Phase A adopts every owner/reason
root and is enabled only by the audited #168/#44 Phase B. fx does not acquire
Gremlin scheduling policy.

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
modes, paths, trees and action results. Manifests reject absolute/escaping paths,
duplicates, file/directory prefix collisions, invalid IDs, special files,
escaping symlinks and unsupported case/Unicode collisions. Descriptor-rooted
materialization cannot escape through concurrent directory/symlink replacement.
Existing verified blobs are reused
without a rewrite. Equal action/result publication is idempotent; different
result IDs for one action are nondeterminism or an incomplete-key failure and
remain visible. Such an action is quarantined from reuse while both result IDs
and conflict evidence are retained; conflict lookup supersedes the old binding
at one durable linearization point, including crash recovery. A corrected
key/schema produces a distinct action. Materialization may use reflink/clone with byte-copy fallback.
No database, cache daemon or bulk RAM cache is added. No payload copy or full
store traversal runs under a global lock; #44 may use one short metadata
synchronization boundary for root acquisition, graph publication and sweep.

Keep four identities distinct: a content ID covers raw bytes; a tree ID covers
the canonical path/type/mode graph; an action ID covers the complete computation
contract; and a result ID covers the complete output/companion manifest. The
action binding is small metadata over immutable objects. Concurrent producers
write private temporaries; existing verified payloads are never rewritten.

Keep legacy `fx_cache` replacement semantics isolated. New store writes use a
versioned namespace; eligible store/v1 payloads may be lazily imported only
after independent hashing/validation. `nopayload` link records never become
executable hits. The #44 ownership/publication protocol lands before collection
so #43 result graphs and fo manifests can acquire durable roots without another
retention mechanism. Collection begins only after fo provides owner-specific roots;
the current one-bit Gremlin pin is not sufficient retention authority.
Root/lease acquisition, final graph/action publication and collection use one
short metadata synchronization boundary; payload copying happens outside it.
Publishers register a lease before writing. Collection discovers candidates
outside the boundary, then revalidates the root/publication epoch and unlinks a
bounded batch while holding it; an epoch change aborts the batch. A newly live
graph therefore cannot be swept between reachability discovery and unlink.

Maintenance is thresholded and bounded rather than a per-command store walk.
Recently used roots receive only coarse low-frequency retention touches; idle
lookups do not rewrite blobs. The fo policy layer chooses time/byte thresholds
and owner roots, while fx supplies deterministic clocks/limits for behavioral
oracles. Ordinary one-shot fo retains its stat-memo-assisted fast path and is
not required to recapture a complete tree merely to use this store.

## Delivery gate

Cache changes need a behavioral producer/consumer oracle, forced misses for
each changed input, safe hits for unrelated changes, concurrent identical and
conflicting publication, crash/race stress, and focused fo consumer evidence.
GitHub CI is asynchronous post-submit evidence. Diagnostic/LSP
changes need an end-to-end FortFront-to-client test with success, error,
revision change, and forced child failure. Contract documentation lands with
the change.
