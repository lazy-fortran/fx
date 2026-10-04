# fx roadmap

Snapshot: 2026-10-04. fx supplies shared cache, JSON, DAG, MCP, watch, and LSP
infrastructure. In the compiler path it consumes the structured
fo/FortFront diagnostic contract. It does not define parsing or lowering.

## Current truth and priority

Current code audit checkpoint is `8730572`; `6beeec4` is the immutable-store
implementation checkpoint and `c1da80d` is the watcher-lifecycle checkpoint. The digest-part concurrency issue #36 and watcher
issues #39/#40 are closed with behavioral evidence. The active provider work is
one immutable filesystem store shared by ordinary fo and Gremlin:

1. **Complete:** [#42](https://github.com/lazy-fortran/fx/issues/42) publishes
   verified raw blobs and canonical trees without rewriting existing content.
2. **Complete through `ecca77c`:** [#43](https://github.com/lazy-fortran/fx/issues/43)
   publishes complete action results, materializes complete executable/archive/
   shared/runtime graphs and durably quarantines conflicting outputs for one
   action instead of overwriting.
3. **Phase A complete through `93ee0d8`; Phase B deferred:**
   [#44](https://github.com/lazy-fortran/fx/issues/44) provides owner/reason
   roots, pre-lookup read/publication leases and a monotonic epoch with collection
   disabled. Native external-lock oracles prove pending P-before-binding reads,
   P-to-R publication, complete companion restoration, legacy import and conflict
   quarantine; the action-result production test hook was removed. After fo #168
   exposes and audits every semantic root, Phase B may collect only unreachable
   objects with bounded, crash-safe work.
4. **Complete through `084bac3`:** [#45](https://github.com/lazy-fortran/fx/issues/45)
   replaces the remaining JavaScript MCP system fixture with an independent
   Fortran process oracle. Its narrow C process shim lives in a static test-only
   development dependency: cold FPM, four protocol mutants and the exact fo
   consumer pipeline pass while production fx contains no test symbols.
5. **Complete through `679f692`:**
   [#46](https://github.com/lazy-fortran/fx/issues/46)--[#49](https://github.com/lazy-fortran/fx/issues/49)
   finish the all-Fortran executable-test contract. Immutable-store race control
   is now Fortran with narrow exact-PID OS primitives; cache/watcher fixtures
   now use a native test-only filesystem API. Self-capture and MCP orchestration
   are Fortran over the shared narrow process API; C owns no FPM/build policy.
   The final no-interpreter gate has no tracked script fixtures or interpreter
   packages: Fortran creates temporary inventory controls and copies its native
   image under forbidden names for descendant tracing. The exact-head strict
   build/focused/full trace and five behavioral mutants pass independent review.
   Declarative workflow/TOML files remain data; test programs, scheduling,
   parsing and assertions are Fortran with only narrow C OS primitives.
6. **Complete through `bca1693`:**
   [#50](https://github.com/lazy-fortran/fx/issues/50) restores strict Darwin
   visibility for action-result no-follow and locking APIs. Cold macOS
   publication/conflict/recovery oracles and the complete Linux fx gate pass
   without weakening either platform's semantics.
7. **Active cleanup:** [#51](https://github.com/lazy-fortran/fx/issues/51)
   removes the older immutable-store production test synchronization hooks found
   during #44's final review. Independent Fortran oracles will control the same
   real locks/process boundaries through test-only OS shims; collection remains
   disabled and the Phase A public root/lease contract does not change.

fo #165--#168 consumes those primitives as compact input generations,
lane-private transactional build sessions, one ordinary/Gremlin build engine and
low-churn semantic retention. #44 Phase A is an explicit prerequisite of fo
#166; collection stays disabled while fo #168 Phase A adopts every owner/reason
root and is enabled only by the audited #168/#44 Phase B. fx does not acquire
Gremlin scheduling policy.

## Test cost and remaining architecture

At `8730572`, tracked production source has 14,331 physical lines and native
tests/support/bench have 12,066, including blanks/comments. `fx_sys.c` (1,895)
mixes OS services; source size alone does not justify a blanket rewrite. Keep
generic OS primitives narrow and Fortran policy cohesive as actual callers move.
Legacy cache replacement APIs and the new immutable store temporarily coexist;
retire duplicate fo use through fo #165--#167 without breaking other consumers.

[#52](https://github.com/lazy-fortran/fx/issues/52) owns concrete test-cost and
oracle cleanup. Remove duplicate assertions, arbitrary waits and repository-doc
conformity tests if found, preserving observable retention/publication/recovery
contracts. The first confirmed redundancy is a repeated retained-blob assertion
in `test_immutable_leases` whose name incorrectly claims absence of a delete API.
The prior assertion already follows the complete lifecycle and stays intact.
The controller integrated the duplicate removal at `3b0daf9` and native Git-free
source-export inventory at `54b3c5c`. Independent Luna review found no blocker;
the exact combined native lease/inventory gate passes 2/2 in 0.80 seconds with
pinned driver `f31d0faa`. This is an initial slice, not a completed suite-wide
quality audit. No special Gremlin Git artifact.

[#53](https://github.com/lazy-fortran/fx/issues/53) removes production watcher
test failure counters/descriptor entry points; #51 owns immutable-store hooks.
Keep test-only measurement and real external errors. Do not recreate internal
phases using interception frameworks. [#54](https://github.com/lazy-fortran/fx/issues/54)
provides the shared strict JSON codec consumed by fo #150; generic grammar fixes
belong here rather than another fo scanner.

#53's cleanup is integrated at `fc9a1ec`: production watcher controls, including
the old synthetic ENOSPC trigger, are removed. Descriptor measurement lives in
the static test-only filesystem helper; native tests exercise real missing-root
errors, recovery/events and 32 complete start/stop cycles. The combined Linux
watch/lifecycle gate passes 2/2 in 0.93s. Forced registration-phase/ENOSPC tests
were removed without recreating interception machinery. Real kernel error paths
remain; the forced windows and this new test slice are not macOS-verified yet.
The platform milestone tracks that remaining evidence.

Luna workers use isolated task worktrees, warm focused behavioral gates and
bounded readiness deadlines. Record before/after work and the concrete faults
still detected. No new permanent quality/doc meta gate, no CI orchestration,
and no unrelated cleanup blocker for small green increments. Two substantive
failed repairs trigger a task-specific Sol handoff, not a team-wide escalation.

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

## Remove compliance meta-testing (#55)

The initial audit found no repository-documentation conformity tests. Further
inspection found an actual implementation-policy meta-test: the 558-line
`test_no_interpreter_gate`, its executable/shebang classifier and dedicated
process-tracing machinery. [#55](https://github.com/lazy-fortran/fx/issues/55)
removes that registered target, its separate CI step and sole-consumer helpers.
Fortran remains the implementation requirement, checked during source review;
no replacement runtime compliance/security framework or doc/layout test is added.
Retain native process/filesystem APIs used by public MCP/store/watch oracles.
Coordinate #51's shared test-support additions, and verify retained behavior with
existing focused native targets. This removes policy enforcement cost without
claiming a measured speedup or deleting meaningful product behavior.

## Delivery gate

Cache changes need a behavioral producer/consumer oracle, forced misses for
each changed input, safe hits for unrelated changes, concurrent identical and
conflicting publication, crash/race stress, and focused fo consumer evidence.
GitHub CI is asynchronous post-submit evidence. Diagnostic/LSP
changes need an end-to-end FortFront-to-client test with success, error,
revision change, and forced child failure. Contract documentation lands with
the change.
