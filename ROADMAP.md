# Fx goals

Fx supplies reusable infrastructure for Fo and other consumers. Apply
[goals and architectural freedom](https://github.com/lazy-fortran/fo/blob/main/doc/GOAL_DRIVEN_DEVELOPMENT.md):
choose the smallest adequate design and defer internal architecture until needed.
Public consumer contracts and independently correct behavior remain binding.

## Current priorities

- Support the working Fo resident loop with cheap correct storage/cache use
  ([#57](https://github.com/lazy-fortran/fx/issues/57)). Repair demonstrated
  consumer costs and recheck the exact Fo consumer; no speculative cache layer.
- Substantially reduce maintained production/test/support/documentation through
  [Fo #205](https://github.com/lazy-fortran/fo/issues/205) and #52 while retaining
  useful independent failure detection.
- Keep strict JSON, filesystem events, publication and owned resource behavior
  correct for current consumers (#54, #53, #51).
- Reclaim unneeded immutable material safely and with bounded maintenance (#44),
  enabling deletion only after live consumer ownership is fully covered.

## Required outcomes

| Area | Success |
| --- | --- |
| Shared reuse | Exact immutable bytes and complete result graphs; conflicts are explicit |
| Publication/recovery | Concurrent or interrupted writers cannot create partial reusable success |
| Retention | Live/retained work survives cleanup; unused material can be reclaimed safely |
| Warm operation | Valid local results avoid needless initialization, revalidation and metadata writes |
| JSON | Strict lossless values and truthful errors remain compatible across consumers |
| Watch/process services | Useful events and bounded lifecycle cleanup preserve unrelated work |
| Native tests | Independent meaningful failures remain covered without production fixture policy |

Held-FD materialization (`537a165`) and warm local preflight (`76bdfbd`, main
through `c8fb7ef`) passed their owning checks and original Fo consumer checks.
This does not establish final end-to-end speed. Earlier storage, watcher,
JSON and native-test deliveries should be reused; remaining open issues are
their actual unresolved goals, not instructions to recreate finished work.

## Delivery

Use the exact controller-supplied Fo driver and the smallest affected native
oracle. Preserve bytes/modes, companions, binding conflicts, concurrent use and
crash durability while simplifying implementation. Recheck the original consumer
after a provider change. Push locally verified increments without waiting for CI
or broad benchmarks. Performance measurements are advisory independent evidence.

The initial collection-disabled boundary remains until protection for all
active paths is demonstrated. The collector's mechanism, metadata layout and
cache handle design are agent decisions, not fixed recipes.

### 2026-10-05 bounded lease publication repair

Fo Gremlin's shared lease snapshot reached 64,962 rows against the prior
65,536-row parser limit. FFC generation capture then failed while acquiring
publication leases for its complete input closure. An isolated independent
reproducer started with 65,000 existing root rows and acquired a 1,000-object
publication lease: the prior implementation failed
`test_immutable_leases` in Fo Gremlin session
`3967236-1791213594-220184661`, generation
`962a27fa53415c614e35f10bd53e8ab68194126fcf80a6bc1c58ace30ad2d3fb`, with
“large publication fits…” expected status 0, actual 1.

On base `ee897aa9757da2ff50d34bf1b230a229ea7056c7`, source/test patch SHA256
`521590562ab3ef97674da58726068f456ec0cefafde234fb1a60e8b50b2d1d48` raises
the row ceiling while keeping the 16 MiB snapshot bound authoritative. Snapshot
writes enforce that byte ceiling as they stream and remove incomplete temporary
files on failure. The same Gremlin test passed in session
`3995766-1791214120-906510161`, generation
`a01c0236352b1c37efd6a36a36ad207fe9f90c7a3787d98d2dcc52c4c32c785c`; its case
log SHA256 is `be55e3342a0503932555145d20b6190b73436e5b18160a9336e9f752b3dd13d8`.
The Fo candidate containing this provider fix subsequently captured and began
building the current FFC generation, rechecking the original consumer path.

### 2026-10-06 Fo bulk-lease publication path

The FFC resident start then wrote about 25.7 GB in 6m42s while repeatedly
reading Fo's 16 MiB `.fx-metadata/leases` snapshot before any generation was
captured. Fo already held one publication lease for the input closure, but Fx
`immutable_store_put_blob` acquired and released another lease for each missing
blob, rewriting the whole snapshot twice per object.

On base `8df747d7794cf2e6222b6808f25130308b93cf8f`, implementation and oracle
patch SHA256
`23b46b77b75c95860bc5082498022f973a176159a68b728ff0c78ac47050ea67` adds an
optional covering-lease path. Fx validates that the active lease belongs to
this store and names the blob before publishing under it; an uncovered blob is
rejected. The independent test checks exact bytes and no metadata epoch change
for a covered blob, plus rejection without blob or metadata changes outside the
lease scope.

Fo Gremlin `test_immutable_leases` passed 1/1 in session
`3948852-1791265049-118683619`, generation
`f0a721ee55b2ec019bc3381ba6dd56a713882e971d8ede0e9edf9f4f74b97374`, using
driver SHA256
`f4eea47357a42e472178c399836184720315cf9f18dbf5f16aaa7db34f8c03c0`. The case
log SHA256 is
`a99880657651cbaac8637dc810ddc5169d3d74ae23980c7c8864c40c3f97b849`. Next,
rebuild Fo with the new provider and recheck generation capture on the FFC
consumer before closing this #57 increment.

Earlier detailed evidence remains at
[the pre-revision roadmap](https://github.com/lazy-fortran/fx/blob/c8fb7efeb9b9d98cb9bbb06563b5f20c46d80483/ROADMAP.md).
Fo's [PLAN](https://github.com/lazy-fortran/fo/blob/main/PLAN.md) orders the consumer
program; Fx does not acquire compiler, agent or CI scheduling policy.
