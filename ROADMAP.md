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

The 2026-10-08 LSP increment replaces save/change callbacks with one document
diagnostic callback and migrates Fo in the same delivery. The shared reader
debounces per URI/version, drains queued edits before publication and clears
closed documents. Strict JSON extraction now counts nested array/object
elements correctly. Four owning Linux checks and four Darwin checks pass;
the actual Fo framed consumer passes parser/semantic diagnostics, current
versions, stale-edit rejection, UTF-16 spans and correction/close clearing.
Native Windows transport is implemented; its full Fo consumer gate remains
part of the platform delivery.

Use the exact controller-supplied Fo driver and the smallest affected native
oracle. Preserve bytes/modes, companions, binding conflicts, concurrent use and
crash durability while simplifying implementation. Recheck the original consumer
after a provider change. Push locally verified increments without waiting for CI
or broad benchmarks. Performance measurements are advisory independent evidence.

The bounded collector and automatic maintenance are available. Successful
action publication checks whether maintenance is due; an explicit tick is
available for idle owners. Fo `b3a41e6` calls it at owner start, during idle
periods and on stop, alongside bounded old-generation root compaction.
The collector's mechanism, metadata layout and cache handle design are agent
decisions, not fixed recipes.

Fx `a18ff23` removes orphan `.leases.<number>.<number>.<number>` snapshot
temps on the next immutable lease update while holding the metadata lock. The
focused `test_immutable_leases` Gremlin gate passed 1/1; its independent
fixture checks orphan removal, unrelated-file retention and the authoritative
lease row. This repairs an observed accumulation of about 340 MiB from
interrupted writers without shortening persistent cache lifetime.

### 2026-10-07 bounded immutable collection and action retirement

Fx `39a11a9` adds an explicit age-guarded collector with byte and inode
pressure, inventory and deletion caps, transitive tree marking, and a metadata
epoch recheck before unlink. Fx `0bf3182` adds durable age-guarded retirement
of old action bindings and a once-daily refresh of successful hits. The
integrated focused gate passed `test_immutable_gc`, `test_action_result_store`,
`test_action_result_read_lease`, and `test_action_cache` (4/4). A bounded
automatic action scanner and maintenance trigger are delivered by Fx `271e5cc`.
It scans at most 64 raw action entries per tick, persists a cursor, skips busy
bindings, and runs at most 32 GC deletions per due pass. Its focused native gate
passed `test_action_result_store` and `test_immutable_gc` (2/2). The Fo
integration passed its focused current-Fx 3-case gate.

Fx `3a6ebdf` removed the v1 action cache reader/writer, record module and debug
APIs; its focused gate passed 2/2, and Fo `17893dc` removed the consumers. Fx
`c75ed04` removed unused parser state and compatibility wrappers (3/3 gate).
Fx `e236bd0` adds bounded exact-group compaction of old Fo generation roots.
The focused lease, GC and compaction gate passed 3/3. Two 12-child groups
shrunk an isolated snapshot from 1,951 to 469 bytes; active and changed groups
were skipped. Public Fo resident session `533484-1791356967-930118364`
migrated a real group: compacted old Fo groups fell from 178 to 177 before the
owner stopped. Further bounded idle passes remain.

### 2026-10-07 immutable publication protection

Fx `dd92af4` now acquires a publication lease before accepting an existing blob,
protects a tree and its direct children while verifying and publishing it, and
keeps output blobs leased until their action graph is committed. The exact
candidate passed the four focused publication/action-result cases in Fo Gremlin
generation `c4b1d12d8531b0379f2af1cbda7de5d0388e662b384307ada1eab4d5c3963e58`
with zero failures and a green local gate. On integrated Fx `main`, Fo's
`test_gremlin_manifest` and `test_gremlin_execution_view` passed 2/2 with the
installed driver SHA256
`2eda4f4ac3d7a04f550d43d38123f5fe8937f9a235a0b306a0d2f822eb834254`.
Fo now releases per-materialization roots after explicit safe generation prune.

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

### 2026-10-06 compact lease snapshot for Fo capture

The original-cache FFC recheck reached 76,287 lease rows and a 16,753,072-byte
snapshot, just below the 16 MiB byte limit. 65,596 retained `fo-generation`
roots accounted for 14,955,888 bytes, with repeated owner/start/reason fields.
Generation capture then failed before any FFC test ran with `cannot protect
generation objects during publication` (Fo Gremlin session
`4038897-1791266815-690662295`, generation
`3c9a6a5cce7230934433a0699a9b50f980f91e02b5b453f1f28d41201c462ded`). The row
count was below 262,144; the byte limit was saturated by retained roots.

On base `b5b553b7dbbd30bd6630cae8f50a0891cb2ed057`, source/test patch SHA256
`a541abf28afd14ed0e4de4b094389f14f0f33ce5ed64ec3d4b2d762367e1525d` writes
long consecutive groups in `fxleases2` form: one shared lease scope followed
by the object IDs. Reads accept both v1 and v2, expanding groups into the same
logical rows before lease operations. The logical row ceiling remains 262,144
and the on-disk snapshot stays bounded at 16 MiB. The independent fixture
starts with a valid v1 snapshot of 16,663,492 bytes. Its 16,272 long roots each
have 1,023-byte row text, within the previous 1,024-byte reader limit. Adding
the 1,000 publication rows would produce 16,793,492 bytes and exceed the cap.
The API oracle acquires and releases graph leases for all 16 root groups,
releases each exact root identity, checks absence afterward, and confirms an
unrelated sentinel graph remains readable.

Fo Gremlin `test_immutable_leases` passed 1/1 in session
`4098610-1791268859-117803425`, generation
`70ce0b3ef51375bb95c5b1ba13f7a56e061da949d892504cf67c04cfd25b78fb`, with
driver SHA256
`1b31346c237ad759c3d5cf459435dc0f29a9b71b1de6ab9b54f95daca883a25f`. The case
log SHA256 is
`486a0c69a4eafc058585c63e30793dcac70f6449f6a9501ac0f87393bcc79079`.
Older Fx binaries reject the v2 header and fail closed, so all active consumers
sharing this lease store must use the updated provider before it writes a v2
snapshot. Next, rebuild Fo with this provider and recheck the original FFC
capture plus its four required compiler targets and 32-sample pilot.

Earlier detailed evidence remains at
[the pre-revision roadmap](https://github.com/lazy-fortran/fx/blob/c8fb7efeb9b9d98cb9bbb06563b5f20c46d80483/ROADMAP.md).
Fo's [PLAN](https://github.com/lazy-fortran/fo/blob/main/PLAN.md) orders the consumer
program; Fx does not acquire compiler, agent or CI scheduling policy.
