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

Earlier detailed evidence remains at
[the pre-revision roadmap](https://github.com/lazy-fortran/fx/blob/c8fb7efeb9b9d98cb9bbb06563b5f20c46d80483/ROADMAP.md).
Fo's [PLAN](https://github.com/lazy-fortran/fo/blob/main/PLAN.md) orders the consumer
program; Fx does not acquire compiler, agent or CI scheduling policy.
