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

Shared Fortran infrastructure library for `fo` and other toolchain consumers.

## Build and Test

```bash
fo build
fo test
```

No package dependencies; C sources in `src/proc` provide system interfaces.
OpenMP parallel directives appear as comments in routines where parallelism applies.

## Rules

- Fortran implementation with small C OS interfaces; organize them as needed.
- `use ..., only:` before `implicit none`.
- `real(dp)` with `use, intrinsic :: iso_fortran_env, only: dp => real64`.
- All args have `intent`.
- Derived types end in `_t`.
- Keep responsibilities cohesive and code readable; reduce duplication and maintained volume without arbitrary file/procedure limits.
- fprettify: 88 cols, 4-space indent.
