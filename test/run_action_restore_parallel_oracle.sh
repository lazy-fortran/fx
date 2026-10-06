#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
fpm test --target test_action_cache --profile debug
mapfile -t libraries < <(find build -path '*/fx/libfx.a' -type f -print)
if [[ ${#libraries[@]} -ne 1 ]]; then
    printf 'expected one fpm FX archive, found %s\n' "${#libraries[@]}" >&2
    exit 1
fi
archive=${libraries[0]}
module_dir=$(dirname "$(dirname "$archive")")
oracle="${TMPDIR:-/var/tmp}/fx-action-restore-parallel-oracle-$$"
trap 'rm -f "$oracle"' EXIT

gfortran -fopenmp -I "$module_dir" test/action_restore_parallel_oracle.f90 \
    "$archive" -o "$oracle"
OMP_NUM_THREADS=8 "$oracle"
