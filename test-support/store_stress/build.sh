#!/bin/bash
# build.sh FX_CHECKOUT OUT_DIR: build store_stress with plain gcc/gfortran
# (no fpm), for hosts that only have a compiler. Module order is found by
# repeated passes.
set -euo pipefail
src=$(cd "$1" && pwd); out=$2
mkdir -p "$out/obj"; cd "$out/obj"
for c in "$src"/src/proc/*.c; do
    gcc -O2 -std=gnu11 -I"$src/include" -c "$c" -o "c_$(basename "$c" .c).o"
done
pending=("$src"/src/*.f90)
for pass in $(seq 1 40); do
    next=()
    for f in "${pending[@]}"; do
        gfortran -O2 -fopenmp -c "$f" -o "$(basename "$f" .f90).o" 2>/dev/null \
            || next+=("$f")
    done
    pending=("${next[@]+"${next[@]}"}")
    [ ${#pending[@]} -eq 0 ] && break
done
if [ ${#pending[@]} -ne 0 ]; then gfortran -O2 -fopenmp -c "${pending[0]}"; exit 1; fi
gfortran -O2 -fopenmp "$src/test-support/store_stress/store_stress.f90" ./*.o \
    -o "$out/store_stress"
echo "built $out/store_stress"
