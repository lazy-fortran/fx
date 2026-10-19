#!/bin/bash
# run_stress.sh workers BIN STORE SCRATCH NWORKERS SECONDS NKEYS GRACE CAP EVERY KILLS
#   Runs NWORKERS store_stress workers against STORE for SECONDS; KILLS times
#   a random worker is killed with SIGKILL mid-run and replaced. Prints the
#   summed counters; exit 3 on any wrong hit.
# run_stress.sh check BIN STORE SCRATCH NKEYS GRACE
#   After all workers on all hosts ended: verify every key, check that each
#   blob/tree file hashes to its name, then (after the grace period) evict
#   with a 1-byte cap and report what remains (expected: nothing).
set -uo pipefail
mode=$1; bin=$2; store=$3; scratch=$4; shift 4
export STRESS_SCRATCH=$scratch
mkdir -p "$scratch" "$store"
host=$(hostname -s)
if [ "$mode" = workers ]; then
    n=$1; secs=$2; keys=$3; grace=$4; cap=$5; every=$6; kills=$7
    pids=(); seed=$(( $(date +%s) % 100000 * 100 + $$ % 97 ))
    start() { "$bin" worker "$store" $((seed + $1)) "$2" "$keys" "$grace" "$cap" \
        "$every" > "$scratch/w-$host-$1.log" 2>&1 & pids[$1]=$!; }
    for i in $(seq 1 "$n"); do start "$i" "$secs"; done
    t0=$(date +%s)
    for k in $(seq 1 "$kills"); do
        sleep $(( secs / (kills + 1) ))
        v=$(( RANDOM % n + 1 )); kill -9 "${pids[$v]}" 2>/dev/null
        wait "${pids[$v]}" 2>/dev/null
        echo "killed worker $v on $host"
        left=$(( secs - ($(date +%s) - t0) )); [ $left -lt 2 ] && left=2
        start "$((n + k))" "$left"; pids[$v]=${pids[$((n + k))]}
        unset "pids[$((n + k))]"
    done
    rc=0; for p in "${pids[@]}"; do wait "$p" || rc=$?; done
    cat "$scratch"/w-$host-*.log | awk -v h="$host" '
        /^WORKER/ { for (i = 2; i <= NF; i++) { split($i, a, "="); s[a[1]] += a[2] } }
        /^WRONG/ { print h, $0 }
        END { printf "%s SUM", h; for (k in s) printf " %s=%d", k, s[k]; print "" }'
    grep -h "^WRONG\|INIT_FAILED\|Error\|error" "$scratch"/w-$host-*.log | head -5
    exit $rc
fi
if [ "$mode" = check ]; then
    keys=$1; grace=$2
    "$bin" verify "$store" "$keys"; vrc=$?
    bad=0; total=0
    for f in "$store"/blobs/sha256/*/* "$store"/trees/sha256/*/*; do
        [ -f "$f" ] || continue
        case $(basename "$f") in .*) continue ;; esac
        total=$((total + 1))
        [ "$(sha256sum < "$f" | cut -c1-64)" = "$(basename "$f")" ] || {
            bad=$((bad + 1)); echo "CORRUPT $f"; }
    done
    echo "FSCK objects=$total corrupt=$bad"
    temps=$(find "$store" -name '.fx-owned-*' -o -name '.tmp.*' -o -name '.fx-copy-*' | wc -l)
    echo "TEMPS before final eviction: $temps"
    sleep $((grace + 2))
    "$bin" evict "$store" "$grace" 1
    left=$(find "$store"/blobs "$store"/trees "$store"/actions -type f \
        ! -name '*.lock' 2>/dev/null | wc -l)
    echo "RESIDUAL files after full eviction: $left"
    [ $vrc -eq 0 ] && [ $bad -eq 0 ] && [ "$left" -eq 0 ]
    exit $?
fi
exit 2
