#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
workdir="$repo_root/src/programmable-tokens-test"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

default_cache_home="${XDG_CACHE_HOME:-$HOME/.cache}"
if [[ ! -w "$default_cache_home" ]]; then
  export XDG_CACHE_HOME="$tmpdir/xdg-cache"
  mkdir -p "$XDG_CACHE_HOME"
fi

extract_table() {
  awk '
    function trim(s) {
      sub(/^[[:space:]]+/, "", s)
      sub(/[[:space:]]+$/, "", s)
      return s
    }

    index($0, " | ") {
      column_count = split($0, columns, / \| /)
      case_name = trim(columns[1])

      if (case_name == "Case" || column_count < 5) {
        next
      }

      result = trim(columns[2])
      cpu = trim(columns[3])
      mem = trim(columns[5])

      print case_name "\t" result "\t" cpu "\t" mem
    }
  ' "$1"
}

cd "$workdir"

cabal build benchmark-onchain-scripts benchmark-onchain-aiken-scripts >/dev/null

plutarch_bin="$(cabal list-bin benchmark-onchain-scripts)"
aiken_bin="$(cabal list-bin benchmark-onchain-aiken-scripts)"

"$plutarch_bin" > "$tmpdir/plutarch.out"
"$aiken_bin" > "$tmpdir/aiken.out"

extract_table "$tmpdir/plutarch.out" > "$tmpdir/plutarch.tsv"
extract_table "$tmpdir/aiken.out" > "$tmpdir/aiken.tsv"

awk -F '\t' '
  function as_number(s,    n) {
    n = s
    gsub(/,/, "", n)
    return n + 0
  }

  BEGIN {
    OFS = "\t"
    print "Case", "Plutarch", "Aiken", "Plutarch CPU", "Aiken CPU", "CPU x", "Plutarch Mem", "Aiken Mem", "Mem x"
  }

  NR == FNR {
    case_name = $1
    order[++count] = case_name
    plutarch_result[case_name] = $2
    plutarch_cpu[case_name] = $3
    plutarch_mem[case_name] = $4
    next
  }

  {
    case_name = $1
    aiken_result[case_name] = $2
    aiken_cpu[case_name] = $3
    aiken_mem[case_name] = $4
  }

  END {
    for (i = 1; i <= count; i++) {
      case_name = order[i]
      p_cpu = as_number(plutarch_cpu[case_name])
      a_cpu = as_number(aiken_cpu[case_name])
      p_mem = as_number(plutarch_mem[case_name])
      a_mem = as_number(aiken_mem[case_name])

      cpu_ratio = p_cpu == 0 ? "n/a" : sprintf("%.2fx", a_cpu / p_cpu)
      mem_ratio = p_mem == 0 ? "n/a" : sprintf("%.2fx", a_mem / p_mem)

      print \
        case_name, \
        plutarch_result[case_name], \
        aiken_result[case_name], \
        plutarch_cpu[case_name], \
        aiken_cpu[case_name], \
        cpu_ratio, \
        plutarch_mem[case_name], \
        aiken_mem[case_name], \
        mem_ratio
    }

    for (case_name in aiken_cpu) {
      if (!(case_name in plutarch_cpu)) {
        print case_name, "-", aiken_result[case_name], "-", aiken_cpu[case_name], "n/a", "-", aiken_mem[case_name], "n/a"
      }
    }
  }
' "$tmpdir/plutarch.tsv" "$tmpdir/aiken.tsv" > "$tmpdir/diff.tsv"

printf 'Plutarch vs Aiken ex-unit comparison\n'

if command -v column >/dev/null 2>&1; then
  column -t -s $'\t' "$tmpdir/diff.tsv"
else
  cat "$tmpdir/diff.tsv"
fi
