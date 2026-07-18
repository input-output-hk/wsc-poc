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

extract_totals_table() {
  awk '
    function trim(s) {
      sub(/^[[:space:]]+/, "", s)
      sub(/[[:space:]]+$/, "", s)
      return s
    }

    $0 == "Transaction totals" {
      in_table = 1
      next
    }

    $0 == "Per-script breakdown" {
      exit
    }

    in_table && index($0, " | ") {
      column_count = split($0, columns, / \| /)
      case_name = trim(columns[1])

      if (case_name == "Case" || column_count < 5) {
        next
      }

      result = trim(columns[2])
      cpu = trim(columns[3])
      mem = trim(columns[5])
      scripts = trim(columns[7])

      print case_name "\t" result "\t" cpu "\t" mem "\t" scripts
    }
  ' "$1"
}

extract_breakdown_table() {
  awk '
    function trim(s) {
      sub(/^[[:space:]]+/, "", s)
      sub(/[[:space:]]+$/, "", s)
      return s
    }

    $0 == "Per-script breakdown" {
      in_table = 1
      next
    }

    in_table && index($0, " | ") {
      column_count = split($0, columns, / \| /)
      case_name = trim(columns[1])

      if (case_name == "Case" || column_count < 5) {
        next
      }

      script_name = trim(columns[2])
      count = trim(columns[3])
      cpu = trim(columns[4])
      mem = trim(columns[5])

      print case_name "\t" script_name "\t" count "\t" cpu "\t" mem
    }
  ' "$1"
}

cd "$workdir"

cabal build benchmark-onchain-scripts benchmark-onchain-aiken-scripts >/dev/null

plutarch_bin="$(cabal list-bin benchmark-onchain-scripts)"
aiken_bin="$(cabal list-bin benchmark-onchain-aiken-scripts)"

"$plutarch_bin" > "$tmpdir/plutarch.out"
"$aiken_bin" > "$tmpdir/aiken.out"

extract_totals_table "$tmpdir/plutarch.out" > "$tmpdir/plutarch.tsv"
extract_totals_table "$tmpdir/aiken.out" > "$tmpdir/aiken.tsv"
extract_breakdown_table "$tmpdir/plutarch.out" > "$tmpdir/plutarch-breakdown.tsv"
extract_breakdown_table "$tmpdir/aiken.out" > "$tmpdir/aiken-breakdown.tsv"

max_line="$(awk '/^Max tx ex units:/ { print; exit }' "$tmpdir/plutarch.out")"
read -r max_cpu_text max_mem_text < <(
  awk '
    match($0, /^Max tx ex units: CPU ([0-9,]+) \| Mem ([0-9,]+)/, matches) {
      print matches[1], matches[2]
      exit
    }
  ' "$tmpdir/plutarch.out"
)
max_cpu="${max_cpu_text//,/}"
max_mem="${max_mem_text//,/}"

awk -F '\t' '
  function as_number(s,    n) {
    n = s
    gsub(/,/, "", n)
    return n + 0
  }

  BEGIN {
    OFS = "\t"
    print "Case", "Plutarch", "Aiken", "Plutarch CPU", "Aiken CPU", "CPU x", "Plutarch Mem", "Aiken Mem", "Mem x", "Plutarch Scripts", "Aiken Scripts"
  }

  NR == FNR {
    case_name = $1
    order[++count] = case_name
    plutarch_result[case_name] = $2
    plutarch_cpu[case_name] = $3
    plutarch_mem[case_name] = $4
    plutarch_scripts[case_name] = $5
    next
  }

  {
    case_name = $1
    aiken_result[case_name] = $2
    aiken_cpu[case_name] = $3
    aiken_mem[case_name] = $4
    aiken_scripts[case_name] = $5
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
        mem_ratio, \
        plutarch_scripts[case_name], \
        aiken_scripts[case_name]
    }

    for (case_name in aiken_cpu) {
      if (!(case_name in plutarch_cpu)) {
        print case_name, "-", aiken_result[case_name], "-", aiken_cpu[case_name], "n/a", "-", aiken_mem[case_name], "n/a", "-", aiken_scripts[case_name]
      }
    }
  }
' "$tmpdir/plutarch.tsv" "$tmpdir/aiken.tsv" > "$tmpdir/diff.tsv"

awk -F '\t' '
  function as_number(s,    n) {
    n = s
    gsub(/,/, "", n)
    return n + 0
  }

  BEGIN {
    OFS = "\t"
    print "Case", "Script", "Plutarch Count", "Aiken Count", "Plutarch CPU", "Aiken CPU", "CPU x", "Plutarch Mem", "Aiken Mem", "Mem x"
  }

  NR == FNR {
    key = $1 SUBSEP $2
    order[++count] = key
    case_name[key] = $1
    script_name[key] = $2
    plutarch_count[key] = $3
    plutarch_cpu[key] = $4
    plutarch_mem[key] = $5
    next
  }

  {
    key = $1 SUBSEP $2
    aiken_case_name[key] = $1
    aiken_script_name[key] = $2
    aiken_count[key] = $3
    aiken_cpu[key] = $4
    aiken_mem[key] = $5
  }

  END {
    for (i = 1; i <= count; i++) {
      key = order[i]
      p_cpu = as_number(plutarch_cpu[key])
      a_cpu = as_number(aiken_cpu[key])
      p_mem = as_number(plutarch_mem[key])
      a_mem = as_number(aiken_mem[key])

      cpu_ratio = p_cpu == 0 ? "n/a" : sprintf("%.2fx", a_cpu / p_cpu)
      mem_ratio = p_mem == 0 ? "n/a" : sprintf("%.2fx", a_mem / p_mem)

      print \
        case_name[key], \
        script_name[key], \
        plutarch_count[key], \
        aiken_count[key], \
        plutarch_cpu[key], \
        aiken_cpu[key], \
        cpu_ratio, \
        plutarch_mem[key], \
        aiken_mem[key], \
        mem_ratio
    }

    for (key in aiken_cpu) {
      if (!(key in plutarch_cpu)) {
        print \
          aiken_case_name[key], \
          aiken_script_name[key], \
          "-", \
          aiken_count[key], \
          "-", \
          aiken_cpu[key], \
          "n/a", \
          "-", \
          aiken_mem[key], \
          "n/a"
      }
    }
  }
' "$tmpdir/plutarch-breakdown.tsv" "$tmpdir/aiken-breakdown.tsv" > "$tmpdir/breakdown-diff.tsv"

printf 'Plutarch vs Aiken ex-unit comparison\n'
printf '%s\n\n' "$max_line"

awk -F '\t' -v max_cpu="$max_cpu" -v max_mem="$max_mem" '
  function as_number(s,    n) {
    n = s
    gsub(/,/, "", n)
    return n + 0
  }

  function ratio_text(numerator, denominator) {
    return denominator == 0 ? "n/a" : sprintf("%.2fx", numerator / denominator)
  }

  function percent_text(value, max_value) {
    return max_value == 0 ? "n/a" : sprintf("%.2f%%", (value * 100) / max_value)
  }

  function append_case(case_name) {
    if (!(case_name in seen_case)) {
      seen_case[case_name] = 1
      case_order[++case_count] = case_name
    }
  }

  function append_contract(case_name, contract_name,    key) {
    key = case_name SUBSEP contract_name
    if (!(key in seen_contract)) {
      seen_contract[key] = 1
      contract_order[case_name, ++contract_count[case_name]] = contract_name
    }
  }

  FNR == NR {
    if (FNR == 1) {
      next
    }

    case_name = $1
    append_case(case_name)
    plutarch_result[case_name] = $2
    aiken_result[case_name] = $3
    plutarch_cpu[case_name] = $4
    aiken_cpu[case_name] = $5
    plutarch_mem[case_name] = $7
    aiken_mem[case_name] = $8
    plutarch_scripts[case_name] = $10
    aiken_scripts[case_name] = $11
    next
  }

  {
    if (FNR == 1) {
      next
    }

    case_name = $1
    contract_name = $2
    append_case(case_name)
    append_contract(case_name, contract_name)
    plutarch_count[case_name, contract_name] = $3
    aiken_count[case_name, contract_name] = $4
    plutarch_contract_cpu[case_name, contract_name] = $5
    aiken_contract_cpu[case_name, contract_name] = $6
    plutarch_contract_mem[case_name, contract_name] = $8
    aiken_contract_mem[case_name, contract_name] = $9
  }

  END {
    separator = "================================================================================"

    for (i = 1; i <= case_count; i++) {
      case_name = case_order[i]
      p_cpu_num = as_number(plutarch_cpu[case_name])
      a_cpu_num = as_number(aiken_cpu[case_name])
      p_mem_num = as_number(plutarch_mem[case_name])
      a_mem_num = as_number(aiken_mem[case_name])

      scenario_status = (plutarch_result[case_name] == "PASS" && aiken_result[case_name] == "PASS") ? "[PASS]" : "[FAIL]"

      print separator
      print "Scenario: " case_name " " scenario_status
      print "Results: Plutarch " plutarch_result[case_name] " | Aiken " aiken_result[case_name]
      print "Scripts: Plutarch " plutarch_scripts[case_name] " | Aiken " aiken_scripts[case_name]

      if (contract_count[case_name] == 0) {
        print "Contracts: none"
      } else {
        print "Contracts:"
        for (j = 1; j <= contract_count[case_name]; j++) {
          contract_name = contract_order[case_name, j]
          print "  - " contract_name
        }
      }

      if (contract_count[case_name] > 0) {
        print "Contract totals:"
        for (j = 1; j <= contract_count[case_name]; j++) {
          contract_name = contract_order[case_name, j]
          p_contract_cpu_num = as_number(plutarch_contract_cpu[case_name, contract_name])
          a_contract_cpu_num = as_number(aiken_contract_cpu[case_name, contract_name])
          p_contract_mem_num = as_number(plutarch_contract_mem[case_name, contract_name])
          a_contract_mem_num = as_number(aiken_contract_mem[case_name, contract_name])

          print \
            "  - " contract_name " (" plutarch_count[case_name, contract_name] " vs " aiken_count[case_name, contract_name] "):" \
            " CPU " plutarch_contract_cpu[case_name, contract_name] " vs " aiken_contract_cpu[case_name, contract_name] \
            " | CPU x " ratio_text(a_contract_cpu_num, p_contract_cpu_num) \
            " | Mem " plutarch_contract_mem[case_name, contract_name] " vs " aiken_contract_mem[case_name, contract_name] \
            " | Mem x " ratio_text(a_contract_mem_num, p_contract_mem_num)
        }
      }

      print "Totals:"
      print "  CPU: " plutarch_cpu[case_name] " vs " aiken_cpu[case_name] " | CPU x: " ratio_text(a_cpu_num, p_cpu_num)
      print "  Mem: " plutarch_mem[case_name] " vs " aiken_mem[case_name] " | Mem x: " ratio_text(a_mem_num, p_mem_num)
      print "  CPU %: " percent_text(p_cpu_num, max_cpu) " vs " percent_text(a_cpu_num, max_cpu) " | Mem %: " percent_text(p_mem_num, max_mem) " vs " percent_text(a_mem_num, max_mem)

      if (i < case_count) {
        print ""
      }
    }
  }
' "$tmpdir/diff.tsv" "$tmpdir/breakdown-diff.tsv"

# ---------------------------------------------------------------------------
# Script size comparison (serialised/deployed bytes). Extracted directly from
# the Per-script breakdown "Size" column of each raw output, deduplicated per
# script. x = Aiken/Plutarch, so x > 1.00 means the Plutarch script is smaller.
# ---------------------------------------------------------------------------
extract_sizes() {
  awk -F ' \\| ' '
    function trim(s) { sub(/^[[:space:]]+/, "", s); sub(/[[:space:]]+$/, "", s); return s }
    $0 == "Per-script breakdown" { in_table = 1; next }
    in_table && index($0, " | ") && NF >= 6 {
      script = trim($2)
      size = trim($6)
      if (script == "Script" || script == "") next
      if (!(script in seen)) { seen[script] = size; order[++n] = script }
    }
    END { for (i = 1; i <= n; i++) print order[i] "\t" seen[order[i]] }
  ' "$1"
}

extract_sizes "$tmpdir/plutarch.out" > "$tmpdir/plutarch-size.tsv"
extract_sizes "$tmpdir/aiken.out" > "$tmpdir/aiken-size.tsv"

printf '\n\n'
printf 'Script size comparison (serialised bytes; x = Aiken/Plutarch, >1.00 = Plutarch smaller)\n'
printf '%s\n' "================================================================================"

awk -F '\t' '
  NR == FNR { p_size[$1] = $2; order[++count] = $1; next }
  { a_size[$1] = $2 }
  END {
    printf "%-28s %14s %14s %10s\n", "Script", "Plutarch", "Aiken", "x"
    never_larger = 1
    for (i = 1; i <= count; i++) {
      script = order[i]
      p = p_size[script] + 0
      a = (script in a_size) ? a_size[script] + 0 : 0
      ratio = (p > 0 && a > 0) ? sprintf("%.2fx", a / p) : "n/a"
      # Only a genuine regression (Plutarch strictly larger) is a violation.
      # Trivial equal stubs (transferLogic/issuer/minting/alwaysSucceeds, 6 bytes
      # both) are benchmark placeholders, not real counterpart validators.
      flag = (a > 0 && p > a) ? "  <-- LARGER (violation)" : ""
      if (a > 0 && p > a) never_larger = 0
      printf "%-28s %14d %14d %10s%s\n", script, p, a, ratio, flag
    }
    printf "%s\n", "--------------------------------------------------------------------------------"
    printf "Plutarch never larger than Aiken on any counterpart script: %s\n", (never_larger ? "YES" : "NO")
  }
' "$tmpdir/plutarch-size.tsv" "$tmpdir/aiken-size.tsv"
