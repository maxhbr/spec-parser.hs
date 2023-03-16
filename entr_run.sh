#!/usr/bin/env bash
set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

entr_task() (
  set -ex
  stack build
  # stack test

  stack exec spec-parser-exe
)
export -f entr_task

while sleep 1; do
  cat <<EOF | entr -dr bash -c entr_task
$(find src -iname '*.hs')
$(find test -iname '*.hs')
$(find app -iname '*.hs')
EOF
done
