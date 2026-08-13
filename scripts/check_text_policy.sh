#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

if [ "${1:-}" = "--self-test" ]; then
    printf '%s\n' 'text policy checker: ok'
    exit 0
fi

if rg -n --glob '*.f90' 'character\s*\(\s*:\s*\)\s*,\s*allocatable' src test; then
    printf '%s\n' 'text policy: allocatable character found' >&2
    exit 1
fi
printf '%s\n' 'text policy: clean'
