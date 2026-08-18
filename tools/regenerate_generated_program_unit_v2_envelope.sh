#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$root/tools/generate_program_unit_v2_envelope.py" \
    "$root/specs/frontend-program-unit-v2-envelope.sxs" "$root/src/generated"
