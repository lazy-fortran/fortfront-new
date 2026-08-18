#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$root/tools/generate_program_envelope.py" \
    "$root/specs/frontend-program-envelope-v0.sxs" "$root/src/generated"
