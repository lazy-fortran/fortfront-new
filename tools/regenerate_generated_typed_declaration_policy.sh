#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$root/tools/generate_typed_declaration_policy.py" \
    "$root/specs/frontend-typed-declaration-policy-v0.sxs" "$root/src/generated"
