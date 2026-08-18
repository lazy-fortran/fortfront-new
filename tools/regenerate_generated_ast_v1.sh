#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
schema="$root/../lazy-fortran-new/contracts/frontend-ast-v1.sxs"
output="$root/src/generated"

python3 "$root/tools/generate_ast.py" "$schema" "$output"
python3 "$root/tools/generate_type_specs.py" \
    "$root/specs/frontend-type-spec-v0.sxs" "$output"
python3 "$root/tools/generate_typed_declaration_policy.py" \
    "$root/specs/frontend-typed-declaration-policy-v0.sxs" "$output"
