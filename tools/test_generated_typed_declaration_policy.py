#!/usr/bin/env python3
"""Check freshness and the declarative typed-AST cardinality boundary."""

from pathlib import Path
import tempfile

from generate_typed_declaration_policy import render


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT / "specs" / "frontend-typed-declaration-policy-v0.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_typed_declaration_policy_generated.f90"


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="fortfront-typed-policy-") as directory:
        fresh = Path(directory) / EXPECTED.name
        fresh.write_text(render(SCHEMA.read_text(encoding="utf-8")), encoding="utf-8")
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in typed declaration policy is stale")

    generated = EXPECTED.read_text(encoding="utf-8")
    for required in (
        "typed_program_declaration_cardinality = 1_int64",
        "typed_variable_declaration_cardinality = 1_int64",
        "typed_declaration_policy_matches",
    ):
        if required not in generated:
            raise AssertionError(f"generated policy omitted {required!r}")
    print("typed declaration policy generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
