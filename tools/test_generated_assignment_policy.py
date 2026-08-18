#!/usr/bin/env python3
"""Check assignment-policy freshness and the declarative literal range."""

from pathlib import Path
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT / "specs" / "frontend-assignment-policy-v0.sxs"
GENERATOR = ROOT / "tools" / "generate_assignment_policy.py"
EXPECTED = ROOT / "src" / "generated" / "frontend_assignment_policy_generated.f90"


def main() -> int:
    schema = SCHEMA.read_text(encoding="utf-8")
    if "(literal-range integer-literal 0 2047)" not in schema:
        raise AssertionError("assignment policy omitted its source literal range")

    with tempfile.TemporaryDirectory(prefix="fortfront-assignment-policy-") as directory:
        subprocess.run(
            ["python3", str(GENERATOR), str(SCHEMA), directory], check=True
        )
        fresh = Path(directory) / EXPECTED.name
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in assignment policy is stale")

    generated = EXPECTED.read_text(encoding="utf-8")
    for required in (
        "assignment_policy_variable_expression_row =",
        "'variable R902 R901 R903'",
        "assignment_policy_integer_literal_min = 0",
        "assignment_policy_integer_literal_max = 2047",
    ):
        if required not in generated:
            raise AssertionError(f"generated policy omitted {required!r}")
    print("assignment policy generator freshness and range checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
