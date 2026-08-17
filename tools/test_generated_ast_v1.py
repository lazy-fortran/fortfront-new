#!/usr/bin/env python3
"""Check that the checked-in v1 AST is mechanically reproducible."""

from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT.parent / "lazy-fortran-new" / "contracts" / "frontend-ast-v1.sxs"
GENERATOR = ROOT / "tools" / "generate_ast.py"
EXPECTED = ROOT / "src" / "generated" / "frontend_ast_v1_generated.f90"


def run(*args: str, expected: int = 0) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(args, cwd=ROOT, text=True, capture_output=True)
    if result.returncode != expected:
        raise AssertionError(
            f"command failed unexpectedly: {' '.join(args)}\n"
            f"stdout={result.stdout}\nstderr={result.stderr}"
        )
    return result


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="fortfront-generated-v1-") as directory:
        output_dir = Path(directory) / "generated"
        run("python3", str(GENERATOR), str(SCHEMA), str(output_dir))
        fresh = output_dir / EXPECTED.name
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in v1 generated Fortran is stale")

        malformed = Path(directory) / "malformed.sxs"
        malformed.write_text("(schema broken (record x (field name))", encoding="utf-8")
        result = run("python3", str(GENERATOR), str(malformed), str(output_dir), expected=1)
        if "unterminated S-expression" not in result.stderr:
            raise AssertionError("malformed schema diagnostic is not explicit")

        generated = fresh.read_text(encoding="utf-8")
        for required in (
            "type, public :: variable_declaration_t",
            "type(variable_declaration_t) :: variable",
            "variable-declaration",
        ):
            if required not in generated:
                raise AssertionError(f"generated v1 record is missing {required!r}")
    print("frontend AST v1 generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
