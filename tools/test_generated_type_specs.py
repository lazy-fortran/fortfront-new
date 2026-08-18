#!/usr/bin/env python3
"""Check freshness and independent table contents for intrinsic type specs."""

from pathlib import Path
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[1]
GENERATOR = ROOT / "tools" / "generate_type_specs.py"
SCHEMA = ROOT / "specs" / "frontend-type-spec-v0.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_type_specs_generated.f90"


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="fortfront-type-specs-") as directory:
        output = Path(directory) / "generated"
        subprocess.run(["python3", str(GENERATOR), str(SCHEMA), str(output)],
                       cwd=ROOT, check=True)
        fresh = output / EXPECTED.name
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in type-spec artifact is stale")
        generated = fresh.read_text(encoding="utf-8")
        for spelling in ("integer", "real", "double precision", "complex"):
            if f"  {spelling} :: " not in generated:
                raise AssertionError(f"generated table omitted {spelling!r}")
    print("frontend type-spec generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
