from pathlib import Path
import tempfile

from generate_program_unit_v2_envelope import parse, render


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT / "specs" / "frontend-program-unit-v2-envelope.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_program_unit_v2_envelope_generated.f90"


def main() -> None:
    values = parse(SCHEMA.read_text(encoding="utf-8"))
    with tempfile.TemporaryDirectory(prefix="fortfront-envelope-v2-") as directory:
        fresh = Path(directory) / EXPECTED.name
        fresh.write_text(render(values), encoding="utf-8")
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in v2 envelope generated Fortran is stale")
    generated = EXPECTED.read_text(encoding="utf-8")
    for value in ("program-unit-v2", "execution-part", "R509", "J3-24-007", "98_int64"):
        if value not in generated:
            raise AssertionError(f"generated v2 envelope omitted {value!r}")
    print("generated program-unit-v2 envelope freshness and witness checks: ok")


if __name__ == "__main__":
    main()
