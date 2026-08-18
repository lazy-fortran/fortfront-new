"""Check freshness and the declarative R501 witness contents."""

from pathlib import Path
import tempfile

from generate_program_envelope import parse, render


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT / "specs" / "frontend-program-envelope-v0.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_program_envelope_generated.f90"


def main() -> None:
    tokens, witness = parse(SCHEMA.read_text(encoding="utf-8"))
    with tempfile.TemporaryDirectory(prefix="fortfront-envelope-") as directory:
        fresh = Path(directory) / EXPECTED.name
        fresh.write_text(render(tokens, witness), encoding="utf-8")
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in program-envelope generated Fortran is stale")

    generated = EXPECTED.read_text(encoding="utf-8")
    for value in ("program_envelope_program_witness", "R501", "J3-24-007", "53_int64"):
        if value not in generated:
            raise AssertionError(f"generated witness omitted {value!r}")
    print("generated program envelope freshness and witness checks: ok")


if __name__ == "__main__":
    main()
