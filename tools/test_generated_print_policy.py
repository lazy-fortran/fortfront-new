from pathlib import Path
import tempfile

from generate_print_policy import render


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT / "specs" / "frontend-print-policy-v0.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_print_policy_generated.f90"


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="fortfront-print-policy-") as directory:
        fresh = Path(directory) / EXPECTED.name
        fresh.write_text(render(SCHEMA.read_text(encoding="utf-8")), encoding="utf-8")
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in print policy is stale")
    generated = EXPECTED.read_text(encoding="utf-8")
    for required in (
        "R1212", "R1215", "R1217", "print_policy_output_2_value = 8_int64",
        "print_policy_output_3_value = 9_int64",
        "print_policy_output_4_value = 10_int64",
        "12.6.1", "12.6.2.2", "12.6.3",
        "242_int64", "244_int64", "248_int64",
    ):
        if required not in generated:
            raise AssertionError(f"generated print policy omitted {required!r}")
    print("print policy generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
