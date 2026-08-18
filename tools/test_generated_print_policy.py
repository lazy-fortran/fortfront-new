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
        "print_policy_output_5_value = 11_int64",
        "print_policy_output_6_value = 12_int64",
        "print_policy_output_7_value = 13_int64",
        "print_policy_output_8_value = 14_int64",
        "print_policy_output_9_value = 15_int64",
        "print_policy_output_10_value = 16_int64",
        "12.6.1", "12.6.2.2", "12.6.3",
        "242_int64", "244_int64", "248_int64",
        "print_policy_variable_value_6 = 9_int64",
        "value%output_count > 100_int64",
    ):
        if required not in generated:
            raise AssertionError(f"generated print policy omitted {required!r}")
    print("print policy generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
