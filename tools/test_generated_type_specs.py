#!/usr/bin/env python3
"""Check freshness and independent table contents for intrinsic type specs."""

from pathlib import Path
import subprocess
import tempfile

from generate_type_specs import SchemaError, parse


ROOT = Path(__file__).resolve().parents[1]
GENERATOR = ROOT / "tools" / "generate_type_specs.py"
SCHEMA = ROOT / "specs" / "frontend-type-spec-v0.sxs"
EXPECTED = ROOT / "src" / "generated" / "frontend_type_specs_generated.f90"


def main() -> int:
    entries = parse(SCHEMA.read_text(encoding="utf-8"))
    expected_rules = {
        "integer": "R705", "real": "R706", "double-precision": "R707",
    }
    for entry in entries:
        name = str(entry["canonical"])
        if name in expected_rules and entry["source_rule"] != expected_rules[name]:
            raise AssertionError(f"source rule for {name!r} is not bound")
        if name == "complex" and any(entry[key] for key in (
                "source_rule", "source_document", "source_clause", "source_page",
                "source_hash")):
            raise AssertionError("complex source rule is not explicitly unbound")
    mutated = SCHEMA.read_text(encoding="utf-8").replace("source-rule R705", "source-rule R799")
    try:
        parse(mutated)
    except SchemaError:
        pass
    else:
        raise AssertionError("mutated source rule was accepted")
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
        if "intrinsic_type_spec_declaration" not in generated:
            raise AssertionError("generated declaration helper is missing")
        for value in ("R705", "R706", "R707", "J3-24-007", "67_int64", "source_hash"):
            if value not in generated:
                raise AssertionError(f"generated source-rule field omitted {value!r}")
        if "declaration(prefix_length + 1:prefix_length + name_length) = trim(name)" \
                not in generated:
            raise AssertionError("generated declaration helper does not append the identifier")
    print("frontend type-spec generator checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
