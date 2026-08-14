#!/usr/bin/env python3
"""Independent behavioral and freshness test for the frontend AST generator."""

from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCHEMA = ROOT.parent / "lazy-fortran-new" / "contracts" / "frontend-ast-v0.sxs"
GENERATOR = ROOT / "tools" / "generate_ast.py"
EXPECTED = ROOT / "src" / "generated" / "frontend_ast_v0_generated.f90"


def run(*args: str, expected: int = 0) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(args, cwd=ROOT, text=True, capture_output=True)
    if result.returncode != expected:
        raise AssertionError(
            f"command failed unexpectedly: {' '.join(args)}\n"
            f"stdout={result.stdout}\nstderr={result.stderr}"
        )
    return result


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="fortfront-generated-") as directory:
        output_dir = Path(directory) / "generated"
        run("python3", str(GENERATOR), str(SCHEMA), str(output_dir))
        fresh = output_dir / EXPECTED.name
        if fresh.read_bytes() != EXPECTED.read_bytes():
            raise AssertionError("checked-in generated Fortran is stale")

        malformed = Path(directory) / "malformed.sxs"
        malformed.write_text("(schema broken (record x (field name))", encoding="utf-8")
        result = run("python3", str(GENERATOR), str(malformed), str(output_dir), expected=1)
        if "unterminated S-expression" not in result.stderr:
            raise AssertionError("malformed schema diagnostic is not explicit")

        unsupported = Path(directory) / "unsupported.sxs"
        unsupported.write_text(
            "(schema broken (primitive string) (record x (field string)))",
            encoding="utf-8",
        )
        result = run("python3", str(GENERATOR), str(unsupported), str(output_dir), expected=1)
        if "unsupported primitive" not in result.stderr:
            raise AssertionError("unsupported schema diagnostic is not explicit")

        witness = Path(directory) / "witness.f90"
        witness.write_text(
            """program generated_ast_witness
    use frontend_ast_v0_generated, only: program_declaration_t, program_root_t, &
        program_unit_t, source_span_t, generated_ast_to_sx, generated_ast_validate
    implicit none
    type(source_span_t) :: span
    type(program_root_t) :: root
    type(program_declaration_t) :: declaration
    type(program_unit_t) :: unit
    character(len=2048) :: output, message
    logical :: ok

    span%file = 'fixture.f90'
    span%start_byte = 0
    span%end_byte = 10
    span%source_hash = 'abc'
    root%name = 'demo'
    root%span = span
    declaration%declaration_kind = 'program'
    declaration%name = 'demo'
    declaration%span = span
    unit%root = root
    unit%declaration_count = 1
    unit%declaration = declaration
    call generated_ast_to_sx(unit, output, ok, message)
    if (.not. ok) error stop 'valid witness was rejected'
    if (trim(output) /= '(program-unit (root (program-root (name demo) '// &
        '(span (source-span (file fixture.f90) (start-byte 0) '// &
        '(end-byte 10) (source-hash abc))))) (declaration-count 1) '// &
        '(declaration (program-declaration (declaration-kind program) '// &
        '(name demo) '// &
        '(span (source-span (file fixture.f90) (start-byte 0) '// &
        '(end-byte 10) (source-hash abc))))))') error stop 'canonical generated SX changed'

    span%start_byte = -1
    unit%root%span = span
    unit%declaration%span = span
    ok = generated_ast_validate(unit, message)
    if (ok) error stop 'negative span byte was accepted'
    if (trim(message) /= 'negative-source-span-start-byte') error stop 'wrong negative span diagnostic'

    span%start_byte = 5
    span%end_byte = 4
    unit%root%span = span
    unit%declaration%span = span
    ok = generated_ast_validate(unit, message)
    if (ok) error stop 'reversed span was accepted'
    if (trim(message) /= 'invalid-source-span-span') error stop 'wrong span ordering diagnostic'

    span%start_byte = 0
    span%end_byte = 10
    unit%root%span = span
    unit%declaration%span = span
    unit%declaration_count = 0
    ok = generated_ast_validate(unit, message)
    if (ok) error stop 'inconsistent declaration count was accepted'
    if (trim(message) /= 'invalid-program-unit-declaration-count') &
        error stop 'wrong declaration count diagnostic'

    unit%root%name = ''
    ok = generated_ast_validate(unit, message)
    if (ok) error stop 'invalid field value was accepted'
    if (trim(message) /= 'invalid-program-root-name') error stop 'wrong invalid field diagnostic'
    write (*, '(a)') 'generated AST behavioral checks: ok'
end program generated_ast_witness
""",
            encoding="utf-8",
        )
        executable = Path(directory) / "generated_ast_witness"
        (Path(directory) / "mod").mkdir()
        run(
            "gfortran",
            "-std=f2018",
            "-Wall",
            "-Wextra",
            "-J",
            str(Path(directory) / "mod"),
            "-o",
            str(executable),
            str(fresh),
            str(witness),
        )
        run(str(executable))
    print("schema generator behavioral checks: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
