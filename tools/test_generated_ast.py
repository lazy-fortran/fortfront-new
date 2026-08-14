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
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v0_generated, only: program_declaration_t, program_root_t, &
        program_unit_t, source_span_t, generated_ast_to_sx, generated_ast_validate, &
        generated_ast_visit, generated_ast_visitor_t, generated_ast_kind_count
    implicit none
    type(source_span_t) :: span
    type(program_root_t) :: root
    type(program_declaration_t) :: declaration
    type(program_unit_t) :: unit
    character(len=2048) :: output, message
    character(len=32) :: visited(5)
    integer :: visit_count
    type(generated_ast_visitor_t) :: visitor
    logical :: ok
    integer(int64) :: node_count

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

    visitor%visit_source_span => record_source_span
    visitor%visit_program_root => record_program_root
    visitor%visit_program_declaration => record_program_declaration
    visitor%visit_program_unit => record_program_unit
    visit_count = 0
    call generated_ast_visit(unit, visitor)
    if (visit_count /= 5) error stop 'wrong visitor count'
    if (trim(visited(1)) /= 'program-unit') error stop 'wrong visitor root order'
    if (trim(visited(2)) /= 'program-root') error stop 'wrong visitor nested order'
    if (trim(visited(3)) /= 'source-span') error stop 'wrong visitor first span order'
    if (trim(visited(4)) /= 'program-declaration') error stop 'wrong visitor declaration order'
    if (trim(visited(5)) /= 'source-span') error stop 'wrong visitor second span order'
    visitor = generated_ast_visitor_t()
    visitor%visit_program_unit => record_program_unit
    visit_count = 0
    call generated_ast_visit(unit, visitor)
    if (visit_count /= 1) error stop 'unset visitor callbacks were not optional'

    call generated_ast_kind_count(unit, 'program-unit', node_count, ok, message)
    if (.not. ok .or. node_count /= 1_int64) error stop 'wrong program-unit kind count'
    call generated_ast_kind_count(unit, 'source-span', node_count, ok, message)
    if (.not. ok .or. node_count /= 2_int64) error stop 'wrong nested source-span count'
    call generated_ast_kind_count(unit, 'missing-kind', node_count, ok, message)
    if (.not. ok .or. node_count /= 0_int64) error stop 'missing kind was counted'
    call generated_ast_kind_count(source_span_t(), 'source-span', node_count, ok, message)
    if (.not. ok .or. node_count /= 1_int64) error stop 'empty record was not counted'
    call generated_ast_kind_count(kind='source-span', count=node_count, ok=ok, message=message)
    if (.not. ok .or. node_count /= 0_int64) error stop 'empty input was not counted as zero'
    call generated_ast_kind_count(unit, '', node_count, ok, message)
    if (ok .or. trim(message) /= 'empty-generated-record-kind') &
        error stop 'empty kind diagnostic changed'
    call generated_ast_kind_count(0, 'program-unit', node_count, ok, message)
    if (ok .or. trim(message) /= 'unsupported-generated-record-kind-query') &
        error stop 'unsupported kind query diagnostic changed'
    write (*, '(a)') 'generated AST behavioral checks: ok'

contains

    subroutine append_visit(label)
        character(len=*), intent(in) :: label

        visit_count = visit_count + 1
        visited(visit_count) = label
    end subroutine append_visit

    subroutine record_source_span(value)
        type(source_span_t), intent(in) :: value

        call append_visit('source-span')
    end subroutine record_source_span

    subroutine record_program_root(value)
        type(program_root_t), intent(in) :: value

        call append_visit('program-root')
    end subroutine record_program_root

    subroutine record_program_declaration(value)
        type(program_declaration_t), intent(in) :: value

        call append_visit('program-declaration')
    end subroutine record_program_declaration

    subroutine record_program_unit(value)
        type(program_unit_t), intent(in) :: value

        call append_visit('program-unit')
    end subroutine record_program_unit
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
