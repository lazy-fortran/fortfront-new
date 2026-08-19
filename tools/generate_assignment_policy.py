#!/usr/bin/env python3
"""Generate the bounded R1033 assignment-statement policy."""

from pathlib import Path
import re
import sys


def main() -> int:
    source = Path(sys.argv[1]).read_text(encoding="utf-8").strip()
    signed_match = re.search(
        r"\(literal-range integer-literal signed-decimal (-?\d+) (-?\d+)\)",
        source,
    )
    if signed_match is None:
        raise SystemExit("assignment policy omitted its signed literal range")
    source = re.sub(
        r"\s*\(literal-range integer-literal signed-decimal -?\d+ -?\d+\)",
        "",
        source,
    )
    variable_add_match = re.search(
        r"\(binary-expression (add-variable) (R\d+) (R\d+) (x) (x) (\+)\)",
        source,
    )
    if variable_add_match is None:
        raise SystemExit("assignment policy omitted its variable-add row")
    source = source.replace(variable_add_match.group(0), "", 1)
    variable_multiply_match = re.search(
        r"\(binary-expression (multiply-variable) (R\d+) (R\d+) (x) (x) (\*)\)",
        source,
    )
    if variable_multiply_match is None:
        raise SystemExit("assignment policy omitted its variable-multiply row")
    source = source.replace(variable_multiply_match.group(0), "", 1)
    variable_divide_match = re.search(
        r"\(binary-expression (divide-variable) (R\d+) (R\d+) (x) (x) (/)\)",
        source,
    )
    if variable_divide_match is None:
        raise SystemExit("assignment policy omitted its variable-divide row")
    source = source.replace(variable_divide_match.group(0), "", 1)
    variable_subtract_match = re.search(
        r"\(binary-expression (subtract-variable) (R\d+) (R\d+) (x) (x) (-)\)",
        source,
    )
    if variable_subtract_match is None:
        raise SystemExit("assignment policy omitted its variable-subtract row")
    source = source.replace(variable_subtract_match.group(0), "", 1)
    match = re.fullmatch(
        r"\(schema frontend-assignment-policy-v0\s+"
        r"\(policy assignment-stmt assignment-stmt (R\d+)\)\s+"
        r"\(token assignment (=)\)\s+"
        r"\(variable-expression (variable) (R\d+) (R\d+) (R\d+)\)\s+"
        r"\(expression integer-literal (R\d+)\)\s+"
        r"\(binary-expression (add) (R\d+) (R\d+) (1) (2) (\+)\)\s+"
        r"\(binary-expression (subtract) (R\d+) (R\d+) (5) (3) (–)\)\s+"
        r"\(binary-expression (multiply) (R\d+) (R\d+) (2) (3) (\*)\)\s+"
        r"\(binary-expression (divide) (R\d+) (R\d+) (6) (2) (/)\)\s+"
        r"\(sequence (two-assignment) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (three-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (four-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (five-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (six-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (seven-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (eight-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (nine-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(sequence (ten-assignment) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt) (assignment-stmt)\)\s+"
        r"\(literal-range integer-literal (-?\d+) (-?\d+)\)\s+"
        r"\(binary-expression (power) (R\d+) (R\d+) (2) (3) (\*\*)\)\)", source
    )
    if match is None:
        raise SystemExit("invalid assignment policy schema")
    output = Path(sys.argv[2])
    output.mkdir(parents=True, exist_ok=True)
    rows = [
        ("integer-literal", match.group(7), "", match.group(1), "", "", "", ""),
        (match.group(8), match.group(9), match.group(10), match.group(1),
            f"{match.group(11)} {match.group(13)} {match.group(12)}", match.group(11),
            match.group(12), match.group(13)),
        ("subtract", match.group(15), match.group(16), match.group(1),
            f"{match.group(17)} {match.group(19)} {match.group(18)}", match.group(17),
            match.group(18), match.group(19)),
        ("multiply", match.group(21), match.group(22), match.group(1),
            f"{match.group(23)} {match.group(25)} {match.group(24)}", match.group(23),
            match.group(24), match.group(25)),
        ("divide", match.group(27), match.group(28), match.group(1),
            f"{match.group(29)} {match.group(31)} {match.group(30)}", match.group(29),
            match.group(30), match.group(31)),
        (variable_add_match.group(1), variable_add_match.group(2),
            variable_add_match.group(3), match.group(1),
            f"{variable_add_match.group(4)} {variable_add_match.group(6)} "
            f"{variable_add_match.group(5)}", variable_add_match.group(4),
            variable_add_match.group(5), variable_add_match.group(6)),
        (variable_multiply_match.group(1), variable_multiply_match.group(2),
            variable_multiply_match.group(3), match.group(1),
            f"{variable_multiply_match.group(4)} {variable_multiply_match.group(6)} "
            f"{variable_multiply_match.group(5)}", variable_multiply_match.group(4),
            variable_multiply_match.group(5), variable_multiply_match.group(6)),
        (variable_divide_match.group(1), variable_divide_match.group(2),
            variable_divide_match.group(3), match.group(1),
            f"{variable_divide_match.group(4)} {variable_divide_match.group(6)} "
            f"{variable_divide_match.group(5)}", variable_divide_match.group(4),
            variable_divide_match.group(5), variable_divide_match.group(6)),
        (variable_subtract_match.group(1), variable_subtract_match.group(2),
            variable_subtract_match.group(3), match.group(1),
            f"{variable_subtract_match.group(4)} {variable_subtract_match.group(6)} "
            f"{variable_subtract_match.group(5)}", variable_subtract_match.group(4),
            variable_subtract_match.group(5), variable_subtract_match.group(6)),
        (match.group(97), match.group(98), match.group(99), match.group(1),
            f"{match.group(100)} {match.group(102)} {match.group(101)}", match.group(100),
            match.group(101), match.group(102)),
    ]
    row_text = "\n".join(
        "        assignment_policy_row_t('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')%s" % (
            *row, ", &" if index < len(rows) - 1 else ""
        )
        for index, row in enumerate(rows)
    )
    (output / "frontend_assignment_policy_generated.f90").write_text(
        "! Generated by tools/generate_assignment_policy.py. Origin: MECHANICAL.\n"
        "module frontend_assignment_policy_generated\n"
        "    implicit none\n"
        "    private\n"
        "    character(len=*), parameter, public :: assignment_policy_lhs = 'assignment-stmt'\n"
        f"    character(len=*), parameter, public :: assignment_policy_source_rule = '{match.group(1)}'\n"
        "    character(len=*), parameter, public :: assignment_policy_operator = '='\n"
        f"    character(len=*), parameter, public :: assignment_policy_variable_expression_kind = '{match.group(3)}'\n"
        f"    character(len=*), parameter, public :: assignment_policy_variable_expression_rule = '{match.group(4)}'\n"
        f"    character(len=*), parameter, public :: assignment_policy_variable_designator_rule = '{match.group(5)}'\n"
        f"    character(len=*), parameter, public :: assignment_policy_variable_name_rule = '{match.group(6)}'\n"
        "    character(len=*), parameter, public :: assignment_policy_variable_name = 'x'\n"
        "    character(len=*), parameter, public :: assignment_policy_integer_literal_rule = &\n"
        f"        '{match.group(7)}'\n"
        f"    character(len=*), parameter, public :: assignment_policy_sequence_name = '{match.group(32)}'\n"
        "    integer, parameter, public :: assignment_policy_sequence_count = 2\n"
        f"    character(len=*), parameter, public :: assignment_policy_three_sequence_name = '{match.group(35)}'\n"
        "    integer, parameter, public :: assignment_policy_three_sequence_count = 3\n"
        f"    character(len=*), parameter, public :: assignment_policy_four_sequence_name = '{match.group(39)}'\n"
        "    integer, parameter, public :: assignment_policy_four_sequence_count = 4\n"
        f"    character(len=*), parameter, public :: assignment_policy_five_sequence_name = '{match.group(44)}'\n"
        "    integer, parameter, public :: assignment_policy_five_sequence_count = 5\n"
        f"    character(len=*), parameter, public :: assignment_policy_six_sequence_name = '{match.group(50)}'\n"
        "    integer, parameter, public :: assignment_policy_six_sequence_count = 6\n"
        f"    character(len=*), parameter, public :: assignment_policy_seven_sequence_name = '{match.group(57)}'\n"
        "    integer, parameter, public :: assignment_policy_seven_sequence_count = 7\n"
        f"    character(len=*), parameter, public :: assignment_policy_eight_sequence_name = '{match.group(65)}'\n"
        "    integer, parameter, public :: assignment_policy_eight_sequence_count = 8\n"
        f"    character(len=*), parameter, public :: assignment_policy_nine_sequence_name = '{match.group(74)}'\n"
        "    integer, parameter, public :: assignment_policy_nine_sequence_count = 9\n"
        f"    character(len=*), parameter, public :: assignment_policy_ten_sequence_name = '{match.group(84)}'\n"
        "    integer, parameter, public :: assignment_policy_ten_sequence_count = 10\n"
        "    integer, parameter, public :: assignment_policy_sequence_max_count = 10\n"
        "    type, public :: assignment_policy_sequence_row_t\n"
        "        character(len=32) :: name\n"
        "        character(len=32) :: first_record\n"
        "        character(len=32) :: second_record\n"
        "        character(len=32) :: third_record\n"
        "        character(len=32) :: fourth_record\n"
        "        character(len=32) :: fifth_record\n"
        "        character(len=32) :: sixth_record\n"
        "        character(len=32) :: seventh_record\n"
        "        character(len=32) :: eighth_record\n"
        "        character(len=32) :: ninth_record\n"
        "        character(len=32) :: tenth_record\n"
        "    end type assignment_policy_sequence_row_t\n"
        "    type(assignment_policy_sequence_row_t), parameter, public :: &\n"
        "        assignment_policy_sequence_rows(9) = [ &\n"
        f"        assignment_policy_sequence_row_t('{match.group(32)}', '{match.group(33)}', '{match.group(34)}', '', '', '', '', '', '', '', ''), &\n"
        f"        assignment_policy_sequence_row_t('{match.group(35)}', '{match.group(36)}', '{match.group(37)}', '{match.group(38)}', '', '', '', '', '', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(39)}', '{match.group(40)}', '{match.group(41)}', '{match.group(42)}', &\n"
        f"        '{match.group(43)}', '', '', '', '', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(44)}', '{match.group(45)}', '{match.group(46)}', '{match.group(47)}', &\n"
        f"        '{match.group(48)}', '{match.group(49)}', '', '', '', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(50)}', '{match.group(51)}', '{match.group(52)}', '{match.group(53)}', &\n"
        f"        '{match.group(54)}', '{match.group(55)}', '{match.group(56)}', '', '', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(57)}', '{match.group(58)}', '{match.group(59)}', '{match.group(60)}', &\n"
        f"        '{match.group(61)}', '{match.group(62)}', '{match.group(63)}', '{match.group(64)}', '', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(65)}', '{match.group(66)}', '{match.group(67)}', '{match.group(68)}', &\n"
        f"        '{match.group(69)}', '{match.group(70)}', '{match.group(71)}', '{match.group(72)}', &\n"
        f"        '{match.group(73)}', '', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(74)}', '{match.group(75)}', '{match.group(76)}', '{match.group(77)}', &\n"
        f"        '{match.group(78)}', '{match.group(79)}', '{match.group(80)}', '{match.group(81)}', &\n"
        f"        '{match.group(82)}', '{match.group(83)}', ''), &\n"
        "        assignment_policy_sequence_row_t( &\n"
        f"        '{match.group(84)}', '{match.group(85)}', '{match.group(86)}', '{match.group(87)}', &\n"
        f"        '{match.group(88)}', '{match.group(89)}', '{match.group(90)}', '{match.group(91)}', &\n"
        f"        '{match.group(92)}', '{match.group(93)}', '{match.group(94)}') &\n"
        "        ]\n"
        f"    integer, parameter, public :: assignment_policy_integer_literal_min = {match.group(95)}\n"
        f"    integer, parameter, public :: assignment_policy_integer_literal_max = {match.group(96)}\n"
        f"    integer, parameter, public :: assignment_policy_signed_integer_literal_min = {signed_match.group(1)}\n"
        f"    integer, parameter, public :: assignment_policy_signed_integer_literal_max = {signed_match.group(2)}\n"
        "    type, public :: assignment_policy_row_t\n"
        "        character(len=24) :: expression_kind\n"
        "        character(len=16) :: expression_rule\n"
        "        character(len=16) :: operator_rule\n"
        "        character(len=16) :: source_rule\n"
        "        character(len=32) :: source_spelling\n"
        "        character(len=16) :: left_operand\n"
        "        character(len=16) :: right_operand\n"
        "        character(len=8) :: operator\n"
        "    end type assignment_policy_row_t\n"
        "    character(len=*), parameter, public :: assignment_policy_variable_expression_row = &\n"
        f"        'variable {match.group(4)} {match.group(5)} {match.group(6)}'\n"
        f"    type(assignment_policy_row_t), parameter, public :: assignment_policy_rows({len(rows)}) = [ &\n"
        f"{row_text} &\n"
        "        ]\n"
        f"    integer, parameter, public :: assignment_policy_row_count = {len(rows)}\n"
        "    integer, parameter, public :: assignment_policy_source_page = 155\n"
        "end module frontend_assignment_policy_generated\n",
        encoding="utf-8",
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
