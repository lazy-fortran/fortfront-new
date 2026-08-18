#!/usr/bin/env python3
"""Generate the bounded, source-backed PRINT *, 7 through 16 typed AST policy."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


SCHEMA = re.compile(
    r"\(schema frontend-print-policy-v0\s+"
    r"\(statement (print-stmt) (PRINT) (R\d+)\)\s+"
    r"\(format (default-char-expr) (\*) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (7) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (8) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (9) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (10) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (11) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (12) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (13) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (14) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (15) (R\d+)\)\s+"
    r"\(output-item (integer-literal) (16) (R\d+)\)\s+"
    r"\(variable-output (variable) (x) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (17) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (23) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (21) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (12) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (8) (R\d+)\)\s+"
    r"\(variable-value (integer-literal) (9) (R\d+)\)\s+"
    r"\(source (J3-24-007) ([^\s()]+) ([^\s()]+) ([^\s()]+) "
    r"(242) (244) (248) ([^\s()]+)\)\)"
)


def _replace_generated_routes(generated: str) -> str:
    generated = generated.replace(
        "    type, public :: print_stmt_t\n",
        "    type, public :: print_output_item_t\n"
        "        character(len=32) :: kind = ''\n"
        "        character(len=32) :: name = ''\n"
        "        integer(int64) :: value = 0_int64\n"
        "        character(len=32) :: rule = ''\n"
        "        character(len=32) :: clause = ''\n"
        "        integer(int64) :: page = 0_int64\n"
        "    end type print_output_item_t\n\n"
        "    type, public :: print_stmt_t\n",
    )
    generated = generated.replace(
        "        integer(int64) :: output_count = 0_int64\n",
        "        integer(int64) :: output_count = 0_int64\n"
        "        integer(int64) :: output_sequence_start = 7_int64\n"
        "        integer(int64) :: output_sequence_length = 0_int64\n",
    )
    validate = "\n".join([
        "    logical function print_stmt_validate(value, message)",
        "        type(print_stmt_t), intent(in) :: value",
        "        character(len=*), intent(out) :: message",
        "        type(print_output_item_t) :: item",
        "        integer :: index",
        "        message = ''",
        "        if (trim(value%format_kind) /= trim(print_policy_format_kind) .or. &",
        "            trim(value%format_value) /= trim(print_policy_format_value) .or. &",
        "            value%output_count < 1_int64 .or. value%output_count > 10_int64 .or. &",
        "            (value%output_sequence_length > 0_int64 .and. &",
        "            value%output_count /= value%output_sequence_length) .or. &",
        "            (value%output_sequence_start /= 7_int64 .and. &",
        "            value%output_sequence_start /= 17_int64)) then",
        "            message = 'invalid-print-policy-value'",
        "            print_stmt_validate = .false.",
        "            return",
        "        end if",
        "        do index = 1, int(value%output_count)",
        "            call print_stmt_output_item(value, index, item)",
        "            if (trim(item%kind) == trim(print_policy_output_kind)) then",
        "                if (item%value /= value%output_sequence_start + int(index - 1, int64)) then",
        "                    message = 'invalid-print-policy-value'",
        "                    print_stmt_validate = .false.",
        "                    return",
        "                end if",
        "            else if (trim(item%kind) == trim(print_policy_variable_output_kind)) then",
        "                if ((index /= 1 .and. index /= 2 .and. index /= 3 .and. &",
        "                    index /= 4 .and. index /= 5) .or. &",
        "                    trim(item%name) /= trim(print_policy_variable_output_name) .or. &",
        "                    (item%value /= print_policy_variable_value .and. &",
        "                    item%value /= print_policy_variable_value_2 .and. &",
        "                    item%value /= print_policy_variable_value_3 .and. &",
        "                    item%value /= print_policy_variable_value_4 .and. &",
        "                    item%value /= print_policy_variable_value_5 .and. &",
        "                    item%value /= print_policy_variable_value_6)) then",
        "                    message = 'invalid-print-policy-value'",
        "                    print_stmt_validate = .false.",
        "                    return",
        "                end if",
        "            else",
        "                message = 'invalid-print-policy-value'",
        "                print_stmt_validate = .false.",
        "                return",
        "            end if",
        "            if ((trim(item%rule) /= trim(print_policy_output_rule) .and. &",
        "                trim(item%rule) /= trim(print_policy_variable_output_rule)) .or. &",
        "                trim(item%clause) /= trim(print_policy_output_clause) .or. &",
        "                item%page /= print_policy_output_page) then",
        "                message = 'invalid-print-policy-rule'",
        "                print_stmt_validate = .false.",
        "                return",
        "            end if",
        "        end do",
        "        if (trim(value%statement_rule) /= trim(print_policy_statement_rule) .or. &",
        "            trim(value%format_rule) /= trim(print_policy_format_rule) .or. &",
        "            (trim(value%output_rule) /= trim(print_policy_output_rule) .and. &",
        "            trim(value%output_rule) /= trim(print_policy_variable_output_rule))) then",
        "            message = 'invalid-print-policy-rule'",
        "            print_stmt_validate = .false.",
        "            return",
        "        end if",
        "        if (trim(value%source_document) /= trim(print_policy_document) .or. &",
        "            trim(value%statement_clause) /= trim(print_policy_statement_clause) .or. &",
        "            trim(value%format_clause) /= trim(print_policy_format_clause) .or. &",
        "            trim(value%output_clause) /= trim(print_policy_output_clause) .or. &",
        "            value%statement_page /= print_policy_statement_page .or. &",
        "            value%format_page /= print_policy_format_page .or. &",
        "            value%output_page /= print_policy_output_page .or. &",
        "            trim(value%source_hash) /= trim(print_policy_source_hash)) then",
        "            message = 'invalid-print-policy-source'",
        "            print_stmt_validate = .false.",
        "            return",
        "        end if",
        "        print_stmt_validate = source_span_validate(value%span, message)",
        "    end function print_stmt_validate",
    ]) + "\n"
    generated = re.sub(
        r"    logical function print_stmt_validate.*?    end function print_stmt_validate\n",
        validate,
        generated,
        flags=re.S,
    )
    serialize = """    subroutine print_stmt_to_sx(value, output, ok, message)
        type(print_stmt_t), intent(in) :: value
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(print_output_item_t) :: item
        character(len=2048) :: span_sx
        character(len=32) :: index_s, value_s, count_s, page_s
        character(len=32) :: format_page_s, output_page_s
        integer :: index

        output = ''
        ok = .false.
        message = ''
        if (.not. print_stmt_validate(value, message)) return
        call source_span_to_sx(value%span, span_sx, ok, message)
        if (.not. ok) return
        output = '(print-stmt (format-kind '//trim(value%format_kind)// &
            ') (format-value '//trim(value%format_value)//') '
        call print_stmt_output_item(value, 1, item)
        if (trim(item%kind) == trim(print_policy_variable_output_kind)) then
            output = trim(output)//'(output-kind '//trim(item%kind)//') (output-name '// &
                trim(item%name)//')'
        else
            write (value_s, '(i0)') item%value
            output = trim(output)//'(output-kind '//trim(item%kind)//') (output-value '// &
                trim(value_s)//')'
        end if
        if (value%output_count > 1_int64) then
            write (count_s, '(i0)') value%output_count
            output = trim(output)//' (output-count '//trim(count_s)//')'
            do index = 2, int(value%output_count)
                call print_stmt_output_item(value, index, item)
                write (index_s, '(i0)') index
                write (value_s, '(i0)') item%value
                output = trim(output)//' (output-kind-'//trim(index_s)//' '// &
                    trim(item%kind)//')'
                if (trim(item%kind) == trim(print_policy_variable_output_kind)) then
                    output = trim(output)//' (output-name-'//trim(index_s)//' '// &
                        trim(item%name)//')'
                else
                    output = trim(output)//' (output-value-'//trim(index_s)//' '// &
                        trim(value_s)//')'
                end if
                output = trim(output)//' (output-rule-'//trim(index_s)//' '// &
                    trim(item%rule)//')'
            end do
        end if
        write (page_s, '(i0)') value%statement_page
        write (format_page_s, '(i0)') value%format_page
        write (output_page_s, '(i0)') value%output_page
        output = trim(output)//' (span '//trim(span_sx)//') (statement-rule '// &
            trim(value%statement_rule)//') (format-rule '//trim(value%format_rule)// &
            ') (output-rule '//trim(value%output_rule)//') (source-document '// &
            trim(value%source_document)//') (statement-clause '// &
            trim(value%statement_clause)//') (format-clause '//trim(value%format_clause)// &
            ') (output-clause '//trim(value%output_clause)//') (statement-page '// &
            trim(page_s)//') (format-page '//trim(format_page_s)//') (output-page '// &
            trim(output_page_s)//') (source-hash '//trim(value%source_hash)//'))'
        ok = .true.
    end subroutine print_stmt_to_sx

"""
    generated = re.sub(
        r"    subroutine print_stmt_to_sx.*?    end subroutine print_stmt_to_sx\n",
        serialize,
        generated,
        flags=re.S,
    )
    item_helper = """    subroutine print_stmt_output_item(value, index, item)
        type(print_stmt_t), intent(in) :: value
        integer, intent(in) :: index
        type(print_output_item_t), intent(out) :: item

        item = print_output_item_t()
        select case (index)
        case (1)
            item%kind = value%output_kind
            item%name = value%output_name
            item%value = value%output_value
            item%rule = value%output_rule
            item%clause = value%output_clause
            item%page = value%output_page
        case default
            if (index == 2) then
                item%kind = value%output_2_kind
                item%name = value%output_2_name
                item%value = value%output_2_value
                item%rule = value%output_2_rule
                item%clause = value%output_2_clause
                item%page = value%output_2_page
            else if (index == 3) then
                item%kind = value%output_3_kind
                item%name = value%output_3_name
                item%value = value%output_3_value
                item%rule = value%output_3_rule
                item%clause = value%output_3_clause
                item%page = value%output_3_page
            else if (index == 4) then
                item%kind = value%output_4_kind
                item%value = value%output_4_value
                item%rule = value%output_4_rule
                item%clause = value%output_4_clause
                item%page = value%output_4_page
            else if (index == 5) then
                item%kind = value%output_5_kind
                item%value = value%output_5_value
                item%rule = value%output_5_rule
                item%clause = value%output_5_clause
                item%page = value%output_5_page
            else if (index == 6) then
                item%kind = value%output_6_kind
                item%value = value%output_6_value
                item%rule = value%output_6_rule
                item%clause = value%output_6_clause
                item%page = value%output_6_page
            else if (index == 7) then
                item%kind = value%output_7_kind
                item%value = value%output_7_value
                item%rule = value%output_7_rule
                item%clause = value%output_7_clause
                item%page = value%output_7_page
            else if (index == 8) then
                item%kind = value%output_8_kind
                item%value = value%output_8_value
                item%rule = value%output_8_rule
                item%clause = value%output_8_clause
                item%page = value%output_8_page
            else if (index == 9) then
                item%kind = value%output_9_kind
                item%value = value%output_9_value
                item%rule = value%output_9_rule
                item%clause = value%output_9_clause
                item%page = value%output_9_page
            else if (index == 10) then
                item%kind = value%output_10_kind
                item%value = value%output_10_value
                item%rule = value%output_10_rule
                item%clause = value%output_10_clause
                item%page = value%output_10_page
            end if
        end select
    end subroutine print_stmt_output_item

"""
    generated = generated.replace(
        "end module frontend_print_policy_generated\n",
        item_helper + "end module frontend_print_policy_generated\n",
    )
    for index in range(2, 11):
        if index == 2 or index == 3:
            continue
        name = f"value%output_{index}_name" if index in (4, 5) else "print_policy_variable_output_name"
        generated = generated.replace(
            f"                item%kind = value%output_{index}_kind\n",
            f"                item%kind = value%output_{index}_kind\n"
            f"                item%name = {name}\n",
        )
    return generated


def render(source: str) -> str:
    match = SCHEMA.fullmatch(source.strip())
    if match is None:
        raise ValueError("invalid print policy schema")
    (
        statement_kind, statement_token, statement_rule, format_kind, format_value,
        format_rule, output_kind, output_value, output_rule, output_2_kind,
        output_2_value, output_2_rule, output_3_kind, output_3_value, output_3_rule,
        output_4_kind, output_4_value, output_4_rule,
        output_5_kind, output_5_value, output_5_rule,
        output_6_kind, output_6_value, output_6_rule,
        output_7_kind, output_7_value, output_7_rule,
        output_8_kind, output_8_value, output_8_rule,
        output_9_kind, output_9_value, output_9_rule,
        output_10_kind, output_10_value, output_10_rule,
        variable_kind, variable_name, variable_rule, variable_value_kind,
        variable_value, variable_value_rule, variable_value_2_kind,
        variable_value_2, variable_value_2_rule, variable_value_3_kind,
        variable_value_3, variable_value_3_rule, variable_value_4_kind,
        variable_value_4, variable_value_4_rule, variable_value_5_kind,
        variable_value_5, variable_value_5_rule, variable_value_6_kind,
        variable_value_6, variable_value_6_rule, document,
        statement_clause, format_clause, output_clause, statement_page,
        format_page, output_page, source_hash,
    ) = match.groups()
    generated = f'''! Generated by tools/generate_print_policy.py. Origin: MECHANICAL.
module frontend_print_policy_generated
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: source_span_t, source_span_validate, source_span_to_sx
    implicit none
    private

    character(len=*), parameter, public :: print_policy_statement_kind = '{statement_kind}'
    character(len=*), parameter, public :: print_policy_statement_token = '{statement_token}'
    character(len=*), parameter, public :: print_policy_statement_rule = '{statement_rule}'
    character(len=*), parameter, public :: print_policy_format_kind = '{format_kind}'
    character(len=*), parameter, public :: print_policy_format_value = '{format_value}'
    character(len=*), parameter, public :: print_policy_format_rule = '{format_rule}'
    character(len=*), parameter, public :: print_policy_output_kind = '{output_kind}'
    integer(int64), parameter, public :: print_policy_output_value = {output_value}_int64
    character(len=*), parameter, public :: print_policy_output_rule = '{output_rule}'
    character(len=*), parameter, public :: print_policy_variable_output_kind = '{variable_kind}'
    character(len=*), parameter, public :: print_policy_variable_output_name = '{variable_name}'
    character(len=*), parameter, public :: print_policy_variable_output_rule = '{variable_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_kind = '{variable_value_kind}'
    integer(int64), parameter, public :: print_policy_variable_value = {variable_value}_int64
    character(len=*), parameter, public :: print_policy_variable_value_rule = '{variable_value_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_2_kind = '{variable_value_2_kind}'
    integer(int64), parameter, public :: print_policy_variable_value_2 = {variable_value_2}_int64
    character(len=*), parameter, public :: print_policy_variable_value_2_rule = '{variable_value_2_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_3_kind = '{variable_value_3_kind}'
    integer(int64), parameter, public :: print_policy_variable_value_3 = {variable_value_3}_int64
    character(len=*), parameter, public :: print_policy_variable_value_3_rule = '{variable_value_3_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_4_kind = '{variable_value_4_kind}'
    integer(int64), parameter, public :: print_policy_variable_value_4 = {variable_value_4}_int64
    character(len=*), parameter, public :: print_policy_variable_value_4_rule = '{variable_value_4_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_5_kind = '{variable_value_5_kind}'
    integer(int64), parameter, public :: print_policy_variable_value_5 = {variable_value_5}_int64
    character(len=*), parameter, public :: print_policy_variable_value_5_rule = '{variable_value_5_rule}'
    character(len=*), parameter, public :: print_policy_variable_value_6_kind = '{variable_value_6_kind}'
    integer(int64), parameter, public :: print_policy_variable_value_6 = {variable_value_6}_int64
    character(len=*), parameter, public :: print_policy_variable_value_6_rule = '{variable_value_6_rule}'
    character(len=*), parameter, public :: print_policy_output_2_kind = '{output_2_kind}'
    integer(int64), parameter, public :: print_policy_output_2_value = {output_2_value}_int64
    character(len=*), parameter, public :: print_policy_output_2_rule = '{output_2_rule}'
    character(len=*), parameter, public :: print_policy_output_3_kind = '{output_3_kind}'
    integer(int64), parameter, public :: print_policy_output_3_value = {output_3_value}_int64
    character(len=*), parameter, public :: print_policy_output_3_rule = '{output_3_rule}'
    character(len=*), parameter, public :: print_policy_output_4_kind = '{output_4_kind}'
    integer(int64), parameter, public :: print_policy_output_4_value = {output_4_value}_int64
    character(len=*), parameter, public :: print_policy_output_4_rule = '{output_4_rule}'
    character(len=*), parameter, public :: print_policy_output_5_kind = '{output_5_kind}'
    integer(int64), parameter, public :: print_policy_output_5_value = {output_5_value}_int64
    character(len=*), parameter, public :: print_policy_output_5_rule = '{output_5_rule}'
    character(len=*), parameter, public :: print_policy_output_6_kind = '{output_6_kind}'
    integer(int64), parameter, public :: print_policy_output_6_value = {output_6_value}_int64
    character(len=*), parameter, public :: print_policy_output_6_rule = '{output_6_rule}'
    character(len=*), parameter, public :: print_policy_output_7_kind = '{output_7_kind}'
    integer(int64), parameter, public :: print_policy_output_7_value = {output_7_value}_int64
    character(len=*), parameter, public :: print_policy_output_7_rule = '{output_7_rule}'
    character(len=*), parameter, public :: print_policy_output_8_kind = '{output_8_kind}'
    integer(int64), parameter, public :: print_policy_output_8_value = {output_8_value}_int64
    character(len=*), parameter, public :: print_policy_output_8_rule = '{output_8_rule}'
    character(len=*), parameter, public :: print_policy_output_9_kind = '{output_9_kind}'
    integer(int64), parameter, public :: print_policy_output_9_value = {output_9_value}_int64
    character(len=*), parameter, public :: print_policy_output_9_rule = '{output_9_rule}'
    character(len=*), parameter, public :: print_policy_output_10_kind = '{output_10_kind}'
    integer(int64), parameter, public :: print_policy_output_10_value = {output_10_value}_int64
    character(len=*), parameter, public :: print_policy_output_10_rule = '{output_10_rule}'
    character(len=*), parameter, public :: print_policy_document = '{document}'
    character(len=*), parameter, public :: print_policy_statement_clause = '{statement_clause}'
    character(len=*), parameter, public :: print_policy_format_clause = '{format_clause}'
    character(len=*), parameter, public :: print_policy_output_clause = '{output_clause}'
    integer(int64), parameter, public :: print_policy_statement_page = {statement_page}_int64
    integer(int64), parameter, public :: print_policy_format_page = {format_page}_int64
    integer(int64), parameter, public :: print_policy_output_page = {output_page}_int64
    character(len=*), parameter, public :: print_policy_source_hash = &
        '{source_hash}'

    type, public :: print_stmt_t
        character(len=32) :: format_kind = ''
        character(len=32) :: format_value = ''
        character(len=32) :: output_kind = ''
        character(len=32) :: output_name = ''
        integer(int64) :: output_value = 0_int64
        integer(int64) :: output_count = 0_int64
        character(len=32) :: output_2_kind = ''
        character(len=32) :: output_2_name = ''
        integer(int64) :: output_2_value = 0_int64
        character(len=32) :: output_3_kind = ''
        character(len=32) :: output_3_name = ''
        integer(int64) :: output_3_value = 0_int64
        character(len=32) :: output_4_kind = ''
        character(len=32) :: output_4_name = ''
        integer(int64) :: output_4_value = 0_int64
        character(len=32) :: output_5_kind = ''
        character(len=32) :: output_5_name = ''
        integer(int64) :: output_5_value = 0_int64
        character(len=32) :: output_6_kind = ''
        integer(int64) :: output_6_value = 0_int64
        character(len=32) :: output_7_kind = ''
        integer(int64) :: output_7_value = 0_int64
        character(len=32) :: output_8_kind = ''
        integer(int64) :: output_8_value = 0_int64
        character(len=32) :: output_9_kind = ''
        integer(int64) :: output_9_value = 0_int64
        character(len=32) :: output_10_kind = ''
        integer(int64) :: output_10_value = 0_int64
        type(source_span_t) :: span
        character(len=32) :: statement_rule = ''
        character(len=32) :: format_rule = ''
        character(len=32) :: output_rule = ''
        character(len=32) :: output_2_rule = ''
        character(len=32) :: output_3_rule = ''
        character(len=32) :: output_4_rule = ''
        character(len=32) :: output_5_rule = ''
        character(len=32) :: output_6_rule = ''
        character(len=32) :: output_7_rule = ''
        character(len=32) :: output_8_rule = ''
        character(len=32) :: output_9_rule = ''
        character(len=32) :: output_10_rule = ''
        character(len=128) :: source_document = ''
        character(len=32) :: statement_clause = ''
        character(len=32) :: format_clause = ''
        character(len=32) :: output_clause = ''
        character(len=32) :: output_2_clause = ''
        character(len=32) :: output_3_clause = ''
        character(len=32) :: output_4_clause = ''
        character(len=32) :: output_5_clause = ''
        character(len=32) :: output_6_clause = ''
        character(len=32) :: output_7_clause = ''
        character(len=32) :: output_8_clause = ''
        character(len=32) :: output_9_clause = ''
        character(len=32) :: output_10_clause = ''
        integer(int64) :: statement_page = 0_int64
        integer(int64) :: format_page = 0_int64
        integer(int64) :: output_page = 0_int64
        integer(int64) :: output_2_page = 0_int64
        integer(int64) :: output_3_page = 0_int64
        integer(int64) :: output_4_page = 0_int64
        integer(int64) :: output_5_page = 0_int64
        integer(int64) :: output_6_page = 0_int64
        integer(int64) :: output_7_page = 0_int64
        integer(int64) :: output_8_page = 0_int64
        integer(int64) :: output_9_page = 0_int64
        integer(int64) :: output_10_page = 0_int64
        character(len=128) :: source_hash = ''
    end type print_stmt_t

    public :: print_stmt_validate
    public :: print_stmt_to_sx

contains

    logical function print_stmt_validate(value, message)
        type(print_stmt_t), intent(in) :: value
        character(len=*), intent(out) :: message

        message = ''
        if (trim(value%format_kind) /= trim(print_policy_format_kind) .or. &
            trim(value%format_value) /= trim(print_policy_format_value) .or. &
            trim(value%output_kind) /= trim(print_policy_output_kind) .or. &
            value%output_value /= print_policy_output_value) then
            message = 'invalid-print-policy-value'
            print_stmt_validate = .false.
            return
        end if
        if (value%output_count < 1_int64 .or. value%output_count > 10_int64) then
            message = 'invalid-print-policy-output-count'
            print_stmt_validate = .false.
            return
        end if
        if (value%output_count >= 2_int64) then
            if (trim(value%output_2_kind) /= trim(print_policy_output_2_kind) .or. &
                value%output_2_value /= print_policy_output_2_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_2_rule) /= trim(print_policy_output_2_rule) .or. &
                trim(value%output_2_clause) /= trim(print_policy_output_clause) .or. &
                value%output_2_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 3_int64) then
            if (trim(value%output_3_kind) /= trim(print_policy_output_3_kind) .or. &
                value%output_3_value /= print_policy_output_3_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_3_rule) /= trim(print_policy_output_3_rule) .or. &
                trim(value%output_3_clause) /= trim(print_policy_output_clause) .or. &
                value%output_3_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 4_int64) then
            if (trim(value%output_4_kind) /= trim(print_policy_output_4_kind) .or. &
                value%output_4_value /= print_policy_output_4_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_4_rule) /= trim(print_policy_output_4_rule) .or. &
                trim(value%output_4_clause) /= trim(print_policy_output_clause) .or. &
                value%output_4_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 5_int64) then
            if (trim(value%output_5_kind) /= trim(print_policy_output_5_kind) .or. &
                value%output_5_value /= print_policy_output_5_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_5_rule) /= trim(print_policy_output_5_rule) .or. &
                trim(value%output_5_clause) /= trim(print_policy_output_clause) .or. &
                value%output_5_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 6_int64) then
            if (trim(value%output_6_kind) /= trim(print_policy_output_6_kind) .or. &
                value%output_6_value /= print_policy_output_6_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_6_rule) /= trim(print_policy_output_6_rule) .or. &
                trim(value%output_6_clause) /= trim(print_policy_output_clause) .or. &
                value%output_6_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 7_int64) then
            if (trim(value%output_7_kind) /= trim(print_policy_output_7_kind) .or. &
                value%output_7_value /= print_policy_output_7_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_7_rule) /= trim(print_policy_output_7_rule) .or. &
                trim(value%output_7_clause) /= trim(print_policy_output_clause) .or. &
                value%output_7_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 8_int64) then
            if (trim(value%output_8_kind) /= trim(print_policy_output_8_kind) .or. &
                value%output_8_value /= print_policy_output_8_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_8_rule) /= trim(print_policy_output_8_rule) .or. &
                trim(value%output_8_clause) /= trim(print_policy_output_clause) .or. &
                value%output_8_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 9_int64) then
            if (trim(value%output_9_kind) /= trim(print_policy_output_9_kind) .or. &
                value%output_9_value /= print_policy_output_9_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_9_rule) /= trim(print_policy_output_9_rule) .or. &
                trim(value%output_9_clause) /= trim(print_policy_output_clause) .or. &
                value%output_9_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (value%output_count == 10_int64) then
            if (trim(value%output_9_kind) /= trim(print_policy_output_9_kind) .or. &
                value%output_9_value /= print_policy_output_9_value .or. &
                trim(value%output_10_kind) /= trim(print_policy_output_10_kind) .or. &
                value%output_10_value /= print_policy_output_10_value) then
                message = 'invalid-print-policy-value'
                print_stmt_validate = .false.
                return
            end if
            if (trim(value%output_9_rule) /= trim(print_policy_output_9_rule) .or. &
                trim(value%output_9_clause) /= trim(print_policy_output_clause) .or. &
                value%output_9_page /= print_policy_output_page .or. &
                trim(value%output_10_rule) /= trim(print_policy_output_10_rule) .or. &
                trim(value%output_10_clause) /= trim(print_policy_output_clause) .or. &
                value%output_10_page /= print_policy_output_page) then
                message = 'invalid-print-policy-rule'
                print_stmt_validate = .false.
                return
            end if
        end if
        if (trim(value%statement_rule) /= trim(print_policy_statement_rule) .or. &
            trim(value%format_rule) /= trim(print_policy_format_rule) .or. &
            trim(value%output_rule) /= trim(print_policy_output_rule)) then
            message = 'invalid-print-policy-rule'
            print_stmt_validate = .false.
            return
        end if
        if (trim(value%source_document) /= trim(print_policy_document) .or. &
            trim(value%statement_clause) /= trim(print_policy_statement_clause) .or. &
            trim(value%format_clause) /= trim(print_policy_format_clause) .or. &
            trim(value%output_clause) /= trim(print_policy_output_clause) .or. &
            value%statement_page /= print_policy_statement_page .or. &
            value%format_page /= print_policy_format_page .or. &
            value%output_page /= print_policy_output_page .or. &
            trim(value%source_hash) /= trim(print_policy_source_hash)) then
            message = 'invalid-print-policy-source'
            print_stmt_validate = .false.
            return
        end if
        print_stmt_validate = source_span_validate(value%span, message)
    end function print_stmt_validate

    subroutine print_stmt_to_sx(value, output, ok, message)
        type(print_stmt_t), intent(in) :: value
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=2048) :: span_sx
        character(len=32) :: output_value_s, output_2_value_s, output_3_value_s
        character(len=32) :: output_4_value_s, output_5_value_s, output_6_value_s
        character(len=32) :: output_7_value_s
        character(len=32) :: output_8_value_s
        character(len=32) :: output_9_value_s, output_10_value_s
        character(len=32) :: output_count_s
        character(len=32) :: statement_page_s
        character(len=32) :: format_page_s, output_page_s

        output = ''
        ok = .false.
        message = ''
        if (.not. print_stmt_validate(value, message)) return
        call source_span_to_sx(value%span, span_sx, ok, message)
        if (.not. ok) return
        write (output_value_s, '(i0)') value%output_value
        write (output_2_value_s, '(i0)') value%output_2_value
        write (output_3_value_s, '(i0)') value%output_3_value
        write (output_4_value_s, '(i0)') value%output_4_value
        write (output_5_value_s, '(i0)') value%output_5_value
        write (output_6_value_s, '(i0)') value%output_6_value
        write (output_7_value_s, '(i0)') value%output_7_value
        write (output_8_value_s, '(i0)') value%output_8_value
        write (output_9_value_s, '(i0)') value%output_9_value
        write (output_10_value_s, '(i0)') value%output_10_value
        write (output_count_s, '(i0)') value%output_count
        write (statement_page_s, '(i0)') value%statement_page
        write (format_page_s, '(i0)') value%format_page
        write (output_page_s, '(i0)') value%output_page
        if (value%output_count == 1_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (span '//trim(span_sx)//') (statement-rule '// &
                trim(value%statement_rule)//') (format-rule '//trim(value%format_rule)// &
                ') (output-rule '//trim(value%output_rule)//') (source-document '// &
                trim(value%source_document)//') (statement-clause '// &
                trim(value%statement_clause)//') (format-clause '//trim(value%format_clause)// &
                ') (output-clause '//trim(value%output_clause)//') (statement-page '// &
                trim(statement_page_s)//') (format-page '//trim(format_page_s)// &
                ') (output-page '//trim(output_page_s)//') (source-hash '// &
                trim(value%source_hash)//'))'
        else if (value%output_count == 2_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '// &
                trim(output_2_value_s)//') (output-rule-2 '//trim(value%output_2_rule)// &
                ') (span '//trim(span_sx)//') (statement-rule '// &
                trim(value%statement_rule)//') (format-rule '//trim(value%format_rule)// &
                ') (output-rule '//trim(value%output_rule)//') (source-document '// &
                trim(value%source_document)//') (statement-clause '// &
                trim(value%statement_clause)//') (format-clause '//trim(value%format_clause)// &
                ') (output-clause '//trim(value%output_clause)//') (statement-page '// &
                trim(statement_page_s)//') (format-page '//trim(format_page_s)// &
                ') (output-page '//trim(output_page_s)//') (source-hash '// &
                trim(value%source_hash)//'))'
        else if (value%output_count == 3_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '// &
                trim(output_2_value_s)//') (output-rule-2 '//trim(value%output_2_rule)// &
                ') (output-kind-3 '//trim(value%output_3_kind)// &
                ') (output-value-3 '//trim(output_3_value_s)//') (output-rule-3 '// &
                trim(value%output_3_rule)//') (span '//trim(span_sx)//') '// &
                '(statement-rule '//trim(value%statement_rule)//') (format-rule '// &
                trim(value%format_rule)//') (output-rule '//trim(value%output_rule)// &
                ') (source-document '//trim(value%source_document)//') '// &
                '(statement-clause '//trim(value%statement_clause)//') '// &
                '(format-clause '//trim(value%format_clause)//') (output-clause '// &
                trim(value%output_clause)//') (statement-page '//trim(statement_page_s)// &
                ') (format-page '//trim(format_page_s)//') (output-page '// &
                trim(output_page_s)//') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 4_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 5_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 6_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (output-kind-6 '// &
                trim(value%output_6_kind)//') (output-value-6 '//trim(output_6_value_s)// &
                ') (output-rule-6 '//trim(value%output_6_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 7_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (output-kind-6 '// &
                trim(value%output_6_kind)//') (output-value-6 '//trim(output_6_value_s)// &
                ') (output-rule-6 '//trim(value%output_6_rule)//') (output-kind-7 '// &
                trim(value%output_7_kind)//') (output-value-7 '//trim(output_7_value_s)// &
                ') (output-rule-7 '//trim(value%output_7_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 8_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (output-kind-6 '// &
                trim(value%output_6_kind)//') (output-value-6 '//trim(output_6_value_s)// &
                ') (output-rule-6 '//trim(value%output_6_rule)//') (output-kind-7 '// &
                trim(value%output_7_kind)//') (output-value-7 '//trim(output_7_value_s)// &
                ') (output-rule-7 '//trim(value%output_7_rule)//') (output-kind-8 '// &
                trim(value%output_8_kind)//') (output-value-8 '//trim(output_8_value_s)// &
                ') (output-rule-8 '//trim(value%output_8_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else if (value%output_count == 9_int64) then
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (output-kind-6 '// &
                trim(value%output_6_kind)//') (output-value-6 '//trim(output_6_value_s)// &
                ') (output-rule-6 '//trim(value%output_6_rule)//') (output-kind-7 '// &
                trim(value%output_7_kind)//') (output-value-7 '//trim(output_7_value_s)// &
                ') (output-rule-7 '//trim(value%output_7_rule)//') (output-kind-8 '// &
                trim(value%output_8_kind)//') (output-value-8 '//trim(output_8_value_s)// &
                ') (output-rule-8 '//trim(value%output_8_rule)//') (output-kind-9 '// &
                trim(value%output_9_kind)//') (output-value-9 '//trim(output_9_value_s)// &
                ') (output-rule-9 '//trim(value%output_9_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        else
            output = '(print-stmt (format-kind '//trim(value%format_kind)// &
                ') (format-value '//trim(value%format_value)//') (output-kind '// &
                trim(value%output_kind)//') (output-value '//trim(output_value_s)// &
                ') (output-count '//trim(output_count_s)//') (output-kind-2 '// &
                trim(value%output_2_kind)//') (output-value-2 '//trim(output_2_value_s)// &
                ') (output-rule-2 '//trim(value%output_2_rule)//') (output-kind-3 '// &
                trim(value%output_3_kind)//') (output-value-3 '//trim(output_3_value_s)// &
                ') (output-rule-3 '//trim(value%output_3_rule)//') (output-kind-4 '// &
                trim(value%output_4_kind)//') (output-value-4 '//trim(output_4_value_s)// &
                ') (output-rule-4 '//trim(value%output_4_rule)//') (output-kind-5 '// &
                trim(value%output_5_kind)//') (output-value-5 '//trim(output_5_value_s)// &
                ') (output-rule-5 '//trim(value%output_5_rule)//') (output-kind-6 '// &
                trim(value%output_6_kind)//') (output-value-6 '//trim(output_6_value_s)// &
                ') (output-rule-6 '//trim(value%output_6_rule)//') (output-kind-7 '// &
                trim(value%output_7_kind)//') (output-value-7 '//trim(output_7_value_s)// &
                ') (output-rule-7 '//trim(value%output_7_rule)//') (output-kind-8 '// &
                trim(value%output_8_kind)//') (output-value-8 '//trim(output_8_value_s)// &
                ') (output-rule-8 '//trim(value%output_8_rule)//') (output-kind-9 '// &
                trim(value%output_9_kind)//') (output-value-9 '//trim(output_9_value_s)// &
                ') (output-rule-9 '//trim(value%output_9_rule)//') (output-kind-10 '// &
                trim(value%output_10_kind)//') (output-value-10 '//trim(output_10_value_s)// &
                ') (output-rule-10 '//trim(value%output_10_rule)//') (span '// &
                trim(span_sx)//') (statement-rule '//trim(value%statement_rule)// &
                ') (format-rule '//trim(value%format_rule)//') (output-rule '// &
                trim(value%output_rule)//') (source-document '//trim(value%source_document)// &
                ') (statement-clause '//trim(value%statement_clause)//') (format-clause '// &
                trim(value%format_clause)//') (output-clause '//trim(value%output_clause)// &
                ') (statement-page '//trim(statement_page_s)//') (format-page '// &
                trim(format_page_s)//') (output-page '//trim(output_page_s)// &
                ') (source-hash '//trim(value%source_hash)//'))'
        end if
        ok = .true.
    end subroutine print_stmt_to_sx

end module frontend_print_policy_generated
'''
    generated = _replace_generated_routes(generated)
    return generated


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("schema", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        generated = render(args.schema.read_text(encoding="utf-8"))
    except (OSError, ValueError) as error:
        parser.error(str(error))
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "frontend_print_policy_generated.f90").write_text(
        generated, encoding="utf-8"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
