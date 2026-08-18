#!/usr/bin/env python3
"""Generate the bounded, source-backed intrinsic type-spec lookup table."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


NAME = re.compile(r"[a-z][a-z0-9-]*\Z")


class SchemaError(ValueError):
    pass


def parse(text: str) -> list[dict[str, object]]:
    tokens = re.findall(r"[()]|[^\s()]+", text)
    if not tokens or tokens[0] != "(":
        raise SchemaError("schema contains no S-expression")

    def expression(position: int) -> tuple[list, int]:
        if position >= len(tokens) or tokens[position] != "(":
            raise SchemaError("expected opening parenthesis")
        values: list = []
        position += 1
        while position < len(tokens) and tokens[position] != ")":
            if tokens[position] == "(":
                value, position = expression(position)
            else:
                value = tokens[position]
                position += 1
            values.append(value)
        if position >= len(tokens):
            raise SchemaError("unterminated S-expression")
        return values, position + 1

    root, position = expression(0)
    if position != len(tokens) or len(root) < 2 or root[0] != "schema":
        raise SchemaError("schema root is invalid")
    if root[1] != "frontend-type-spec-v0":
        raise SchemaError("schema name is invalid")

    result: list[dict[str, object]] = []
    for item in root[2:]:
        if not isinstance(item, list) or len(item) != 7 or item[0] != "type-spec":
            raise SchemaError("type-spec declaration is invalid")
        name = item[1]
        if not isinstance(name, str) or not NAME.fullmatch(name):
            raise SchemaError("type-spec name is invalid")
        fields: dict[str, object] = {}
        for field in item[2:]:
            if not isinstance(field, list) or len(field) < 2:
                raise SchemaError(f"field in {name} is invalid")
            key = field[0]
            if key in fields or not isinstance(key, str):
                raise SchemaError(f"duplicate or invalid field in {name}")
            fields[key] = field[1:]
        if set(fields) != {"source-prefix", "parser-type", "canonical", "variable-name", "source-rule"}:
            raise SchemaError(f"fields in {name} are incomplete")
        prefix = fields["source-prefix"]
        canonical = fields["canonical"]
        parser_type = fields["parser-type"]
        variable_name = fields["variable-name"]
        source_rule = fields["source-rule"]
        if (not isinstance(prefix, list) or not prefix or
                not all(isinstance(value, str) for value in prefix)):
            raise SchemaError(f"source-prefix in {name} is invalid")
        if (not isinstance(canonical, list) or len(canonical) != 1 or
                not isinstance(canonical[0], str) or
                not NAME.fullmatch(canonical[0])):
            raise SchemaError(f"canonical in {name} is invalid")
        if (not isinstance(parser_type, list) or not parser_type or
                not all(isinstance(value, str) for value in parser_type)):
            raise SchemaError(f"parser-type in {name} is invalid")
        if (not isinstance(variable_name, list) or len(variable_name) != 1 or
                variable_name[0] not in {"any", "x"}):
            raise SchemaError(f"variable-name in {name} is invalid")
        if (not isinstance(source_rule, list) or len(source_rule) not in {1, 5} or
                not all(isinstance(value, str) for value in source_rule)):
            raise SchemaError(f"source-rule in {name} is invalid")
        if len(source_rule) == 1:
            if source_rule[0] != "unbound":
                raise SchemaError(f"source-rule in {name} is invalid")
            source_values = ("", "", "", "", "")
        else:
            rule, document, clause, page, source_hash = source_rule
            if (not re.fullmatch(r"[RC][0-9]+", rule) or document != "J3-24-007" or
                    clause != "5" or page not in {"67", "80"} or
                    not re.fullmatch(r"[0-9a-f]{64}", source_hash)):
                raise SchemaError(f"source-rule in {name} is invalid")
            expected_rule = {"integer": "R705", "real": "R706",
                             "double-precision": "R707", "logical": "R704",
                             "character": "R704"}.get(canonical[0])
            expected_page = "80" if canonical[0] in {"logical", "character"} else "67"
            if expected_rule is not None and (rule != expected_rule or page != expected_page):
                raise SchemaError(f"source-rule in {name} is invalid")
            source_values = (rule, document, clause, page, source_hash)
        result.append({
            "name": name,
            "prefix": "  " + " ".join(prefix) + " :: ",
            "parser_type": " ".join(parser_type),
            "canonical": canonical[0],
            "variable_name": variable_name[0],
            "source_rule": source_values[0],
            "source_document": source_values[1],
            "source_clause": source_values[2],
            "source_page": source_values[3],
            "source_hash": source_values[4],
        })
    if not result:
        raise SchemaError("schema has no type-spec declarations")
    return result


def fortran_string(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def render(entries: list[dict[str, object]]) -> str:
    lines = [
        "! Generated by tools/generate_type_specs.py. Origin: MECHANICAL.",
        "module frontend_type_specs_generated",
        "    use, intrinsic :: iso_fortran_env, only: int64",
        "    implicit none",
        "    private",
        "",
        "    type, public :: intrinsic_type_spec_t",
        "        character(len=64) :: source_prefix = ''",
        "        character(len=32) :: canonical = ''",
        "        character(len=32) :: parser_type = ''",
        "        character(len=32) :: variable_name = ''",
        "        character(len=64) :: source_rule = ''",
        "        character(len=128) :: source_document = ''",
        "        character(len=64) :: source_clause = ''",
        "        integer(int64) :: source_page = 0_int64",
        "        character(len=128) :: source_hash = ''",
        "    end type intrinsic_type_spec_t",
        "",
        "    type(intrinsic_type_spec_t), parameter, public :: &",
        "        intrinsic_type_spec_table(%d) = [ &" % len(entries),
    ]
    for index, entry in enumerate(entries):
        comma = ", &" if index < len(entries) - 1 else " ]"
        lines.append(
            "        intrinsic_type_spec_t(%s, %s, %s, %s, %s, %s, %s, %s_int64, %s)%s" % (
                fortran_string(str(entry["prefix"])),
                fortran_string(str(entry["canonical"])),
                fortran_string(str(entry["parser_type"])),
                fortran_string("" if entry["variable_name"] == "any" else "x"),
                fortran_string(str(entry["source_rule"])),
                fortran_string(str(entry["source_document"])),
                fortran_string(str(entry["source_clause"])),
                str(entry["source_page"] or "0"),
                fortran_string(str(entry["source_hash"])),
                comma,
            )
        )
    lines += [
        "",
        "    public :: intrinsic_type_spec_lookup",
        "    public :: intrinsic_type_spec_declaration",
        "    public :: intrinsic_type_spec_variable_allowed",
        "",
        "contains",
        "",
        "    logical function intrinsic_type_spec_lookup(line, spec_index, variable_start)",
        "        character(len=*), intent(in) :: line",
        "        integer, intent(out) :: spec_index",
        "        integer, intent(out) :: variable_start",
        "        integer :: index",
        "",
        "        spec_index = 0",
        "        variable_start = 0",
        "        do index = 1, size(intrinsic_type_spec_table)",
        "            if (len(line) < len_trim(intrinsic_type_spec_table(index)%source_prefix)) cycle",
        "            if (line(:len_trim(intrinsic_type_spec_table(index)%source_prefix)) == &",
        "                trim(intrinsic_type_spec_table(index)%source_prefix)) then",
        "                spec_index = index",
        "                variable_start = len_trim(intrinsic_type_spec_table(index)%source_prefix) + 2",
        "                intrinsic_type_spec_lookup = .true.",
        "                return",
        "            end if",
        "        end do",
        "        intrinsic_type_spec_lookup = .false.",
        "    end function intrinsic_type_spec_lookup",
        "",
        "    logical function intrinsic_type_spec_declaration(spec_index, name, declaration)",
        "        integer, intent(in) :: spec_index",
        "        character(len=*), intent(in) :: name",
        "        character(len=*), intent(out) :: declaration",
        "        integer :: prefix_length, name_length",
        "",
        "        declaration = ''",
        "        intrinsic_type_spec_declaration = .false.",
        "        if (spec_index < 1) return",
        "        if (spec_index > size(intrinsic_type_spec_table)) return",
        "        prefix_length = len_trim(intrinsic_type_spec_table(spec_index)%source_prefix) + 1",
        "        name_length = len_trim(name)",
        "        if (name_length == 0) return",
        "        if (prefix_length + name_length > len(declaration)) return",
        "        declaration(:prefix_length) = intrinsic_type_spec_table(spec_index)%source_prefix(:prefix_length)",
        "        declaration(prefix_length + 1:prefix_length + name_length) = trim(name)",
        "        intrinsic_type_spec_declaration = .true.",
        "    end function intrinsic_type_spec_declaration",
        "",
        "    logical function intrinsic_type_spec_variable_allowed(spec_index, name)",
        "        integer, intent(in) :: spec_index",
        "        character(len=*), intent(in) :: name",
        "",
        "        intrinsic_type_spec_variable_allowed = .false.",
        "        if (spec_index < 1 .or. spec_index > size(intrinsic_type_spec_table)) return",
        "        if (len_trim(intrinsic_type_spec_table(spec_index)%variable_name) == 0) then",
        "            intrinsic_type_spec_variable_allowed = .true.",
        "        else if (trim(name) == trim(intrinsic_type_spec_table(spec_index)%variable_name)) then",
        "            intrinsic_type_spec_variable_allowed = .true.",
        "        end if",
        "    end function intrinsic_type_spec_variable_allowed",
        "",
        "end module frontend_type_specs_generated",
        "",
    ]
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("schema", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        generated = render(parse(args.schema.read_text(encoding="utf-8")))
    except (OSError, SchemaError) as error:
        parser.error(str(error))
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "frontend_type_specs_generated.f90").write_text(generated, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
