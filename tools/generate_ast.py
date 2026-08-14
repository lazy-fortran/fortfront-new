#!/usr/bin/env python3
"""Generate the compact Fortran API for the supported frontend AST schema."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


NAME = re.compile(r"[a-z][a-z0-9-]*\Z")


class SchemaError(ValueError):
    pass


def tokenize(text: str) -> list[str]:
    tokens = re.findall(r"[()]|[^\s()]+", text)
    if "(" not in tokens and ")" not in tokens:
        raise SchemaError("schema contains no S-expression")
    return tokens


def parse_expression(tokens: list[str], position: int = 0) -> tuple[list, int]:
    if position >= len(tokens) or tokens[position] != "(":
        raise SchemaError("expected opening parenthesis")
    values: list = []
    position += 1
    while position < len(tokens) and tokens[position] != ")":
        if tokens[position] == "(":
            value, position = parse_expression(tokens, position)
        else:
            if tokens[position] == ")":
                raise SchemaError("unexpected closing parenthesis")
            value = tokens[position]
            position += 1
        values.append(value)
    if position >= len(tokens):
        raise SchemaError("unterminated S-expression")
    return values, position + 1


def atom(value: object, context: str) -> str:
    if not isinstance(value, str) or not NAME.fullmatch(value):
        raise SchemaError(f"{context} is not a canonical schema name")
    return value


def parse_schema(text: str) -> tuple[str, list[dict]]:
    tokens = tokenize(text)
    root, position = parse_expression(tokens)
    if position != len(tokens):
        raise SchemaError("schema has trailing expressions")
    if len(root) < 2 or root[0] != "schema":
        raise SchemaError("schema root must be (schema name ...)")
    schema_name = atom(root[1], "schema name")
    declarations: list[dict] = []
    declared: set[str] = set()
    for declaration in root[2:]:
        if not isinstance(declaration, list) or len(declaration) < 2:
            raise SchemaError("schema declaration must be a non-empty list")
        kind = declaration[0]
        name = atom(declaration[1], "declaration name")
        if name in declared:
            raise SchemaError(f"duplicate declaration: {name}")
        declared.add(name)
        if kind == "primitive":
            if len(declaration) != 2 or name not in {"name", "int"}:
                raise SchemaError(f"unsupported primitive: {name}")
            declarations.append({"kind": "primitive", "name": name})
            continue
        if kind != "record":
            raise SchemaError(f"unsupported declaration kind: {kind}")
        if len(declaration) < 3:
            raise SchemaError(f"record has no fields: {name}")
        fields: list[tuple[str, str]] = []
        field_names: set[str] = set()
        for field in declaration[2:]:
            if not isinstance(field, list) or len(field) != 2:
                raise SchemaError(f"record field is not (name type): {name}")
            field_name = atom(field[0], f"field name in {name}")
            field_type = atom(field[1], f"field type in {name}")
            if field_name in field_names:
                raise SchemaError(f"duplicate field in {name}: {field_name}")
            field_names.add(field_name)
            fields.append((field_name, field_type))
        declarations.append({"kind": "record", "name": name, "fields": fields})

    if not any(item["kind"] == "primitive" and item["name"] == "name"
               for item in declarations):
        raise SchemaError("schema must declare primitive name")
    if not any(item["kind"] == "primitive" and item["name"] == "int"
               for item in declarations):
        raise SchemaError("schema must declare primitive int")
    known = {item["name"] for item in declarations}
    for item in declarations:
        if item["kind"] == "record":
            for field_name, field_type in item["fields"]:
                if field_type not in known:
                    raise SchemaError(
                        f"unknown field type in {item['name']}.{field_name}: {field_type}"
                    )
    return schema_name, declaration_order(declarations)


def declaration_order(declarations: list[dict]) -> list[dict]:
    records = {item["name"]: item for item in declarations if item["kind"] == "record"}
    state: dict[str, int] = {}
    ordered: list[dict] = []

    def visit(name: str) -> None:
        if state.get(name) == 1:
            raise SchemaError("cyclic record dependency")
        if state.get(name) == 2:
            return
        state[name] = 1
        item = records[name]
        for _, field_type in item["fields"]:
            if field_type in records:
                visit(field_type)
        state[name] = 2
        ordered.append(item)

    for item in declarations:
        if item["kind"] == "record":
            visit(item["name"])
    return ordered


def fort_name(name: str) -> str:
    return name.replace("-", "_")


def record_type(name: str) -> str:
    return fort_name(name) + "_t"


def emit_publics(records: list[dict]) -> list[str]:
    names = [record_type(item["name"]) for item in records]
    names += [
        "generated_ast_to_sx",
        "generated_ast_validate",
    ]
    for item in records:
        base = fort_name(item["name"])
        names += [f"{base}_to_sx", f"{base}_validate"]
    lines = ["    public :: " + names[0]]
    for name in names[1:]:
        lines.append("    public :: " + name)
    return lines


def emit_types(records: list[dict]) -> list[str]:
    lines: list[str] = []
    for item in records:
        lines.append(f"    type, public :: {record_type(item['name'])}")
        for field_name, field_type in item["fields"]:
            component = fort_name(field_name)
            if field_type == "name":
                declaration = f"character(len=256) :: {component} = ''"
            elif field_type == "int":
                declaration = f"integer(int64) :: {component} = 0_int64"
            else:
                declaration = f"type({record_type(field_type)}) :: {component}"
            lines.append("        " + declaration)
        lines += [f"    end type {record_type(item['name'])}", ""]
    return lines


def emit_validators(records: list[dict]) -> list[str]:
    lines: list[str] = []
    for item in records:
        base = fort_name(item["name"])
        lines += [
            f"    logical function {base}_validate(value, message)",
            f"        type({record_type(item['name'])}), intent(in) :: value",
            "        character(len=*), intent(out) :: message",
            "",
            "        message = ''",
        ]
        for field_name, field_type in item["fields"]:
            component = fort_name(field_name)
            if field_type == "name":
                lines += [
                    f"        if (.not. generated_valid_atom(value%{component})) then",
                    f"            message = 'invalid-{item['name']}-{field_name}'",
                    "            " + base + "_validate = .false.",
                    "            return",
                    "        end if",
                ]
            elif field_type not in {"int"}:
                lines += [
                    f"        if (.not. {fort_name(field_type)}_validate(value%{component}, &",
                    "            message)) then",
                    "            " + base + "_validate = .false.",
                    "            return",
                    "        end if",
                ]
        field_types = dict(item["fields"])
        if field_types.get("start-byte") == "int":
            lines += [
                f"        if (value%start_byte < 0_int64) then",
                f"            message = 'negative-{item['name']}-start-byte'",
                "            " + base + "_validate = .false.",
                "            return",
                "        end if",
            ]
        if field_types.get("end-byte") == "int" and field_types.get("start-byte") == "int":
            lines += [
                "        if (value%end_byte < value%start_byte) then",
                f"            message = 'invalid-{item['name']}-span'",
                "            " + base + "_validate = .false.",
                "            return",
                "        end if",
            ]
        if field_types.get("declaration-count") == "int" and "declaration" in field_types:
            lines += [
                "        if (value%declaration_count /= 1_int64) then",
                f"            message = 'invalid-{item['name']}-declaration-count'",
                "            " + base + "_validate = .false.",
                "            return",
                "        end if",
            ]
        lines += [
            "        " + base + "_validate = .true.",
            f"    end function {base}_validate",
            "",
        ]
    return lines


def emit_text_functions(records: list[dict]) -> list[str]:
    lines: list[str] = []
    for item in records:
        base = fort_name(item["name"])
        lines += [
            f"    function {base}_sx_text(value, ok, message) result(text)",
            f"        type({record_type(item['name'])}), intent(in) :: value",
            "        logical, intent(out) :: ok",
            "        character(len=*), intent(out) :: message",
            "        character(len=65536) :: text",
            "        character(len=65536) :: child",
            "        character(len=64) :: integer_text",
            "",
            f"        ok = {base}_validate(value, message)",
            "        if (.not. ok) then",
            "            text = ''",
            "            return",
            "        end if",
            f"        text = '({item['name']}'",
        ]
        for field_name, field_type in item["fields"]:
            component = fort_name(field_name)
            if field_type == "name":
                lines += [
                    f"        text = trim(text)//' ({field_name} '// &",
                    f"            trim(value%{component})//')'",
                ]
            elif field_type == "int":
                lines += [
                    f"        write (integer_text, '(i0)') value%{component}",
                    f"        text = trim(text)//' ({field_name} '// &",
                    "            trim(integer_text)//')'",
                ]
            else:
                lines += [
                    f"        child = {fort_name(field_type)}_sx_text(value%{component}, &",
                    "            ok, message)",
                    "        if (.not. ok) then",
                    "            text = ''",
                    "            return",
                    "        end if",
                    f"        text = trim(text)//' ({field_name} '//trim(child)//')'",
                ]
        lines += [
            "        text = trim(text)//')'",
            "    end function " + base + "_sx_text",
            "",
        ]
    return lines


def emit_public_serializers(records: list[dict]) -> list[str]:
    lines: list[str] = []
    for item in records:
        base = fort_name(item["name"])
        lines += [
            f"    subroutine {base}_to_sx(value, output, ok, message)",
            f"        type({record_type(item['name'])}), intent(in) :: value",
            "        character(len=*), intent(out) :: output",
            "        logical, intent(out) :: ok",
            "        character(len=*), intent(out) :: message",
            "        character(len=65536) :: text",
            "",
            "        output = ''",
            f"        text = {base}_sx_text(value, ok, message)",
            "        if (.not. ok) return",
            "        call generated_copy_text(text, output, ok, message)",
            f"    end subroutine {base}_to_sx",
            "",
        ]
    return lines


def emit_dispatch(records: list[dict]) -> list[str]:
    lines = [
        "    subroutine generated_ast_to_sx(value, output, ok, message)",
        "        class(*), intent(in) :: value",
        "        character(len=*), intent(out) :: output",
        "        logical, intent(out) :: ok",
        "        character(len=*), intent(out) :: message",
        "",
        "        output = ''",
        "        select type (value)",
    ]
    for item in records:
        base = fort_name(item["name"])
        lines += [
            f"            type is ({record_type(item['name'])})",
            f"            call {base}_to_sx(value, output, ok, message)",
        ]
    lines += [
        "        class default",
        "            ok = .false.",
        "            message = 'unsupported-generated-record'",
        "        end select",
        "    end subroutine generated_ast_to_sx",
        "",
        "    logical function generated_ast_validate(value, message)",
        "        class(*), intent(in) :: value",
        "        character(len=*), intent(out) :: message",
        "",
        "        select type (value)",
    ]
    for item in records:
        base = fort_name(item["name"])
        lines += [
            f"            type is ({record_type(item['name'])})",
            f"            generated_ast_validate = {base}_validate(value, message)",
        ]
    lines += [
        "        class default",
        "            generated_ast_validate = .false.",
        "            message = 'unsupported-generated-record'",
        "        end select",
        "    end function generated_ast_validate",
    ]
    return lines


def emit_helpers() -> list[str]:
    return [
        "    logical function generated_valid_atom(value)",
        "        character(len=*), intent(in) :: value",
        "        integer :: index",
        "",
        "        generated_valid_atom = .false.",
        "        if (len_trim(value) == 0) return",
        "        do index = 1, len_trim(value)",
        "            if (value(index:index) == ' ' .or. value(index:index) == achar(9) .or. &",
        "                value(index:index) == '(' .or. value(index:index) == ')') return",
        "        end do",
        "        generated_valid_atom = .true.",
        "    end function generated_valid_atom",
        "",
        "    subroutine generated_copy_text(text, output, ok, message)",
        "        character(len=*), intent(in) :: text",
        "        character(len=*), intent(out) :: output",
        "        logical, intent(out) :: ok",
        "        character(len=*), intent(out) :: message",
        "        integer :: text_length",
        "",
        "        text_length = len_trim(text)",
        "        if (text_length > len(output)) then",
        "            output = ''",
        "            ok = .false.",
        "            message = 'sx-output-too-short'",
        "            return",
        "        end if",
        "        output = ''",
        "        if (text_length > 0) output(:text_length) = text(:text_length)",
        "        ok = .true.",
        "        message = ''",
        "    end subroutine generated_copy_text",
    ]


def generate(schema_text: str) -> tuple[str, str]:
    schema_name, records = parse_schema(schema_text)
    module_name = fort_name(schema_name) + "_generated"
    lines = [
        "! Generated by tools/generate_ast.py. Origin: MECHANICAL.",
        f"! Schema: {schema_name}",
        f"module {module_name}",
        "    use, intrinsic :: iso_fortran_env, only: int64",
        "    implicit none",
        "    private",
        "",
    ]
    lines += emit_publics(records) + [""]
    lines += emit_types(records)
    lines += ["contains", ""]
    lines += emit_validators(records)
    lines += emit_text_functions(records)
    lines += emit_public_serializers(records)
    lines += emit_dispatch(records)
    lines += emit_helpers()
    lines += [f"end module {module_name}", ""]
    return module_name, "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("schema", type=Path)
    parser.add_argument("output_dir", type=Path)
    args = parser.parse_args()
    try:
        schema_text = args.schema.read_text(encoding="utf-8")
        module_name, output = generate(schema_text)
        args.output_dir.mkdir(parents=True, exist_ok=True)
        (args.output_dir / f"{module_name}.f90").write_text(output, encoding="utf-8")
    except (OSError, SchemaError) as error:
        parser.exit(1, f"generate_ast: {error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
