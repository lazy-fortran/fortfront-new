module fortfront_frontend
    use frontend_ast_v0_generated, only: generated_source_span_t => source_span_t, &
        generated_program_root_t => program_root_t, &
        generated_program_declaration_t => program_declaration_t, &
        generated_program_unit_t => program_unit_t, &
        generated_source_span_validate => source_span_validate, &
        generated_program_unit_to_sx => program_unit_to_sx, &
        generated_program_unit_validate => program_unit_validate
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    character(len=*), parameter, public :: frontend_accepted = 'accepted'
    character(len=*), parameter, public :: frontend_rejected = 'rejected'
    character(len=*), parameter, public :: severity_note = 'note'
    character(len=*), parameter, public :: severity_warning = 'warning'
    character(len=*), parameter, public :: severity_error = 'error'
    character(len=*), parameter, public :: root_kind_source = 'source'
    character(len=*), parameter, public :: root_kind_program = 'program'
    character(len=*), parameter, public :: root_kind_module = 'module'
    character(len=*), parameter, public :: root_kind_none = 'none'
    character(len=*), parameter, public :: declaration_kind_program = 'program'
    character(len=*), parameter, public :: declaration_kind_module = 'module'
    integer, parameter, public :: program_unit_declaration_capacity = 16
    integer, parameter, public :: semantic_item_table_capacity = 16
    integer, parameter, public :: diagnostic_table_capacity = 16

    type, public :: source_span_t
        character(len=256) :: file = ''
        integer(int64) :: start_byte = 0_int64
        integer(int64) :: end_byte = 0_int64
        character(len=128) :: source_hash = ''
    end type source_span_t

    type, public :: standardir_source_ref_t
        character(len=128) :: document = ''
        character(len=64) :: clause = ''
        character(len=64) :: rule = ''
        integer(int64) :: page = 0_int64
        character(len=128) :: source_hash = ''
    end type standardir_source_ref_t

    type, public :: standardir_syntax_item_t
        character(len=64) :: id = ''
        character(len=64) :: lhs = ''
        character(len=32) :: origin = ''
        character(len=32) :: resolution = ''
        type(standardir_source_ref_t) :: source
    end type standardir_syntax_item_t

    type, public :: standardir_semantic_item_t
        character(len=64) :: id = ''
        character(len=64) :: subject = ''
        character(len=32) :: origin = ''
        character(len=32) :: resolution = ''
        type(standardir_source_ref_t) :: source
    end type standardir_semantic_item_t

    type, public :: diagnostic_t
        character(len=8) :: status = frontend_rejected
        character(len=8) :: severity = severity_error
        character(len=128) :: message = ''
        type(source_span_t) :: span
    end type diagnostic_t

    type, public :: frontend_root_t
        character(len=32) :: kind = root_kind_none
        character(len=128) :: name = ''
        type(source_span_t) :: span
    end type frontend_root_t

    type, public :: program_root_t
        character(len=128) :: name = ''
        type(source_span_t) :: span
    end type program_root_t

    type, public :: program_declaration_t
        character(len=32) :: declaration_kind = ''
        character(len=128) :: name = ''
        type(source_span_t) :: span
    end type program_declaration_t

    type, public :: program_unit_t
        type(program_root_t) :: root
        integer(int64) :: declaration_count = 0_int64
        type(program_declaration_t) :: declarations(program_unit_declaration_capacity)
    end type program_unit_t

    type, public :: frontend_result_t
        character(len=8) :: status = frontend_rejected
        character(len=32) :: root_kind = root_kind_none
        integer(int64) :: diagnostic_count = 0_int64
        type(frontend_root_t) :: root
        type(diagnostic_t), allocatable :: diagnostics(:)
    end type frontend_result_t

    type, public :: frontend_result_header_t
        character(len=8) :: status = frontend_rejected
        character(len=32) :: root_kind = root_kind_none
        character(len=128) :: root_name = ''
        integer(int64) :: diagnostic_count = 0_int64
        type(source_span_t) :: span
    end type frontend_result_header_t

    public :: frontend_parse, frontend_read, frontend_result_from_sx, &
        frontend_parse_generated_program_unit, &
        frontend_result_to_sx, frontend_validate, &
        frontend_result_to_program_root, frontend_result_to_program_root_sx, &
        frontend_result_to_program_unit, &
        frontend_result_to_program_unit_sx, &
        frontend_query_program_unit, &
        frontend_query_program_declaration_count, &
        frontend_query_program_declaration_at, &
        frontend_query_diagnostic, &
        frontend_query_diagnostic_at, &
        frontend_query_diagnostic_count, &
        frontend_query_result_span, &
        frontend_query_result_header, &
        frontend_validate_program_unit_handoff, &
        standardir_syntax_item_to_sx, standardir_syntax_item_from_sx, &
        standardir_syntax_item_validate, &
        frontend_validate_semantic_item, program_root_to_sx, &
        frontend_validate_semantic_table, &
        diagnostic_to_sx, diagnostic_from_sx, diagnostic_validate, &
        frontend_validate_diagnostic_table, &
        program_root_from_sx, program_root_validate, &
        program_declaration_to_sx, program_declaration_from_sx, &
        program_declaration_validate, program_unit_to_sx, &
        program_unit_from_sx, program_unit_validate, &
        generated_source_span_t, generated_program_root_t, &
        generated_program_declaration_t, generated_program_unit_t, &
        frontend_generated_program_unit_to_sx, &
        frontend_validate_generated_program_unit

contains

    subroutine frontend_generated_program_unit_to_sx(unit, output, ok, message)
        type(generated_program_unit_t), intent(in) :: unit
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generated_program_unit_to_sx(unit, output, ok, message)
    end subroutine frontend_generated_program_unit_to_sx

    logical function frontend_validate_generated_program_unit(unit, message)
        type(generated_program_unit_t), intent(in) :: unit
        character(len=*), intent(out) :: message

        frontend_validate_generated_program_unit = &
            generated_program_unit_validate(unit, message)
    end function frontend_validate_generated_program_unit

    subroutine frontend_read(file_name, source, source_hash, result)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
        type(frontend_result_t), intent(out) :: result

        type(standardir_syntax_item_t) :: missing_witness

        call frontend_parse(file_name, source, source_hash, missing_witness, result)
    end subroutine frontend_read

    subroutine frontend_parse_generated_program_unit(file_name, source, source_hash, &
            syntax_item, unit, ok, message)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
        type(standardir_syntax_item_t), intent(in) :: syntax_item
        type(generated_program_unit_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(frontend_result_t) :: result
        type(generated_source_span_t) :: span
        type(generated_source_span_t) :: declaration_span
        character(len=128) :: program_name
        character(len=32) :: unit_kind
        integer(int64) :: declaration_end
        integer(int64) :: declaration_start
        integer(int64) :: unit_end
        integer(int64) :: unit_start

        unit = generated_program_unit_t()
        ok = .false.
        message = ''

        span%file = file_name
        span%start_byte = 0_int64
        span%end_byte = int(len(source), int64)
        span%source_hash = source_hash
        if (len_trim(file_name) > len(span%file)) then
            message = 'invalid-source-span-file'
            return
        end if
        if (len_trim(source_hash) > len(span%source_hash)) then
            message = 'invalid-source-span-source-hash'
            return
        end if
        if (.not. generated_source_span_validate(span, message)) return

        call frontend_parse(file_name, source, source_hash, syntax_item, result)
        if (trim(result%status) /= frontend_accepted) then
            if (allocated(result%diagnostics)) then
                message = result%diagnostics(1)%message
            else
                message = 'frontend-rejected'
            end if
            return
        end if
        program_name = result%root%name
        unit_kind = trim(result%root_kind)
        if (.not. parse_program_witness(source, program_name, message, &
            unit_start=unit_start, unit_end=unit_end, &
            declaration_start=declaration_start, declaration_end=declaration_end, &
            expected_kind=unit_kind)) return
        span%start_byte = unit_start
        span%end_byte = unit_end
        if (.not. generated_source_span_validate(span, message)) return
        declaration_span = span
        declaration_span%start_byte = declaration_start
        declaration_span%end_byte = declaration_end
        if (.not. generated_source_span_validate(declaration_span, message)) return

        unit%root%name = program_name
        unit%root%span = span
        unit%declaration_count = 1_int64
        unit%declaration%declaration_kind = unit_kind
        unit%declaration%name = program_name
        unit%declaration%span = declaration_span
        ok = generated_program_unit_validate(unit, message)
    end subroutine frontend_parse_generated_program_unit

    subroutine frontend_parse(file_name, source, source_hash, syntax_item, result)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
        type(standardir_syntax_item_t), intent(in) :: syntax_item
        type(frontend_result_t), intent(out) :: result

        type(source_span_t) :: span
        character(len=128) :: program_name
        character(len=128) :: diagnostic_message

        span%file = file_name
        span%start_byte = 0_int64
        span%end_byte = int(len(source), int64)
        span%source_hash = source_hash
        result%root%span = span

        if (len(source) == 0) then
            diagnostic_message = 'empty-source'
        else
            if (validate_syntax_item(syntax_item, diagnostic_message)) then
                if (parse_program_witness(source, program_name, diagnostic_message, &
                    expected_kind=syntax_item%lhs)) then
                    result%status = frontend_accepted
                    result%root_kind = lowercase(trim(syntax_item%lhs))
                    result%root%kind = result%root_kind
                    result%root%name = program_name
                    result%diagnostic_count = 0_int64
                    return
                end if
            end if
        end if

        result%status = frontend_rejected
        result%root_kind = root_kind_none
        result%root%kind = root_kind_none
        result%root%name = ''
        result%diagnostic_count = 1_int64
        allocate (result%diagnostics(1))
        result%diagnostics(1)%status = frontend_rejected
        result%diagnostics(1)%severity = severity_error
        result%diagnostics(1)%message = diagnostic_message
        result%diagnostics(1)%span = span
    end subroutine frontend_parse

    subroutine standardir_syntax_item_to_sx(item, output, ok, message)
        type(standardir_syntax_item_t), intent(in) :: item
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=2048) :: canonical
        character(len=32) :: page

        output = ''
        ok = standardir_syntax_item_validate(item, message)
        if (.not. ok) return

        write (page, '(i0)') item%source%page
        canonical = '(syntax-item (id '//trim(item%id)//') (lhs '// &
            trim(item%lhs)//') (source (source-ref (document '// &
            trim(item%source%document)//') (clause '// &
            trim(item%source%clause)//') (rule '//trim(item%source%rule)//') '// &
            '(page '//trim(page)//') (source-hash '// &
            trim(item%source%source_hash)//'))) (origin '// &
            trim(item%origin)//') (resolution '//trim(item%resolution)//'))'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine standardir_syntax_item_to_sx

    subroutine standardir_syntax_item_from_sx(input, item, ok, message, &
            expected_source_hash)
        character(len=*), intent(in) :: input
        type(standardir_syntax_item_t), intent(out) :: item
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_source_hash

        character(len=64) :: id, lhs
        character(len=32) :: origin, resolution
        character(len=2048) :: source_expression
        integer :: position

        item = standardir_syntax_item_t()
        id = ''
        lhs = ''
        origin = ''
        resolution = ''
        source_expression = ''
        position = 1

        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(syntax-item')) then
            ok = .false.
            message = 'malformed-syntax-item'
            return
        end if
        if (.not. consume_sx_field(input, position, 'id', id)) then
            ok = .false.
            message = 'malformed-syntax-item-id'
            return
        end if
        if (.not. consume_sx_field(input, position, 'lhs', lhs)) then
            ok = .false.
            message = 'malformed-syntax-item-lhs'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        if (.not. consume_sx_text(input, position, 'source')) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        if (.not. consume_sx_expression(input, position, source_expression)) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        call parse_standardir_source_ref_sx(trim(source_expression), item%source, &
            ok, message)
        if (.not. ok) return
        if (.not. consume_sx_field(input, position, 'origin', origin)) then
            ok = .false.
            message = 'malformed-syntax-item-origin'
            return
        end if
        if (.not. consume_sx_field(input, position, 'resolution', resolution)) then
            ok = .false.
            message = 'malformed-syntax-item-resolution'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-syntax-item'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-syntax-item'
            return
        end if

        item%id = id
        item%lhs = lhs
        item%origin = origin
        item%resolution = resolution
        if (present(expected_source_hash)) then
            ok = standardir_syntax_item_validate(item, message, expected_source_hash)
        else
            ok = standardir_syntax_item_validate(item, message)
        end if
    end subroutine standardir_syntax_item_from_sx

    logical function standardir_syntax_item_validate(item, message, &
            expected_source_hash)
        type(standardir_syntax_item_t), intent(in) :: item
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_source_hash

        message = ''
        if (len_trim(item%id) == 0) then
            message = 'missing-syntax-item-id'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(item%id)) then
            message = 'invalid-syntax-item-id'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (len_trim(item%lhs) == 0) then
            message = 'missing-syntax-item-lhs'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(item%lhs)) then
            message = 'invalid-syntax-item-lhs'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (.not. valid_origin(item%origin)) then
            message = 'invalid-syntax-item-origin'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (.not. valid_resolution(item%resolution)) then
            message = 'invalid-syntax-item-resolution'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (len_trim(item%source%document) == 0 .or. &
            len_trim(item%source%clause) == 0 .or. &
            len_trim(item%source%rule) == 0 .or. &
            item%source%page <= 0_int64 .or. &
            len_trim(item%source%source_hash) == 0) then
            message = 'invalid-syntax-item-provenance'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(item%source%document) .or. &
            .not. valid_sx_atom(item%source%clause) .or. &
            .not. valid_sx_atom(item%source%rule) .or. &
            .not. valid_sx_atom(item%source%source_hash)) then
            message = 'invalid-syntax-item-provenance'
            standardir_syntax_item_validate = .false.
            return
        end if
        if (present(expected_source_hash)) then
            if (len_trim(expected_source_hash) == 0) then
                message = 'missing-expected-source-hash'
                standardir_syntax_item_validate = .false.
                return
            end if
            if (trim(item%source%source_hash) /= trim(expected_source_hash)) then
                message = 'syntax-item-source-hash-mismatch'
                standardir_syntax_item_validate = .false.
                return
            end if
        end if
        standardir_syntax_item_validate = .true.
    end function standardir_syntax_item_validate

    subroutine parse_standardir_source_ref_sx(input, source, ok, message)
        character(len=*), intent(in) :: input
        type(standardir_source_ref_t), intent(out) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: document, source_hash
        character(len=64) :: clause, rule
        integer(int64) :: page
        integer :: position

        source = standardir_source_ref_t()
        document = ''
        clause = ''
        rule = ''
        source_hash = ''
        page = 0_int64
        position = 1

        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(source-ref')) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        if (.not. consume_sx_field(input, position, 'document', document)) then
            ok = .false.
            message = 'malformed-syntax-item-document'
            return
        end if
        if (.not. consume_sx_field(input, position, 'clause', clause)) then
            ok = .false.
            message = 'malformed-syntax-item-clause'
            return
        end if
        if (.not. consume_sx_field(input, position, 'rule', rule)) then
            ok = .false.
            message = 'malformed-syntax-item-rule'
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'page', page, message)) then
            call map_syntax_item_integer_failure(message)
            ok = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'source-hash', source_hash)) then
            ok = .false.
            message = 'malformed-syntax-item-source-hash'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-syntax-item-source'
            return
        end if

        source%document = document
        source%clause = clause
        source%rule = rule
        source%page = page
        source%source_hash = source_hash
        ok = .true.
        message = ''
    end subroutine parse_standardir_source_ref_sx

    subroutine map_syntax_item_integer_failure(message)
        character(len=*), intent(inout) :: message

        select case (trim(message))
        case ('negative-diagnostic-count')
            message = 'negative-syntax-item-page'
        case ('diagnostic-count-too-large')
            message = 'syntax-item-page-too-large'
        case default
            message = 'malformed-syntax-item-page'
        end select
    end subroutine map_syntax_item_integer_failure

    subroutine frontend_result_to_program_root(result, root, ok, message)
        type(frontend_result_t), intent(in) :: result
        type(program_root_t), intent(out) :: root
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        root = program_root_t()
        ok = frontend_validate(result, message)
        if (.not. ok) return
        if (trim(result%status) /= frontend_accepted) then
            message = 'rejected-frontend-result'
            ok = .false.
            return
        end if
        if ((trim(result%root_kind) /= root_kind_program .and. &
            trim(result%root_kind) /= root_kind_module) .or. &
            trim(result%root%kind) /= trim(result%root_kind)) then
            message = 'non-program-root'
            ok = .false.
            return
        end if

        root%name = result%root%name
        root%span = result%root%span
        message = ''
    end subroutine frontend_result_to_program_root

    subroutine frontend_result_to_program_unit(result, unit, ok, message)
        type(frontend_result_t), intent(in) :: result
        type(program_unit_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        unit = program_unit_t()
        call frontend_result_to_program_root(result, unit%root, ok, message)
        if (.not. ok) return

        unit%declaration_count = 0_int64
        ok = program_unit_validate(unit, message)
    end subroutine frontend_result_to_program_unit

    subroutine frontend_result_to_program_unit_sx(result, output, ok, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(program_unit_t) :: unit

        output = ''
        call frontend_result_to_program_unit(result, unit, ok, message)
        if (.not. ok) return
        if (.not. frontend_validate_program_unit_handoff(result, unit, message)) then
            ok = .false.
            return
        end if
        call program_unit_to_sx(unit, output, ok, message)
    end subroutine frontend_result_to_program_unit_sx

    logical function frontend_query_program_unit(result, expected_file, &
            expected_source_hash, unit, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        type(program_unit_t), intent(out) :: unit
        character(len=*), intent(out) :: message

        logical :: ok

        unit = program_unit_t()
        frontend_query_program_unit = .false.
        if (.not. frontend_validate(result, message)) return
        call frontend_result_to_program_unit(result, unit, ok, message)
        if (.not. ok) return
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if
        if (trim(unit%root%span%file) /= trim(expected_file)) then
            message = 'program-unit-source-file-mismatch'
            return
        end if
        if (trim(unit%root%span%source_hash) /= trim(expected_source_hash)) then
            message = 'program-unit-source-hash-mismatch'
            return
        end if
        message = ''
        frontend_query_program_unit = .true.
    end function frontend_query_program_unit

    logical function frontend_query_program_declaration_count(result, &
            expected_file, expected_source_hash, declaration_count, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        integer(int64), intent(out) :: declaration_count
        character(len=*), intent(out) :: message

        type(program_unit_t) :: unit
        logical :: ok

        declaration_count = 0_int64
        frontend_query_program_declaration_count = .false.
        if (.not. frontend_validate(result, message)) return
        call frontend_result_to_program_unit(result, unit, ok, message)
        if (.not. ok) return
        if (.not. frontend_validate_program_unit_handoff(result, unit, message)) then
            return
        end if
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if
        if (trim(unit%root%span%file) /= trim(expected_file)) then
            message = 'program-unit-source-file-mismatch'
            return
        end if
        if (trim(unit%root%span%source_hash) /= trim(expected_source_hash)) then
            message = 'program-unit-source-hash-mismatch'
            return
        end if
        declaration_count = unit%declaration_count
        message = ''
        frontend_query_program_declaration_count = .true.
    end function frontend_query_program_declaration_count

    logical function frontend_query_program_declaration_at(result, &
            declaration_index, expected_file, expected_source_hash, &
            declaration, message)
        type(frontend_result_t), intent(in) :: result
        integer(int64), intent(in) :: declaration_index
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        type(program_declaration_t), intent(out) :: declaration
        character(len=*), intent(out) :: message

        type(program_unit_t) :: unit
        logical :: ok

        declaration = program_declaration_t()
        frontend_query_program_declaration_at = .false.
        if (.not. frontend_validate(result, message)) return
        call frontend_result_to_program_unit(result, unit, ok, message)
        if (.not. ok) return
        if (declaration_index < 0_int64) then
            message = 'negative-program-declaration-index'
            return
        end if

        unit%declaration_count = 1_int64
        unit%declarations(1)%declaration_kind = result%root_kind
        unit%declarations(1)%name = unit%root%name
        unit%declarations(1)%span = unit%root%span
        if (.not. frontend_validate_program_unit_handoff(result, unit, message)) then
            return
        end if
        if (declaration_index == 0_int64 .or. &
            declaration_index > unit%declaration_count) then
            message = 'program-declaration-index-out-of-range'
            return
        end if
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if

        declaration = unit%declarations(declaration_index)
        if (.not. program_declaration_validate(declaration, message)) return
        if (trim(declaration%span%file) /= trim(expected_file)) then
            message = 'program-declaration-source-file-mismatch'
            return
        end if
        if (trim(declaration%span%source_hash) /= trim(expected_source_hash)) then
            message = 'program-declaration-source-hash-mismatch'
            return
        end if
        message = ''
        frontend_query_program_declaration_at = .true.
    end function frontend_query_program_declaration_at

    logical function frontend_query_diagnostic(result, expected_file, &
            expected_source_hash, diagnostic, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        type(diagnostic_t), intent(out) :: diagnostic
        character(len=*), intent(out) :: message

        logical :: ok

        diagnostic = diagnostic_t()
        frontend_query_diagnostic = .false.
        if (.not. frontend_validate(result, message)) return
        if (trim(result%status) /= frontend_rejected) then
            message = 'accepted-frontend-result'
            return
        end if
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if

        diagnostic = result%diagnostics(1)
        ok = diagnostic_validate(diagnostic, message, expected_source_hash)
        if (.not. ok) return
        if (trim(diagnostic%status) /= trim(result%status)) then
            message = 'diagnostic-result-status-mismatch'
            return
        end if
        if (trim(diagnostic%span%file) /= trim(expected_file)) then
            message = 'diagnostic-source-file-mismatch'
            return
        end if
        message = ''
        frontend_query_diagnostic = .true.
    end function frontend_query_diagnostic

    logical function frontend_query_diagnostic_at(result, diagnostic_index, &
            expected_file, expected_source_hash, diagnostic, message)
        type(frontend_result_t), intent(in) :: result
        integer(int64), intent(in) :: diagnostic_index
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        type(diagnostic_t), intent(out) :: diagnostic
        character(len=*), intent(out) :: message

        logical :: ok

        diagnostic = diagnostic_t()
        frontend_query_diagnostic_at = .false.
        if (.not. frontend_validate(result, message)) return
        if (trim(result%status) /= frontend_rejected) then
            message = 'accepted-frontend-result'
            return
        end if
        if (diagnostic_index < 0_int64) then
            message = 'negative-diagnostic-index'
            return
        end if
        if (diagnostic_index == 0_int64 .or. &
            diagnostic_index > result%diagnostic_count) then
            message = 'diagnostic-index-out-of-range'
            return
        end if
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if

        diagnostic = result%diagnostics(diagnostic_index)
        ok = diagnostic_validate(diagnostic, message, expected_source_hash)
        if (.not. ok) return
        if (trim(diagnostic%status) /= trim(result%status)) then
            message = 'diagnostic-result-status-mismatch'
            return
        end if
        if (trim(diagnostic%span%file) /= trim(expected_file)) then
            message = 'diagnostic-source-file-mismatch'
            return
        end if
        message = ''
        frontend_query_diagnostic_at = .true.
    end function frontend_query_diagnostic_at

    logical function frontend_query_diagnostic_count(result, expected_file, &
            expected_source_hash, diagnostic_count, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        integer(int64), intent(out) :: diagnostic_count
        character(len=*), intent(out) :: message

        integer :: index
        type(diagnostic_t) :: diagnostic
        type(program_root_t) :: root

        diagnostic_count = 0_int64
        frontend_query_diagnostic_count = .false.
        if (.not. frontend_validate(result, message)) return
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if

        select case (trim(result%status))
        case (frontend_accepted)
            if (trim(result%root%kind) /= trim(result%root_kind)) then
                message = 'frontend-result-root-kind-mismatch'
                return
            end if
            root%name = result%root%name
            root%span = result%root%span
            if (.not. program_root_validate(root, message)) return
            if (trim(root%span%file) /= trim(expected_file)) then
                message = 'frontend-result-source-file-mismatch'
                return
            end if
            if (trim(root%span%source_hash) /= trim(expected_source_hash)) then
                message = 'frontend-result-source-hash-mismatch'
                return
            end if
        case (frontend_rejected)
            do index = 1, int(result%diagnostic_count)
                diagnostic = result%diagnostics(index)
                if (.not. diagnostic_validate(diagnostic, message)) return
                if (trim(diagnostic%status) /= trim(result%status)) then
                    message = 'diagnostic-result-status-mismatch'
                    return
                end if
                if (trim(diagnostic%span%file) /= trim(expected_file)) then
                    message = 'diagnostic-source-file-mismatch'
                    return
                end if
                if (trim(diagnostic%span%source_hash) /= &
                    trim(expected_source_hash)) then
                    message = 'diagnostic-source-hash-mismatch'
                    return
                end if
            end do
        end select

        diagnostic_count = result%diagnostic_count
        message = ''
        frontend_query_diagnostic_count = .true.
    end function frontend_query_diagnostic_count

    logical function frontend_query_result_span(result, expected_root_kind, &
            expected_file, expected_source_hash, expected_start_byte, &
            expected_end_byte, span, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_root_kind
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        integer(int64), intent(in) :: expected_start_byte
        integer(int64), intent(in) :: expected_end_byte
        type(source_span_t), intent(out) :: span
        character(len=*), intent(out) :: message

        type(program_root_t) :: root

        span = source_span_t()
        frontend_query_result_span = .false.
        if (.not. frontend_validate(result, message)) return
        if (trim(result%status) /= frontend_accepted) then
            message = 'rejected-frontend-result'
            return
        end if
        if (.not. valid_root_kind(expected_root_kind) .or. &
            trim(expected_root_kind) == root_kind_none) then
            message = 'invalid-expected-root-kind'
            return
        end if
        if (trim(result%root_kind) /= trim(expected_root_kind) .or. &
            trim(result%root%kind) /= trim(expected_root_kind)) then
            message = 'frontend-result-root-kind-mismatch'
            return
        end if

        root%name = result%root%name
        root%span = result%root%span
        if (.not. program_root_validate(root, message)) return
        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if
        if (expected_start_byte < 0_int64 .or. &
            expected_end_byte < expected_start_byte) then
            message = 'invalid-expected-source-span'
            return
        end if
        if (trim(root%span%file) /= trim(expected_file)) then
            message = 'frontend-result-source-file-mismatch'
            return
        end if
        if (trim(root%span%source_hash) /= trim(expected_source_hash)) then
            message = 'frontend-result-source-hash-mismatch'
            return
        end if
        if (root%span%start_byte /= expected_start_byte .or. &
            root%span%end_byte /= expected_end_byte) then
            message = 'frontend-result-source-span-mismatch'
            return
        end if

        span = root%span
        message = ''
        frontend_query_result_span = .true.
    end function frontend_query_result_span

    logical function frontend_query_result_header(result, expected_file, &
            expected_source_hash, header, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected_file
        character(len=*), intent(in) :: expected_source_hash
        type(frontend_result_header_t), intent(out) :: header
        character(len=*), intent(out) :: message

        type(diagnostic_t) :: diagnostic
        type(program_root_t) :: root

        header = frontend_result_header_t()
        frontend_query_result_header = .false.
        if (.not. frontend_validate(result, message)) return

        if (len_trim(expected_file) == 0) then
            message = 'missing-expected-source-file'
            return
        end if
        if (len_trim(expected_source_hash) == 0) then
            message = 'missing-expected-source-hash'
            return
        end if

        header%status = result%status
        header%root_kind = result%root_kind
        header%root_name = result%root%name
        header%diagnostic_count = result%diagnostic_count

        select case (trim(result%status))
        case (frontend_accepted)
            if (trim(result%root%kind) /= trim(result%root_kind)) then
                message = 'frontend-result-root-kind-mismatch'
                return
            end if
            root%name = result%root%name
            root%span = result%root%span
            if (.not. program_root_validate(root, message)) return
            header%span = root%span
        case (frontend_rejected)
            diagnostic = result%diagnostics(1)
            if (.not. diagnostic_validate(diagnostic, message)) return
            if (trim(diagnostic%status) /= trim(result%status)) then
                message = 'diagnostic-result-status-mismatch'
                return
            end if
            header%span = diagnostic%span
        end select

        if (trim(header%span%file) /= trim(expected_file)) then
            message = 'frontend-result-source-file-mismatch'
            return
        end if
        if (trim(header%span%source_hash) /= trim(expected_source_hash)) then
            message = 'frontend-result-source-hash-mismatch'
            return
        end if
        message = ''
        frontend_query_result_header = .true.
    end function frontend_query_result_header

    logical function frontend_validate_program_unit_handoff(result, unit, message)
        type(frontend_result_t), intent(in) :: result
        type(program_unit_t), intent(in) :: unit
        character(len=*), intent(out) :: message

        type(program_root_t) :: expected_root
        logical :: ok

        message = ''
        call frontend_result_to_program_root(result, expected_root, ok, message)
        if (.not. ok) then
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        if (.not. program_unit_validate(unit, message)) then
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        if (trim(unit%root%name) /= trim(expected_root%name)) then
            message = 'program-unit-root-name-mismatch'
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        if (trim(unit%root%span%file) /= trim(expected_root%span%file)) then
            message = 'program-unit-root-file-mismatch'
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        if (unit%root%span%start_byte /= expected_root%span%start_byte .or. &
            unit%root%span%end_byte /= expected_root%span%end_byte) then
            message = 'program-unit-root-span-mismatch'
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        if (trim(unit%root%span%source_hash) /= &
            trim(expected_root%span%source_hash)) then
            message = 'program-unit-root-source-hash-mismatch'
            frontend_validate_program_unit_handoff = .false.
            return
        end if
        frontend_validate_program_unit_handoff = .true.
    end function frontend_validate_program_unit_handoff

    subroutine frontend_result_to_program_root_sx(result, output, ok, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(program_root_t) :: root

        output = ''
        call frontend_result_to_program_root(result, root, ok, message)
        if (.not. ok) return
        call program_root_to_sx(root, output, ok, message)
    end subroutine frontend_result_to_program_root_sx

    subroutine program_declaration_to_sx(declaration, output, ok, message)
        type(program_declaration_t), intent(in) :: declaration
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=1024) :: canonical
        character(len=32) :: start_byte, end_byte

        output = ''
        ok = program_declaration_validate(declaration, message)
        if (.not. ok) return

        write (start_byte, '(i0)') declaration%span%start_byte
        write (end_byte, '(i0)') declaration%span%end_byte
        canonical = '(program-declaration (declaration-kind '// &
            trim(declaration%declaration_kind)//') (name '//trim(declaration%name)//') '// &
            '(span (file '//trim(declaration%span%file)//') (start-byte '// &
            trim(start_byte)//') (end-byte '//trim(end_byte)//') '// &
            '(source-hash '//trim(declaration%span%source_hash)//')))'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine program_declaration_to_sx

    subroutine program_declaration_from_sx(input, declaration, ok, message)
        character(len=*), intent(in) :: input
        type(program_declaration_t), intent(out) :: declaration
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32) :: declaration_kind
        character(len=128) :: name, file, source_hash
        integer(int64) :: start_byte, end_byte
        integer :: position

        declaration = program_declaration_t()
        declaration_kind = ''
        name = ''
        file = ''
        source_hash = ''
        start_byte = 0_int64
        end_byte = 0_int64
        position = 1
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(program-declaration')) then
            ok = .false.
            message = 'malformed-program-declaration'
            return
        end if
        if (.not. consume_sx_field(input, position, 'declaration-kind', &
            declaration_kind)) then
            ok = .false.
            message = 'malformed-program-declaration-kind'
            return
        end if
        if (.not. consume_sx_field(input, position, 'name', name)) then
            ok = .false.
            message = 'malformed-program-declaration-name'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) then
            ok = .false.
            message = 'malformed-program-declaration-span'
            return
        end if
        if (.not. consume_sx_text(input, position, 'span')) then
            ok = .false.
            message = 'malformed-program-declaration-span'
            return
        end if
        if (.not. consume_sx_field(input, position, 'file', file)) then
            ok = .false.
            message = 'malformed-program-declaration-file'
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'start-byte', &
            start_byte, message)) then
            ok = .false.
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'end-byte', &
            end_byte, message)) then
            ok = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'source-hash', source_hash)) then
            ok = .false.
            message = 'malformed-program-declaration-source-hash'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-program-declaration-span'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-program-declaration'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-program-declaration'
            return
        end if

        declaration%declaration_kind = declaration_kind
        declaration%name = name
        declaration%span%file = file
        declaration%span%start_byte = start_byte
        declaration%span%end_byte = end_byte
        declaration%span%source_hash = source_hash
        ok = program_declaration_validate(declaration, message)
    end subroutine program_declaration_from_sx

    logical function program_declaration_validate(declaration, message)
        type(program_declaration_t), intent(in) :: declaration
        character(len=*), intent(out) :: message

        message = ''
        if (len_trim(declaration%declaration_kind) == 0) then
            message = 'missing-program-declaration-kind'
            program_declaration_validate = .false.
            return
        end if
        if (trim(declaration%declaration_kind) /= declaration_kind_program .and. &
            trim(declaration%declaration_kind) /= declaration_kind_module) then
            message = 'invalid-program-declaration-kind'
            program_declaration_validate = .false.
            return
        end if
        if (len_trim(declaration%name) == 0) then
            message = 'missing-program-declaration-name'
            program_declaration_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(declaration%name)) then
            message = 'invalid-program-declaration-name'
            program_declaration_validate = .false.
            return
        end if
        if (len_trim(declaration%span%file) == 0) then
            message = 'missing-program-declaration-file'
            program_declaration_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(declaration%span%file)) then
            message = 'invalid-program-declaration-file'
            program_declaration_validate = .false.
            return
        end if
        if (declaration%span%start_byte < 0_int64) then
            message = 'negative-program-declaration-start-byte'
            program_declaration_validate = .false.
            return
        end if
        if (declaration%span%end_byte < declaration%span%start_byte) then
            message = 'invalid-program-declaration-span'
            program_declaration_validate = .false.
            return
        end if
        if (len_trim(declaration%span%source_hash) == 0) then
            message = 'missing-program-declaration-source-hash'
            program_declaration_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(declaration%span%source_hash)) then
            message = 'invalid-program-declaration-source-hash'
            program_declaration_validate = .false.
            return
        end if
        program_declaration_validate = .true.
    end function program_declaration_validate

    subroutine diagnostic_to_sx(diagnostic, output, ok, message)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=2048) :: canonical
        character(len=32) :: start_byte, end_byte

        output = ''
        ok = diagnostic_validate(diagnostic, message)
        if (.not. ok) return

        write (start_byte, '(i0)') diagnostic%span%start_byte
        write (end_byte, '(i0)') diagnostic%span%end_byte
        canonical = '(diagnostic (status '//trim(diagnostic%status)//') '// &
            '(severity '//trim(diagnostic%severity)//') (message '// &
            trim(diagnostic%message)//') (span (file '// &
            trim(diagnostic%span%file)//') (start-byte '//trim(start_byte)//') '// &
            '(end-byte '//trim(end_byte)//') (source-hash '// &
            trim(diagnostic%span%source_hash)// &
            ')))'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine diagnostic_to_sx

    subroutine diagnostic_from_sx(input, diagnostic, ok, message, &
            expected_source_hash)
        character(len=*), intent(in) :: input
        type(diagnostic_t), intent(out) :: diagnostic
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_source_hash

        character(len=8) :: status, severity
        character(len=128) :: diagnostic_message, source_hash
        character(len=256) :: file
        integer(int64) :: start_byte, end_byte
        integer :: position

        diagnostic = diagnostic_t()
        status = ''
        severity = ''
        diagnostic_message = ''
        file = ''
        source_hash = ''
        start_byte = 0_int64
        end_byte = 0_int64
        position = 1

        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(diagnostic')) then
            ok = .false.
            message = 'malformed-diagnostic'
            return
        end if
        if (.not. consume_sx_field(input, position, 'status', status)) then
            ok = .false.
            message = 'malformed-diagnostic-status'
            return
        end if
        if (.not. consume_sx_field(input, position, 'severity', severity)) then
            ok = .false.
            message = 'malformed-diagnostic-severity'
            return
        end if
        if (.not. consume_sx_field(input, position, 'message', diagnostic_message)) then
            ok = .false.
            message = 'malformed-diagnostic-message'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) then
            ok = .false.
            message = 'malformed-diagnostic-span'
            return
        end if
        if (.not. consume_sx_text(input, position, 'span')) then
            ok = .false.
            message = 'malformed-diagnostic-span'
            return
        end if
        if (.not. consume_sx_field(input, position, 'file', file)) then
            ok = .false.
            message = 'malformed-diagnostic-file'
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'start-byte', &
            start_byte, message)) then
            call map_diagnostic_integer_failure('start-byte', message)
            ok = .false.
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'end-byte', &
            end_byte, message)) then
            call map_diagnostic_integer_failure('end-byte', message)
            ok = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'source-hash', source_hash)) then
            ok = .false.
            message = 'malformed-diagnostic-source-hash'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-diagnostic-span'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-diagnostic'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-diagnostic'
            return
        end if

        diagnostic%status = status
        diagnostic%severity = severity
        diagnostic%message = diagnostic_message
        diagnostic%span%file = file
        diagnostic%span%start_byte = start_byte
        diagnostic%span%end_byte = end_byte
        diagnostic%span%source_hash = source_hash
        if (present(expected_source_hash)) then
            ok = diagnostic_validate(diagnostic, message, expected_source_hash)
        else
            ok = diagnostic_validate(diagnostic, message)
        end if
    end subroutine diagnostic_from_sx

    logical function diagnostic_validate(diagnostic, message, expected_source_hash)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_source_hash

        message = ''
        if (.not. valid_status(diagnostic%status)) then
            message = 'invalid-diagnostic-status'
            diagnostic_validate = .false.
            return
        end if
        if (.not. valid_severity(diagnostic%severity)) then
            message = 'invalid-diagnostic-severity'
            diagnostic_validate = .false.
            return
        end if
        if (len_trim(diagnostic%message) == 0) then
            message = 'missing-diagnostic-message'
            diagnostic_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(diagnostic%message)) then
            message = 'invalid-diagnostic-message'
            diagnostic_validate = .false.
            return
        end if
        if (len_trim(diagnostic%span%file) == 0) then
            message = 'missing-diagnostic-file'
            diagnostic_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(diagnostic%span%file)) then
            message = 'invalid-diagnostic-file'
            diagnostic_validate = .false.
            return
        end if
        if (diagnostic%span%start_byte < 0_int64) then
            message = 'negative-diagnostic-start-byte'
            diagnostic_validate = .false.
            return
        end if
        if (diagnostic%span%end_byte < diagnostic%span%start_byte) then
            message = 'invalid-diagnostic-span'
            diagnostic_validate = .false.
            return
        end if
        if (len_trim(diagnostic%span%source_hash) == 0) then
            message = 'missing-diagnostic-source-hash'
            diagnostic_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(diagnostic%span%source_hash)) then
            message = 'invalid-diagnostic-source-hash'
            diagnostic_validate = .false.
            return
        end if
        if (present(expected_source_hash)) then
            if (len_trim(expected_source_hash) == 0) then
                message = 'missing-expected-source-hash'
                diagnostic_validate = .false.
                return
            end if
            if (trim(diagnostic%span%source_hash) /= trim(expected_source_hash)) then
                message = 'diagnostic-source-hash-mismatch'
                diagnostic_validate = .false.
                return
            end if
        end if
        diagnostic_validate = .true.
    end function diagnostic_validate

    logical function frontend_validate_diagnostic_table(items, count, message, &
            expected_source_hash)
        type(diagnostic_t), intent(in) :: items(:)
        integer(int64), intent(in) :: count
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_source_hash

        integer(int64) :: index
        character(len=128) :: item_message

        message = ''
        if (count < 0_int64) then
            message = 'negative-diagnostic-count'
            frontend_validate_diagnostic_table = .false.
            return
        end if
        if (count > int(diagnostic_table_capacity, int64)) then
            message = 'diagnostic-table-capacity-exceeded'
            frontend_validate_diagnostic_table = .false.
            return
        end if
        if (count > int(size(items), int64)) then
            message = 'diagnostic-count-exceeds-array'
            frontend_validate_diagnostic_table = .false.
            return
        end if

        do index = 1_int64, count
            if (present(expected_source_hash)) then
                if (.not. diagnostic_validate(items(index), item_message, &
                    expected_source_hash)) then
                    message = item_message
                    frontend_validate_diagnostic_table = .false.
                    return
                end if
            else
                if (.not. diagnostic_validate(items(index), item_message)) then
                    message = item_message
                    frontend_validate_diagnostic_table = .false.
                    return
                end if
            end if
        end do
        frontend_validate_diagnostic_table = .true.
    end function frontend_validate_diagnostic_table

    subroutine map_diagnostic_integer_failure(field_name, message)
        character(len=*), intent(in) :: field_name
        character(len=*), intent(inout) :: message

        select case (trim(field_name))
        case ('start-byte')
            select case (trim(message))
            case ('negative-diagnostic-count')
                message = 'negative-diagnostic-start-byte'
            case ('diagnostic-count-too-large')
                message = 'diagnostic-start-byte-too-large'
            case default
                message = 'malformed-diagnostic-start-byte'
            end select
        case ('end-byte')
            select case (trim(message))
            case ('negative-diagnostic-count')
                message = 'negative-diagnostic-end-byte'
            case ('diagnostic-count-too-large')
                message = 'diagnostic-end-byte-too-large'
            case default
                message = 'malformed-diagnostic-end-byte'
            end select
        case default
            message = 'malformed-diagnostic-byte'
        end select
    end subroutine map_diagnostic_integer_failure

    subroutine program_unit_to_sx(unit, output, ok, message)
        type(program_unit_t), intent(in) :: unit
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=16384) :: canonical
        character(len=2048) :: child_sx
        character(len=32) :: count_text
        integer :: index

        output = ''
        ok = program_unit_validate(unit, message)
        if (.not. ok) return

        call program_root_to_sx(unit%root, child_sx, ok, message)
        if (.not. ok) return
        write (count_text, '(i0)') unit%declaration_count
        canonical = '(program-unit (root '//trim(child_sx)//') '// &
            '(declaration-count '//trim(count_text)//') (declarations'
        do index = 1, int(unit%declaration_count)
            call program_declaration_to_sx(unit%declarations(index), child_sx, &
                ok, message)
            if (.not. ok) return
            canonical = trim(canonical)//' '//trim(child_sx)
        end do
        canonical = trim(canonical)//'))'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine program_unit_to_sx

    subroutine program_unit_from_sx(input, unit, ok, message)
        character(len=*), intent(in) :: input
        type(program_unit_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=2048) :: expression, root_expression
        integer(int64) :: parsed_count
        integer :: index, position, root_position

        unit = program_unit_t()
        parsed_count = 0_int64
        position = 1
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(program-unit')) then
            ok = .false.
            message = 'malformed-program-unit'
            return
        end if
        if (.not. consume_sx_expression(input, position, expression)) then
            ok = .false.
            message = 'malformed-program-unit-root'
            return
        end if
        root_position = 1
        if (.not. consume_sx_text(expression, root_position, '(root')) then
            ok = .false.
            message = 'malformed-program-unit-root'
            return
        end if
        if (.not. consume_sx_expression(expression, root_position, &
            root_expression)) then
            ok = .false.
            message = 'malformed-program-unit-root'
            return
        end if
        if (.not. consume_sx_character(expression, root_position, ')')) then
            ok = .false.
            message = 'malformed-program-unit-root'
            return
        end if
        call skip_sx_spaces(expression, root_position)
        if (root_position <= len(expression)) then
            ok = .false.
            message = 'malformed-program-unit-root'
            return
        end if
        call program_root_from_sx(trim(root_expression), unit%root, ok, message)
        if (.not. ok) return
        if (.not. consume_sx_integer_field(input, position, &
            'declaration-count', parsed_count, message)) then
            if (trim(message) == 'negative-diagnostic-count') then
                message = 'negative-program-unit-declaration-count'
            else if (trim(message) == 'diagnostic-count-too-large') then
                message = 'program-unit-declaration-count-too-large'
            end if
            ok = .false.
            return
        end if
        if (parsed_count > int(huge(0), int64)) then
            ok = .false.
            message = 'program-unit-declaration-count-too-large'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(declarations')) then
            ok = .false.
            message = 'malformed-program-unit-declarations'
            return
        end if
        index = 0
        do
            call skip_sx_spaces(input, position)
            if (position > len(input)) then
                ok = .false.
                message = 'malformed-program-unit-declarations'
                return
            end if
            if (input(position:position) == ')') then
                position = position + 1
                exit
            end if
            index = index + 1
            if (index > program_unit_declaration_capacity) then
                ok = .false.
                message = 'program-unit-declaration-capacity-exceeded'
                return
            end if
            if (.not. consume_sx_expression(input, position, expression)) then
                ok = .false.
                message = 'malformed-program-unit-declaration'
                return
            end if
            call program_declaration_from_sx(trim(expression), &
                unit%declarations(index), ok, message)
            if (.not. ok) return
        end do
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-program-unit'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-program-unit'
            return
        end if
        if (parsed_count /= int(index, int64)) then
            ok = .false.
            message = 'program-unit-declaration-count-mismatch'
            return
        end if
        unit%declaration_count = index
        ok = program_unit_validate(unit, message)
    end subroutine program_unit_from_sx

    logical function program_unit_validate(unit, message)
        type(program_unit_t), intent(in) :: unit
        character(len=*), intent(out) :: message

        integer :: index

        message = ''
        if (.not. program_root_validate(unit%root, message)) then
            program_unit_validate = .false.
            return
        end if
        if (unit%declaration_count < 0) then
            message = 'negative-program-unit-declaration-count'
            program_unit_validate = .false.
            return
        end if
        if (unit%declaration_count > program_unit_declaration_capacity) then
            message = 'program-unit-declaration-capacity-exceeded'
            program_unit_validate = .false.
            return
        end if
        do index = 1, int(unit%declaration_count)
            if (.not. program_declaration_validate(unit%declarations(index), &
                message)) then
                program_unit_validate = .false.
                return
            end if
        end do
        program_unit_validate = .true.
    end function program_unit_validate

    subroutine program_root_to_sx(root, output, ok, message)
        type(program_root_t), intent(in) :: root
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=1024) :: canonical
        character(len=32) :: start_byte, end_byte

        output = ''
        ok = program_root_validate(root, message)
        if (.not. ok) return

        write (start_byte, '(i0)') root%span%start_byte
        write (end_byte, '(i0)') root%span%end_byte
        canonical = '(program-root (name '//trim(root%name)//') '// &
            '(span (file '//trim(root%span%file)//') (start-byte '// &
            trim(start_byte)//') (end-byte '//trim(end_byte)//') '// &
            '(source-hash '//trim(root%span%source_hash)//')))'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine program_root_to_sx

    subroutine program_root_from_sx(input, root, ok, message)
        character(len=*), intent(in) :: input
        type(program_root_t), intent(out) :: root
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: name, file, source_hash
        integer(int64) :: start_byte, end_byte
        integer :: position

        root = program_root_t()
        name = ''
        file = ''
        source_hash = ''
        start_byte = 0_int64
        end_byte = 0_int64
        position = 1
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(program-root')) then
            ok = .false.
            message = 'malformed-program-root'
            return
        end if
        if (.not. consume_sx_field(input, position, 'name', name)) then
            ok = .false.
            message = 'malformed-program-root-name'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) then
            ok = .false.
            message = 'malformed-program-root-span'
            return
        end if
        if (.not. consume_sx_text(input, position, 'span')) then
            ok = .false.
            message = 'malformed-program-root-span'
            return
        end if
        if (.not. consume_sx_field(input, position, 'file', file)) then
            ok = .false.
            message = 'malformed-program-root-file'
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'start-byte', &
            start_byte, message)) then
            ok = .false.
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'end-byte', &
            end_byte, message)) then
            ok = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'source-hash', source_hash)) then
            ok = .false.
            message = 'malformed-program-root-source-hash'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-program-root-span'
            return
        end if
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, ')')) then
            ok = .false.
            message = 'malformed-program-root'
            return
        end if
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            ok = .false.
            message = 'malformed-program-root'
            return
        end if

        root%name = name
        root%span%file = file
        root%span%start_byte = start_byte
        root%span%end_byte = end_byte
        root%span%source_hash = source_hash
        ok = program_root_validate(root, message)
    end subroutine program_root_from_sx

    logical function program_root_validate(root, message)
        type(program_root_t), intent(in) :: root
        character(len=*), intent(out) :: message

        message = ''
        if (len_trim(root%name) == 0) then
            message = 'missing-program-root-name'
            program_root_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(root%name)) then
            message = 'invalid-program-root-name'
            program_root_validate = .false.
            return
        end if
        if (len_trim(root%span%file) == 0) then
            message = 'missing-program-root-file'
            program_root_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(root%span%file)) then
            message = 'invalid-program-root-file'
            program_root_validate = .false.
            return
        end if
        if (root%span%start_byte < 0_int64) then
            message = 'negative-program-root-start-byte'
            program_root_validate = .false.
            return
        end if
        if (root%span%end_byte < root%span%start_byte) then
            message = 'invalid-program-root-span'
            program_root_validate = .false.
            return
        end if
        if (len_trim(root%span%source_hash) == 0) then
            message = 'missing-program-root-source-hash'
            program_root_validate = .false.
            return
        end if
        if (.not. valid_sx_atom(root%span%source_hash)) then
            message = 'invalid-program-root-source-hash'
            program_root_validate = .false.
            return
        end if
        program_root_validate = .true.
    end function program_root_validate

    logical function valid_sx_atom(value)
        character(len=*), intent(in) :: value

        integer :: index

        valid_sx_atom = .false.
        if (len_trim(value) == 0) return
        do index = 1, len_trim(value)
            if (value(index:index) == ' ' .or. value(index:index) == '(' .or. &
                value(index:index) == ')') return
        end do
        valid_sx_atom = .true.
    end function valid_sx_atom

    logical function frontend_validate_semantic_item(item, message)
        type(standardir_semantic_item_t), intent(in) :: item
        character(len=*), intent(out) :: message

        message = ''
        if (len_trim(item%id) == 0) then
            message = 'missing-semantic-id'
            frontend_validate_semantic_item = .false.
            return
        end if
        if (len_trim(item%subject) == 0) then
            message = 'missing-semantic-subject'
            frontend_validate_semantic_item = .false.
            return
        end if
        if (.not. valid_origin(item%origin)) then
            message = 'invalid-semantic-origin'
            frontend_validate_semantic_item = .false.
            return
        end if
        if (lowercase(trim(item%resolution)) /= 'resolved') then
            message = 'unresolved-semantic'
            frontend_validate_semantic_item = .false.
            return
        end if
        if (len_trim(item%source%document) == 0 .or. &
            len_trim(item%source%clause) == 0 .or. &
            len_trim(item%source%rule) == 0 .or. &
            item%source%page <= 0_int64 .or. &
            len_trim(item%source%source_hash) == 0) then
            message = 'invalid-semantic-provenance'
            frontend_validate_semantic_item = .false.
            return
        end if
        frontend_validate_semantic_item = .true.
    end function frontend_validate_semantic_item

    logical function frontend_validate_semantic_table(items, count, message)
        type(standardir_semantic_item_t), intent(in) :: items(:)
        integer(int64), intent(in) :: count
        character(len=*), intent(out) :: message

        integer(int64) :: index
        character(len=128) :: item_message

        message = ''
        if (count < 0_int64) then
            message = 'negative-semantic-item-count'
            frontend_validate_semantic_table = .false.
            return
        end if
        if (count > int(semantic_item_table_capacity, int64)) then
            message = 'semantic-item-table-capacity-exceeded'
            frontend_validate_semantic_table = .false.
            return
        end if
        if (count > int(size(items), int64)) then
            message = 'semantic-item-count-exceeds-array'
            frontend_validate_semantic_table = .false.
            return
        end if

        do index = 1_int64, count
            if (.not. frontend_validate_semantic_item(items(index), item_message)) then
                message = item_message
                frontend_validate_semantic_table = .false.
                return
            end if
        end do
        frontend_validate_semantic_table = .true.
    end function frontend_validate_semantic_table

    subroutine frontend_result_to_sx(result, output, ok, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32) :: count_text
        character(len=2048) :: diagnostic_sx
        character(len=32768) :: canonical
        integer :: index

        output = ''
        ok = frontend_validate(result, message)
        if (.not. ok) return

        write (count_text, '(i0)') result%diagnostic_count
        canonical = '(frontend-result (status '//trim(result%status)//') '// &
            '(root-kind '//trim(result%root_kind)//') (diagnostic-count '// &
            trim(count_text)//')'
        if (trim(result%status) == frontend_rejected) then
            canonical = trim(canonical)//' (diagnostics'
            do index = 1, int(result%diagnostic_count)
                call diagnostic_to_sx(result%diagnostics(index), diagnostic_sx, &
                    ok, message)
                if (.not. ok) return
                canonical = trim(canonical)//' '//trim(diagnostic_sx)
            end do
            canonical = trim(canonical)//')'
        end if
        canonical = trim(canonical)//')'
        if (len_trim(canonical) > len(output)) then
            ok = .false.
            message = 'sx-output-too-short'
            return
        end if
        output(:len_trim(canonical)) = canonical(:len_trim(canonical))
        message = ''
    end subroutine frontend_result_to_sx

    subroutine frontend_result_from_sx(input, result, ok, message)
        character(len=*), intent(in) :: input
        type(frontend_result_t), intent(out) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        result = frontend_result_t()
        ok = parse_frontend_result_sx(input, result, message)
        if (.not. ok) then
            result = frontend_result_t()
            return
        end if
        ok = frontend_validate(result, message)
        if (.not. ok) return
        message = ''
    end subroutine frontend_result_from_sx

    logical function frontend_validate(result, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: message

        integer(int64) :: actual_diagnostic_count

        message = ''
        if (.not. valid_status(result%status)) then
            message = 'invalid-result-status'
            frontend_validate = .false.
            return
        end if
        if (.not. valid_root_kind(result%root_kind)) then
            message = 'invalid-result-root-kind'
            frontend_validate = .false.
            return
        end if
        if (result%diagnostic_count < 0_int64) then
            message = 'negative-diagnostic-count'
            frontend_validate = .false.
            return
        end if

        if (allocated(result%diagnostics)) then
            actual_diagnostic_count = int(size(result%diagnostics), int64)
        else
            actual_diagnostic_count = 0_int64
        end if
        if (actual_diagnostic_count /= result%diagnostic_count) then
            message = 'diagnostic-count-mismatch'
            frontend_validate = .false.
            return
        end if

        select case (trim(result%status))
        case (frontend_accepted)
            if (trim(result%root_kind) == root_kind_none .or. &
                result%diagnostic_count /= 0_int64) then
                message = 'invalid-accepted-result'
                frontend_validate = .false.
                return
            end if
        case (frontend_rejected)
            if (trim(result%root_kind) /= root_kind_none .or. &
                result%diagnostic_count == 0_int64) then
                message = 'invalid-rejected-result'
                frontend_validate = .false.
                return
            end if
        end select

        if (allocated(result%diagnostics)) then
            if (.not. frontend_validate_diagnostic_table(result%diagnostics, &
                result%diagnostic_count, message)) then
                frontend_validate = .false.
                return
            end if
        end if
        frontend_validate = .true.
    end function frontend_validate

    logical function parse_frontend_result_sx(input, result, message)
        character(len=*), intent(in) :: input
        type(frontend_result_t), intent(out) :: result
        character(len=*), intent(out) :: message

        character(len=8) :: status
        character(len=32) :: root_kind
        character(len=2048) :: diagnostic_expression
        integer(int64) :: diagnostic_count_value
        integer :: diagnostic_count, index, position
        logical :: diagnostic_ok

        result = frontend_result_t()
        status = ''
        root_kind = ''
        diagnostic_count_value = 0_int64
        position = 1
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_text(input, position, '(frontend-result')) then
            message = 'malformed-sx-record'
            parse_frontend_result_sx = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'status', status)) then
            message = 'malformed-sx-status'
            parse_frontend_result_sx = .false.
            return
        end if
        if (.not. consume_sx_field(input, position, 'root-kind', root_kind)) then
            message = 'malformed-sx-root-kind'
            parse_frontend_result_sx = .false.
            return
        end if
        if (.not. consume_sx_integer_field(input, position, 'diagnostic-count', &
            diagnostic_count_value, message)) then
            parse_frontend_result_sx = .false.
            return
        end if
        result%status = status
        result%root_kind = root_kind
        result%diagnostic_count = diagnostic_count_value
        if (diagnostic_count_value > int(diagnostic_table_capacity, int64)) then
            message = 'diagnostic-table-capacity-exceeded'
            parse_frontend_result_sx = .false.
            return
        end if
        if (diagnostic_count_value > 0_int64) then
            diagnostic_count = int(diagnostic_count_value)
            allocate (result%diagnostics(diagnostic_count))
        end if
        if (trim(status) == frontend_rejected .and. diagnostic_count_value > 0_int64) then
            call skip_sx_spaces(input, position)
            if (.not. consume_sx_text(input, position, '(diagnostics')) then
                message = 'malformed-sx-diagnostics'
                parse_frontend_result_sx = .false.
                return
            end if
            do index = 1, diagnostic_count
                if (.not. consume_sx_expression(input, position, &
                    diagnostic_expression)) then
                    message = 'malformed-diagnostic'
                    parse_frontend_result_sx = .false.
                    return
                end if
                call diagnostic_from_sx(trim(diagnostic_expression), &
                    result%diagnostics(index), diagnostic_ok, message)
                if (.not. diagnostic_ok) then
                    parse_frontend_result_sx = .false.
                    return
                end if
            end do
            call skip_sx_spaces(input, position)
            if (.not. consume_sx_character(input, position, ')')) then
                message = 'diagnostic-count-mismatch'
                parse_frontend_result_sx = .false.
                return
            end if
        end if
        call skip_sx_spaces(input, position)
        if (position > len(input)) then
            message = 'malformed-sx-record'
            parse_frontend_result_sx = .false.
            return
        end if
        if (input(position:position) /= ')') then
            message = 'malformed-sx-record'
            parse_frontend_result_sx = .false.
            return
        end if
        position = position + 1
        call skip_sx_spaces(input, position)
        if (position <= len(input)) then
            message = 'malformed-sx-record'
            parse_frontend_result_sx = .false.
            return
        end if
        parse_frontend_result_sx = .true.
    end function parse_frontend_result_sx

    logical function consume_sx_field(input, position, field_name, value)
        character(len=*), intent(in) :: input, field_name
        integer, intent(inout) :: position
        character(len=*), intent(out) :: value

        character(len=128) :: token

        consume_sx_field = .false.
        value = ''
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) return
        if (.not. consume_sx_token(input, position, token)) return
        if (trim(token) /= field_name) return
        if (.not. consume_sx_token(input, position, value)) return
        call skip_sx_spaces(input, position)
        if (position > len(input)) return
        if (input(position:position) /= ')') return
        position = position + 1
        consume_sx_field = .true.
    end function consume_sx_field

    logical function consume_sx_integer_field(input, position, field_name, value, &
            message)
        character(len=*), intent(in) :: input, field_name
        integer, intent(inout) :: position
        integer(int64), intent(out) :: value
        character(len=*), intent(out) :: message

        character(len=128) :: token

        value = 0_int64
        message = 'malformed-sx-count'
        consume_sx_integer_field = .false.
        call skip_sx_spaces(input, position)
        if (.not. consume_sx_character(input, position, '(')) return
        if (.not. consume_sx_token(input, position, token)) return
        if (trim(token) /= field_name) return
        if (.not. consume_sx_token(input, position, token)) return
        if (.not. parse_sx_integer(token, value, message)) return
        call skip_sx_spaces(input, position)
        if (position > len(input)) return
        if (input(position:position) /= ')') return
        position = position + 1
        consume_sx_integer_field = .true.
    end function consume_sx_integer_field

    logical function consume_sx_expression(input, position, expression)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        character(len=*), intent(out) :: expression

        integer :: depth, start

        expression = ''
        call skip_sx_spaces(input, position)
        if (position > len(input)) then
            consume_sx_expression = .false.
            return
        end if
        if (input(position:position) /= '(') then
            consume_sx_expression = .false.
            return
        end if
        start = position
        depth = 0
        do while (position <= len(input))
            if (input(position:position) == '(') then
                depth = depth + 1
            else if (input(position:position) == ')') then
                depth = depth - 1
                if (depth == 0) then
                    position = position + 1
                    if (position - start > len(expression)) then
                        consume_sx_expression = .false.
                        return
                    end if
                    expression(:position - start) = input(start:position - 1)
                    consume_sx_expression = .true.
                    return
                end if
            end if
            position = position + 1
        end do
        consume_sx_expression = .false.
    end function consume_sx_expression

    logical function parse_sx_integer(token, value, message)
        character(len=*), intent(in) :: token
        integer(int64), intent(out) :: value
        character(len=*), intent(out) :: message

        integer :: index, first_digit
        integer(int64) :: digit, maximum

        value = 0_int64
        message = 'malformed-sx-count'
        if (len_trim(token) == 0) then
            parse_sx_integer = .false.
            return
        end if
        first_digit = 1
        if (token(1:1) == '-') then
            message = 'negative-diagnostic-count'
            parse_sx_integer = .false.
            return
        end if
        maximum = huge(value)
        do index = first_digit, len_trim(token)
            if (token(index:index) < '0' .or. token(index:index) > '9') then
                parse_sx_integer = .false.
                return
            end if
            digit = int(iachar(token(index:index)) - iachar('0'), int64)
            if (value > (maximum - digit) / 10_int64) then
                message = 'diagnostic-count-too-large'
                parse_sx_integer = .false.
                return
            end if
            value = value * 10_int64 + digit
        end do
        parse_sx_integer = .true.
    end function parse_sx_integer

    logical function consume_sx_character(input, position, expected)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        character, intent(in) :: expected

        consume_sx_character = .false.
        if (position > len(input)) return
        if (input(position:position) /= expected) return
        position = position + 1
        consume_sx_character = .true.
    end function consume_sx_character

    logical function consume_sx_text(input, position, expected)
        character(len=*), intent(in) :: input, expected
        integer, intent(inout) :: position

        integer :: last_position

        consume_sx_text = .false.
        last_position = position + len(expected) - 1
        if (last_position > len(input)) return
        if (input(position:last_position) /= expected) return
        position = last_position + 1
        consume_sx_text = .true.
    end function consume_sx_text

    logical function consume_sx_token(input, position, token)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        character(len=*), intent(out) :: token

        integer :: start

        token = ''
        call skip_sx_spaces(input, position)
        if (position > len(input)) then
            consume_sx_token = .false.
            return
        end if
        start = position
        do while (position <= len(input))
            if (input(position:position) == ' ' .or. input(position:position) == ')' &
                .or. input(position:position) == '(') exit
            position = position + 1
        end do
        if (position == start .or. position - start > len(token)) then
            consume_sx_token = .false.
            return
        end if
        token(:position - start) = input(start:position - 1)
        consume_sx_token = .true.
    end function consume_sx_token

    subroutine skip_sx_spaces(input, position)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position

        do while (position <= len(input))
            if (input(position:position) /= ' ') return
            position = position + 1
        end do
    end subroutine skip_sx_spaces

    logical function valid_status(status)
        character(len=*), intent(in) :: status

        valid_status = trim(status) == frontend_accepted .or. &
            trim(status) == frontend_rejected
    end function valid_status

    logical function valid_root_kind(root_kind)
        character(len=*), intent(in) :: root_kind

        valid_root_kind = trim(root_kind) == root_kind_source .or. &
            trim(root_kind) == root_kind_program .or. &
            trim(root_kind) == root_kind_module .or. &
            trim(root_kind) == root_kind_none
    end function valid_root_kind

    logical function valid_severity(severity)
        character(len=*), intent(in) :: severity

        valid_severity = trim(severity) == severity_note .or. &
            trim(severity) == severity_warning .or. &
            trim(severity) == severity_error
    end function valid_severity

    logical function validate_syntax_item(syntax_item, message)
        type(standardir_syntax_item_t), intent(in) :: syntax_item
        character(len=*), intent(out) :: message

        message = ''
        if (len_trim(syntax_item%id) == 0) then
            message = 'missing-syntax-witness'
            validate_syntax_item = .false.
            return
        end if
        if (trim(syntax_item%id) /= 'R501') then
            message = 'unsupported-syntax-item'
            validate_syntax_item = .false.
            return
        end if
        select case (lowercase(trim(syntax_item%lhs)))
        case ('program', 'module')
        case default
            message = 'unsupported-syntax-item'
            validate_syntax_item = .false.
            return
        end select
        if (lowercase(trim(syntax_item%resolution)) /= 'resolved') then
            message = 'unresolved-syntax'
            validate_syntax_item = .false.
            return
        end if
        if (.not. valid_origin(syntax_item%origin)) then
            message = 'invalid-syntax-origin'
            validate_syntax_item = .false.
            return
        end if
        if (len_trim(syntax_item%source%document) == 0 .or. &
            len_trim(syntax_item%source%clause) == 0 .or. &
            len_trim(syntax_item%source%rule) == 0 .or. &
            syntax_item%source%page <= 0_int64 .or. &
            len_trim(syntax_item%source%source_hash) == 0) then
            message = 'invalid-syntax-provenance'
            validate_syntax_item = .false.
            return
        end if
        validate_syntax_item = .true.
    end function validate_syntax_item

    logical function valid_origin(origin)
        character(len=*), intent(in) :: origin

        select case (lowercase(trim(origin)))
        case ('mechanical', 'search', 'smt', 'llm', 'llm-repair', 'human', &
                'imported', 'differential')
            valid_origin = .true.
        case default
            valid_origin = .false.
        end select
    end function valid_origin

    logical function valid_resolution(resolution)
        character(len=*), intent(in) :: resolution

        select case (lowercase(trim(resolution)))
        case ('resolved', 'unresolved', 'disputed')
            valid_resolution = .true.
        case default
            valid_resolution = .false.
        end select
    end function valid_resolution

    logical function parse_program_witness(source, program_name, message, unit_start, &
            unit_end, declaration_start, declaration_end, expected_kind)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: program_name
        character(len=*), intent(out) :: message
        integer(int64), intent(out), optional :: unit_start
        integer(int64), intent(out), optional :: unit_end
        integer(int64), intent(out), optional :: declaration_start
        integer(int64), intent(out), optional :: declaration_end
        character(len=*), intent(in), optional :: expected_kind

        integer :: first_line_end
        integer :: first_line_first
        integer :: first_line_limit
        integer :: first_newline
        integer :: header_name_end
        integer :: header_name_start
        integer :: header_keyword_end
        integer :: header_keyword_start
        integer :: position
        integer :: second_line_end
        integer :: second_line_first
        integer :: second_line_limit
        integer :: second_line_start
        integer :: second_newline
        integer :: terminator_end
        integer :: terminator_keyword_end
        integer :: terminator_keyword_start
        integer :: terminator_name_end
        integer :: terminator_name_start
        integer :: token_end
        integer :: token_start
        logical :: has_token
        character(len=32) :: header_kind
        character(len=128) :: terminator_name

        program_name = ''
        message = 'unsupported-syntax'
        header_kind = ''
        if (present(unit_start)) unit_start = 0_int64
        if (present(unit_end)) unit_end = 0_int64
        if (present(declaration_start)) declaration_start = 0_int64
        if (present(declaration_end)) declaration_end = 0_int64

        first_newline = index(source, new_line('a'))
        if (first_newline == 0) then
            position = 1
            call next_line_token(source, len(source), position, token_start, &
                token_end, has_token)
            if (has_token) then
                if (trim(lowercase(source(token_start:token_end))) == 'program' .or. &
                    trim(lowercase(source(token_start:token_end))) == 'module') then
                    message = 'invalid-program'
                end if
            end if
            parse_program_witness = .false.
            return
        end if

        first_line_end = first_newline - 1
        first_line_limit = first_line_end
        second_line_start = first_newline + 1
        second_line_end = len(source)
        second_newline = 0
        if (second_line_start <= len(source)) then
            second_newline = index(source(second_line_start:), new_line('a'))
        end if
        if (second_newline > 0) then
            if (first_newline + second_newline == len(source)) then
                second_line_end = len(source) - 1
            else
                message = 'invalid-program'
                parse_program_witness = .false.
                return
            end if
        end if
        second_line_limit = second_line_end

        call trim_line_bounds(source, 1, first_line_limit, first_line_first, &
            first_line_end)
        call trim_line_bounds(source, second_line_start, second_line_limit, &
            second_line_first, second_line_end)
        if (first_line_first > first_line_end .or. &
            second_line_first > second_line_end) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        position = first_line_first
        call next_line_token(source, first_line_end, position, token_start, &
            token_end, has_token)
        if (.not. has_token) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        header_keyword_start = token_start
        header_keyword_end = token_end
        select case (trim(lowercase(source(header_keyword_start:header_keyword_end))))
        case ('program')
            header_kind = root_kind_program
        case ('module')
            header_kind = root_kind_module
        case default
            if (header_keyword_end - header_keyword_start + 1 >= len('program')) then
                if (trim(lowercase(source(header_keyword_start:header_keyword_start + &
                    len('program') - 1))) == 'program') message = 'invalid-program'
            end if
            if (header_keyword_end - header_keyword_start + 1 >= len('module')) then
                if (trim(lowercase(source(header_keyword_start:header_keyword_start + &
                    len('module') - 1))) == 'module') message = 'invalid-program'
            end if
            parse_program_witness = .false.
            return
        end select

        if (present(expected_kind)) then
            if (trim(lowercase(expected_kind)) /= trim(header_kind)) then
                message = 'unsupported-syntax-item'
                parse_program_witness = .false.
                return
            end if
        end if

        call next_line_token(source, first_line_end, position, header_name_start, &
            header_name_end, has_token)
        if (.not. has_token) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        call next_line_token(source, first_line_end, position, token_start, &
            token_end, has_token)
        if (has_token) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        if (header_name_end - header_name_start + 1 > len(program_name)) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        program_name(:header_name_end - header_name_start + 1) = &
            source(header_name_start:header_name_end)
        if (.not. valid_program_identifier(program_name(:header_name_end - &
            header_name_start + 1))) then
            program_name = ''
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        position = second_line_first
        call next_line_token(source, second_line_end, position, token_start, &
            token_end, has_token)
        if (.not. has_token) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        terminator_keyword_start = token_start
        terminator_keyword_end = token_end
        if (trim(lowercase(source(terminator_keyword_start: &
            terminator_keyword_end))) /= 'end') then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        call next_line_token(source, second_line_end, position, token_start, &
            token_end, has_token)
        if (.not. has_token) then
            terminator_end = terminator_keyword_end
        else
            if (trim(lowercase(source(token_start:token_end))) /= trim(header_kind)) then
                message = 'invalid-program'
                parse_program_witness = .false.
                return
            end if
            call next_line_token(source, second_line_end, position, &
                terminator_name_start, terminator_name_end, has_token)
            if (.not. has_token) then
                terminator_end = token_end
            else
                terminator_name = ''
                if (terminator_name_end - terminator_name_start + 1 > &
                    len(terminator_name)) then
                    message = 'invalid-program'
                    parse_program_witness = .false.
                    return
                end if
                terminator_name(:terminator_name_end - terminator_name_start + 1) = &
                    source(terminator_name_start:terminator_name_end)
                if (trim(lowercase(terminator_name)) /= &
                    trim(lowercase(program_name))) then
                    message = 'invalid-program'
                    parse_program_witness = .false.
                    return
                end if
                terminator_end = terminator_name_end
                call next_line_token(source, second_line_end, position, token_start, &
                    token_end, has_token)
                if (has_token) then
                    message = 'invalid-program'
                    parse_program_witness = .false.
                    return
                end if
            end if
        end if

        if (present(unit_start)) unit_start = int(first_line_first - 1, int64)
        if (present(unit_end)) unit_end = int(terminator_end, int64)
        if (present(declaration_start)) then
            declaration_start = int(header_keyword_start - 1, int64)
        end if
        if (present(declaration_end)) declaration_end = int(header_name_end, int64)
        parse_program_witness = .true.
    end function parse_program_witness

    subroutine trim_line_bounds(source, line_start, line_end, first, last)
        character(len=*), intent(in) :: source
        integer, intent(in) :: line_start
        integer, intent(in) :: line_end
        integer, intent(out) :: first
        integer, intent(out) :: last

        first = line_start
        last = line_end
        do while (first <= last)
            if (.not. is_line_whitespace(source(first:first))) exit
            first = first + 1
        end do
        do while (last >= first)
            if (.not. is_line_whitespace(source(last:last))) exit
            last = last - 1
        end do
    end subroutine trim_line_bounds

    subroutine next_line_token(source, line_end, position, token_start, token_end, &
            has_token)
        character(len=*), intent(in) :: source
        integer, intent(in) :: line_end
        integer, intent(inout) :: position
        integer, intent(out) :: token_start
        integer, intent(out) :: token_end
        logical, intent(out) :: has_token

        token_start = 0
        token_end = -1
        has_token = .false.
        do while (position <= line_end)
            if (.not. is_line_whitespace(source(position:position))) exit
            position = position + 1
        end do
        if (position > line_end) return

        token_start = position
        do while (position <= line_end)
            if (is_line_whitespace(source(position:position))) exit
            position = position + 1
        end do
        token_end = position - 1
        has_token = .true.
    end subroutine next_line_token

    logical function is_line_whitespace(value)
        character(len=1), intent(in) :: value

        is_line_whitespace = value == ' ' .or. value == achar(9) .or. &
            value == achar(13)
    end function is_line_whitespace

    logical function valid_program_identifier(value)
        character(len=*), intent(in) :: value

        integer :: position

        valid_program_identifier = .false.
        if (len(value) == 0 .or. len(value) > 128) return
        if (.not. is_identifier_character(value(1:1), .true.)) return
        do position = 2, len(value)
            if (.not. is_identifier_character(value(position:position), .false.)) &
                return
        end do
        valid_program_identifier = .true.
    end function valid_program_identifier

    logical function is_identifier_character(value, first_character)
        character(len=1), intent(in) :: value
        logical, intent(in) :: first_character
        character(len=*), parameter :: letters = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
        character(len=*), parameter :: digits = '0123456789'

        if (first_character) then
            is_identifier_character = scan(letters, value) > 0
        else
            is_identifier_character = scan(letters//digits//'_', value) > 0
        end if
    end function is_identifier_character

    pure function lowercase(value) result(lowered)
        character(len=*), intent(in) :: value
        character(len=len(value)) :: lowered
        integer :: position
        integer :: code

        lowered = value
        do position = 1, len(value)
            code = iachar(lowered(position:position))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lowered(position:position) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lowercase

end module fortfront_frontend
