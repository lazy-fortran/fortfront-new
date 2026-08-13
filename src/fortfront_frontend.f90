module fortfront_frontend
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
    character(len=*), parameter, public :: root_kind_none = 'none'

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

    type, public :: frontend_result_t
        character(len=8) :: status = frontend_rejected
        character(len=32) :: root_kind = root_kind_none
        integer(int64) :: diagnostic_count = 0_int64
        type(frontend_root_t) :: root
        type(diagnostic_t), allocatable :: diagnostics(:)
    end type frontend_result_t

    public :: frontend_parse, frontend_read, frontend_result_from_sx, &
        frontend_result_to_sx, frontend_validate, &
        frontend_result_to_program_root, frontend_result_to_program_root_sx, &
        frontend_validate_semantic_item, program_root_to_sx, &
        program_root_from_sx, program_root_validate

contains

    subroutine frontend_read(file_name, source, source_hash, result)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
        type(frontend_result_t), intent(out) :: result

        type(standardir_syntax_item_t) :: missing_witness

        call frontend_parse(file_name, source, source_hash, missing_witness, result)
    end subroutine frontend_read

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
                if (parse_program_witness(source, program_name, diagnostic_message)) then
                    result%status = frontend_accepted
                    result%root_kind = root_kind_program
                    result%root%kind = root_kind_program
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
        if (trim(result%root_kind) /= root_kind_program .or. &
            trim(result%root%kind) /= root_kind_program) then
            message = 'non-program-root'
            ok = .false.
            return
        end if

        root%name = result%root%name
        root%span = result%root%span
        message = ''
    end subroutine frontend_result_to_program_root

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

    subroutine frontend_result_to_sx(result, output, ok, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32) :: count_text
        character(len=256) :: canonical

        output = ''
        ok = frontend_validate(result, message)
        if (.not. ok) return

        write (count_text, '(i0)') result%diagnostic_count
        canonical = '(frontend-result (status '//trim(result%status)//') '// &
            '(root-kind '//trim(result%root_kind)//') (diagnostic-count '// &
            trim(count_text)//'))'
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

        character(len=32) :: parsed_root_kind
        character(len=8) :: parsed_status
        integer(int64) :: parsed_diagnostic_count
        integer :: diagnostic_count

        result = frontend_result_t()
        ok = parse_frontend_result_sx(input, parsed_status, parsed_root_kind, &
            parsed_diagnostic_count, message)
        if (.not. ok) return

        result%status = parsed_status
        result%root_kind = parsed_root_kind
        result%diagnostic_count = parsed_diagnostic_count
        if (parsed_diagnostic_count > 0_int64) then
            if (parsed_diagnostic_count > int(huge(0), int64)) then
                ok = .false.
                message = 'diagnostic-count-too-large'
                return
            end if
            diagnostic_count = int(parsed_diagnostic_count)
            allocate (result%diagnostics(diagnostic_count))
            result%diagnostics%status = frontend_rejected
            result%diagnostics%severity = severity_error
        end if

        ok = frontend_validate(result, message)
        if (.not. ok) return
        message = ''
    end subroutine frontend_result_from_sx

    logical function frontend_validate(result, message)
        type(frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: message

        integer(int64) :: actual_diagnostic_count
        integer :: index

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
            do index = 1, size(result%diagnostics)
                if (.not. valid_status(result%diagnostics(index)%status)) then
                    message = 'invalid-diagnostic-status'
                    frontend_validate = .false.
                    return
                end if
                if (.not. valid_severity(result%diagnostics(index)%severity)) then
                    message = 'invalid-diagnostic-severity'
                    frontend_validate = .false.
                    return
                end if
            end do
        end if
        frontend_validate = .true.
    end function frontend_validate

    logical function parse_frontend_result_sx(input, status, root_kind, &
            diagnostic_count, message)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: status, root_kind
        integer(int64), intent(out) :: diagnostic_count
        character(len=*), intent(out) :: message

        integer :: position

        status = ''
        root_kind = ''
        diagnostic_count = 0_int64
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
            diagnostic_count, message)) then
            parse_frontend_result_sx = .false.
            return
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
        if (trim(syntax_item%id) /= 'R501' .or. &
            lowercase(trim(syntax_item%lhs)) /= 'program') then
            message = 'unsupported-syntax-item'
            validate_syntax_item = .false.
            return
        end if
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

    logical function parse_program_witness(source, program_name, message)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: program_name
        character(len=*), intent(out) :: message

        integer :: newline_position
        character(len=256) :: header
        character(len=256) :: trailer

        program_name = ''
        message = 'unsupported-syntax'
        newline_position = index(source, new_line('a'))
        if (newline_position == 0) then
            if (lowercase(trim(source)) == 'program') message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        header = trim(source(:newline_position - 1))
        trailer = trim(source(newline_position + 1:))
        if (lowercase(trailer) /= 'end') then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        if (len_trim(header) <= len('program')) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        if (lowercase(header(:len('program'))) /= 'program') then
            parse_program_witness = .false.
            return
        end if
        if (header(len('program') + 1:len('program') + 1) /= ' ') then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if

        program_name = adjustl(header(len('program') + 2:len_trim(header)))
        if (len_trim(program_name) == 0) then
            message = 'invalid-program'
            parse_program_witness = .false.
            return
        end if
        parse_program_witness = .true.
    end function parse_program_witness

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
