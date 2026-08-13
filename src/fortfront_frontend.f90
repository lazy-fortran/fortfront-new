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

    type, public :: frontend_result_t
        character(len=8) :: status = frontend_rejected
        character(len=32) :: root_kind = root_kind_none
        integer(int64) :: diagnostic_count = 0_int64
        type(frontend_root_t) :: root
        type(diagnostic_t), allocatable :: diagnostics(:)
    end type frontend_result_t

    public :: frontend_parse, frontend_read, frontend_result_to_sx, &
        frontend_validate

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
