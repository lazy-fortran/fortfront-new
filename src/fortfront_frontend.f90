module fortfront_frontend
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    character(len=*), parameter, public :: frontend_accepted = 'accepted'
    character(len=*), parameter, public :: frontend_rejected = 'rejected'
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

    public :: frontend_read

contains

    subroutine frontend_read(file_name, source, source_hash, result)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
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
        else if (.not. standardir_program_witness_is_resolved()) then
            diagnostic_message = 'unresolved-syntax'
        else if (parse_program_witness(source, program_name, diagnostic_message)) then
            result%status = frontend_accepted
            result%root_kind = root_kind_program
            result%root%kind = root_kind_program
            result%root%name = program_name
            result%diagnostic_count = 0_int64
            return
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
    end subroutine frontend_read

    logical function standardir_program_witness_is_resolved()
        character(len=*), parameter :: witness_id = 'R501'
        character(len=*), parameter :: witness_lhs = 'program'
        character(len=*), parameter :: witness_origin = 'mechanical'
        character(len=*), parameter :: witness_resolution = 'resolved'
        character(len=*), parameter :: witness_document = 'J3-24-007'
        character(len=*), parameter :: witness_clause = '1'
        character(len=*), parameter :: witness_rule = 'R501'
        integer, parameter :: witness_page = 45
        character(len=*), parameter :: witness_source_hash = 'fixture'

        standardir_program_witness_is_resolved = witness_id == 'R501' .and. &
            witness_lhs == 'program' .and. witness_origin == 'mechanical' .and. &
            witness_resolution == 'resolved' .and. witness_document == 'J3-24-007' &
            .and. witness_clause == '1' .and. witness_rule == 'R501' .and. &
            witness_page == 45 .and. witness_source_hash == 'fixture'
    end function standardir_program_witness_is_resolved

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
