module fortfront_frontend
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    character(len=*), parameter, public :: frontend_accepted = 'accepted'
    character(len=*), parameter, public :: frontend_rejected = 'rejected'
    character(len=*), parameter, public :: severity_error = 'error'
    character(len=*), parameter, public :: root_kind_source = 'source'
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

        span%file = file_name
        span%start_byte = 0_int64
        span%end_byte = int(len(source), int64)
        span%source_hash = source_hash
        result%root%span = span

        if (len(source) == 0) then
            result%status = frontend_rejected
            result%root_kind = root_kind_none
            result%root%kind = root_kind_none
            result%diagnostic_count = 1_int64
            allocate (result%diagnostics(1))
            result%diagnostics(1)%status = frontend_rejected
            result%diagnostics(1)%severity = severity_error
            result%diagnostics(1)%message = 'empty-source'
            result%diagnostics(1)%span = span
        else
            result%status = frontend_accepted
            result%root_kind = root_kind_source
            result%root%kind = root_kind_source
            result%diagnostic_count = 0_int64
        end if
    end subroutine frontend_read

end module fortfront_frontend
