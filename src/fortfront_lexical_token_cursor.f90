module fortfront_lexical_token_cursor
    !! Bounded, caller-supplied cursor over lexical token records.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical_tokens, only: fortfront_lexical_token_ambiguous, &
        fortfront_lexical_token_match, fortfront_lexical_token_no_match, &
        fortfront_lexical_token_t, fortfront_lexical_token_unsupported
    implicit none
    private

    integer, parameter, public :: fortfront_lexical_token_cursor_ok = 0
    integer, parameter, public :: fortfront_lexical_token_cursor_end_of_stream = 1
    integer, parameter, public :: fortfront_lexical_token_cursor_malformed = 2
    integer, parameter, public :: fortfront_lexical_token_cursor_end = &
        fortfront_lexical_token_cursor_end_of_stream

    type, public :: fortfront_lexical_token_cursor_t
        type(fortfront_lexical_token_t), allocatable :: tokens(:)
        integer :: count = 0
        integer :: position = 1
        logical :: initialized = .false.
    end type fortfront_lexical_token_cursor_t

    public :: fortfront_lexical_token_cursor_initialize
    public :: fortfront_lexical_token_cursor_peek
    public :: fortfront_lexical_token_cursor_advance
    public :: initialize, peek, advance

    interface initialize
        module procedure fortfront_lexical_token_cursor_initialize
    end interface initialize
    interface peek
        module procedure fortfront_lexical_token_cursor_peek
    end interface peek
    interface advance
        module procedure fortfront_lexical_token_cursor_advance
    end interface advance

contains

    subroutine fortfront_lexical_token_cursor_initialize(cursor, tokens, count, status, message)
        type(fortfront_lexical_token_cursor_t), intent(out) :: cursor
        type(fortfront_lexical_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i

        cursor = fortfront_lexical_token_cursor_t()
        status = fortfront_lexical_token_cursor_malformed
        message = ''
        if (count < 0 .or. count > size(tokens)) then
            message = 'lexical-token-cursor-count-is-out-of-range'
            return
        end if
        if (count > 0) then
            allocate (cursor%tokens(count))
            do i = 1, count
                if (.not. token_is_valid(tokens(i))) then
                    deallocate (cursor%tokens)
                    message = 'lexical-token-cursor-token-is-malformed'
                    return
                end if
            end do
            cursor%tokens = tokens(1:count)
        else
            allocate (cursor%tokens(0))
        end if
        cursor%count = count
        cursor%position = 1
        cursor%initialized = .true.
        status = fortfront_lexical_token_cursor_ok
        message = 'lexical token cursor is initialized'
    end subroutine fortfront_lexical_token_cursor_initialize

    subroutine fortfront_lexical_token_cursor_peek(cursor, token, status, message)
        type(fortfront_lexical_token_cursor_t), intent(in) :: cursor
        type(fortfront_lexical_token_t), intent(out) :: token
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        token = fortfront_lexical_token_t()
        status = fortfront_lexical_token_cursor_malformed
        message = ''
        if (.not. cursor_is_valid(cursor)) then
            message = 'lexical-token-cursor-is-malformed'
            return
        end if
        if (cursor%position > cursor%count) then
            status = fortfront_lexical_token_cursor_end_of_stream
            message = 'lexical token cursor is at end of stream'
            return
        end if
        if (.not. token_is_valid(cursor%tokens(cursor%position))) then
            message = 'lexical-token-cursor-token-is-malformed'
            return
        end if
        token = cursor%tokens(cursor%position)
        status = fortfront_lexical_token_cursor_ok
        message = 'lexical token is available'
    end subroutine fortfront_lexical_token_cursor_peek

    subroutine fortfront_lexical_token_cursor_advance(cursor, token, status, message)
        type(fortfront_lexical_token_cursor_t), intent(inout) :: cursor
        type(fortfront_lexical_token_t), intent(out) :: token
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        call fortfront_lexical_token_cursor_peek(cursor, token, status, message)
        if (status == fortfront_lexical_token_cursor_ok) cursor%position = cursor%position + 1
    end subroutine fortfront_lexical_token_cursor_advance

    logical function cursor_is_valid(cursor)
        type(fortfront_lexical_token_cursor_t), intent(in) :: cursor

        cursor_is_valid = cursor%initialized
        if (.not. cursor_is_valid) return
        if (.not. allocated(cursor%tokens)) then
            cursor_is_valid = .false.
            return
        end if
        cursor_is_valid = cursor%count >= 0 .and. cursor%count <= size(cursor%tokens) .and. &
            cursor%position >= 1 .and. cursor%position <= cursor%count + 1
    end function cursor_is_valid

    logical function token_is_valid(token)
        type(fortfront_lexical_token_t), intent(in) :: token

        token_is_valid = len_trim(token%symbol) > 0
        if (.not. token_is_valid) return
        select case (token%status)
        case (fortfront_lexical_token_match, fortfront_lexical_token_no_match, &
                fortfront_lexical_token_unsupported, fortfront_lexical_token_ambiguous)
        case default
            token_is_valid = .false.
            return
        end select
        if (token%start_byte < 0_int64 .or. token%end_byte <= token%start_byte) then
            token_is_valid = .false.
            return
        end if
        token_is_valid = token%scalar_count > 0
    end function token_is_valid

end module fortfront_lexical_token_cursor
