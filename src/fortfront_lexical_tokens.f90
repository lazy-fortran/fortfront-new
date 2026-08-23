module fortfront_lexical_tokens
    !! Projection from scanned lexical spans to caller-named token records.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t, &
        fortfront_lexical_scanned_span_t, fortfront_lexical_span_ambiguous, &
        fortfront_lexical_span_invalid_bounds, fortfront_lexical_span_invalid_facts, &
        fortfront_lexical_span_invalid_utf8, fortfront_lexical_span_match, &
        fortfront_lexical_span_mixed_facts, fortfront_lexical_span_no_match, &
        fortfront_lexical_span_unsupported
    implicit none
    private

    integer, parameter, public :: fortfront_lexical_token_match = 0
    integer, parameter, public :: fortfront_lexical_token_no_match = 1
    integer, parameter, public :: fortfront_lexical_token_unsupported = 2
    integer, parameter, public :: fortfront_lexical_token_ambiguous = 3
    integer, parameter, public :: fortfront_lexical_token_malformed = 4
    integer, parameter, public :: fortfront_lexical_token_capacity = 5

    type, public :: fortfront_lexical_token_t
        character(len=128) :: symbol = ''
        integer(int64) :: start_byte = 0_int64
        integer(int64) :: end_byte = 0_int64
        integer :: scalar_count = 0
        type(fortfront_lexical_fact_t) :: fact
        integer :: status = fortfront_lexical_token_malformed
        character(len=256) :: message = ''
    end type fortfront_lexical_token_t

    public :: fortfront_lexical_tokens_from_scan

contains

    subroutine fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, &
            output_count, status, message)
        type(fortfront_lexical_scanned_span_t), intent(in) :: scanned(:)
        integer, intent(in) :: scanned_count
        character(len=*), intent(in) :: symbols(:)
        type(fortfront_lexical_token_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        integer :: i, mapped_status

        output = fortfront_lexical_token_t()
        output_count = 0
        status = fortfront_lexical_token_malformed
        message = ''
        if (scanned_count < 0 .or. scanned_count > size(scanned)) then
            message = 'lexical-token-scan-count-is-out-of-range'
            return
        end if
        if (scanned_count > size(symbols)) then
            message = 'lexical-token-symbol-count-is-too-small'
            return
        end if

        do i = 1, scanned_count
            if (len_trim(symbols(i)) == 0) then
                message = 'lexical-token-symbol-is-empty'
                return
            end if
            if (output_count >= size(output)) then
                status = fortfront_lexical_token_capacity
                message = 'lexical-token-output-capacity-was-exhausted'
                return
            end if
            output_count = output_count + 1
            output(output_count)%symbol = symbols(i)
            output(output_count)%start_byte = scanned(i)%span%start_byte
            output(output_count)%end_byte = scanned(i)%span%end_byte
            output(output_count)%scalar_count = scanned(i)%span%scalar_count
            output(output_count)%fact = scanned(i)%span%fact
            output(output_count)%message = scanned(i)%message
            mapped_status = map_span_status(scanned(i)%status)
            output(output_count)%status = mapped_status
            if (mapped_status == fortfront_lexical_token_malformed) then
                if (len_trim(output(output_count)%message) == 0) then
                    output(output_count)%message = 'lexical scan span is malformed'
                end if
                status = fortfront_lexical_token_malformed
                message = output(output_count)%message
                return
            end if
            if (mapped_status == fortfront_lexical_token_match) then
                if (output(output_count)%start_byte < 0_int64 .or. &
                    output(output_count)%end_byte <= output(output_count)%start_byte .or. &
                    output(output_count)%scalar_count <= 0) then
                    output(output_count)%status = fortfront_lexical_token_malformed
                    output(output_count)%message = 'lexical scan match metadata is malformed'
                    status = fortfront_lexical_token_malformed
                    message = output(output_count)%message
                    return
                end if
            end if
        end do
        status = fortfront_lexical_token_match
        message = 'lexical token stream is complete'
    contains
        integer function map_span_status(span_status)
            integer, intent(in) :: span_status

            select case (span_status)
            case (fortfront_lexical_span_match)
                map_span_status = fortfront_lexical_token_match
            case (fortfront_lexical_span_no_match)
                map_span_status = fortfront_lexical_token_no_match
            case (fortfront_lexical_span_unsupported)
                map_span_status = fortfront_lexical_token_unsupported
            case (fortfront_lexical_span_ambiguous)
                map_span_status = fortfront_lexical_token_ambiguous
            case (fortfront_lexical_span_invalid_utf8, fortfront_lexical_span_invalid_bounds, &
                    fortfront_lexical_span_mixed_facts, fortfront_lexical_span_invalid_facts)
                map_span_status = fortfront_lexical_token_malformed
            case default
                map_span_status = fortfront_lexical_token_malformed
            end select
        end function map_span_status
    end subroutine fortfront_lexical_tokens_from_scan

end module fortfront_lexical_tokens
