module fortfront_lexical
    !! Caller-supplied, source-backed lexical classification facts.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    integer, parameter, public :: fortfront_max_lexical_facts = 16
    integer, parameter, public :: fortfront_lexical_max_ranges = 4
    integer, parameter, public :: fortfront_lexical_lookup_match = 0
    integer, parameter, public :: fortfront_lexical_lookup_no_match = 1
    integer, parameter, public :: fortfront_lexical_lookup_unsupported = 2
    integer, parameter, public :: fortfront_lexical_lookup_ambiguous = 3
    integer, parameter, public :: fortfront_lexical_lookup_invalid_scalar = 4
    integer, parameter, public :: fortfront_lexical_lookup_invalid_facts = 5
    integer, parameter, public :: fortfront_lexical_scalar_ok = 0
    integer, parameter, public :: fortfront_lexical_scalar_end = 1
    integer, parameter, public :: fortfront_lexical_scalar_invalid_utf8 = 2
    integer, parameter, public :: fortfront_lexical_scalar_invalid_offset = 3
    integer, parameter, public :: fortfront_lexical_span_match = 0
    integer, parameter, public :: fortfront_lexical_span_empty = 1
    integer, parameter, public :: fortfront_lexical_span_invalid_utf8 = 2
    integer, parameter, public :: fortfront_lexical_span_invalid_bounds = 3
    integer, parameter, public :: fortfront_lexical_span_mixed_facts = 4
    integer, parameter, public :: fortfront_lexical_span_no_match = 5
    integer, parameter, public :: fortfront_lexical_span_unsupported = 6
    integer, parameter, public :: fortfront_lexical_span_ambiguous = 7
    integer, parameter, public :: fortfront_lexical_span_invalid_facts = 8
    integer, parameter, public :: fortfront_lexical_scan_complete = 0
    integer, parameter, public :: fortfront_lexical_scan_empty = 1
    integer, parameter, public :: fortfront_lexical_scan_invalid_facts = 2
    integer, parameter, public :: fortfront_lexical_scan_capacity = 3

    type, public :: fortfront_lexical_fact_t
        character(len=256) :: source_term = ''
        character(len=64) :: class_name = ''
        character(len=128) :: target_name = ''
        character(len=64) :: source_rule = ''
        character(len=64) :: source_page = ''
        character(len=128) :: document = ''
        character(len=128) :: clause = ''
        character(len=128) :: source_hash = ''
        character(len=64) :: codepoint = ''
        integer :: range_count = 0
        integer(int64) :: range_first(fortfront_lexical_max_ranges) = 0_int64
        integer(int64) :: range_last(fortfront_lexical_max_ranges) = 0_int64
    end type fortfront_lexical_fact_t

    type, public :: fortfront_lexical_facts_t
        integer :: count = 0
        type(fortfront_lexical_fact_t) :: facts(fortfront_max_lexical_facts)
    end type fortfront_lexical_facts_t

    type, public :: fortfront_lexical_span_result_t
        integer(int64) :: start_byte = 0_int64
        integer(int64) :: end_byte = 0_int64
        integer :: scalar_count = 0
        type(fortfront_lexical_fact_t) :: fact
    end type fortfront_lexical_span_result_t

    type, public :: fortfront_lexical_scanned_span_t
        type(fortfront_lexical_span_result_t) :: span
        integer :: status = fortfront_lexical_span_invalid_bounds
        character(len=256) :: message = ''
    end type fortfront_lexical_scanned_span_t

    public :: fortfront_lexical_lookup
    public :: fortfront_lexical_reset
    public :: fortfront_lexical_validate
    public :: fortfront_lexical_next_scalar
    public :: fortfront_lexical_classify_span
    public :: fortfront_lexical_scan

contains

    subroutine fortfront_lexical_next_scalar(source, byte_offset, scalar, &
            next_byte_offset, status, message)
        character(len=*), intent(in) :: source
        integer(int64), intent(in) :: byte_offset
        integer(int64), intent(out) :: scalar
        integer(int64), intent(out) :: next_byte_offset
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: first, width, i, byte_value, continuation
        integer(int64) :: value

        scalar = 0_int64
        next_byte_offset = byte_offset
        status = fortfront_lexical_scalar_invalid_offset
        message = ''
        if (byte_offset < 0_int64 .or. byte_offset > int(len(source), int64)) then
            message = 'UTF-8 scalar offset is outside source bounds'
            return
        end if
        if (byte_offset == int(len(source), int64)) then
            status = fortfront_lexical_scalar_end
            message = 'end of source'
            return
        end if

        first = int(byte_offset) + 1
        byte_value = iachar(source(first:first))
        if (byte_value < 128) then
            scalar = int(byte_value, int64)
            next_byte_offset = byte_offset + 1_int64
            status = fortfront_lexical_scalar_ok
            return
        end if
        if (byte_value >= 194 .and. byte_value <= 223) then
            width = 2
            value = int(byte_value - 192, int64)
        else if (byte_value >= 224 .and. byte_value <= 239) then
            width = 3
            value = int(byte_value - 224, int64)
        else if (byte_value >= 240 .and. byte_value <= 244) then
            width = 4
            value = int(byte_value - 240, int64)
        else
            message = 'invalid UTF-8 leading byte'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if

        if (byte_offset + int(width, int64) > int(len(source), int64)) then
            message = 'truncated UTF-8 scalar'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if
        do i = 2, width
            continuation = iachar(source(first + i - 1:first + i - 1))
            if (continuation < 128) then
                message = 'invalid UTF-8 continuation byte'
                status = fortfront_lexical_scalar_invalid_utf8
                return
            end if
            if (continuation > 191) then
                message = 'invalid UTF-8 continuation byte'
                status = fortfront_lexical_scalar_invalid_utf8
                return
            end if
            value = value * 64_int64 + int(continuation - 128, int64)
        end do
        if (width == 2 .and. value < 128_int64) then
            message = 'UTF-8 sequence is overlong'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if
        if (width == 3 .and. value < 2048_int64) then
            message = 'UTF-8 sequence is overlong'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if
        if (width == 4 .and. value < 65536_int64) then
            message = 'UTF-8 sequence is overlong'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if
        if (.not. is_unicode_scalar(value)) then
            message = 'UTF-8 sequence is not a Unicode scalar'
            status = fortfront_lexical_scalar_invalid_utf8
            return
        end if
        scalar = value
        next_byte_offset = byte_offset + int(width, int64)
        status = fortfront_lexical_scalar_ok
    end subroutine fortfront_lexical_next_scalar

    subroutine fortfront_lexical_classify_span(source, start_byte, end_byte, facts, &
            result, status, message)
        character(len=*), intent(in) :: source
        integer(int64), intent(in) :: start_byte, end_byte
        type(fortfront_lexical_facts_t), intent(in) :: facts
        type(fortfront_lexical_span_result_t), intent(out) :: result
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer(int64) :: offset, next_offset, scalar
        integer :: scalar_status, lookup_status
        type(fortfront_lexical_fact_t) :: candidate
        logical :: facts_ok

        result = fortfront_lexical_span_result_t()
        result%start_byte = start_byte
        result%end_byte = end_byte
        status = fortfront_lexical_span_invalid_bounds
        message = ''
        if (start_byte < 0_int64 .or. end_byte < start_byte) then
            message = 'lexical span bounds are invalid'
            return
        end if
        if (end_byte > int(len(source), int64)) then
            message = 'lexical span exceeds source bounds'
            return
        end if
        if (start_byte == end_byte) then
            status = fortfront_lexical_span_empty
            message = 'lexical span is empty'
            return
        end if
        call fortfront_lexical_validate(facts, facts_ok, message)
        if (.not. facts_ok) then
            status = fortfront_lexical_span_invalid_facts
            return
        end if

        offset = start_byte
        do while (offset < end_byte)
            call fortfront_lexical_next_scalar(source, offset, scalar, next_offset, &
                scalar_status, message)
            if (scalar_status /= fortfront_lexical_scalar_ok) then
                result = fortfront_lexical_span_result_t()
                result%start_byte = start_byte
                result%end_byte = end_byte
                status = fortfront_lexical_span_invalid_utf8
                return
            end if
            if (next_offset > end_byte) then
                result = fortfront_lexical_span_result_t()
                result%start_byte = start_byte
                result%end_byte = end_byte
                status = fortfront_lexical_span_invalid_utf8
                message = 'lexical span ends inside a UTF-8 scalar'
                return
            end if
            call fortfront_lexical_lookup(facts, scalar, candidate, lookup_status, message)
            if (lookup_status /= fortfront_lexical_lookup_match) then
                result = fortfront_lexical_span_result_t()
                result%start_byte = start_byte
                result%end_byte = end_byte
                select case (lookup_status)
                case (fortfront_lexical_lookup_no_match)
                    status = fortfront_lexical_span_no_match
                case (fortfront_lexical_lookup_unsupported)
                    status = fortfront_lexical_span_unsupported
                case (fortfront_lexical_lookup_ambiguous)
                    status = fortfront_lexical_span_ambiguous
                case default
                    status = fortfront_lexical_span_invalid_facts
                end select
                return
            end if
            if (result%scalar_count == 0) then
                result%fact = candidate
            else if (result%fact%target_name /= candidate%target_name .or. &
                    result%fact%class_name /= candidate%class_name) then
                result = fortfront_lexical_span_result_t()
                result%start_byte = start_byte
                result%end_byte = end_byte
                status = fortfront_lexical_span_mixed_facts
                message = 'lexical span contains different facts'
                return
            end if
            result%scalar_count = result%scalar_count + 1
            offset = next_offset
        end do
        status = fortfront_lexical_span_match
    end subroutine fortfront_lexical_classify_span

    subroutine fortfront_lexical_scan(source, facts, output, output_count, status, &
            message)
        character(len=*), intent(in) :: source
        type(fortfront_lexical_facts_t), intent(in) :: facts
        type(fortfront_lexical_scanned_span_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer(int64) :: offset, next_offset, scalar
        integer :: scalar_status, span_status
        type(fortfront_lexical_span_result_t) :: classified
        type(fortfront_lexical_scanned_span_t) :: segment
        logical :: active, facts_ok, same_segment
        character(len=256) :: local_message

        output = fortfront_lexical_scanned_span_t()
        output_count = 0
        status = fortfront_lexical_scan_complete
        message = ''
        active = .false.

        call fortfront_lexical_validate(facts, facts_ok, message)
        if (.not. facts_ok) then
            status = fortfront_lexical_scan_invalid_facts
            return
        end if
        if (len(source) == 0) then
            status = fortfront_lexical_scan_empty
            message = 'source is empty'
            return
        end if

        offset = 0_int64
        do while (offset < int(len(source), int64))
            call fortfront_lexical_next_scalar(source, offset, scalar, next_offset, &
                scalar_status, local_message)
            if (scalar_status == fortfront_lexical_scalar_ok) then
                call fortfront_lexical_classify_span(source, offset, next_offset, facts, &
                    classified, span_status, local_message)
            else
                classified = fortfront_lexical_span_result_t()
                classified%start_byte = offset
                classified%end_byte = offset + 1_int64
                span_status = fortfront_lexical_span_invalid_utf8
                next_offset = offset + 1_int64
            end if
            if (span_status /= fortfront_lexical_span_invalid_utf8) then
                classified%scalar_count = 1
            end if

            if (active) then
                same_segment = span_status == segment%status
                if (span_status == fortfront_lexical_span_match) then
                    same_segment = same_segment .and. lexical_facts_equal( &
                        classified%fact, segment%span%fact)
                end if
            else
                same_segment = .false.
            end if
            if (same_segment) then
                segment%span%end_byte = classified%end_byte
                segment%span%scalar_count = segment%span%scalar_count + &
                    classified%scalar_count
            else
                if (active) then
                    if (.not. emit_scanned_span(segment, output, output_count, &
                        message)) then
                        status = fortfront_lexical_scan_capacity
                        return
                    end if
                end if
                segment = fortfront_lexical_scanned_span_t()
                segment%span = classified
                segment%status = span_status
                segment%message = local_message
                active = .true.
            end if
            offset = next_offset
        end do

        if (active) then
            if (.not. emit_scanned_span(segment, output, output_count, message)) then
                status = fortfront_lexical_scan_capacity
                return
            end if
        end if
    contains
        logical function emit_scanned_span(value, destination, count, diagnostic)
            type(fortfront_lexical_scanned_span_t), intent(in) :: value
            type(fortfront_lexical_scanned_span_t), intent(inout) :: destination(:)
            integer, intent(inout) :: count
            character(len=*), intent(out) :: diagnostic

            diagnostic = ''
            if (count >= size(destination)) then
                diagnostic = 'lexical scan output capacity was exhausted'
                emit_scanned_span = .false.
                return
            end if
            count = count + 1
            destination(count) = value
            emit_scanned_span = .true.
        end function emit_scanned_span

        logical function lexical_facts_equal(left, right)
            type(fortfront_lexical_fact_t), intent(in) :: left, right

            lexical_facts_equal = trim(left%source_term) == trim(right%source_term)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%class_name) == trim(right%class_name)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%target_name) == trim(right%target_name)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%source_rule) == trim(right%source_rule)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%source_page) == trim(right%source_page)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%document) == trim(right%document)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%clause) == trim(right%clause)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%source_hash) == trim(right%source_hash)
            lexical_facts_equal = lexical_facts_equal .and. &
                trim(left%codepoint) == trim(right%codepoint)
            lexical_facts_equal = lexical_facts_equal .and. &
                left%range_count == right%range_count
            lexical_facts_equal = lexical_facts_equal .and. &
                all(left%range_first == right%range_first)
            lexical_facts_equal = lexical_facts_equal .and. &
                all(left%range_last == right%range_last)
        end function lexical_facts_equal
    end subroutine fortfront_lexical_scan

    subroutine fortfront_lexical_reset(facts)
        type(fortfront_lexical_facts_t), intent(out) :: facts

        facts%count = 0
    end subroutine fortfront_lexical_reset

    subroutine fortfront_lexical_lookup(facts, scalar, result, status, message)
        type(fortfront_lexical_facts_t), intent(in) :: facts
        integer(int64), intent(in) :: scalar
        type(fortfront_lexical_fact_t), intent(out) :: result
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i, j, match_count
        logical :: facts_ok, processor_defined

        result = fortfront_lexical_fact_t()
        status = fortfront_lexical_lookup_no_match
        message = ''
        if (.not. is_unicode_scalar(scalar)) then
            status = fortfront_lexical_lookup_invalid_scalar
            message = 'lookup value is not a Unicode scalar'
            return
        end if
        call fortfront_lexical_validate(facts, facts_ok, message)
        if (.not. facts_ok) then
            status = fortfront_lexical_lookup_invalid_facts
            result = fortfront_lexical_fact_t()
            return
        end if

        match_count = 0
        processor_defined = .false.
        do i = 1, facts%count
            if (facts%facts(i)%range_count == 0) then
                processor_defined = .true.
                if (len_trim(result%target_name) == 0) result = facts%facts(i)
            else
                do j = 1, facts%facts(i)%range_count
                    if (scalar >= facts%facts(i)%range_first(j)) then
                        if (scalar <= facts%facts(i)%range_last(j)) then
                            match_count = match_count + 1
                            result = facts%facts(i)
                            exit
                        end if
                    end if
                end do
            end if
        end do

        if (match_count > 1) then
            result = fortfront_lexical_fact_t()
            status = fortfront_lexical_lookup_ambiguous
            message = 'lookup value matches overlapping lexical facts'
        else if (match_count == 1) then
            status = fortfront_lexical_lookup_match
        else if (processor_defined) then
            status = fortfront_lexical_lookup_unsupported
            message = 'lookup requires a processor-defined lexical fact'
        else
            result = fortfront_lexical_fact_t()
            status = fortfront_lexical_lookup_no_match
            message = 'lookup value matches no lexical fact'
        end if
    end subroutine fortfront_lexical_lookup

    subroutine fortfront_lexical_validate(facts, ok, message)
        type(fortfront_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, k

        ok = .false.
        message = ''
        if (facts%count < 0 .or. facts%count > size(facts%facts)) then
            message = 'lexical fact count is outside storage'
            return
        end if
        do i = 1, facts%count
            if (len_trim(facts%facts(i)%source_term) == 0) then
                message = 'lexical fact lacks source term'
                return
            end if
            if (len_trim(facts%facts(i)%class_name) == 0) then
                message = 'lexical fact lacks class'
                return
            end if
            if (len_trim(facts%facts(i)%target_name) == 0) then
                message = 'lexical fact lacks target'
                return
            end if
            if (.not. valid_provenance(facts%facts(i))) then
                message = 'lexical fact lacks valid source provenance'
                return
            end if
            if (facts%facts(i)%range_count < 0 .or. &
                facts%facts(i)%range_count > fortfront_lexical_max_ranges) then
                message = 'lexical fact has too many scalar ranges'
                return
            end if
            if (facts%facts(i)%range_count == 0) then
                if (trim(facts%facts(i)%codepoint) /= 'processor-defined') then
                    message = 'lexical fact has no scalar range or processor marker'
                    return
                end if
            else
                if (len_trim(facts%facts(i)%codepoint) == 0) then
                    message = 'lexical fact lacks codepoint description'
                    return
                end if
            end if
            do j = 1, facts%facts(i)%range_count
                if (.not. is_unicode_scalar(facts%facts(i)%range_first(j)) .or. &
                    .not. is_unicode_scalar(facts%facts(i)%range_last(j))) then
                    message = 'lexical scalar range is outside Unicode scalar range'
                    return
                end if
                if (facts%facts(i)%range_first(j) > facts%facts(i)%range_last(j)) then
                    message = 'lexical scalar range is reversed'
                    return
                end if
                do k = 1, j - 1
                    if (ranges_overlap(facts%facts(i)%range_first(k), &
                        facts%facts(i)%range_last(k), facts%facts(i)%range_first(j), &
                        facts%facts(i)%range_last(j))) then
                        message = 'lexical scalar ranges overlap within a fact'
                        return
                    end if
                end do
            end do
            do j = 1, i - 1
                if (trim(facts%facts(i)%source_term) == &
                    trim(facts%facts(j)%source_term)) then
                    message = 'duplicate lexical source term'
                    return
                end if
            end do
        end do
        ok = .true.
    end subroutine fortfront_lexical_validate

    logical function valid_provenance(fact)
        type(fortfront_lexical_fact_t), intent(in) :: fact

        valid_provenance = len_trim(fact%source_rule) > 0
        if (.not. valid_provenance) return
        valid_provenance = len_trim(fact%source_page) > 0
        if (.not. valid_provenance) return
        valid_provenance = len_trim(fact%document) > 0
        if (.not. valid_provenance) return
        valid_provenance = len_trim(fact%clause) > 0
        if (.not. valid_provenance) return
        valid_provenance = is_sha256(fact%source_hash)
    end function valid_provenance

    logical function ranges_overlap(first_a, last_a, first_b, last_b)
        integer(int64), intent(in) :: first_a, last_a, first_b, last_b

        ranges_overlap = first_a <= last_b
        if (.not. ranges_overlap) return
        ranges_overlap = first_b <= last_a
    end function ranges_overlap

    logical function is_unicode_scalar(value)
        integer(int64), intent(in) :: value

        is_unicode_scalar = value >= 0_int64
        if (.not. is_unicode_scalar) return
        is_unicode_scalar = value <= int(z'10ffff', int64)
        if (.not. is_unicode_scalar) return
        if (value >= int(z'd800', int64)) then
            if (value <= int(z'dfff', int64)) is_unicode_scalar = .false.
        end if
    end function is_unicode_scalar

    logical function is_sha256(value)
        character(len=*), intent(in) :: value

        integer :: i

        is_sha256 = len_trim(value) == 64
        if (.not. is_sha256) return
        do i = 1, 64
            if (hex_digit(value(i:i)) < 0) then
                is_sha256 = .false.
                return
            end if
        end do
    end function is_sha256

    integer function hex_digit(value)
        character(len=1), intent(in) :: value

        select case (value)
        case ('0':'9')
            hex_digit = iachar(value) - iachar('0')
        case ('a':'f')
            hex_digit = iachar(value) - iachar('a') + 10
        case ('A':'F')
            hex_digit = iachar(value) - iachar('A') + 10
        case default
            hex_digit = -1
        end select
    end function hex_digit

end module fortfront_lexical
