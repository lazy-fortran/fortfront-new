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

    public :: fortfront_lexical_lookup
    public :: fortfront_lexical_reset
    public :: fortfront_lexical_validate

contains

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
