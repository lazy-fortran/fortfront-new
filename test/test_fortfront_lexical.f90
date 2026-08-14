program test_fortfront_lexical
    !! Constructed facts establish the generic frontend classifier boundary.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t, &
        fortfront_lexical_facts_t, fortfront_lexical_lookup, &
        fortfront_lexical_lookup_ambiguous, fortfront_lexical_lookup_invalid_scalar, &
        fortfront_lexical_lookup_invalid_facts, &
        fortfront_lexical_lookup_match, fortfront_lexical_lookup_no_match, &
        fortfront_lexical_lookup_unsupported, fortfront_lexical_reset, &
        fortfront_lexical_validate, fortfront_lexical_next_scalar, &
        fortfront_lexical_classify_span, fortfront_lexical_span_result_t, &
        fortfront_lexical_scalar_ok, fortfront_lexical_scalar_end, &
        fortfront_lexical_scalar_invalid_utf8, fortfront_lexical_span_match, &
        fortfront_lexical_span_invalid_utf8, fortfront_lexical_span_mixed_facts
    implicit none

    type(fortfront_lexical_facts_t) :: facts
    type(fortfront_lexical_fact_t) :: result
    integer :: status
    character(len=256) :: message
    logical :: ok

    call make_facts(facts)
    call fortfront_lexical_validate(facts, ok, message)
    call require(ok, 'valid constructed facts were rejected: '//trim(message))

    call fortfront_lexical_lookup(facts, 70_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_match, 'range lookup failed')
    call require(trim(result%target_name) == 'TARGET_RANGE', 'range target differs')
    call require(trim(result%class_name) == 'constructed-class', 'range class differs')
    call require(trim(result%source_hash) == repeat('a', 64), &
        'range provenance differs')

    call fortfront_lexical_lookup(facts, 945_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_match, 'exact lookup failed')
    call require(trim(result%target_name) == 'TARGET_EXACT', 'exact target differs')

    call fortfront_lexical_lookup(facts, 32_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_unsupported, &
        'processor-defined fact was not reported as unsupported')

    facts%count = 2
    call fortfront_lexical_lookup(facts, 32_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_no_match, &
        'no-match status differs')

    call fortfront_lexical_lookup(facts, -1_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_invalid_scalar, &
        'negative scalar was accepted')
    call fortfront_lexical_lookup(facts, int(z'd800', int64), result, status, message)
    call require(status == fortfront_lexical_lookup_invalid_scalar, &
        'surrogate scalar was accepted')
    call fortfront_lexical_lookup(facts, int(z'110000', int64), result, status, message)
    call require(status == fortfront_lexical_lookup_invalid_scalar, &
        'out-of-range scalar was accepted')

    call make_facts(facts)
    facts%facts(2)%range_first(1) = 70_int64
    facts%facts(2)%range_last(1) = 980_int64
    call fortfront_lexical_lookup(facts, 70_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_ambiguous, &
        'overlapping facts were not reported as ambiguous')
    call require(index(message, 'overlapping') > 0, 'ambiguity diagnostic differs')

    call make_facts(facts)
    facts%facts(1)%source_hash = 'not-a-sha256'
    call fortfront_lexical_validate(facts, ok, message)
    call require(.not. ok, 'invalid provenance was accepted')
    call require(index(message, 'provenance') > 0, 'provenance diagnostic differs')
    result%target_name = 'stale-result'
    call fortfront_lexical_lookup(facts, 70_int64, result, status, message)
    call require(status == fortfront_lexical_lookup_invalid_facts, &
        'lookup accepted invalid provenance')
    call require(len_trim(result%target_name) == 0, &
        'invalid-facts lookup retained a stale result')

    call make_facts(facts)
    call test_source_scalars(facts)

    print '(a)', 'fortfront lexical fact behavioral checks: ok'

contains

    subroutine test_source_scalars(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts

        character(len=3) :: source
        character(len=3) :: invalid_source
        integer(int64) :: scalar, next_offset
        integer :: status
        character(len=256) :: message
        type(fortfront_lexical_span_result_t) :: span

        source = achar(65)//achar(206)//achar(177)
        call fortfront_lexical_next_scalar(source, 0_int64, scalar, next_offset, &
            status, message)
        call require(status == fortfront_lexical_scalar_ok, &
            'ASCII scalar iteration failed')
        call require(scalar == 65_int64 .and. next_offset == 1_int64, &
            'ASCII scalar span differs')
        call fortfront_lexical_next_scalar(source, next_offset, scalar, next_offset, &
            status, message)
        call require(status == fortfront_lexical_scalar_ok, &
            'multibyte scalar iteration failed')
        call require(scalar == 945_int64 .and. next_offset == 3_int64, &
            'multibyte scalar span differs')
        call fortfront_lexical_next_scalar(source, next_offset, scalar, next_offset, &
            status, message)
        call require(status == fortfront_lexical_scalar_end, &
            'end-of-source status differs')

        call fortfront_lexical_classify_span(source, 0_int64, 1_int64, facts, span, &
            status, message)
        call require(status == fortfront_lexical_span_match, &
            'ASCII span classification failed')
        call require(span%scalar_count == 1 .and. span%start_byte == 0_int64 .and. &
            span%end_byte == 1_int64, 'ASCII span metadata differs')
        call require(trim(span%fact%target_name) == 'TARGET_RANGE', &
            'ASCII span provenance fact differs')

        call fortfront_lexical_classify_span(source, 1_int64, 3_int64, facts, span, &
            status, message)
        call require(status == fortfront_lexical_span_match, &
            'multibyte span classification failed')
        call require(span%scalar_count == 1 .and. trim(span%fact%target_name) == &
            'TARGET_EXACT', 'multibyte span fact differs')

        call fortfront_lexical_classify_span(source, 0_int64, 3_int64, facts, span, &
            status, message)
        call require(status == fortfront_lexical_span_mixed_facts, &
            'mixed lexical facts were not rejected')
        call require(span%scalar_count == 0, 'mixed span retained stale result')

        call fortfront_lexical_classify_span(source, 0_int64, 2_int64, facts, span, &
            status, message)
        call require(status == fortfront_lexical_span_invalid_utf8, &
            'partial UTF-8 span was accepted')

        invalid_source = achar(192)//achar(128)//achar(128)
        call fortfront_lexical_next_scalar(invalid_source, 0_int64, scalar, next_offset, &
            status, message)
        call require(status == fortfront_lexical_scalar_invalid_utf8, &
            'invalid UTF-8 was accepted')
        call fortfront_lexical_classify_span(invalid_source, 0_int64, 3_int64, facts, &
            span, status, message)
        call require(status == fortfront_lexical_span_invalid_utf8, &
            'invalid UTF-8 span was accepted')
        invalid_source = achar(224)//achar(128)//achar(128)
        call fortfront_lexical_next_scalar(invalid_source, 0_int64, scalar, next_offset, &
            status, message)
        call require(status == fortfront_lexical_scalar_invalid_utf8, &
            'overlong UTF-8 was accepted')
    end subroutine test_source_scalars

    subroutine make_facts(output)
        type(fortfront_lexical_facts_t), intent(out) :: output

        call fortfront_lexical_reset(output)
        output%count = 3
        call set_fact(output%facts(1), 'range', 'constructed-class', 'TARGET_RANGE', &
            'U+0041-U+005A', 65_int64, 90_int64)
        call set_fact(output%facts(2), 'exact', 'constructed-class', 'TARGET_EXACT', &
            'U+03B1', 945_int64, 945_int64)
        call set_fact(output%facts(3), 'processor', 'constructed-class', &
            'TARGET_PROCESSOR', 'processor-defined', 0_int64, 0_int64)
        output%facts(3)%range_count = 0
    end subroutine make_facts

    subroutine set_fact(fact, source_term, class_name, target_name, codepoint, first, last)
        type(fortfront_lexical_fact_t), intent(out) :: fact
        character(len=*), intent(in) :: source_term, class_name, target_name, codepoint
        integer(int64), intent(in) :: first, last

        fact = fortfront_lexical_fact_t()
        fact%source_term = source_term
        fact%class_name = class_name
        fact%target_name = target_name
        fact%source_rule = 'RULE-'//trim(source_term)
        fact%source_page = '1'
        fact%document = 'constructed-document'
        fact%clause = 'constructed-clause'
        fact%source_hash = repeat('a', 64)
        fact%codepoint = codepoint
        fact%range_count = 1
        fact%range_first(1) = first
        fact%range_last(1) = last
    end subroutine set_fact

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_lexical
