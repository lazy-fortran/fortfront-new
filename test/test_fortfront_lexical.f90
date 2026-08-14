program test_fortfront_lexical
    !! Constructed facts establish the generic frontend classifier boundary.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t, &
        fortfront_lexical_facts_t, fortfront_lexical_lookup, &
        fortfront_lexical_lookup_ambiguous, fortfront_lexical_lookup_invalid_scalar, &
        fortfront_lexical_lookup_invalid_facts, &
        fortfront_lexical_lookup_match, fortfront_lexical_lookup_no_match, &
        fortfront_lexical_lookup_unsupported, fortfront_lexical_reset, &
        fortfront_lexical_validate
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

    print '(a)', 'fortfront lexical fact behavioral checks: ok'

contains

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
