program test_fortfront_lexical_tokens
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t, fortfront_lexical_facts_t, &
        fortfront_lexical_reset, fortfront_lexical_scan, fortfront_lexical_scanned_span_t, &
        fortfront_lexical_span_ambiguous, fortfront_lexical_span_invalid_utf8, &
        fortfront_lexical_span_no_match
    use fortfront_lexical_tokens, only: fortfront_lexical_token_ambiguous, &
        fortfront_lexical_token_capacity, fortfront_lexical_token_malformed, &
        fortfront_lexical_token_match, fortfront_lexical_token_no_match, &
        fortfront_lexical_token_t, fortfront_lexical_token_unsupported, &
        fortfront_lexical_tokens_from_scan
    implicit none

    type(fortfront_lexical_facts_t) :: facts

    call make_facts(facts)
    call test_ordered_projection(facts)
    call test_status_projection(facts)
    call test_mutation_failures(facts)
    print '(a)', 'fortfront lexical token adapter behavioral checks: ok'

contains

    subroutine test_ordered_projection(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts
        character(len=4) :: source
        character(len=128) :: symbols(2)
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: scanned(8)
        type(fortfront_lexical_token_t) :: output(8)
        integer :: scanned_count, scan_status, count, status

        source = achar(65)//achar(66)//char(206)//char(177)
        symbols = [character(len=128) :: 'CALLER-UPPER', 'CALLER-GREEK']
        call fortfront_lexical_scan(source, facts, scanned, scanned_count, scan_status, message)
        call fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, count, &
            status, message)
        call require(status == fortfront_lexical_token_match .and. count == 2, &
            'ordered token projection did not complete')
        call require(trim(output(1)%symbol) == 'CALLER-UPPER' .and. &
            output(1)%start_byte == 0_int64 .and. output(1)%end_byte == 2_int64 .and. &
            output(1)%scalar_count == 2 .and. trim(output(1)%fact%target_name) == 'UPPER', &
            'first token record differs')
        call require(trim(output(2)%symbol) == 'CALLER-GREEK' .and. &
            output(2)%start_byte == 2_int64 .and. output(2)%end_byte == 4_int64 .and. &
            output(2)%scalar_count == 1 .and. trim(output(2)%fact%source_hash) == repeat('a', 64), &
            'UTF-8 token record or provenance differs')
    end subroutine test_ordered_projection

    subroutine test_status_projection(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts
        type(fortfront_lexical_facts_t) :: mutable_facts
        character(len=2) :: source
        character(len=128) :: symbols(2)
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: scanned(8)
        type(fortfront_lexical_token_t) :: output(8)
        integer :: scanned_count, scan_status, count, status

        source = achar(65)//achar(95)
        symbols = [character(len=128) :: 'LETTER', 'OTHER']
        call fortfront_lexical_scan(source, facts, scanned, scanned_count, scan_status, message)
        call fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, count, &
            status, message)
        call require(count == 2 .and. output(1)%status == fortfront_lexical_token_match .and. &
            output(2)%status == fortfront_lexical_token_unsupported, &
            'no-match/unsupported statuses were not retained')

        mutable_facts = facts
        mutable_facts%count = 2
        call fortfront_lexical_scan(source(1:2), mutable_facts, scanned, scanned_count, &
            scan_status, message)
        call fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, count, &
            status, message)
        call require(output(2)%status == fortfront_lexical_token_no_match, &
            'no-match status was not retained')

        mutable_facts = facts
        mutable_facts%facts(3)%range_first(1) = 65_int64
        mutable_facts%facts(3)%range_last(1) = 65_int64
        call fortfront_lexical_scan(source(1:1), mutable_facts, scanned, scanned_count, &
            scan_status, message)
        call fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, count, &
            status, message)
        call require(output(1)%status == fortfront_lexical_token_ambiguous, &
            'ambiguous status was not retained')

        scanned(1)%status = fortfront_lexical_span_no_match
        call fortfront_lexical_tokens_from_scan(scanned, 1, symbols, output, count, status, &
            message)
        call require(status == fortfront_lexical_token_match .and. &
            output(1)%status == fortfront_lexical_token_no_match, &
            'no-match status was not projected')
        scanned(1)%status = fortfront_lexical_span_ambiguous
        call fortfront_lexical_tokens_from_scan(scanned, 1, symbols, output, count, status, message)
        call require(status == fortfront_lexical_token_match .and. &
            output(1)%status == fortfront_lexical_token_ambiguous, &
            'ambiguous status was not projected')

        scanned(1)%status = 999
        call fortfront_lexical_tokens_from_scan(scanned, 1, symbols, output, count, status, message)
        call require(status == fortfront_lexical_token_malformed .and. count == 1, &
            'unknown scan status was not malformed')
    end subroutine test_status_projection

    subroutine test_mutation_failures(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts
        character(len=2) :: source
        character(len=128) :: symbols(2)
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: scanned(8)
        type(fortfront_lexical_token_t) :: output(1)
        integer :: scanned_count, scan_status, count, status

        source = achar(65)//achar(70)
        symbols = [character(len=128) :: 'A', 'F']
        call fortfront_lexical_scan(source, facts, scanned, scanned_count, scan_status, message)
        output(1)%symbol = 'stale'
        call fortfront_lexical_tokens_from_scan(scanned, scanned_count, symbols, output, count, &
            status, message)
        call require(status == fortfront_lexical_token_capacity .and. count == 1 .and. &
            trim(output(1)%symbol) == 'A', 'capacity or output clearing differs')

        scanned(1)%status = fortfront_lexical_span_invalid_utf8
        call fortfront_lexical_tokens_from_scan(scanned, 1, symbols, output, count, status, message)
        call require(status == fortfront_lexical_token_malformed .and. &
            output(1)%status == fortfront_lexical_token_malformed, &
            'malformed scan state was not explicit')

        call fortfront_lexical_tokens_from_scan(scanned, -1, symbols, output, count, status, &
            message)
        call require(status == fortfront_lexical_token_malformed .and. count == 0 .and. &
            trim(output(1)%symbol) == '', 'invalid count did not clear output')
    end subroutine test_mutation_failures

    subroutine make_facts(output)
        type(fortfront_lexical_facts_t), intent(out) :: output

        call fortfront_lexical_reset(output)
        output%count = 4
        call set_fact(output%facts(1), 'upper', 'constructed-class', 'UPPER', 65_int64, 66_int64)
        call set_fact(output%facts(2), 'greek', 'constructed-class', 'GREEK', 945_int64, 945_int64)
        call set_fact(output%facts(3), 'space', 'constructed-class', 'SPACE', 32_int64, 32_int64)
        call set_fact(output%facts(4), 'processor', 'constructed-class', 'PROCESSOR', 0_int64, &
            0_int64)
        output%facts(4)%range_count = 0
        output%facts(4)%codepoint = 'processor-defined'
    end subroutine make_facts

    subroutine set_fact(fact, source_term, class_name, target_name, first, last)
        type(fortfront_lexical_fact_t), intent(out) :: fact
        character(len=*), intent(in) :: source_term, class_name, target_name
        integer(int64), intent(in) :: first, last

        fact = fortfront_lexical_fact_t()
        fact%source_term = source_term
        fact%class_name = class_name
        fact%target_name = target_name
        fact%source_rule = 'RULE'
        fact%source_page = '1'
        fact%document = 'constructed-document'
        fact%clause = 'constructed-clause'
        fact%source_hash = repeat('a', 64)
        fact%codepoint = 'scalar'
        fact%range_count = 1
        fact%range_first(1) = first
        fact%range_last(1) = last
    end subroutine set_fact

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_lexical_tokens
