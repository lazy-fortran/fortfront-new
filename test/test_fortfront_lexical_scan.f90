program test_fortfront_lexical_scan
    !! The scanner is tested with facts that are independent of Fortran syntax.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t, &
        fortfront_lexical_facts_t, fortfront_lexical_reset, fortfront_lexical_scan, &
        fortfront_lexical_scanned_span_t, fortfront_lexical_scan_complete, &
        fortfront_lexical_scan_empty, &
        fortfront_lexical_scan_capacity, fortfront_lexical_span_ambiguous, &
        fortfront_lexical_span_invalid_bounds, fortfront_lexical_span_invalid_utf8, &
        fortfront_lexical_span_match, fortfront_lexical_span_no_match, &
        fortfront_lexical_span_unsupported
    implicit none

    type(fortfront_lexical_facts_t) :: facts

    call make_facts(facts)
    call test_adjacent_and_multibyte(facts)
    call test_explicit_statuses(facts)
    call test_malformed_utf8(facts)
    call test_capacity_and_clearing(facts)
    print '(a)', 'fortfront lexical scanner behavioral checks: ok'

contains

    subroutine test_adjacent_and_multibyte(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts

        character(len=4) :: source
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: output(8)
        integer :: count, status

        source = char(65)//char(66)//char(206)//char(177)
        call fortfront_lexical_scan(source, facts, output, count, status, message)
        call require(status == fortfront_lexical_scan_complete, &
            'complete scan status differs')
        call require(count == 2, 'same-fact spans were not coalesced')
        call require(output(1)%status == fortfront_lexical_span_match, &
            'ASCII span status differs')
        call require(output(1)%span%start_byte == 0_int64 .and. &
            output(1)%span%end_byte == 2_int64 .and. &
            output(1)%span%scalar_count == 2, 'ASCII span bounds differ')
        call require(trim(output(1)%span%fact%target_name) == 'UPPER', &
            'ASCII source fact differs')
        call require(output(2)%span%start_byte == 2_int64 .and. &
            output(2)%span%end_byte == 4_int64 .and. &
            output(2)%span%scalar_count == 1, 'multibyte span bounds differ')
        call require(trim(output(2)%span%fact%target_name) == 'GREEK', &
            'multibyte source fact differs')
    end subroutine test_adjacent_and_multibyte

    subroutine test_explicit_statuses(input_facts)
        type(fortfront_lexical_facts_t), intent(in) :: input_facts

        character(len=3) :: source
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: output(8)
        type(fortfront_lexical_facts_t) :: facts
        integer :: count, status

        source = achar(65)//achar(95)//achar(32)
        call fortfront_lexical_scan(source, input_facts, output, count, status, message)
        call require(count == 2, 'unsupported spans were not coalesced')
        call require(output(1)%status == fortfront_lexical_span_match, &
            'matched status differs')
        call require(output(2)%status == fortfront_lexical_span_unsupported, &
            'unsupported status differs')
        call require(output(2)%span%start_byte == 1_int64 .and. &
            output(2)%span%end_byte == 3_int64, 'unsupported span bounds differ')
        facts = input_facts
        facts%count = 2
        call fortfront_lexical_scan(source(1:2), facts, output, count, status, message)
        call require(count == 2 .and. output(2)%status == fortfront_lexical_span_no_match, &
            'unmatched status differs')

        facts = input_facts
        source = achar(67)//achar(68)
        call fortfront_lexical_scan(source(1:2), facts, output, count, status, message)
        call require(count == 1, 'same-fact second range was split')
        call require(trim(output(1)%span%fact%target_name) == 'OTHER', &
            'second source fact differs')

        facts = input_facts
        facts%facts(5)%range_first(1) = 65_int64
        facts%facts(5)%range_last(1) = 65_int64
        source = achar(65)//achar(66)
        call fortfront_lexical_scan(source(1:2), facts, output, count, status, message)
        call require(count == 2, 'fact transition was not emitted')
        call require(output(1)%status == fortfront_lexical_span_ambiguous .and. &
            output(2)%status == fortfront_lexical_span_match, &
            'ambiguous status differs')
    end subroutine test_explicit_statuses

    subroutine test_malformed_utf8(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts

        character(len=4) :: source
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: output(8)
        integer :: count, status

        source = char(65)//char(192)//char(128)//char(70)
        call fortfront_lexical_scan(source, facts, output, count, status, message)
        call require(status == fortfront_lexical_scan_complete, &
            'malformed input did not complete with explicit spans')
        call require(count == 3, 'malformed bytes were silently skipped')
        call require(output(2)%status == fortfront_lexical_span_invalid_utf8, &
            'malformed status differs')
        call require(output(2)%span%start_byte == 1_int64 .and. &
            output(2)%span%end_byte == 3_int64 .and. &
            output(2)%span%scalar_count == 0, 'malformed span metadata differs')
        call require(output(3)%status == fortfront_lexical_span_match .and. &
            trim(output(3)%span%fact%target_name) == 'AMBIGUOUS', &
            'post-malformed source was not scanned')
    end subroutine test_malformed_utf8

    subroutine test_capacity_and_clearing(facts)
        type(fortfront_lexical_facts_t), intent(in) :: facts

        character(len=5) :: source
        character(len=256) :: message
        type(fortfront_lexical_scanned_span_t) :: output(2)
        type(fortfront_lexical_facts_t) :: no_facts
        integer :: count, status

        output = fortfront_lexical_scanned_span_t()
        source = char(65)//char(67)//char(206)//char(177)//char(70)
        call fortfront_lexical_scan(source, facts, output, count, status, message)
        call require(status == fortfront_lexical_scan_capacity, &
            'capacity exhaustion was not reported')
        call require(count == 2, 'capacity failure did not retain the valid prefix')
        call require(output(1)%status == fortfront_lexical_span_match .and. &
            output(2)%status == fortfront_lexical_span_match, &
            'capacity prefix statuses differ')

        output(1)%status = 123
        output(2)%status = 456
        no_facts = fortfront_lexical_facts_t()
        call fortfront_lexical_scan('', no_facts, output, count, status, message)
        call require(status == fortfront_lexical_scan_empty, 'empty status differs')
        call require(count == 0 .and. output(1)%status == fortfront_lexical_span_invalid_bounds &
            .and. output(2)%status == fortfront_lexical_span_invalid_bounds, &
            'empty scan did not clear stale output')

        call fortfront_lexical_scan(source(1:2), no_facts, output, count, status, message)
        call require(status == fortfront_lexical_scan_complete .and. count == 1, &
            'empty fact scan differs')
        call require(output(1)%status == fortfront_lexical_span_no_match .and. &
            output(1)%span%start_byte == 0_int64 .and. &
            output(1)%span%end_byte == 2_int64, &
            'unmatched output status differs')
    end subroutine test_capacity_and_clearing

    subroutine make_facts(output)
        type(fortfront_lexical_facts_t), intent(out) :: output

        call fortfront_lexical_reset(output)
        output%count = 5
        call set_fact(output%facts(1), 'upper', 'constructed-class', 'UPPER', &
            'U+0041-U+0042', 65_int64, 66_int64)
        call set_fact(output%facts(2), 'other', 'constructed-class', 'OTHER', &
            'U+0043-U+0044', 67_int64, 68_int64)
        call set_fact(output%facts(3), 'greek', 'constructed-class', 'GREEK', &
            'U+03B1', 945_int64, 945_int64)
        call set_fact(output%facts(4), 'processor', 'constructed-class', &
            'PROCESSOR', 'processor-defined', 0_int64, 0_int64)
        output%facts(4)%range_count = 0
        call set_fact(output%facts(5), 'ambiguous', 'constructed-class', &
            'AMBIGUOUS', 'U+0046-U+0046', 70_int64, 70_int64)
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

end program test_fortfront_lexical_scan
