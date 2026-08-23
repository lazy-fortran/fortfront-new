program test_fortfront_lexical_grammar_session
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_provenance_t, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_token, fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analyze
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_lexical_grammar_session, only: &
        fortfront_lexical_grammar_session_accepted, &
        fortfront_lexical_grammar_session_advance, &
        fortfront_lexical_grammar_session_end_of_stream, &
        fortfront_lexical_grammar_session_finalize, &
        fortfront_lexical_grammar_session_initialize, &
        fortfront_lexical_grammar_session_initialized, &
        fortfront_lexical_grammar_session_ambiguous, &
        fortfront_lexical_grammar_session_capacity, &
        fortfront_lexical_grammar_session_malformed, &
        fortfront_lexical_grammar_session_no_match, &
        fortfront_lexical_grammar_session_rejected, &
        fortfront_lexical_grammar_session_t, &
        fortfront_lexical_grammar_session_token_ambiguous, &
        fortfront_lexical_grammar_session_token_malformed, &
        fortfront_lexical_grammar_session_unresolved, &
        fortfront_lexical_grammar_session_unsupported
    use fortfront_lexical_tokens, only: fortfront_lexical_token_match, &
        fortfront_lexical_token_ambiguous, fortfront_lexical_token_malformed, &
        fortfront_lexical_token_no_match, &
        fortfront_lexical_token_t, fortfront_lexical_token_unsupported
    implicit none

    call test_ordered_match_and_projection()
    call test_lexical_failures_are_transactional()
    call test_grammar_outcomes()
    call test_malformed_input()
    print '(a)', 'fortfront lexical grammar session behavioral checks: ok'

contains

    subroutine test_ordered_match_and_projection()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(2), token
        type(fortfront_grammar_frontier_result_t) :: output(2)
        integer :: fact_count, status, output_count
        character(len=256) :: message

        call make_table(table)
        call analyze(table, facts, fact_count)
        call make_tokens(tokens)
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 2, status, message)
        call require(status == fortfront_lexical_grammar_session_initialized, &
            'session initialization failed')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_rejected .and. &
            token%symbol == 'first' .and. token%start_byte == 10_int64 .and. output_count == 0, &
            'first matched token was not projected')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_accepted .and. &
            token%symbol == 'second' .and. output_count == 1, 'second token was not ordered')
        call fortfront_lexical_grammar_session_finalize(session, output, output_count, status, &
            message)
        call require(status == fortfront_lexical_grammar_session_accepted .and. output_count == 1, &
            'final grammar outcome was not preserved')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_end_of_stream, &
            'end of stream was not explicit')
    end subroutine test_ordered_match_and_projection

    subroutine test_lexical_failures_are_transactional()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(2), token
        type(fortfront_grammar_frontier_result_t) :: output(2)
        integer :: fact_count, status, output_count
        character(len=256) :: message

        call make_single_table(table, 'SINGLE', 'first')
        call analyze(table, facts, fact_count)
        call make_tokens(tokens)
        tokens(2)%status = fortfront_lexical_token_no_match
        tokens(2)%symbol = 'no-match'
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 2, status, message)
        call require(status == fortfront_lexical_grammar_session_initialized, &
            'no-match session did not initialize')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_accepted, &
            'accepted prefix was not observed')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_no_match .and. &
            output_count == 0, 'no-match was forwarded')
        call fortfront_lexical_grammar_session_finalize(session, output, output_count, status, &
            message)
        call require(status == fortfront_lexical_grammar_session_no_match, &
            'no-match prefix finalized as grammar acceptance')

        call exercise_failure_status(table, facts, fact_count, fortfront_lexical_token_unsupported, &
            'unsupported', &
            fortfront_lexical_grammar_session_unsupported)
        call exercise_failure_status(table, facts, fact_count, fortfront_lexical_token_ambiguous, &
            'ambiguous', &
            fortfront_lexical_grammar_session_token_ambiguous)

        call make_tokens(tokens(1:1))
        tokens(1)%status = fortfront_lexical_token_malformed
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed token became an initialized session')
    end subroutine test_lexical_failures_are_transactional

    subroutine exercise_failure_status(table, facts, fact_count, bad_status, symbol, &
            expected_status)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count, bad_status, expected_status
        character(len=*), intent(in) :: symbol
        type(fortfront_lexical_grammar_session_t) :: local_session
        type(fortfront_lexical_token_t) :: local_tokens(2), local_token
        type(fortfront_grammar_frontier_result_t) :: local_output(2)
        integer :: local_status, local_count
        character(len=256) :: local_message

        call make_tokens(local_tokens)
        local_tokens(2)%status = bad_status
        local_tokens(2)%symbol = symbol
        call fortfront_lexical_grammar_session_initialize(local_session, table, facts, fact_count, &
            'S', local_tokens, 2, local_status, local_message)
        call require(local_status == fortfront_lexical_grammar_session_initialized, &
            'lexical failure session did not initialize')
        call fortfront_lexical_grammar_session_advance(local_session, local_token, local_output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_lexical_grammar_session_accepted, &
            'failure prefix was not accepted')
        call fortfront_lexical_grammar_session_advance(local_session, local_token, local_output, &
            local_count, local_status, local_message)
        call require(local_status == expected_status .and. local_count == 0, &
            'lexical failure was forwarded')
        call fortfront_lexical_grammar_session_finalize(local_session, local_output, local_count, &
            local_status, local_message)
        call require(local_status == expected_status, 'lexical failure finalized as acceptance')
    end subroutine exercise_failure_status

    subroutine test_grammar_outcomes()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(4)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(1), token
        type(fortfront_grammar_frontier_result_t) :: output(2), small_output(1)
        integer :: fact_count, status, output_count
        character(len=256) :: message

        call make_ambiguous_table(table)
        call analyze(table, facts, fact_count)
        call make_tokens(tokens)
        tokens(1)%symbol = 'choice'
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_ambiguous .and. output_count == 2, &
            'ambiguous grammar outcome was not passed through')

        small_output = fortfront_grammar_frontier_result_t()
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call fortfront_lexical_grammar_session_advance(session, token, small_output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_capacity .and. output_count == 0, &
            'grammar capacity outcome was not passed through')

        call make_unresolved_table(table)
        call analyze(table, facts, fact_count)
        tokens(1)%symbol = 'anything'
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_unresolved, &
            'unresolved grammar outcome was not passed through')

        table = fortfront_grammar_table_t()
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, '', &
            tokens, 1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed grammar outcome was not passed through')
    end subroutine test_grammar_outcomes

    subroutine test_malformed_input()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(1), token
        type(fortfront_grammar_frontier_result_t) :: output(1)
        integer :: fact_count, status, output_count
        character(len=256) :: message

        call make_table(table)
        call analyze(table, facts, fact_count)
        call make_tokens(tokens)
        tokens(1)%start_byte = -1_int64
        tokens(1)%fact%source_term = 'token'
        tokens(1)%fact%class_name = 'class'
        tokens(1)%fact%target_name = 'target'
        tokens(1)%fact%source_rule = 'rule'
        tokens(1)%fact%source_page = '1'
        tokens(1)%fact%document = 'document'
        tokens(1)%fact%clause = 'clause'
        tokens(1)%fact%source_hash = repeat('a', 64)
        tokens(1)%fact%codepoint = 'scalar'
        tokens(1)%fact%range_count = 1
        tokens(1)%fact%range_first(1) = 1_int64
        tokens(1)%fact%range_last(1) = 1_int64
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed span was accepted')
        tokens(1)%start_byte = 10_int64
        tokens(1)%fact%source_hash = ''
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed provenance was accepted')
        output(1)%identity = 'stale'
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 2, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'token-count upper bound was accepted')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_malformed .and. &
            output_count == 0 .and. trim(output(1)%identity) == '', &
            'upper-bound failure did not clear state and output')
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, -1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed cursor count was accepted')
        output(1)%identity = 'stale'
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_malformed .and. &
            output_count == 0 .and. trim(output(1)%identity) == '', &
            'negative-count failure did not clear state and output')
    end subroutine test_malformed_input

    subroutine make_tokens(tokens)
        type(fortfront_lexical_token_t), intent(out) :: tokens(:)
        integer :: i

        tokens = fortfront_lexical_token_t()
        do i = 1, size(tokens)
            write (tokens(i)%symbol, '(a,i0)') 'token-', i
            tokens(i)%start_byte = int(i + 9, int64)
            tokens(i)%end_byte = tokens(i)%start_byte + 1_int64
            tokens(i)%scalar_count = 1
            tokens(i)%status = fortfront_lexical_token_match
            tokens(i)%fact%source_term = 'token'
            tokens(i)%fact%class_name = 'class'
            tokens(i)%fact%target_name = 'target'
            tokens(i)%fact%source_rule = 'rule'
            tokens(i)%fact%source_page = '1'
            tokens(i)%fact%document = 'document'
            tokens(i)%fact%clause = 'clause'
            tokens(i)%fact%source_hash = repeat('a', 64)
            tokens(i)%fact%codepoint = 'scalar'
            tokens(i)%fact%range_count = 1
            tokens(i)%fact%range_first(1) = 1_int64
            tokens(i)%fact%range_last(1) = 1_int64
        end do
        tokens(1)%symbol = 'first'
        if (size(tokens) > 1) tokens(2)%symbol = 'second'
    end subroutine make_tokens

    subroutine make_single_table(table, identity, token)
        type(fortfront_grammar_table_t), intent(out) :: table
        character(len=*), intent(in) :: identity, token
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        table = fortfront_grammar_table_t()
        rule = fortfront_grammar_rule_t(identity=identity, lhs='S', rhs_count=1, &
            provenance=valid_provenance(identity))
        allocate (rule%rhs(1))
        rule%rhs(1)%name = token
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'single rule was not added')
    end subroutine make_single_table

    subroutine make_ambiguous_table(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        call make_single_table(table, 'CHOICE-A', 'choice')
        call add_single_rule(table, 'CHOICE-B', 'choice')
    end subroutine make_ambiguous_table

    subroutine add_single_rule(table, identity, token)
        type(fortfront_grammar_table_t), intent(inout) :: table
        character(len=*), intent(in) :: identity, token
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        rule = fortfront_grammar_rule_t(identity=identity, lhs='S', rhs_count=1, &
            provenance=valid_provenance(identity))
        allocate (rule%rhs(1))
        rule%rhs(1)%name = token
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'ambiguous rule was not added')
    end subroutine add_single_rule

    subroutine make_unresolved_table(table)
        type(fortfront_grammar_table_t), intent(out) :: table
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        table = fortfront_grammar_table_t()
        rule = fortfront_grammar_rule_t(identity='UNRESOLVED', lhs='S', rhs_count=1, &
            provenance=valid_provenance('UNRESOLVED'))
        allocate (rule%rhs(1))
        rule%rhs(1)%name = 'Missing'
        rule%rhs(1)%kind = fortfront_grammar_symbol_reference
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'unresolved rule was not added')
    end subroutine make_unresolved_table

    subroutine make_table(table)
        type(fortfront_grammar_table_t), intent(out) :: table
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        table = fortfront_grammar_table_t()
        rule = fortfront_grammar_rule_t(identity='SEQUENCE', lhs='S', rhs_count=2, &
            provenance=valid_provenance('SEQUENCE'))
        allocate (rule%rhs(2))
        rule%rhs(1)%name = 'first'
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        rule%rhs(2)%name = 'second'
        rule%rhs(2)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'sequence rule was not added')
    end subroutine make_table

    function valid_provenance(rule_name) result(value)
        character(len=*), intent(in) :: rule_name
        type(fortfront_grammar_provenance_t) :: value

        value = fortfront_grammar_provenance_t(document='session-oracle', clause='1', &
            rule=rule_name, page=1_int64, source_hash='session-oracle-hash', start_byte=1_int64, &
            end_byte=2_int64)
    end function valid_provenance

    subroutine analyze(table, facts, fact_count)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: facts(:)
        integer, intent(out) :: fact_count
        integer :: status
        character(len=256) :: message

        call fortfront_grammar_analyze(table, facts, fact_count, status, message)
        call require(status == 0 .or. status == 5 .or. status == 6, 'grammar analysis failed')
    end subroutine analyze

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine require

end program test_fortfront_lexical_grammar_session
