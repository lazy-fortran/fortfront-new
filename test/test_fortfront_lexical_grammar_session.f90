program test_fortfront_lexical_grammar_session
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_provenance_t, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_token, fortfront_grammar_table_t
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
        fortfront_lexical_grammar_session_malformed, &
        fortfront_lexical_grammar_session_no_match, &
        fortfront_lexical_grammar_session_rejected, &
        fortfront_lexical_grammar_session_t, &
        fortfront_lexical_grammar_session_unsupported
    use fortfront_lexical_tokens, only: fortfront_lexical_token_match, &
        fortfront_lexical_token_no_match, fortfront_lexical_token_t, &
        fortfront_lexical_token_unsupported
    implicit none

    call test_ordered_match_and_projection()
    call test_nonmatch_is_transactional()
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

    subroutine test_nonmatch_is_transactional()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(3), token
        type(fortfront_grammar_frontier_result_t) :: output(2)
        integer :: fact_count, status, output_count
        character(len=256) :: message

        call make_table(table)
        call analyze(table, facts, fact_count)
        call make_tokens(tokens(1:2))
        tokens(2)%status = fortfront_lexical_token_no_match
        tokens(2)%symbol = 'no-match'
        tokens(3) = tokens(2)
        tokens(3)%status = fortfront_lexical_token_unsupported
        tokens(3)%symbol = 'unsupported'
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, 3, status, message)
        call require(status == fortfront_lexical_grammar_session_initialized, &
            'transactional fixture did not initialize')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_rejected .and. &
            token%symbol == 'first', 'initial match failed')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_no_match .and. &
            token%symbol == 'no-match' .and. output_count == 0, 'no-match was forwarded')
        call fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        call require(status == fortfront_lexical_grammar_session_no_match .and. &
            token%symbol == 'no-match', 'no-match consumed the cursor')
    end subroutine test_nonmatch_is_transactional

    subroutine test_malformed_input()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(1)
        integer :: fact_count, status
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
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            tokens, -1, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed, &
            'malformed cursor count was accepted')
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
        call require(status == 0, 'grammar analysis failed')
    end subroutine analyze

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine require

end program test_fortfront_lexical_grammar_session
