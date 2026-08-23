program test_fortfront_lexical_grammar_stream
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_provenance_t, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_token, fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analyze
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_lexical_grammar_session, only: &
        fortfront_lexical_grammar_session_accepted, fortfront_lexical_grammar_session_ambiguous, &
        fortfront_lexical_grammar_session_capacity, fortfront_lexical_grammar_session_consume, &
        fortfront_lexical_grammar_session_initialize, fortfront_lexical_grammar_session_initialized, &
        fortfront_lexical_grammar_session_malformed, &
        fortfront_lexical_grammar_session_no_match, fortfront_lexical_grammar_session_rejected, &
        fortfront_lexical_grammar_session_t, fortfront_lexical_grammar_session_token_capacity, &
        fortfront_lexical_grammar_session_unsupported
    use fortfront_lexical_tokens, only: fortfront_lexical_token_match, &
        fortfront_lexical_token_no_match, fortfront_lexical_token_t, fortfront_lexical_token_unsupported
    implicit none

    call test_success_and_statuses()
    call test_rollback_and_clear()
    call test_invalid_sessions()
    print '(a)', 'fortfront lexical grammar stream behavioral checks: ok'

contains

    subroutine test_success_and_statuses()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(1)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: input(2), consumed(2)
        type(fortfront_grammar_frontier_result_t) :: frontier(1)
        integer :: fact_count, status, token_count, frontier_count
        character(len=256) :: message

        call make_table(table, 'first', 'second')
        call analyze(table, facts, fact_count)
        call make_tokens(input, 'first', 'second')
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        call require(status == fortfront_lexical_grammar_session_initialized, 'initialization failed')
        consumed = fortfront_lexical_token_t()
        frontier = fortfront_grammar_frontier_result_t()
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_accepted .and. token_count == 2 .and. &
            frontier_count == 1, 'ordered whole-stream success failed')
        call require(consumed(1)%symbol == 'first' .and. consumed(2)%symbol == 'second' .and. &
            consumed(1)%start_byte == 10_int64 .and. consumed(2)%end_byte == 12_int64 .and. &
            trim(frontier(1)%identity) == 'SEQUENCE', 'source or frontier preservation failed')

        call make_table(table, 'first', 'other')
        call analyze(table, facts, fact_count)
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_rejected, 'rejection was dispatched')
    end subroutine test_success_and_statuses

    subroutine test_rollback_and_clear()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(2)
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: input(2), consumed(2), small_tokens(1)
        type(fortfront_grammar_frontier_result_t) :: frontier(2), small_frontier(1)
        integer :: fact_count, status, token_count, frontier_count
        character(len=256) :: message

        call make_table(table, 'first', 'second')
        call analyze(table, facts, fact_count)
        call make_tokens(input, 'first', 'second')
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        consumed%symbol = 'stale'
        frontier%identity = 'stale'
        call fortfront_lexical_grammar_session_consume(session, small_tokens, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_token_capacity .and. token_count == 0 &
            .and. frontier_count == 0 .and. len_trim(frontier(1)%identity) == 0, &
            'token capacity did not roll back and clear')
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_accepted .and. token_count == 2, &
            'token-capacity retry did not preserve session')

        call add_duplicate(table)
        call analyze(table, facts, fact_count)
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, small_frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_capacity .and. token_count == 0 .and. &
            frontier_count == 0, 'frontier capacity did not roll back')
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_ambiguous .and. token_count == 2, &
            'frontier-capacity retry did not preserve session')

        input(2)%status = fortfront_lexical_token_no_match
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_no_match .and. token_count == 0 .and. &
            frontier_count == 0, 'lexical no-match was not transactional')
        input(2)%status = fortfront_lexical_token_unsupported
        call fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            input, 2, status, message)
        call fortfront_lexical_grammar_session_consume(session, consumed, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_unsupported, 'unsupported changed')
    end subroutine test_rollback_and_clear

    subroutine test_invalid_sessions()
        type(fortfront_lexical_grammar_session_t) :: session
        type(fortfront_lexical_token_t) :: tokens(1)
        type(fortfront_grammar_frontier_result_t) :: frontier(1)
        integer :: status, token_count, frontier_count
        character(len=256) :: message

        tokens(1) = fortfront_lexical_token_t()
        frontier(1)%identity = 'stale'
        call fortfront_lexical_grammar_session_consume(session, tokens, token_count, frontier, &
            frontier_count, status, message)
        call require(status == fortfront_lexical_grammar_session_malformed .and. token_count == 0 .and. &
            frontier_count == 0 .and. len_trim(frontier(1)%identity) == 0, &
            'uninitialized session was not cleared')
    end subroutine test_invalid_sessions

    subroutine make_table(table, first, second)
        type(fortfront_grammar_table_t), intent(out) :: table
        character(len=*), intent(in) :: first, second
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        table = fortfront_grammar_table_t()
        rule = fortfront_grammar_rule_t(identity='SEQUENCE', lhs='S', rhs_count=2, &
            provenance=valid_provenance())
        allocate(rule%rhs(2))
        rule%rhs(1)%name = first
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        rule%rhs(2)%name = second
        rule%rhs(2)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'table construction failed')
    end subroutine make_table

    subroutine add_duplicate(table)
        type(fortfront_grammar_table_t), intent(inout) :: table
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        rule = fortfront_grammar_rule_t(identity='DUPLICATE', lhs='S', rhs_count=2, &
            provenance=valid_provenance())
        allocate(rule%rhs(2))
        rule%rhs(1)%name = 'first'
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        rule%rhs(2)%name = 'second'
        rule%rhs(2)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'duplicate construction failed')
    end subroutine add_duplicate

    subroutine make_tokens(tokens, first, second)
        type(fortfront_lexical_token_t), intent(out) :: tokens(:)
        character(len=*), intent(in) :: first, second
        integer :: i

        tokens = fortfront_lexical_token_t()
        do i = 1, size(tokens)
            if (i == 1) then
                tokens(i)%symbol = first
            else
                tokens(i)%symbol = second
            end if
            tokens(i)%start_byte = int(9 + i, int64)
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
    end subroutine make_tokens

    function valid_provenance() result(value)
        type(fortfront_grammar_provenance_t) :: value

        value = fortfront_grammar_provenance_t(document='stream-test', clause='1', rule='S', &
            page=1_int64, source_hash='stream-test-hash', start_byte=1_int64, end_byte=2_int64)
    end function valid_provenance

    subroutine analyze(table, facts, fact_count)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: facts(:)
        integer, intent(out) :: fact_count
        integer :: status
        character(len=256) :: message

        call fortfront_grammar_analyze(table, facts, fact_count, status, message)
        call require(status == 0 .or. status == 5 .or. status == 6, 'analysis failed')
    end subroutine analyze

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine require

end program test_fortfront_lexical_grammar_stream
