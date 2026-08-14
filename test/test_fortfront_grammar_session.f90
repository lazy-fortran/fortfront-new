program test_fortfront_grammar_session
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_provenance_t, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_token, fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analyze
    use fortfront_grammar_frontier, only: fortfront_grammar_advance_frontier, &
        fortfront_grammar_frontier_result_t
    use fortfront_grammar_session, only: fortfront_grammar_session_accepted, &
        fortfront_grammar_session_ambiguous, fortfront_grammar_session_capacity, &
        fortfront_grammar_session_finalize, fortfront_grammar_session_finalization, &
        fortfront_grammar_session_initialize, fortfront_grammar_session_malformed, &
        fortfront_grammar_session_push, fortfront_grammar_session_rejected, &
        fortfront_grammar_session_t, fortfront_grammar_session_unresolved
    implicit none

    call test_incremental_matches_batch_trace()
    call test_ambiguity_and_finalization()
    call test_unresolved_and_malformed()
    call test_capacity_and_output_clearing()
    print '(a)', 'fortfront grammar session behavioral checks: ok'

contains

    subroutine test_incremental_matches_batch_trace()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_session_t) :: session
        type(fortfront_grammar_frontier_result_t) :: output(8), batch(8)
        character(len=128) :: input(2)
        integer :: fact_count, status, output_count, init_status, batch_count, batch_status
        character(len=256) :: message, batch_message

        call make_sequence(table)
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        call require(init_status == 7, 'session did not initialize')

        input = ''
        input(1) = 'a'
        input(2) = 'b'
        call fortfront_grammar_session_push(session, input(1), output, output_count, status, &
            message)
        call require(status == fortfront_grammar_session_rejected .and. output_count == 0, &
            'independent prefix trace rejected outcome differed')
        call fortfront_grammar_session_push(session, input(2), output, output_count, status, &
            message)
        call require(status == fortfront_grammar_session_accepted .and. output_count == 1, &
            'independent prefix trace accepted outcome differed')
        call fortfront_grammar_session_finalize(session, output, output_count, status, message)
        call require(status == fortfront_grammar_session_accepted .and. output_count == 1, &
            'finalization did not preserve accepted outcome')

        call fortfront_grammar_advance_frontier(table, facts, fact_count, 'S', input, 2, batch, &
            batch_count, batch_status, batch_message)
        call require(status == batch_status .and. output_count == batch_count .and. &
            trim(output(1)%identity) == trim(batch(1)%identity), &
            'final session outcome differed from bounded batch path')
    end subroutine test_incremental_matches_batch_trace

    subroutine test_ambiguity_and_finalization()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_session_t) :: session
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: fact_count, status, output_count, init_status
        character(len=256) :: message

        call reset_table(table)
        call add_token_rule(table, 'CHOICE-A', 'S', 'a')
        call add_token_rule(table, 'CHOICE-B', 'S', 'a')
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        call fortfront_grammar_session_push(session, 'a', output, output_count, status, message)
        call require(status == fortfront_grammar_session_ambiguous .and. output_count == 2, &
            'independent ambiguity trace was not preserved')
        call require(trim(output(1)%identity) == 'CHOICE-A' .and. &
            trim(output(2)%identity) == 'CHOICE-B', 'session changed rule order')
        call fortfront_grammar_session_finalize(session, output, output_count, status, message)
        call require(status == fortfront_grammar_session_ambiguous .and. output_count == 2, &
            'finalization changed ambiguity')
        call fortfront_grammar_session_push(session, 'a', output, output_count, status, message)
        call require(status == fortfront_grammar_session_finalization .and. output_count == 0, &
            'push after finalization was not explicit')
    end subroutine test_ambiguity_and_finalization

    subroutine test_unresolved_and_malformed()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_session_t) :: session
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: fact_count, status, output_count, init_status
        character(len=256) :: message

        call reset_table(table)
        call add_reference_rule(table, 'UNRESOLVED', 'S', 'Missing')
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        call fortfront_grammar_session_push(session, 'x', output, output_count, status, message)
        call require(status == fortfront_grammar_session_unresolved .and. output_count == 0, &
            'independent unresolved trace was accepted or rejected')
        call fortfront_grammar_session_finalize(session, output, output_count, status, message)
        call require(status == fortfront_grammar_session_unresolved, &
            'finalization lost unresolved outcome')

        call make_sequence(table)
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        output(1)%identity = 'stale'
        call fortfront_grammar_session_push(session, 'bad token', output, output_count, status, &
            message)
        call require(status == fortfront_grammar_session_malformed .and. output_count == 0 .and. &
            trim(output(1)%identity) == '', 'malformed token did not clear output')

        call reset_table(table)
        call add_token_rule(table, 'TRANSACTIONAL', 'S', 'good')
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        call fortfront_grammar_session_push(session, 'bad token', output, output_count, status, &
            message)
        call require(status == fortfront_grammar_session_malformed .and. output_count == 0, &
            'malformed token changed the transactional input')
        call fortfront_grammar_session_push(session, 'good', output, output_count, status, message)
        call require(status == fortfront_grammar_session_accepted .and. output_count == 1 .and. &
            trim(output(1)%identity) == 'TRANSACTIONAL', &
            'malformed token was committed before valid retry')
    end subroutine test_unresolved_and_malformed

    subroutine test_capacity_and_output_clearing()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_session_t) :: session
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: fact_count, status, output_count, init_status, i
        character(len=256) :: message

        call reset_table(table)
        call add_token_rule(table, 'CAPACITY', 'S', 'x')
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        do i = 1, 16
            call fortfront_grammar_session_push(session, 'x', output, output_count, status, &
                message)
        end do
        call fortfront_grammar_session_push(session, 'x', output, output_count, status, message)
        call require(status == fortfront_grammar_session_capacity .and. output_count == 0 .and. &
            trim(output(1)%identity) == '', 'token capacity or output clearing failed')

        call reset_table(table)
        call add_token_rule(table, 'OUTPUT-A', 'S', 'a')
        call add_token_rule(table, 'OUTPUT-B', 'S', 'a')
        call analyze(table, facts, fact_count)
        call fortfront_grammar_session_initialize(session, table, facts, fact_count, 'S', &
            init_status, message)
        call fortfront_grammar_session_push(session, 'a', output(1:1), output_count, status, &
            message)
        call require(status == fortfront_grammar_session_capacity .and. output_count == 0, &
            'small output capacity was not reported')
        call fortfront_grammar_session_push(session, 'a', output, output_count, status, message)
        call require(status == fortfront_grammar_session_ambiguous .and. output_count == 2 .and. &
            trim(output(1)%identity) == 'OUTPUT-A' .and. &
            trim(output(2)%identity) == 'OUTPUT-B', &
            'capacity failure committed the candidate token')
    end subroutine test_capacity_and_output_clearing

    subroutine make_sequence(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        call reset_table(table)
        call add_token_rule(table, 'SEQ-A', 'A', 'a')
        call add_two_symbol_rule(table, 'SEQ-S', 'S', 'A', 'b')
    end subroutine make_sequence

    subroutine add_token_rule(table, identity, lhs, token)
        type(fortfront_grammar_table_t), intent(inout) :: table
        character(len=*), intent(in) :: identity, lhs, token
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        rule = fortfront_grammar_rule_t(identity=identity, lhs=lhs, rhs_count=1, &
            provenance=valid_provenance(identity))
        rule%rhs(1)%name = token
        rule%rhs(1)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'token rule could not be added')
    end subroutine add_token_rule

    subroutine add_two_symbol_rule(table, identity, lhs, reference, token)
        type(fortfront_grammar_table_t), intent(inout) :: table
        character(len=*), intent(in) :: identity, lhs, reference, token
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        rule = fortfront_grammar_rule_t(identity=identity, lhs=lhs, rhs_count=2, &
            provenance=valid_provenance(identity))
        rule%rhs(1)%name = reference
        rule%rhs(1)%kind = fortfront_grammar_symbol_reference
        rule%rhs(2)%name = token
        rule%rhs(2)%kind = fortfront_grammar_symbol_token
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'sequence rule could not be added')
    end subroutine add_two_symbol_rule

    subroutine add_reference_rule(table, identity, lhs, reference)
        type(fortfront_grammar_table_t), intent(inout) :: table
        character(len=*), intent(in) :: identity, lhs, reference
        type(fortfront_grammar_rule_t) :: rule
        integer :: status
        character(len=256) :: message

        rule = fortfront_grammar_rule_t(identity=identity, lhs=lhs, rhs_count=1, &
            provenance=valid_provenance(identity))
        rule%rhs(1)%name = reference
        rule%rhs(1)%kind = fortfront_grammar_symbol_reference
        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'reference rule could not be added')
    end subroutine add_reference_rule

    function valid_provenance(rule_name) result(provenance)
        character(len=*), intent(in) :: rule_name
        type(fortfront_grammar_provenance_t) :: provenance

        provenance = fortfront_grammar_provenance_t(document='session-witness', clause='1', &
            rule=rule_name, page=1_int64, source_hash='session-witness-hash', &
            start_byte=1_int64, end_byte=2_int64)
    end function valid_provenance

    subroutine analyze(table, facts, fact_count)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: facts(:)
        integer, intent(out) :: fact_count
        integer :: status
        character(len=256) :: message

        call fortfront_grammar_analyze(table, facts, fact_count, status, message)
        call require(status == 0 .or. status == 5 .or. status == 6, &
            'session witness analysis failed')
    end subroutine analyze

    subroutine reset_table(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        table = fortfront_grammar_table_t()
    end subroutine reset_table

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine require

end program test_fortfront_grammar_session
