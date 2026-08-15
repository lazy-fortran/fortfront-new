program test_fortfront_grammar_frontier
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_rule_t, &
        fortfront_grammar_symbol_reference, fortfront_grammar_symbol_token, &
        fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analyze
    use fortfront_grammar_frontier, only: fortfront_grammar_advance_frontier, &
        fortfront_grammar_frontier_accepted, fortfront_grammar_frontier_ambiguous, &
        fortfront_grammar_frontier_capacity, &
        fortfront_grammar_frontier_malformed, &
        fortfront_grammar_frontier_rejected, fortfront_grammar_frontier_result_t, &
        fortfront_grammar_frontier_unresolved
    implicit none

    call test_sequence_and_rejection()
    call test_choice_ambiguity_and_determinism()
    call test_nullable_prefix()
    call test_recursion()
    call test_unresolved_reference()
    call test_malformed_table_and_input_clear_outputs()
    call test_capacity_and_oracle()
    print '(a)', 'fortfront grammar frontier behavioral checks: ok'

contains

    subroutine test_sequence_and_rejection()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=128) :: input(2)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_one(table%rules(1), 'SEQ-A', 'A', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_two(table%rules(2), 'SEQ-S', 'S', 'A', fortfront_grammar_symbol_reference, &
            'b', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call analyze(table, facts, fact_count)
        input = ''
        input(1) = 'a'
        input(2) = 'b'
        call advance(table, facts, fact_count, 'S', input, 2, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_accepted .and. output_count == 1, &
            'simple sequence was not accepted uniquely')
        call require(trim(output(1)%identity) == 'SEQ-S' .and. trim(output(1)%lhs) == 'S', &
            'sequence result did not preserve rule identity')
        call require(trim(output(1)%provenance%rule) == 'SEQ-S' .and. &
            output(1)%next_position == 3 .and. output(1)%consumed == 2, &
            'sequence result did not preserve provenance or advancement')

        input(2) = 'c'
        call advance(table, facts, fact_count, 'S', input, 2, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_rejected .and. output_count == 0, &
            'non-matching sequence was not rejected')
    end subroutine test_sequence_and_rejection

    subroutine test_choice_ambiguity_and_determinism()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8), repeat_output(8)
        character(len=128) :: input(1)
        integer :: fact_count, output_count, repeat_count, status, repeat_status
        character(len=256) :: message, repeat_message

        call reset_table(table)
        call make_one(table%rules(1), 'CHOICE-A', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'CHOICE-B', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call analyze(table, facts, fact_count)
        input = 'a'
        call advance(table, facts, fact_count, 'S', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_ambiguous .and. output_count == 2, &
            'choice ambiguity was silently resolved')
        call require(trim(output(1)%identity) == 'CHOICE-A' .and. &
            trim(output(2)%identity) == 'CHOICE-B', 'choice result order was not deterministic')
        call require(trim(output(1)%provenance%document) == 'frontier-witness' .and. &
            trim(output(2)%provenance%rule) == 'CHOICE-B', &
            'choice results did not preserve source provenance')

        call advance(table, facts, fact_count, 'S', input, 1, repeat_output, repeat_count, &
            repeat_status, repeat_message)
        call require(repeat_status == status .and. repeat_count == output_count .and. &
            trim(repeat_message) == trim(message), 'frontier result status was not deterministic')
        call require(trim(repeat_output(1)%identity) == trim(output(1)%identity) .and. &
            trim(repeat_output(2)%identity) == trim(output(2)%identity), &
            'frontier candidate order changed between equal calls')
    end subroutine test_choice_ambiguity_and_determinism

    subroutine test_nullable_prefix()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=128) :: input(1)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_empty(table%rules(1), 'NULL-EPSILON', 'O')
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'NULL-X', 'O', 'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call make_two(table%rules(3), 'NULL-S', 'S', 'O', fortfront_grammar_symbol_reference, &
            'tail', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(3))
        call analyze(table, facts, fact_count)
        input = 'tail'
        call advance(table, facts, fact_count, 'S', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_accepted .and. output_count == 1, &
            'nullable prefix did not advance to the following symbol')
        call require(trim(output(1)%identity) == 'NULL-S', &
            'nullable-prefix result did not preserve the root rule')
    end subroutine test_nullable_prefix

    subroutine test_recursion()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=128) :: input(1)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_empty(table%rules(1), 'REC-EPSILON', 'R')
        call add_rule(table, table%rules(1))
        call make_two(table%rules(2), 'REC-RX', 'R', 'R', fortfront_grammar_symbol_reference, &
            'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call analyze(table, facts, fact_count)
        input = 'x'
        call advance(table, facts, fact_count, 'R', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_accepted .and. output_count == 1, &
            'recursive witness did not reach a fixed point')
        call require(trim(output(1)%identity) == 'REC-RX', &
            'recursive result did not preserve the advancing rule')
    end subroutine test_recursion

    subroutine test_unresolved_reference()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=128) :: input(1)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_one(table%rules(1), 'UNRESOLVED', 'S', 'Missing', &
            fortfront_grammar_symbol_reference)
        call add_rule(table, table%rules(1))
        call analyze(table, facts, fact_count)
        input = 'x'
        call advance(table, facts, fact_count, 'S', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_unresolved .and. output_count == 0, &
            'unresolved reference was treated as rejected or accepted')
    end subroutine test_unresolved_reference

    subroutine test_malformed_table_and_input_clear_outputs()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=128) :: input(1)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_one(table%rules(1), 'BAD-KIND', 'S', 'x', fortfront_grammar_symbol_token)
        table%rules(1)%rhs(1)%kind = 99
        table%count = 1
        facts = fortfront_grammar_analysis_result_t()
        input = 'x'
        output = fortfront_grammar_frontier_result_t()
        output(1)%identity = 'stale'
        call advance(table, facts, 0, 'S', input, 1, output, output_count, status, message)
        call require(status == fortfront_grammar_frontier_malformed .and. output_count == 0, &
            'malformed table was not reported')
        call require(trim(output(1)%identity) == '', 'malformed table did not clear output')

        call reset_table(table)
        call make_one(table%rules(1), 'BAD-INPUT', 'S', 'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call analyze(table, facts, fact_count)
        input = ''
        output(1)%identity = 'stale'
        call advance(table, facts, fact_count, 'S', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_malformed .and. output_count == 0, &
            'malformed abstract input was not reported')
        call require(trim(output(1)%identity) == '', 'malformed input did not clear output')
    end subroutine test_malformed_table_and_input_clear_outputs

    subroutine test_capacity_and_oracle()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(8)
        type(fortfront_grammar_frontier_result_t) :: output(1)
        character(len=128) :: input(2)
        integer :: fact_count, output_count, status
        character(len=256) :: message

        call reset_table(table)
        call make_one(table%rules(1), 'ORACLE-A', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'ORACLE-B', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call analyze(table, facts, fact_count)

        input = ''
        input(1) = 'a'
        call advance(table, facts, fact_count, 'S', input, 1, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_capacity .and. output_count == 0, &
            'frontier capacity was not reported')
        call require(trim(output(1)%identity) == '', 'capacity failure did not clear output')

        ! Independent finite oracle for the two S -> a alternatives.
        input(2) = 'b'
        call advance(table, facts, fact_count, 'S', input, 2, output, output_count, status, &
            message)
        call require(status == fortfront_grammar_frontier_rejected .and. output_count == 0, &
            'rejection did not clear the capacity-sized output')
        call require(oracle_accepts(input, 1) .and. .not. oracle_accepts(input, 2), &
            'independent frontier oracle was inconsistent')
    end subroutine test_capacity_and_oracle

    logical function oracle_accepts(input, input_count)
        character(len=*), intent(in) :: input(:)
        integer, intent(in) :: input_count

        oracle_accepts = .false.
        if (input_count == 1) then
            oracle_accepts = trim(input(1)) == 'a'
        end if
    end function oracle_accepts

    subroutine advance(table, facts, fact_count, lhs, input, input_count, output, output_count, &
            status, message)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count, input_count
        character(len=*), intent(in) :: lhs, input(:)
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        call fortfront_grammar_advance_frontier(table, facts, fact_count, lhs, input, &
            input_count, output, output_count, status, message)
    end subroutine advance

    subroutine analyze(table, facts, fact_count)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: facts(:)
        integer, intent(out) :: fact_count

        integer :: status
        character(len=256) :: message

        call fortfront_grammar_analyze(table, facts, fact_count, status, message)
        call require(status == 0 .or. status == 5 .or. status == 6, &
            'frontier witness analysis did not produce usable facts: '//trim(message))
    end subroutine analyze

    subroutine add_rule(table, rule)
        type(fortfront_grammar_table_t), intent(inout) :: table
        type(fortfront_grammar_rule_t), intent(in) :: rule

        integer :: status
        character(len=256) :: message

        call fortfront_grammar_add(table, rule, status, message)
        call require(status == 0, 'frontier witness rule was rejected: '//trim(message))
    end subroutine add_rule

    subroutine reset_table(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        table = fortfront_grammar_table_t()
        allocate(table%rules(8))
    end subroutine reset_table

    subroutine make_empty(rule, identity, lhs)
        type(fortfront_grammar_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, lhs

        call initialize_rule(rule, identity, lhs)
    end subroutine make_empty

    subroutine make_one(rule, identity, lhs, name, kind)
        type(fortfront_grammar_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, lhs, name
        integer, intent(in) :: kind

        call initialize_rule(rule, identity, lhs)
        rule%rhs_count = 1
        rule%rhs(1)%name = name
        rule%rhs(1)%kind = kind
    end subroutine make_one

    subroutine make_two(rule, identity, lhs, name_one, kind_one, name_two, kind_two)
        type(fortfront_grammar_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, lhs, name_one, name_two
        integer, intent(in) :: kind_one, kind_two

        call make_one(rule, identity, lhs, name_one, kind_one)
        rule%rhs_count = 2
        rule%rhs(2)%name = name_two
        rule%rhs(2)%kind = kind_two
    end subroutine make_two

    subroutine initialize_rule(rule, identity, lhs)
        type(fortfront_grammar_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, lhs

        rule = fortfront_grammar_rule_t()
        allocate(rule%rhs(2))
        rule%identity = identity
        rule%lhs = lhs
        rule%provenance%document = 'frontier-witness'
        rule%provenance%clause = 'frontier-witness-clause'
        rule%provenance%rule = identity
        rule%provenance%page = 1_int64
        rule%provenance%source_hash = 'frontier-witness-hash'
        rule%provenance%start_byte = 0_int64
        rule%provenance%end_byte = 1_int64
    end subroutine initialize_rule

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_grammar_frontier
