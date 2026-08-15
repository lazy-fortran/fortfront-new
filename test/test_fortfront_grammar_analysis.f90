program test_fortfront_grammar_analysis
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_token, fortfront_grammar_table_t, fortfront_grammar_valid
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_ambiguous, &
        fortfront_grammar_analysis_capacity, fortfront_grammar_analysis_duplicate_identity, &
        fortfront_grammar_analysis_empty, fortfront_grammar_analysis_malformed, &
        fortfront_grammar_analysis_nullable_no, fortfront_grammar_analysis_nullable_unknown, &
        fortfront_grammar_analysis_nullable_yes, fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analysis_unresolved, fortfront_grammar_analysis_valid, &
        fortfront_grammar_analyze
    implicit none

    call test_reference_and_sequence()
    call test_choice_and_order()
    call test_optional_and_repeat()
    call test_cycle()
    call test_unknown_reference()
    call test_ambiguous_first_symbols()
    call test_duplicate_identity()
    call test_capacity_and_malformed_controls()
    print '(a)', 'fortfront grammar analysis behavioral checks: ok'

contains

    subroutine test_reference_and_sequence()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'REF-A', 'A', 'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'REF-S', 'S', 'A', fortfront_grammar_symbol_reference)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'x'
        call analyze_and_require(table, output, output_count, status, message, 'S', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_no, &
            expected, 1, .false., .false.)
        call require(trim(output(1)%lhs) == 'A' .and. trim(output(2)%lhs) == 'S', &
            'analysis did not preserve first LHS order')

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'SEQ-A', 'A', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_two(table%rules(2), 'SEQ-S', 'S', 'A', fortfront_grammar_symbol_reference, &
            'b', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'a'
        call analyze_and_require(table, output, output_count, status, message, 'S', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_no, &
            expected, 1, .false., .false.)
    end subroutine test_reference_and_sequence

    subroutine test_choice_and_order()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'CHOICE-A', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'CHOICE-B', 'S', 'b', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'a'
        expected(2) = 'b'
        call analyze_and_require(table, output, output_count, status, message, 'S', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_no, &
            expected, 2, .false., .false.)
    end subroutine test_choice_and_order

    subroutine test_optional_and_repeat()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_empty(table%rules(1), 'OPTIONAL-EPSILON', 'O')
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'OPTIONAL-X', 'O', 'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'x'
        call analyze_and_require(table, output, output_count, status, message, 'O', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_yes, &
            expected, 1, .false., .false.)

        call fortfront_grammar_reset_local(table)
        call make_empty(table%rules(1), 'REPEAT-EPSILON', 'R')
        call add_rule(table, table%rules(1))
        call make_two(table%rules(2), 'REPEAT-RX', 'R', 'R', fortfront_grammar_symbol_reference, &
            'x', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'x'
        call analyze_and_require(table, output, output_count, status, message, 'R', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_yes, &
            expected, 1, .false., .false.)
    end subroutine test_optional_and_repeat

    subroutine test_cycle()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'CYCLE-A', 'A', 'B', fortfront_grammar_symbol_reference)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'CYCLE-B', 'B', 'A', fortfront_grammar_symbol_reference)
        call add_rule(table, table%rules(2))
        expected = ''
        call analyze_and_require(table, output, output_count, status, message, 'A', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_no, &
            expected, 0, .false., .false.)
        call analyze_and_require(table, output, output_count, status, message, 'B', &
            fortfront_grammar_analysis_valid, fortfront_grammar_analysis_nullable_no, &
            expected, 0, .false., .false.)
    end subroutine test_cycle

    subroutine test_unknown_reference()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'UNKNOWN', 'S', 'Missing', &
            fortfront_grammar_symbol_reference)
        call add_rule(table, table%rules(1))
        expected = ''
        call analyze_and_require(table, output, output_count, status, message, 'S', &
            fortfront_grammar_analysis_unresolved, fortfront_grammar_analysis_nullable_unknown, &
            expected, 0, .true., .false.)

        call make_two(table%rules(1), 'UNKNOWN-SEQUENCE', 'T', 'Missing', &
            fortfront_grammar_symbol_reference, 'x', fortfront_grammar_symbol_token)
        table%rules(1)%identity = 'UNKNOWN-SEQUENCE'
        table%rules(1)%provenance%rule = 'UNKNOWN-SEQUENCE'
        table%count = 0
        call add_rule(table, table%rules(1))
        expected = ''
        expected(1) = 'x'
        call analyze_and_require(table, output, output_count, status, message, 'T', &
            fortfront_grammar_analysis_unresolved, fortfront_grammar_analysis_nullable_no, &
            expected, 1, .true., .false.)
    end subroutine test_unknown_reference

    subroutine test_ambiguous_first_symbols()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        character(len=128) :: expected(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'AMBIGUOUS-A', 'S', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_two(table%rules(2), 'AMBIGUOUS-AB', 'S', 'a', fortfront_grammar_symbol_token, &
            'b', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        expected = ''
        expected(1) = 'a'
        call analyze_and_require(table, output, output_count, status, message, 'S', &
            fortfront_grammar_analysis_ambiguous, fortfront_grammar_analysis_nullable_no, &
            expected, 1, .false., .true.)
    end subroutine test_ambiguous_first_symbols

    subroutine test_duplicate_identity()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4)
        integer :: output_count, status
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'DUPLICATE', 'A', 'a', fortfront_grammar_symbol_token)
        table%count = 2
        table%rules(2) = table%rules(1)
        call fortfront_grammar_analyze(table, output, output_count, status, message)
        call require(status == fortfront_grammar_analysis_duplicate_identity .and. &
            output_count == 0, 'duplicate identities were not preserved as a failure')
    end subroutine test_duplicate_identity

    subroutine test_capacity_and_malformed_controls()
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: output(4), small_output(1)
        type(fortfront_grammar_rule_t) :: item
        integer :: output_count, status, i
        character(len=256) :: message

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'CAPACITY-A', 'A', 'a', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(1))
        call make_one(table%rules(2), 'CAPACITY-B', 'B', 'b', fortfront_grammar_symbol_token)
        call add_rule(table, table%rules(2))
        call fortfront_grammar_analyze(table, small_output, output_count, status, message)
        call require(status == fortfront_grammar_analysis_capacity .and. output_count == 0, &
            'analysis output capacity was not reported')

        call require(allocated(table%rules), 'dynamic table fixture lost its storage')
        call grow_table_fixture(table, 65)
        do i = 3, 65
            call make_one(item, 'CAPACITY-'//integer_text(i), 'A', 'a', &
                fortfront_grammar_symbol_token)
            call add_rule(table, item)
        end do
        call fortfront_grammar_analyze(table, output, output_count, status, message)
        call require(status == fortfront_grammar_analysis_ambiguous .and. output_count == 2, &
            'analysis did not accept a table beyond the former rule capacity')

        call fortfront_grammar_reset_local(table)
        call make_one(table%rules(1), 'MALFORMED-KIND', 'A', 'a', fortfront_grammar_symbol_token)
        table%rules(1)%rhs(1)%kind = 99
        table%count = 1
        call fortfront_grammar_analyze(table, output, output_count, status, message)
        call require(status == fortfront_grammar_analysis_malformed .and. output_count == 0, &
            'malformed symbol kind was not rejected')

        call fortfront_grammar_reset_local(table)
        call fortfront_grammar_analyze(table, output, output_count, status, message)
        call require(status == fortfront_grammar_analysis_empty .and. output_count == 0, &
            'empty grammar was not explicit')
    end subroutine test_capacity_and_malformed_controls

    subroutine analyze_and_require(table, output, output_count, status, message, lhs, &
            expected_status, expected_nullable, expected_first, expected_first_count, &
            expected_unresolved, expected_ambiguous)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message
        character(len=*), intent(in) :: lhs
        integer, intent(in) :: expected_status, expected_nullable, expected_first_count
        character(len=*), intent(in) :: expected_first(:)
        logical, intent(in) :: expected_unresolved, expected_ambiguous

        integer :: i, match

        call fortfront_grammar_analyze(table, output, output_count, status, message)
        match = 0
        do i = 1, output_count
            if (trim(output(i)%lhs) == lhs) match = i
        end do
        call require(match > 0, 'analysis result did not contain expected LHS')
        call require(status == expected_status, 'analysis status differed from oracle')
        call require(output(match)%status == expected_status .or. &
            (expected_status == fortfront_grammar_analysis_unresolved .and. &
            output(match)%status == fortfront_grammar_analysis_ambiguous), &
            'per-LHS analysis status differed from oracle')
        call require(output(match)%nullable_state == expected_nullable, &
            'nullable state differed from oracle')
        call require(output(match)%first_count == expected_first_count, &
            'first-symbol count differed from oracle')
        do i = 1, expected_first_count
            call require(trim(output(match)%first(i)%name) == trim(expected_first(i)) .and. &
                output(match)%first(i)%kind == fortfront_grammar_symbol_token, &
                'first-symbol order or kind differed from oracle')
        end do
        call require(output(match)%unresolved .eqv. expected_unresolved, &
            'unresolved state differed from oracle')
        call require(output(match)%ambiguous .eqv. expected_ambiguous, &
            'ambiguous state differed from oracle')
    end subroutine analyze_and_require

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
        rule%provenance%document = 'analysis-witness'
        rule%provenance%clause = 'analysis-witness-clause'
        rule%provenance%rule = identity
        rule%provenance%page = 1_int64
        rule%provenance%source_hash = 'analysis-witness-hash'
        rule%provenance%start_byte = 0_int64
        rule%provenance%end_byte = 1_int64
    end subroutine initialize_rule

    subroutine add_rule(table, rule)
        type(fortfront_grammar_table_t), intent(inout) :: table
        type(fortfront_grammar_rule_t), intent(in) :: rule

        integer :: status
        character(len=256) :: message

        call fortfront_grammar_add(table, rule, status, message)
        call require(status == fortfront_grammar_valid, &
            'valid analysis witness was rejected: '//trim(message))
    end subroutine add_rule

    subroutine fortfront_grammar_reset_local(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        table = fortfront_grammar_table_t()
        allocate(table%rules(8))
    end subroutine fortfront_grammar_reset_local

    subroutine grow_table_fixture(table, required)
        type(fortfront_grammar_table_t), intent(inout) :: table
        integer, intent(in) :: required
        type(fortfront_grammar_rule_t), allocatable :: rules(:)

        allocate(rules(required))
        rules = fortfront_grammar_rule_t()
        rules(1:min(table%count, size(table%rules))) = table%rules(1:min(table%count, &
            size(table%rules)))
        call move_alloc(rules, table%rules)
    end subroutine grow_table_fixture

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=8) :: text

        write (text, '(i0)') value
    end function integer_text

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_grammar_analysis
