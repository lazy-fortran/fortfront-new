program test_fortfront_grammar_runtime
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_contract_rule_t, &
        fortfront_grammar_node_choice, fortfront_grammar_node_optional, &
        fortfront_grammar_node_repeat, fortfront_grammar_node_sequence, &
        fortfront_grammar_node_token, fortfront_grammar_resolution_resolved, &
        fortfront_grammar_resolution_unresolved
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_grammar_runtime, only: fortfront_grammar_runtime_accepted, &
        fortfront_grammar_runtime_ambiguous, fortfront_grammar_runtime_initialize, &
        fortfront_grammar_runtime_finalize, &
        fortfront_grammar_runtime_malformed, fortfront_grammar_runtime_push, &
        fortfront_grammar_runtime_rejected, fortfront_grammar_runtime_unresolved, &
        fortfront_grammar_runtime_initialized, &
        fortfront_grammar_runtime_t
    implicit none

    call test_nested_expression_language()
    call test_ambiguity_and_unresolved()
    call test_malformed_contract_and_token_retry()
    call test_large_dynamic_grammar()
    call test_long_source_names()
    print '(a)', 'fortfront grammar runtime behavioral checks: ok'

contains

    subroutine test_nested_expression_language()
        type(fortfront_grammar_contract_rule_t) :: rules(1)
        type(fortfront_grammar_runtime_t) :: runtime
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: status, output_count, i
        character(len=256) :: message
        character(len=1) :: input(4)

        call make_nested_rule(rules(1))
        call fortfront_grammar_runtime_initialize(runtime, rules, 1, 'root', status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'nested generic contract did not initialize')
        input = [character(len=1) :: 'A', 'B', 'D', 'D']
        do i = 1, size(input)
            call fortfront_grammar_runtime_push(runtime, input(i), output, output_count, status, &
                message)
        end do
        call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1, &
            'nested optional, choice and repeat language was not accepted')
        call require(output(1)%lhs == 'root' .and. output(1)%consumed == 4 .and. &
            output(1)%next_position == 5, 'accepted generic span was not preserved')
        call fortfront_grammar_runtime_finalize(runtime, output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1, &
            'runtime finalization changed an accepted outcome')

        call make_nested_rule(rules(1))
        call fortfront_grammar_runtime_initialize(runtime, rules, 1, 'root', status, message)
        call fortfront_grammar_runtime_push(runtime, 'A', output, output_count, status, message)
        call fortfront_grammar_runtime_push(runtime, 'B', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_rejected .and. output_count == 0, &
            'incomplete generic stream was not rejected')

        rules(1)%nodes(7)%minimum = 0
        call fortfront_grammar_runtime_initialize(runtime, rules, 1, 'root', status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'zero-minimum repeat contract did not initialize')
        call fortfront_grammar_runtime_push(runtime, 'A', output, output_count, status, message)
        call fortfront_grammar_runtime_push(runtime, 'B', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1, &
            'zero-minimum repeat did not accept its empty occurrence')
        call fortfront_grammar_runtime_push(runtime, ' ', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_malformed .and. output_count == 0, &
            'malformed token was not explicit after an accepted stream')
    end subroutine test_nested_expression_language

    subroutine test_ambiguity_and_unresolved()
        type(fortfront_grammar_contract_rule_t) :: rules(2)
        type(fortfront_grammar_runtime_t) :: runtime
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: status, output_count
        character(len=256) :: message

        call make_leaf_rule(rules(1), 'CHOICE-A', 'x', fortfront_grammar_resolution_resolved)
        call make_leaf_rule(rules(2), 'CHOICE-B', 'x', fortfront_grammar_resolution_resolved)
        call fortfront_grammar_runtime_initialize(runtime, rules, 2, 'root', status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'ambiguous generic contracts did not initialize')
        call fortfront_grammar_runtime_push(runtime, 'x', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_ambiguous .and. output_count == 2, &
            'generic ambiguity was not preserved')
        call require(output(1)%provenance%rule == 'CHOICE-A' .and. &
            output(2)%provenance%rule == 'CHOICE-B', 'ambiguity order or provenance changed')

        call make_leaf_rule(rules(1), 'UNRESOLVED', 'x', fortfront_grammar_resolution_unresolved)
        call fortfront_grammar_runtime_initialize(runtime, rules(1:1), 1, 'root', status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'unresolved generic contract failed initialization')
        call fortfront_grammar_runtime_push(runtime, 'x', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_unresolved .and. output_count == 0, &
            'unresolved contract outcome was accepted or rejected')
    end subroutine test_ambiguity_and_unresolved

    subroutine test_malformed_contract_and_token_retry()
        type(fortfront_grammar_contract_rule_t) :: rules(1)
        type(fortfront_grammar_runtime_t) :: runtime
        type(fortfront_grammar_frontier_result_t) :: output(8)
        integer :: status, output_count
        character(len=256) :: message

        call make_leaf_rule(rules(1), 'MALFORMED', 'x', fortfront_grammar_resolution_resolved)
        rules(1)%nodes(1)%first_child = 2
        call fortfront_grammar_runtime_initialize(runtime, rules, 1, 'root', status, message)
        call require(status == fortfront_grammar_runtime_malformed, &
            'malformed contract tree was accepted')

        call make_leaf_rule(rules(1), 'TRANSACTIONAL', 'good', &
            fortfront_grammar_resolution_resolved)
        call fortfront_grammar_runtime_initialize(runtime, rules, 1, 'root', status, message)
        call fortfront_grammar_runtime_push(runtime, 'bad token', output, output_count, status, &
            message)
        call require(status == fortfront_grammar_runtime_malformed .and. output_count == 0, &
            'malformed token did not clear the runtime output')
        call fortfront_grammar_runtime_push(runtime, 'good', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1 .and. &
            output(1)%provenance%rule == 'TRANSACTIONAL', &
            'valid token could not retry after malformed input')
    end subroutine test_malformed_contract_and_token_retry

    subroutine test_large_dynamic_grammar()
        type(fortfront_grammar_contract_rule_t) :: rules(40), long_rule, single(1)
        type(fortfront_grammar_runtime_t) :: runtime
        type(fortfront_grammar_frontier_result_t) :: output(40)
        integer :: i, status, output_count
        character(len=256) :: message

        do i = 1, size(rules)
            call make_leaf_rule(rules(i), 'MANY-'//integer_text(i), 'x', &
                fortfront_grammar_resolution_resolved)
        end do
        call fortfront_grammar_runtime_initialize(runtime, rules, size(rules), 'root', status, &
            message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'runtime rejected more rules than the former fixed table')
        call fortfront_grammar_runtime_push(runtime, 'x', output, output_count, status, message)
        call require(status == fortfront_grammar_runtime_ambiguous .and. output_count == 40, &
            'runtime did not preserve all large-table alternatives')

        call make_long_sequence_rule(long_rule)
        single(1) = long_rule
        call fortfront_grammar_runtime_initialize(runtime, single, 1, 'root', status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'runtime rejected an RHS longer than the former fixed capacity')
        do i = 1, 40
            call fortfront_grammar_runtime_push(runtime, 'token', output, output_count, status, &
                message)
        end do
        call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1 .and. &
            output(1)%consumed == 40, 'runtime did not execute the large RHS')
    end subroutine test_large_dynamic_grammar

    subroutine test_long_source_names()
        type(fortfront_grammar_contract_rule_t) :: rules(2)
        type(fortfront_grammar_runtime_t) :: runtime
        type(fortfront_grammar_frontier_result_t) :: output(8)
        character(len=64) :: identity_a, identity_b
        character(len=128) :: lhs
        character(len=256) :: message
        integer :: output_count, status

        identity_a = repeat('i', 63)//'a'
        identity_b = repeat('i', 63)//'b'
        lhs = repeat('l', 127)//'a'
        call make_long_named_repeat_rule(rules(1), identity_a, lhs)
        call make_long_named_repeat_rule(rules(2), identity_b, lhs)
        call fortfront_grammar_runtime_initialize(runtime, rules, 2, lhs, status, message)
        call require(status == fortfront_grammar_runtime_initialized, &
            'runtime rejected valid source names at grammar field capacities')
        call fortfront_grammar_runtime_push(runtime, 'token', output, output_count, status, &
            message)
        call require(status == fortfront_grammar_runtime_ambiguous .and. output_count == 2, &
            'bounded generated names did not preserve distinct long source rules')
        call require(output(1)%lhs == lhs .and. output(2)%lhs == lhs .and. &
            output(1)%provenance%rule == identity_a .and. &
            output(2)%provenance%rule == identity_b, &
            'long source names changed result names or provenance')
    end subroutine test_long_source_names

    subroutine make_long_named_repeat_rule(rule, identity, lhs)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, lhs

        rule = fortfront_grammar_contract_rule_t(identity=identity, alternative=1, lhs=lhs, &
            root=1, node_count=2, origin=1, resolution=fortfront_grammar_resolution_resolved)
        allocate(rule%nodes(2))
        call set_source(rule)
        call set_node(rule, 1, fortfront_grammar_node_repeat, '-', 1, .true., 2, 1)
        call set_node(rule, 2, fortfront_grammar_node_token, 'token', 1, .false., 0, 0)
    end subroutine make_long_named_repeat_rule

    subroutine make_long_sequence_rule(rule)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule
        integer :: i

        rule = fortfront_grammar_contract_rule_t(identity='LONG-RHS', alternative=1, lhs='root', &
            root=1, node_count=41, origin=1, resolution=fortfront_grammar_resolution_resolved)
        allocate(rule%nodes(41))
        call set_source(rule)
        call set_node(rule, 1, fortfront_grammar_node_sequence, '-', 1, .false., 2, 40)
        do i = 2, 41
            call set_node(rule, i, fortfront_grammar_node_token, 'token', 1, .false., 0, 0)
        end do
    end subroutine make_long_sequence_rule

    subroutine make_nested_rule(rule)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule

        rule = fortfront_grammar_contract_rule_t(identity='NESTED', alternative=1, lhs='root', &
            root=1, node_count=8, origin=1, resolution=fortfront_grammar_resolution_resolved)
        allocate(rule%nodes(8))
        call set_source(rule)
        call set_node(rule, 1, fortfront_grammar_node_sequence, '-', 1, .false., 2, 3)
        call set_node(rule, 2, fortfront_grammar_node_optional, '-', 0, .false., 3, 1)
        call set_node(rule, 3, fortfront_grammar_node_token, 'A', 1, .false., 0, 0)
        call set_node(rule, 4, fortfront_grammar_node_choice, '-', 1, .false., 5, 2)
        call set_node(rule, 5, fortfront_grammar_node_token, 'B', 1, .false., 0, 0)
        call set_node(rule, 6, fortfront_grammar_node_token, 'C', 1, .false., 0, 0)
        call set_node(rule, 7, fortfront_grammar_node_repeat, '-', 1, .true., 8, 1)
        call set_node(rule, 8, fortfront_grammar_node_token, 'D', 1, .false., 0, 0)
    end subroutine make_nested_rule

    subroutine make_leaf_rule(rule, identity, token, resolution)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule
        character(len=*), intent(in) :: identity, token
        integer, intent(in) :: resolution

        rule = fortfront_grammar_contract_rule_t(identity=identity, alternative=1, lhs='root', &
            root=1, node_count=1, origin=1, resolution=resolution)
        allocate(rule%nodes(1))
        call set_source(rule)
        call set_node(rule, 1, fortfront_grammar_node_token, token, 1, .false., 0, 0)
    end subroutine make_leaf_rule

    subroutine set_source(rule)
        type(fortfront_grammar_contract_rule_t), intent(inout) :: rule

        rule%source%document = 'runtime-witness'
        rule%source%clause = 'generic'
        rule%source%rule = rule%identity
        rule%source%page = 1_int64
        rule%source%source_hash = 'runtime-witness-hash'
    end subroutine set_source

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=8) :: text

        write (text, '(i0)') value
    end function integer_text

    subroutine set_node(rule, index, kind, name, minimum, unbounded, first_child, child_count)
        type(fortfront_grammar_contract_rule_t), intent(inout) :: rule
        integer, intent(in) :: index, kind, minimum, first_child, child_count
        character(len=*), intent(in) :: name
        logical, intent(in) :: unbounded

        rule%nodes(index)%kind = kind
        rule%nodes(index)%name = name
        rule%nodes(index)%minimum = minimum
        rule%nodes(index)%unbounded = unbounded
        rule%nodes(index)%first_child = first_child
        rule%nodes(index)%child_count = child_count
    end subroutine set_node

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine require

end program test_fortfront_grammar_runtime
