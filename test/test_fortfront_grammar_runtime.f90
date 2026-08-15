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

    subroutine make_nested_rule(rule)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule

        rule = fortfront_grammar_contract_rule_t(identity='NESTED', alternative=1, lhs='root', &
            root=1, node_count=8, origin=1, resolution=fortfront_grammar_resolution_resolved)
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
