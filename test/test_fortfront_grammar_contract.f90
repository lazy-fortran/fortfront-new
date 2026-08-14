program test_fortfront_grammar_contract
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, &
        fortfront_grammar_collect_matches, fortfront_grammar_contract_capacity, &
        fortfront_grammar_contract_invalid_kind, fortfront_grammar_contract_invalid_range, &
        fortfront_grammar_contract_malformed, fortfront_grammar_contract_not_accepted, &
        fortfront_grammar_contract_not_projectable, fortfront_grammar_contract_rule_t, &
        fortfront_grammar_contract_source_t, fortfront_grammar_contract_valid, &
        fortfront_grammar_node_reference, fortfront_grammar_node_sequence, &
        fortfront_grammar_node_t, &
        fortfront_grammar_node_token, fortfront_grammar_origin_mechanical, &
        fortfront_grammar_project_contract_sequence, fortfront_grammar_resolution_disputed, &
        fortfront_grammar_resolution_resolved, fortfront_grammar_resolution_unresolved, &
        fortfront_grammar_rule_t, fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_t, fortfront_grammar_symbol_token, fortfront_grammar_table_t, &
        fortfront_grammar_consume_contract_rule, fortfront_grammar_validate_contract_rule
    implicit none

    type(fortfront_grammar_contract_rule_t) :: input, consumed, malformed
    type(fortfront_grammar_contract_source_t) :: source
    type(fortfront_grammar_rule_t) :: projected, stale_projected
    type(fortfront_grammar_table_t) :: table
    type(fortfront_grammar_rule_t) :: candidates(2)
    type(fortfront_grammar_symbol_t) :: symbols(2)
    integer :: count, status
    character(len=256) :: message

    call make_rule(input, source)
    call fortfront_grammar_validate_contract_rule(input, status, message)
    call require(status == fortfront_grammar_contract_valid, 'valid flat tree was rejected')

    consumed%identity = 'stale'
    call fortfront_grammar_consume_contract_rule(input, consumed, status, message)
    call require(status == fortfront_grammar_contract_valid, 'valid tree was not consumed')
    call require(trim(consumed%identity) == 'R-1' .and. consumed%node_count == 3, &
        'consumed tree lost identity or nodes')
    call require(trim(consumed%source%source_hash) == 'sha256-fixture' .and. &
        consumed%origin == fortfront_grammar_origin_mechanical, &
        'consumed tree lost source provenance or origin')
    call require(consumed%nodes(2)%kind == fortfront_grammar_node_reference .and. &
        trim(consumed%nodes(3)%name) == 'literal', 'consumed tree changed node data')

    call fortfront_grammar_project_contract_sequence(input, projected, status, message)
    call require(status == fortfront_grammar_contract_valid, &
        'leaf sequence was not projected')
    call require(trim(projected%identity) == 'R-1' .and. trim(projected%lhs) == 'root' .and. &
        projected%rhs_count == 2, 'projection changed rule identity or shape')
    call require(trim(projected%provenance%rule) == 'R-1' .and. &
        trim(projected%provenance%source_hash) == 'sha256-fixture', &
        'projection did not preserve provenance')

    call fortfront_grammar_add(table, projected, status, message)
    call require(status == fortfront_grammar_contract_valid, &
        'projected rule was rejected by candidate table')
    symbols = fortfront_grammar_symbol_t()
    symbols(1)%name = 'child'
    symbols(1)%kind = fortfront_grammar_symbol_reference
    symbols(2)%name = 'literal'
    symbols(2)%kind = fortfront_grammar_symbol_token
    call fortfront_grammar_collect_matches(table, 'root', symbols, 2, candidates, count, &
        status, message)
    call require(status == fortfront_grammar_contract_valid .and. count == 1, &
        'projected rule did not compose with candidate machinery')

    malformed = input
    malformed%root = 4
    call fortfront_grammar_validate_contract_rule(malformed, status, message)
    call require(status == fortfront_grammar_contract_invalid_range, &
        'out-of-range root was accepted')
    malformed = input
    malformed%nodes(1)%first_child = 3
    malformed%nodes(1)%child_count = 2
    call fortfront_grammar_validate_contract_rule(malformed, status, message)
    call require(status == fortfront_grammar_contract_invalid_range, &
        'out-of-range child range was accepted')
    malformed = input
    malformed%nodes(2)%kind = 99
    call fortfront_grammar_validate_contract_rule(malformed, status, message)
    call require(status == fortfront_grammar_contract_invalid_kind, &
        'invalid node kind was accepted')

    malformed = input
    malformed%resolution = fortfront_grammar_resolution_unresolved
    consumed%identity = 'stale'
    call fortfront_grammar_consume_contract_rule(malformed, consumed, status, message)
    call require(status == fortfront_grammar_contract_not_accepted .and. &
        len_trim(consumed%identity) == 0, 'unresolved rule was accepted or not cleared')
    malformed%resolution = fortfront_grammar_resolution_disputed
    call fortfront_grammar_project_contract_sequence(malformed, stale_projected, status, message)
    call require(status == fortfront_grammar_contract_not_accepted .and. &
        len_trim(stale_projected%identity) == 0, 'disputed rule was accepted or not cleared')

    malformed = input
    malformed%nodes(1)%child_count = 17
    malformed%nodes(1)%first_child = 2
    malformed%node_count = 18
    do count = 2, 18
        malformed%nodes(count) = fortfront_grammar_node_t()
        malformed%nodes(count)%kind = fortfront_grammar_node_reference
        malformed%nodes(count)%name = 'leaf'
    end do
    call fortfront_grammar_project_contract_sequence(malformed, stale_projected, status, message)
    call require(status == fortfront_grammar_contract_capacity .and. &
        len_trim(stale_projected%identity) == 0, 'projection capacity was not explicit')

    malformed = input
    malformed%nodes(2)%kind = fortfront_grammar_node_sequence
    malformed%nodes(2)%first_child = 3
    malformed%nodes(2)%child_count = 1
    call fortfront_grammar_project_contract_sequence(malformed, stale_projected, status, message)
    call require(status == fortfront_grammar_contract_not_projectable .and. &
        len_trim(stale_projected%identity) == 0, 'non-sequence root was projected')
    print '(a)', 'fortfront grammar contract boundary behavioral checks: ok'

contains

    subroutine make_rule(rule, source)
        type(fortfront_grammar_contract_rule_t), intent(out) :: rule
        type(fortfront_grammar_contract_source_t), intent(out) :: source

        rule = fortfront_grammar_contract_rule_t()
        source = fortfront_grammar_contract_source_t()
        source%document = 'fortran-standard'
        source%clause = 'R501'
        source%rule = 'R-1'
        source%page = 12_int64
        source%source_hash = 'sha256-fixture'
        rule%identity = 'R-1'
        rule%alternative = 0
        rule%lhs = 'root'
        rule%root = 1
        rule%node_count = 3
        rule%nodes(1)%kind = fortfront_grammar_node_sequence
        rule%nodes(1)%first_child = 2
        rule%nodes(1)%child_count = 2
        rule%nodes(2)%kind = fortfront_grammar_node_reference
        rule%nodes(2)%name = 'child'
        rule%nodes(3)%kind = fortfront_grammar_node_token
        rule%nodes(3)%name = 'literal'
        rule%source = source
        rule%origin = fortfront_grammar_origin_mechanical
        rule%resolution = fortfront_grammar_resolution_resolved
    end subroutine make_rule

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_grammar_contract
