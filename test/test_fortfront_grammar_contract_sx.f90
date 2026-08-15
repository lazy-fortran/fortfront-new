program test_fortfront_grammar_contract_sx
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_contract_invalid_provenance, &
        fortfront_grammar_contract_not_accepted, &
        fortfront_grammar_contract_rule_t, fortfront_grammar_contract_valid, &
        fortfront_grammar_node_choice, fortfront_grammar_node_optional, &
        fortfront_grammar_node_reference, fortfront_grammar_node_repeat, &
        fortfront_grammar_node_sequence, fortfront_grammar_node_token, &
        fortfront_grammar_project_contract_sequence, fortfront_grammar_read_contract_sx, &
        fortfront_grammar_rule_t
    implicit none

    character(len=*), parameter :: fixture = &
        '(syntax-rule (id R501) (alternative 1) (lhs program) (root 1) '// &
        '(nodes (grammar-nodes (grammar-node sequence - 1 false 2 2) '// &
        '(grammar-node reference program-unit 1 false 0 0) '// &
        '(grammar-node token IF 1 false 0 0) '// &
        '(grammar-node choice - 1 false 5 1) '// &
        '(grammar-node reference name 1 false 0 0) '// &
        '(grammar-node optional - 0 false 7 1) '// &
        '(grammar-node token THEN 1 false 0 0) '// &
        '(grammar-node repeat statement 1 true 9 1) '// &
        '(grammar-node reference body 1 false 0 0))) '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R501) '// &
        '(page 45) (source-hash fixture))) (origin mechanical) '// &
        '(resolution resolved))'
    type(fortfront_grammar_contract_rule_t) :: rule, stale
    type(fortfront_grammar_rule_t) :: projected
    character(len=32768) :: capacity_fixture
    character(len=256) :: message
    integer :: i, status

    call fortfront_grammar_read_contract_sx(fixture, rule, status, message)
    call require(status == fortfront_grammar_contract_valid, 'fixed SX was rejected')
    call require(rule%node_count == 9 .and. rule%nodes(1)%kind == fortfront_grammar_node_sequence, &
        'root node was not read')
    call require(rule%nodes(2)%kind == fortfront_grammar_node_reference .and. &
        rule%nodes(3)%kind == fortfront_grammar_node_token .and. &
        rule%nodes(4)%kind == fortfront_grammar_node_choice .and. &
        rule%nodes(6)%kind == fortfront_grammar_node_optional .and. &
        rule%nodes(8)%kind == fortfront_grammar_node_repeat, 'node kinds were not preserved')
    call require(trim(rule%source%document) == 'J3-24-007' .and. rule%source%page == 45_int64, &
        'source provenance was not preserved')
    call fortfront_grammar_project_contract_sequence(rule, projected, status, message)
    call require(status == fortfront_grammar_contract_valid .and. projected%rhs_count == 2, &
        'round-trip contract was not consumed by the existing boundary')

    stale = rule
    call fortfront_grammar_read_contract_sx('(syntax-rule', stale, status, message)
    call require(status /= fortfront_grammar_contract_valid .and. len_trim(stale%identity) == 0, &
        'truncated SX did not clear output')
    call fortfront_grammar_read_contract_sx('(standardir-grammar-v1)', stale, status, message)
    call require(status /= fortfront_grammar_contract_valid .and. len_trim(stale%identity) == 0, &
        'unsupported contract version was accepted')
    call fortfront_grammar_read_contract_sx( &
        replace(fixture, '(page 45)', '(page 0)'), stale, status, message)
    call require(status == fortfront_grammar_contract_invalid_provenance, &
        'invalid provenance did not reach contract validation')
    call fortfront_grammar_read_contract_sx( &
        replace(fixture, '(resolution resolved)', '(resolution unresolved)'), stale, status, message)
    call require(status == fortfront_grammar_contract_not_accepted .and. len_trim(stale%identity) == 0, &
        'unresolved rule was accepted or not cleared')

    capacity_fixture = '(syntax-rule (id BIG) (alternative 1) (lhs root) (root 1) '// &
        '(nodes (grammar-nodes (grammar-node sequence - 1 false 2 128) '
    do i = 2, 130
        call append(capacity_fixture, '(grammar-node reference leaf 1 false 0 0) ')
    end do
    call append(capacity_fixture, ')) (source (source-ref (document doc) (clause c) '// &
        '(rule r) (page 1) '// &
        '(source-hash 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef))) '// &
        '(origin mechanical) (resolution resolved))')
    call fortfront_grammar_read_contract_sx(capacity_fixture, stale, status, message)
    call require(status == fortfront_grammar_contract_valid .and. stale%node_count == 130, &
        'dynamic SX node storage rejected the former capacity frontier')
    print '(a)', 'fortfront grammar contract SX behavioral checks: ok'

contains

    function replace(value, old, new) result(output)
        character(len=*), intent(in) :: value, old, new
        character(len=32768) :: output
        integer :: at
        output = ''
        at = index(value, old)
        if (at == 0) then
            output(:len_trim(value)) = value(:len_trim(value))
        else
            output = value(:at - 1)//new//value(at + len(old):)
        end if
    end function replace

    subroutine append(value, addition)
        character(len=*), intent(inout) :: value
        character(len=*), intent(in) :: addition
        integer :: at
        at = len_trim(value)
        value(at + 1:at + len_trim(addition)) = trim(addition)
    end subroutine append

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure
        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_grammar_contract_sx
