program test_fortfront_grammar
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_capacity, &
        fortfront_grammar_candidate_ambiguous, fortfront_grammar_candidate_capacity, &
        fortfront_grammar_candidate_malformed_input, &
        fortfront_grammar_candidate_malformed_table, fortfront_grammar_candidate_no_match, &
        fortfront_grammar_collect_matches, &
        fortfront_grammar_duplicate_identity, fortfront_grammar_invalid_provenance, &
        fortfront_grammar_match_kind_mismatch, fortfront_grammar_match_length_mismatch, &
        fortfront_grammar_match_malformed_input, fortfront_grammar_match_malformed_rule, &
        fortfront_grammar_match_name_mismatch, fortfront_grammar_match_rule, &
        fortfront_grammar_malformed, fortfront_grammar_query_lhs, &
        fortfront_grammar_query_missing, fortfront_grammar_query_table_empty, &
        fortfront_grammar_rule_capacity, fortfront_grammar_rule_t, &
        fortfront_grammar_rhs_capacity, fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_t, fortfront_grammar_symbol_token, fortfront_grammar_table_t, &
        fortfront_grammar_valid, fortfront_grammar_validate_rule, &
        fortfront_grammar_reset
    implicit none

    type(fortfront_grammar_table_t) :: table, empty_table
    type(fortfront_grammar_rule_t) :: rule, duplicate, output(3), matched
    type(fortfront_grammar_symbol_t) :: input(3)
    integer :: count, status
    character(len=256) :: message

    call fortfront_grammar_reset(table)
    call fortfront_grammar_reset(empty_table)
    call make_rule(rule, 'R2', 'root', 'child', fortfront_grammar_symbol_reference)
    call require_add(table, rule)
    call make_rule(rule, 'R1', 'root', 'literal', fortfront_grammar_symbol_token)
    call require_add(table, rule)
    call make_rule(rule, 'R3', 'other', 'leaf', fortfront_grammar_symbol_reference)
    call require_add(table, rule)

    output = fortfront_grammar_rule_t()
    call fortfront_grammar_query_lhs(table, 'root', output, count, status, message)
    call require(status == fortfront_grammar_valid, 'lhs query failed')
    call require(count == 2, 'lhs query returned the wrong count')
    call require(trim(output(1)%identity) == 'R2' .and. trim(output(2)%identity) == 'R1', &
        'lhs query did not preserve insertion order')
    call require(trim(output(1)%rhs(1)%name) == 'child' .and. &
        output(2)%rhs(1)%kind == fortfront_grammar_symbol_token, &
        'lhs query changed RHS symbols')

    call fortfront_grammar_query_lhs(table, 'missing', output, count, status, message)
    call require(status == fortfront_grammar_query_missing .and. count == 0, &
        'missing lhs status differs')
    call require(len_trim(output(1)%identity) == 0, 'missing query retained stale output')

    call fortfront_grammar_query_lhs(empty_table, 'root', output, count, status, message)
    call require(status == fortfront_grammar_query_table_empty .and. count == 0, &
        'empty table status differs')
    call require(len_trim(output(1)%identity) == 0, 'empty query retained stale output')

    call make_rule(rule, 'bad-symbol', 'root', ' ', fortfront_grammar_symbol_reference)
    call fortfront_grammar_validate_rule(rule, status, message)
    call require(status == fortfront_grammar_malformed, 'malformed symbol was accepted')
    call fortfront_grammar_add(table, rule, status, message)
    call require(status == fortfront_grammar_malformed, 'malformed rule was added')

    call make_rule(rule, 'bad-provenance', 'root', 'child', fortfront_grammar_symbol_reference)
    rule%provenance%source_hash = ''
    call fortfront_grammar_validate_rule(rule, status, message)
    call require(status == fortfront_grammar_invalid_provenance, &
        'invalid provenance was accepted')
    call fortfront_grammar_add(table, rule, status, message)
    call require(status == fortfront_grammar_invalid_provenance, &
        'invalid provenance rule was added')

    call make_rule(duplicate, 'R2', 'different', 'leaf', fortfront_grammar_symbol_token)
    call fortfront_grammar_add(table, duplicate, status, message)
    call require(status == fortfront_grammar_duplicate_identity, &
        'duplicate identity was accepted')

    call make_rule(rule, 'too-many', 'root', 'child', fortfront_grammar_symbol_reference)
    rule%rhs_count = fortfront_grammar_rhs_capacity + 1
    call fortfront_grammar_add(table, rule, status, message)
    call require(status == fortfront_grammar_malformed, 'oversized RHS was accepted')

    call test_query_capacity(table)
    call test_table_capacity()
    call test_rhs_matching(rule, matched, input)
    call test_candidate_collection(table, input)
    print '(a)', 'fortfront grammar boundary behavioral checks: ok'

contains

    subroutine make_rule(value, identity, lhs, symbol_name, symbol_kind)
        type(fortfront_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: identity, lhs, symbol_name
        integer, intent(in) :: symbol_kind

        value = fortfront_grammar_rule_t()
        value%identity = identity
        value%lhs = lhs
        value%rhs_count = 1
        value%rhs(1)%name = symbol_name
        value%rhs(1)%kind = symbol_kind
        value%provenance%document = 'constructed-standard'
        value%provenance%clause = 'constructed-clause'
        value%provenance%rule = identity
        value%provenance%page = 1_int64
        value%provenance%source_hash = 'constructed-hash'
        value%provenance%start_byte = 4_int64
        value%provenance%end_byte = 12_int64
    end subroutine make_rule

    subroutine require_add(value, item)
        type(fortfront_grammar_table_t), intent(inout) :: value
        type(fortfront_grammar_rule_t), intent(in) :: item

        call fortfront_grammar_add(value, item, status, message)
        call require(status == fortfront_grammar_valid, &
            'valid constructed rule was rejected: '//trim(message))
    end subroutine require_add

    subroutine test_query_capacity(value)
        type(fortfront_grammar_table_t), intent(in) :: value

        type(fortfront_grammar_rule_t) :: small_output(1)
        integer :: local_count, local_status
        character(len=256) :: local_message

        small_output(1)%identity = 'stale'
        call fortfront_grammar_query_lhs(value, 'root', small_output, local_count, &
            local_status, local_message)
        call require(local_status == fortfront_grammar_capacity .and. local_count == 1, &
            'query output capacity was not reported')
        call require(trim(small_output(1)%identity) == 'R2', &
            'query capacity changed the valid prefix')
    end subroutine test_query_capacity

    subroutine test_table_capacity()
        type(fortfront_grammar_table_t) :: full_table
        type(fortfront_grammar_rule_t) :: item
        integer :: i, local_status
        character(len=256) :: local_message

        call fortfront_grammar_reset(full_table)
        do i = 1, fortfront_grammar_rule_capacity
            call make_rule(item, 'ID-'//integer_text(i), 'lhs', 'symbol', &
                fortfront_grammar_symbol_reference)
            call fortfront_grammar_add(full_table, item, local_status, local_message)
            call require(local_status == fortfront_grammar_valid, &
                'table capacity fixture could not be filled')
        end do
        call make_rule(item, 'overflow', 'lhs', 'symbol', fortfront_grammar_symbol_reference)
        call fortfront_grammar_add(full_table, item, local_status, local_message)
        call require(local_status == fortfront_grammar_capacity, &
            'table capacity was not reported')
    end subroutine test_table_capacity

    subroutine test_rhs_matching(value, result, symbols)
        type(fortfront_grammar_rule_t), intent(out) :: value
        type(fortfront_grammar_rule_t), intent(out) :: result
        type(fortfront_grammar_symbol_t), intent(out) :: symbols(:)

        type(fortfront_grammar_rule_t) :: invalid_rule
        integer :: local_status
        character(len=256) :: local_message

        call make_rule(value, 'MATCH', 'root', 'child', fortfront_grammar_symbol_reference)
        value%rhs_count = 2
        value%rhs(2)%name = 'literal'
        value%rhs(2)%kind = fortfront_grammar_symbol_token
        symbols = fortfront_grammar_symbol_t()
        symbols(1) = value%rhs(1)
        symbols(2) = value%rhs(2)
        result%identity = 'stale'
        call fortfront_grammar_match_rule(value, symbols, 2, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_valid, 'valid RHS did not match')
        call require(trim(result%identity) == 'MATCH' .and. &
            trim(result%provenance%rule) == 'MATCH', 'match did not preserve provenance')

        symbols(2)%name = 'other'
        call fortfront_grammar_match_rule(value, symbols, 2, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_name_mismatch .and. &
            len_trim(result%identity) == 0, 'name mismatch was not reported or cleared')

        symbols(2)%name = value%rhs(2)%name
        symbols(2)%kind = fortfront_grammar_symbol_reference
        call fortfront_grammar_match_rule(value, symbols, 2, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_kind_mismatch .and. &
            len_trim(result%identity) == 0, 'kind mismatch was not reported or cleared')

        call fortfront_grammar_match_rule(value, symbols, 1, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_length_mismatch .and. &
            len_trim(result%identity) == 0, 'length mismatch was not reported or cleared')

        symbols(1)%name = ' '
        call fortfront_grammar_match_rule(value, symbols, 2, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_malformed_input .and. &
            len_trim(result%identity) == 0, 'malformed input was not reported or cleared')

        invalid_rule = value
        invalid_rule%identity = ' '
        symbols(1) = value%rhs(1)
        call fortfront_grammar_match_rule(invalid_rule, symbols, 2, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_malformed_rule .and. &
            len_trim(result%identity) == 0, 'malformed rule was not reported or cleared')

        call fortfront_grammar_match_rule(value, symbols, 4, result, local_status, &
            local_message)
        call require(local_status == fortfront_grammar_match_malformed_input .and. &
            len_trim(result%identity) == 0, 'input capacity was not reported or cleared')
    end subroutine test_rhs_matching

    subroutine test_candidate_collection(value, symbols)
        type(fortfront_grammar_table_t), intent(in) :: value
        type(fortfront_grammar_symbol_t), intent(inout) :: symbols(:)

        type(fortfront_grammar_table_t) :: ambiguous_table, malformed_table
        type(fortfront_grammar_rule_t) :: extra_rule, output(3), small_output(1)
        integer :: local_count, local_status
        character(len=256) :: local_message

        symbols = fortfront_grammar_symbol_t()
        symbols(1)%name = 'child'
        symbols(1)%kind = fortfront_grammar_symbol_reference
        call fortfront_grammar_collect_matches(value, 'root', symbols, 1, output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_valid .and. local_count == 1, &
            'unique grammar candidate was not collected')
        call require(trim(output(1)%identity) == 'R2' .and. &
            trim(output(1)%provenance%rule) == 'R2', &
            'unique candidate did not preserve identity and provenance')

        symbols(1)%name = 'missing'
        output(1)%identity = 'stale'
        call fortfront_grammar_collect_matches(value, 'root', symbols, 1, output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_no_match .and. &
            local_count == 0 .and. len_trim(output(1)%identity) == 0, &
            'no-match candidate result was not explicit and clear')

        ambiguous_table = value
        call make_rule(extra_rule, 'R4', 'root', 'child', fortfront_grammar_symbol_reference)
        call fortfront_grammar_add(ambiguous_table, extra_rule, local_status, local_message)
        call require(local_status == fortfront_grammar_valid, &
            'ambiguity fixture could not be added')
        symbols(1)%name = 'child'
        call fortfront_grammar_collect_matches(ambiguous_table, 'root', symbols, 1, output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_ambiguous .and. &
            local_count == 2, 'ambiguous grammar candidates were not reported')
        call require(trim(output(1)%identity) == 'R2' .and. &
            trim(output(2)%identity) == 'R4', 'candidate order was not preserved')

        malformed_table = value
        malformed_table%rules(1)%identity = ' '
        output(1)%identity = 'stale'
        call fortfront_grammar_collect_matches(malformed_table, 'root', symbols, 1, output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_malformed_table .and. &
            local_count == 0 .and. len_trim(output(1)%identity) == 0, &
            'malformed grammar table was not reported and cleared')

        symbols(1)%name = ' '
        output(1)%identity = 'stale'
        call fortfront_grammar_collect_matches(value, 'root', symbols, 1, output, &
            local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_malformed_input .and. &
            local_count == 0 .and. len_trim(output(1)%identity) == 0, &
            'malformed grammar input was not reported and cleared')

        symbols(1)%name = 'child'
        small_output(1)%identity = 'stale'
        call fortfront_grammar_collect_matches(ambiguous_table, 'root', symbols, 1, &
            small_output, local_count, local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_capacity .and. &
            local_count == 0 .and. len_trim(small_output(1)%identity) == 0, &
            'candidate output capacity was not reported and cleared')

        symbols(1)%name = 'child'
        symbols(2) = symbols(1)
        output(1)%identity = 'stale'
        call fortfront_grammar_collect_matches(value, 'root', symbols, 2, output, local_count, &
            local_status, local_message)
        call require(local_status == fortfront_grammar_candidate_no_match .and. &
            local_count == 0 .and. len_trim(output(1)%identity) == 0, &
            'candidate length mismatch was not reported as no match and cleared')
    end subroutine test_candidate_collection

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

end program test_fortfront_grammar
