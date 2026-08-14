module fortfront_grammar
    !! Generic consumer boundary for normalized, source-backed syntax rules.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    integer, parameter, public :: fortfront_grammar_rule_capacity = 32
    integer, parameter, public :: fortfront_grammar_rhs_capacity = 16
    integer, parameter, public :: fortfront_grammar_symbol_reference = 1
    integer, parameter, public :: fortfront_grammar_symbol_token = 2

    integer, parameter, public :: fortfront_grammar_valid = 0
    integer, parameter, public :: fortfront_grammar_malformed = 1
    integer, parameter, public :: fortfront_grammar_invalid_provenance = 2
    integer, parameter, public :: fortfront_grammar_duplicate_identity = 3
    integer, parameter, public :: fortfront_grammar_capacity = 4
    integer, parameter, public :: fortfront_grammar_query_empty = 5
    integer, parameter, public :: fortfront_grammar_query_table_empty = 6
    integer, parameter, public :: fortfront_grammar_query_missing = 7
    integer, parameter, public :: fortfront_grammar_match_malformed_rule = 8
    integer, parameter, public :: fortfront_grammar_match_malformed_input = 9
    integer, parameter, public :: fortfront_grammar_match_length_mismatch = 10
    integer, parameter, public :: fortfront_grammar_match_name_mismatch = 11
    integer, parameter, public :: fortfront_grammar_match_kind_mismatch = 12
    integer, parameter, public :: fortfront_grammar_candidate_no_match = 13
    integer, parameter, public :: fortfront_grammar_candidate_ambiguous = 14
    integer, parameter, public :: fortfront_grammar_candidate_malformed_table = 15
    integer, parameter, public :: fortfront_grammar_candidate_malformed_input = 16
    integer, parameter, public :: fortfront_grammar_candidate_capacity = 17
    integer, parameter, public :: fortfront_grammar_contract_node_capacity = 128
    integer, parameter, public :: fortfront_grammar_contract_valid = 0
    integer, parameter, public :: fortfront_grammar_contract_malformed = 18
    integer, parameter, public :: fortfront_grammar_contract_invalid_range = 19
    integer, parameter, public :: fortfront_grammar_contract_invalid_kind = 20
    integer, parameter, public :: fortfront_grammar_contract_invalid_provenance = 21
    integer, parameter, public :: fortfront_grammar_contract_not_accepted = 22
    integer, parameter, public :: fortfront_grammar_contract_not_projectable = 23
    integer, parameter, public :: fortfront_grammar_contract_capacity = 24

    integer, parameter, public :: fortfront_grammar_node_reference = 1
    integer, parameter, public :: fortfront_grammar_node_token = 2
    integer, parameter, public :: fortfront_grammar_node_sequence = 3
    integer, parameter, public :: fortfront_grammar_node_choice = 4
    integer, parameter, public :: fortfront_grammar_node_optional = 5
    integer, parameter, public :: fortfront_grammar_node_repeat = 6

    integer, parameter, public :: fortfront_grammar_origin_mechanical = 1
    integer, parameter, public :: fortfront_grammar_origin_search = 2
    integer, parameter, public :: fortfront_grammar_origin_smt = 3
    integer, parameter, public :: fortfront_grammar_origin_llm = 4
    integer, parameter, public :: fortfront_grammar_origin_llm_repair = 5
    integer, parameter, public :: fortfront_grammar_origin_human = 6
    integer, parameter, public :: fortfront_grammar_origin_imported = 7
    integer, parameter, public :: fortfront_grammar_origin_differential = 8

    integer, parameter, public :: fortfront_grammar_resolution_resolved = 1
    integer, parameter, public :: fortfront_grammar_resolution_unresolved = 2
    integer, parameter, public :: fortfront_grammar_resolution_disputed = 3

    type, public :: fortfront_grammar_symbol_t
        character(len=128) :: name = ''
        integer :: kind = 0
    end type fortfront_grammar_symbol_t

    type, public :: fortfront_grammar_provenance_t
        character(len=128) :: document = ''
        character(len=64) :: clause = ''
        character(len=64) :: rule = ''
        integer(int64) :: page = 0_int64
        character(len=128) :: source_hash = ''
        integer(int64) :: start_byte = 0_int64
        integer(int64) :: end_byte = 0_int64
    end type fortfront_grammar_provenance_t

    type, public :: fortfront_grammar_rule_t
        character(len=64) :: identity = ''
        character(len=128) :: lhs = ''
        integer :: rhs_count = 0
        type(fortfront_grammar_symbol_t) :: rhs(fortfront_grammar_rhs_capacity)
        type(fortfront_grammar_provenance_t) :: provenance
    end type fortfront_grammar_rule_t

    type, public :: fortfront_grammar_table_t
        integer :: count = 0
        type(fortfront_grammar_rule_t) :: rules(fortfront_grammar_rule_capacity)
    end type fortfront_grammar_table_t

    type, public :: fortfront_grammar_contract_source_t
        character(len=128) :: document = ''
        character(len=64) :: clause = ''
        character(len=64) :: rule = ''
        integer(int64) :: page = 0_int64
        character(len=128) :: source_hash = ''
    end type fortfront_grammar_contract_source_t

    type, public :: fortfront_grammar_node_t
        integer :: kind = 0
        character(len=128) :: name = ''
        integer :: minimum = 0
        logical :: unbounded = .false.
        integer :: first_child = 0
        integer :: child_count = 0
    end type fortfront_grammar_node_t

    type, public :: fortfront_grammar_contract_rule_t
        character(len=64) :: identity = ''
        integer :: alternative = 0
        character(len=128) :: lhs = ''
        integer :: root = 0
        integer :: node_count = 0
        type(fortfront_grammar_node_t) :: nodes(fortfront_grammar_contract_node_capacity)
        type(fortfront_grammar_contract_source_t) :: source
        integer :: origin = 0
        integer :: resolution = 0
    end type fortfront_grammar_contract_rule_t

    public :: fortfront_grammar_add
    public :: fortfront_grammar_collect_matches
    public :: fortfront_grammar_consume_contract_rule
    public :: fortfront_grammar_match_rule
    public :: fortfront_grammar_project_contract_sequence
    public :: fortfront_grammar_query_lhs
    public :: fortfront_grammar_read_contract_sx
    public :: fortfront_grammar_reset
    public :: fortfront_grammar_validate_contract_rule
    public :: fortfront_grammar_validate_rule

contains

    subroutine fortfront_grammar_reset(table)
        type(fortfront_grammar_table_t), intent(out) :: table

        table = fortfront_grammar_table_t()
    end subroutine fortfront_grammar_reset

    subroutine fortfront_grammar_validate_rule(rule, status, message)
        type(fortfront_grammar_rule_t), intent(in) :: rule
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i

        status = fortfront_grammar_valid
        message = ''
        if (.not. valid_atom(rule%identity)) then
            status = fortfront_grammar_malformed
            message = 'grammar-rule-identity-is-malformed'
            return
        end if
        if (.not. valid_atom(rule%lhs)) then
            status = fortfront_grammar_malformed
            message = 'grammar-rule-lhs-is-malformed'
            return
        end if
        if (rule%rhs_count < 0 .or. rule%rhs_count > fortfront_grammar_rhs_capacity) then
            status = fortfront_grammar_malformed
            message = 'grammar-rule-rhs-count-is-out-of-range'
            return
        end if
        do i = 1, rule%rhs_count
            if (.not. valid_atom(rule%rhs(i)%name)) then
                status = fortfront_grammar_malformed
                message = 'grammar-rule-symbol-is-malformed'
                return
            end if
            if (rule%rhs(i)%kind /= fortfront_grammar_symbol_reference .and. &
                rule%rhs(i)%kind /= fortfront_grammar_symbol_token) then
                status = fortfront_grammar_malformed
                message = 'grammar-rule-symbol-kind-is-invalid'
                return
            end if
        end do
        if (.not. valid_provenance(rule%provenance)) then
            status = fortfront_grammar_invalid_provenance
            message = 'grammar-rule-provenance-is-invalid'
        end if
    end subroutine fortfront_grammar_validate_rule

    subroutine fortfront_grammar_add(table, rule, status, message)
        type(fortfront_grammar_table_t), intent(inout) :: table
        type(fortfront_grammar_rule_t), intent(in) :: rule
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i, rule_status
        character(len=256) :: rule_message

        status = fortfront_grammar_malformed
        message = ''
        if (table%count < 0 .or. table%count > fortfront_grammar_rule_capacity) then
            message = 'grammar-table-count-is-out-of-range'
            return
        end if
        call fortfront_grammar_validate_rule(rule, rule_status, rule_message)
        if (rule_status /= fortfront_grammar_valid) then
            status = rule_status
            message = rule_message
            return
        end if
        do i = 1, table%count
            if (trim(table%rules(i)%identity) == trim(rule%identity)) then
                status = fortfront_grammar_duplicate_identity
                message = 'grammar-rule-identity-is-duplicate'
                return
            end if
        end do
        if (table%count == fortfront_grammar_rule_capacity) then
            status = fortfront_grammar_capacity
            message = 'grammar-table-capacity-exhausted'
            return
        end if
        table%count = table%count + 1
        table%rules(table%count) = rule
        status = fortfront_grammar_valid
    end subroutine fortfront_grammar_add

    subroutine fortfront_grammar_match_rule(rule, input, input_count, matched_rule, &
            status, message)
        type(fortfront_grammar_rule_t), intent(in) :: rule
        type(fortfront_grammar_symbol_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(fortfront_grammar_rule_t), intent(out) :: matched_rule
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i, rule_status
        character(len=256) :: rule_message

        matched_rule = fortfront_grammar_rule_t()
        status = fortfront_grammar_match_malformed_rule
        message = ''
        call fortfront_grammar_validate_rule(rule, rule_status, rule_message)
        if (rule_status /= fortfront_grammar_valid) then
            message = 'grammar-rule-is-invalid-for-matching'
            return
        end if
        if (input_count < 0 .or. input_count > size(input)) then
            status = fortfront_grammar_match_malformed_input
            message = 'grammar-input-count-is-out-of-range'
            return
        end if
        do i = 1, input_count
            if (.not. valid_input_symbol(input(i))) then
                status = fortfront_grammar_match_malformed_input
                message = 'grammar-input-symbol-is-malformed'
                return
            end if
        end do
        if (input_count /= rule%rhs_count) then
            status = fortfront_grammar_match_length_mismatch
            message = 'grammar-rule-and-input-lengths-differ'
            return
        end if
        do i = 1, rule%rhs_count
            if (input(i)%kind /= rule%rhs(i)%kind) then
                status = fortfront_grammar_match_kind_mismatch
                message = 'grammar-input-symbol-kind-does-not-match'
                return
            end if
            if (trim(input(i)%name) /= trim(rule%rhs(i)%name)) then
                status = fortfront_grammar_match_name_mismatch
                message = 'grammar-input-symbol-name-does-not-match'
                return
            end if
        end do
        matched_rule = rule
        status = fortfront_grammar_valid
    end subroutine fortfront_grammar_match_rule

    subroutine fortfront_grammar_collect_matches(table, lhs, input, input_count, output, &
            output_count, status, message)
        type(fortfront_grammar_table_t), intent(in) :: table
        character(len=*), intent(in) :: lhs
        type(fortfront_grammar_symbol_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(fortfront_grammar_rule_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_rule_t) :: lhs_rules(fortfront_grammar_rule_capacity)
        type(fortfront_grammar_rule_t) :: matches(fortfront_grammar_rule_capacity)
        type(fortfront_grammar_rule_t) :: matched_rule
        integer :: i, lhs_count, match_count, query_status, match_status
        character(len=256) :: query_message, match_message

        output = fortfront_grammar_rule_t()
        output_count = 0
        status = fortfront_grammar_candidate_malformed_input
        message = ''
        if (.not. valid_atom(lhs)) then
            message = 'grammar-candidate-lhs-is-empty-or-malformed'
            return
        end if
        if (input_count < 0) then
            message = 'grammar-candidate-input-count-is-negative'
            return
        end if
        if (input_count > size(input)) then
            message = 'grammar-candidate-input-count-is-out-of-range'
            return
        end if
        do i = 1, input_count
            if (.not. valid_input_symbol(input(i))) then
                message = 'grammar-candidate-input-symbol-is-malformed'
                return
            end if
        end do

        call fortfront_grammar_query_lhs(table, lhs, lhs_rules, lhs_count, query_status, &
            query_message)
        if (query_status == fortfront_grammar_query_table_empty .or. &
            query_status == fortfront_grammar_query_missing) then
            status = fortfront_grammar_candidate_no_match
            message = 'grammar-candidate-has-no-matching-rule'
            return
        end if
        if (query_status /= fortfront_grammar_valid) then
            status = fortfront_grammar_candidate_malformed_table
            message = 'grammar-candidate-table-is-invalid'
            return
        end if

        matches = fortfront_grammar_rule_t()
        match_count = 0
        do i = 1, lhs_count
            call fortfront_grammar_match_rule(lhs_rules(i), input, input_count, matched_rule, &
                match_status, match_message)
            if (match_status == fortfront_grammar_valid) then
                match_count = match_count + 1
                matches(match_count) = matched_rule
            else if (match_status == fortfront_grammar_match_malformed_input) then
                status = fortfront_grammar_candidate_malformed_input
                message = 'grammar-candidate-input-is-invalid'
                return
            else if (match_status == fortfront_grammar_match_malformed_rule) then
                status = fortfront_grammar_candidate_malformed_table
                message = 'grammar-candidate-table-rule-is-invalid'
                return
            end if
        end do
        if (match_count == 0) then
            status = fortfront_grammar_candidate_no_match
            message = 'grammar-candidate-has-no-matching-rule'
            return
        end if
        if (match_count > size(output)) then
            status = fortfront_grammar_candidate_capacity
            message = 'grammar-candidate-output-capacity-exhausted'
            return
        end if
        output(1:match_count) = matches(1:match_count)
        output_count = match_count
        if (match_count == 1) then
            status = fortfront_grammar_valid
            message = 'grammar-candidate-is-unique'
        else
            status = fortfront_grammar_candidate_ambiguous
            message = 'grammar-candidate-is-ambiguous'
        end if
    end subroutine fortfront_grammar_collect_matches

    subroutine fortfront_grammar_validate_contract_rule(rule, status, message)
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i, last_child

        status = fortfront_grammar_contract_valid
        message = ''
        if (.not. valid_atom(rule%identity)) then
            status = fortfront_grammar_contract_malformed
            message = 'contract-rule-identity-is-malformed'
            return
        end if
        if (.not. valid_atom(rule%lhs)) then
            status = fortfront_grammar_contract_malformed
            message = 'contract-rule-lhs-is-malformed'
            return
        end if
        if (rule%alternative < 0) then
            status = fortfront_grammar_contract_malformed
            message = 'contract-rule-alternative-is-negative'
            return
        end if
        if (rule%node_count < 0) then
            status = fortfront_grammar_contract_invalid_range
            message = 'contract-rule-node-count-is-negative'
            return
        end if
        if (rule%node_count > fortfront_grammar_contract_node_capacity) then
            status = fortfront_grammar_contract_capacity
            message = 'contract-rule-node-capacity-exhausted'
            return
        end if
        if (rule%node_count == 0) then
            status = fortfront_grammar_contract_invalid_range
            message = 'contract-rule-has-no-root-node'
            return
        end if
        if (rule%root < 1) then
            status = fortfront_grammar_contract_invalid_range
            message = 'contract-rule-root-is-below-one'
            return
        end if
        if (rule%root > rule%node_count) then
            status = fortfront_grammar_contract_invalid_range
            message = 'contract-rule-root-is-out-of-range'
            return
        end if
        if (.not. valid_contract_source(rule%source)) then
            status = fortfront_grammar_contract_invalid_provenance
            message = 'contract-rule-source-is-invalid'
            return
        end if
        if (.not. valid_contract_origin(rule%origin)) then
            status = fortfront_grammar_contract_malformed
            message = 'contract-rule-origin-is-invalid'
            return
        end if
        if (.not. valid_contract_resolution(rule%resolution)) then
            status = fortfront_grammar_contract_malformed
            message = 'contract-rule-resolution-is-invalid'
            return
        end if

        do i = 1, rule%node_count
            if (.not. valid_contract_node_kind(rule%nodes(i)%kind)) then
                status = fortfront_grammar_contract_invalid_kind
                message = 'contract-node-kind-is-invalid'
                return
            end if
            if (rule%nodes(i)%minimum < 0) then
                status = fortfront_grammar_contract_malformed
                message = 'contract-node-minimum-is-negative'
                return
            end if
            if (rule%nodes(i)%child_count < 0) then
                status = fortfront_grammar_contract_invalid_range
                message = 'contract-node-child-count-is-negative'
                return
            end if
            if (rule%nodes(i)%child_count == 0) then
                if (rule%nodes(i)%first_child /= 0) then
                    status = fortfront_grammar_contract_invalid_range
                    message = 'contract-leaf-has-a-child-start'
                    return
                end if
            else
                if (rule%nodes(i)%first_child < 1) then
                    status = fortfront_grammar_contract_invalid_range
                    message = 'contract-child-start-is-below-one'
                    return
                end if
                if (rule%nodes(i)%child_count > rule%node_count) then
                    status = fortfront_grammar_contract_invalid_range
                    message = 'contract-child-count-exceeds-node-count'
                    return
                end if
                last_child = rule%nodes(i)%first_child + rule%nodes(i)%child_count - 1
                if (last_child > rule%node_count) then
                    status = fortfront_grammar_contract_invalid_range
                    message = 'contract-child-range-is-out-of-range'
                    return
                end if
            end if
            if (rule%nodes(i)%kind == fortfront_grammar_node_reference .or. &
                rule%nodes(i)%kind == fortfront_grammar_node_token) then
                if (rule%nodes(i)%child_count /= 0 .or. &
                    .not. valid_atom(rule%nodes(i)%name)) then
                    status = fortfront_grammar_contract_malformed
                    message = 'contract-leaf-is-malformed'
                    return
                end if
            else if (rule%nodes(i)%kind == fortfront_grammar_node_optional .or. &
                    rule%nodes(i)%kind == fortfront_grammar_node_repeat) then
                if (rule%nodes(i)%child_count /= 1) then
                    status = fortfront_grammar_contract_malformed
                    message = 'contract-unary-node-has-the-wrong-arity'
                    return
                end if
            else if (rule%nodes(i)%kind == fortfront_grammar_node_sequence .or. &
                    rule%nodes(i)%kind == fortfront_grammar_node_choice) then
                if (rule%nodes(i)%child_count < 1) then
                    status = fortfront_grammar_contract_malformed
                    message = 'contract-nary-node-has-no-children'
                    return
                end if
            end if
        end do
    end subroutine fortfront_grammar_validate_contract_rule

    subroutine fortfront_grammar_consume_contract_rule(input, output, status, message)
        type(fortfront_grammar_contract_rule_t), intent(in) :: input
        type(fortfront_grammar_contract_rule_t), intent(out) :: output
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        output = fortfront_grammar_contract_rule_t()
        call fortfront_grammar_validate_contract_rule(input, status, message)
        if (status /= fortfront_grammar_contract_valid) return
        if (input%resolution /= fortfront_grammar_resolution_resolved) then
            status = fortfront_grammar_contract_not_accepted
            message = 'contract-rule-resolution-is-not-accepted'
            return
        end if
        output = input
    end subroutine fortfront_grammar_consume_contract_rule

    subroutine fortfront_grammar_read_contract_sx(input, output, status, message)
        character(len=*), intent(in) :: input
        type(fortfront_grammar_contract_rule_t), intent(out) :: output
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_contract_rule_t) :: parsed, accepted
        integer :: position
        logical :: ok

        output = fortfront_grammar_contract_rule_t()
        parsed = fortfront_grammar_contract_rule_t()
        position = 1
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_expect_atom(input, position, 'syntax-rule', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_atom(input, position, 'id', parsed%identity, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_int(input, position, 'alternative', parsed%alternative, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_atom(input, position, 'lhs', parsed%lhs, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_int(input, position, 'root', parsed%root, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_nodes(input, position, parsed, ok, status, message)
        if (.not. ok) return
        call sx_read_source(input, position, parsed%source, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_origin(input, position, parsed%origin, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_read_pair_resolution(input, position, parsed%resolution, ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_expect_character(input, position, ')', ok)
        call sx_skip_spaces(input, position)
        if (.not. ok .or. position <= len(input)) then
            call sx_read_failure(status, message)
            return
        end if

        call fortfront_grammar_consume_contract_rule(parsed, accepted, status, message)
        if (status == fortfront_grammar_contract_valid) output = accepted
    end subroutine fortfront_grammar_read_contract_sx

    subroutine sx_read_failure(status, message)
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        status = fortfront_grammar_contract_malformed
        message = 'malformed-standardir-grammar-v0-record'
    end subroutine sx_read_failure

    subroutine sx_skip_spaces(input, position)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position

        do while (position <= len(input))
            if (input(position:position) /= ' ' .and. input(position:position) /= achar(9) .and. &
                input(position:position) /= achar(10) .and. input(position:position) /= achar(13)) then
                return
            end if
            position = position + 1
        end do
    end subroutine sx_skip_spaces

    subroutine sx_expect_character(input, position, expected, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        character, intent(in) :: expected
        logical, intent(out) :: ok

        call sx_skip_spaces(input, position)
        ok = position <= len(input)
        if (.not. ok) return
        ok = input(position:position) == expected
        if (ok) position = position + 1
    end subroutine sx_expect_character

    subroutine sx_read_token(input, position, token, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        character(len=*), intent(out) :: token
        logical, intent(out) :: ok

        integer :: first

        token = ''
        call sx_skip_spaces(input, position)
        if (position > len(input)) then
            ok = .false.
            return
        end if
        if (input(position:position) == '(' .or. input(position:position) == ')') then
            ok = .false.
            return
        end if
        first = position
        do while (position <= len(input))
            if (input(position:position) == ' ' .or. input(position:position) == achar(9) .or. &
                input(position:position) == achar(10) .or. input(position:position) == achar(13) .or. &
                input(position:position) == '(' .or. input(position:position) == ')') then
                exit
            end if
            position = position + 1
        end do
        if (position - first > len(token)) then
            ok = .false.
            return
        end if
        token(:position - first) = input(first:position - 1)
        ok = position > first
    end subroutine sx_read_token

    subroutine sx_expect_atom(input, position, expected, ok)
        character(len=*), intent(in) :: input, expected
        integer, intent(inout) :: position
        logical, intent(out) :: ok

        character(len=256) :: token

        call sx_read_token(input, position, token, ok)
        if (ok) ok = trim(token) == expected
    end subroutine sx_expect_atom

    subroutine sx_read_pair_atom(input, position, label, value, ok)
        character(len=*), intent(in) :: input, label
        integer, intent(inout) :: position
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok

        value = ''
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, label, ok)
        if (.not. ok) return
        call sx_read_token(input, position, value, ok)
        if (.not. ok) return
        call sx_expect_character(input, position, ')', ok)
    end subroutine sx_read_pair_atom

    subroutine sx_read_pair_int(input, position, label, value, ok)
        character(len=*), intent(in) :: input, label
        integer, intent(inout) :: position
        integer, intent(out) :: value
        logical, intent(out) :: ok

        character(len=256) :: token

        value = 0
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, label, ok)
        if (.not. ok) return
        call sx_read_token(input, position, token, ok)
        if (ok) call sx_parse_integer(token, value, ok)
        if (.not. ok) return
        call sx_expect_character(input, position, ')', ok)
    end subroutine sx_read_pair_int

    subroutine sx_parse_integer(token, value, ok)
        character(len=*), intent(in) :: token
        integer, intent(out) :: value
        logical, intent(out) :: ok

        integer :: i, sign
        integer(int64) :: digit, magnitude, limit

        value = 0
        ok = len_trim(token) > 0
        if (.not. ok) return
        sign = 1
        i = 1
        if (token(i:i) == '-') then
            sign = -1
            i = i + 1
        else if (token(i:i) == '+') then
            i = i + 1
        end if
        ok = i <= len_trim(token)
        if (.not. ok) return
        magnitude = 0_int64
        limit = int(huge(value), int64)
        do while (i <= len_trim(token))
            if (token(i:i) < '0' .or. token(i:i) > '9') then
                ok = .false.
                return
            end if
            digit = int(iachar(token(i:i)) - iachar('0'), int64)
            if (magnitude > (limit - digit) / 10_int64) then
                ok = .false.
                return
            end if
            magnitude = magnitude * 10_int64 + digit
            i = i + 1
        end do
        value = sign * int(magnitude)
    end subroutine sx_parse_integer

    subroutine sx_read_nodes(input, position, rule, ok, status, message)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        type(fortfront_grammar_contract_rule_t), intent(inout) :: rule
        logical, intent(out) :: ok
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_node_t) :: node

        status = fortfront_grammar_contract_malformed
        message = ''
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_expect_atom(input, position, 'nodes', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_expect_atom(input, position, 'grammar-nodes', ok)
        if (.not. ok) then
            call sx_read_failure(status, message)
            return
        end if
        call sx_skip_spaces(input, position)
        do while (position <= len(input))
            if (input(position:position) == ')') exit
            if (rule%node_count == fortfront_grammar_contract_node_capacity) then
                status = fortfront_grammar_contract_capacity
                message = 'standardir-grammar-v0-node-capacity-exhausted'
                ok = .false.
                return
            end if
            call sx_read_node(input, position, node, ok)
            if (.not. ok) then
                call sx_read_failure(status, message)
                return
            end if
            rule%node_count = rule%node_count + 1
            rule%nodes(rule%node_count) = node
            call sx_skip_spaces(input, position)
        end do
        call sx_expect_character(input, position, ')', ok)
        if (ok) call sx_expect_character(input, position, ')', ok)
        if (.not. ok) call sx_read_failure(status, message)
    end subroutine sx_read_nodes

    subroutine sx_read_node(input, position, node, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        type(fortfront_grammar_node_t), intent(out) :: node
        logical, intent(out) :: ok

        character(len=32) :: kind

        node = fortfront_grammar_node_t()
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, 'grammar-node', ok)
        if (.not. ok) return
        call sx_read_token(input, position, kind, ok)
        if (.not. ok) return
        select case (trim(kind))
        case ('reference'); node%kind = fortfront_grammar_node_reference
        case ('token'); node%kind = fortfront_grammar_node_token
        case ('sequence'); node%kind = fortfront_grammar_node_sequence
        case ('choice'); node%kind = fortfront_grammar_node_choice
        case ('optional'); node%kind = fortfront_grammar_node_optional
        case ('repeat'); node%kind = fortfront_grammar_node_repeat
        case default
            ok = .false.
            return
        end select
        call sx_read_token(input, position, node%name, ok)
        if (.not. ok) return
        call sx_read_integer_atom(input, position, node%minimum, ok)
        if (.not. ok) return
        call sx_read_bool_atom(input, position, node%unbounded, ok)
        if (.not. ok) return
        call sx_read_integer_atom(input, position, node%first_child, ok)
        if (.not. ok) return
        call sx_read_integer_atom(input, position, node%child_count, ok)
        if (ok) call sx_expect_character(input, position, ')', ok)
    end subroutine sx_read_node

    subroutine sx_read_integer_atom(input, position, value, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=256) :: token
        call sx_read_token(input, position, token, ok)
        if (ok) call sx_parse_integer(token, value, ok)
    end subroutine sx_read_integer_atom

    subroutine sx_read_bool_atom(input, position, value, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        logical, intent(out) :: value, ok
        character(len=32) :: token
        value = .false.
        call sx_read_token(input, position, token, ok)
        if (.not. ok) return
        if (trim(token) == 'true') then
            value = .true.
        else if (trim(token) /= 'false') then
            ok = .false.
        end if
    end subroutine sx_read_bool_atom

    subroutine sx_read_source(input, position, source, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        type(fortfront_grammar_contract_source_t), intent(out) :: source
        logical, intent(out) :: ok
        integer :: page

        source = fortfront_grammar_contract_source_t()
        page = 0
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, 'source', ok)
        if (.not. ok) return
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, 'source-ref', ok)
        if (.not. ok) return
        call sx_read_pair_atom(input, position, 'document', source%document, ok)
        if (.not. ok) return
        call sx_read_pair_atom(input, position, 'clause', source%clause, ok)
        if (.not. ok) return
        call sx_read_pair_atom(input, position, 'rule', source%rule, ok)
        if (.not. ok) return
        call sx_read_pair_int(input, position, 'page', page, ok)
        if (.not. ok) return
        source%page = int(page, int64)
        call sx_read_pair_atom(input, position, 'source-hash', source%source_hash, ok)
        if (.not. ok) return
        call sx_expect_character(input, position, ')', ok)
        if (ok) call sx_expect_character(input, position, ')', ok)
    end subroutine sx_read_source

    subroutine sx_read_pair_origin(input, position, value, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        integer, intent(out) :: value
        logical, intent(out) :: ok
        call sx_read_pair_enum(input, position, 'origin', value, ok)
    end subroutine sx_read_pair_origin

    subroutine sx_read_pair_resolution(input, position, value, ok)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: position
        integer, intent(out) :: value
        logical, intent(out) :: ok
        call sx_read_pair_enum(input, position, 'resolution', value, ok)
    end subroutine sx_read_pair_resolution

    subroutine sx_read_pair_enum(input, position, label, value, ok)
        character(len=*), intent(in) :: input, label
        integer, intent(inout) :: position
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=32) :: token
        value = 0
        call sx_expect_character(input, position, '(', ok)
        if (.not. ok) return
        call sx_expect_atom(input, position, label, ok)
        if (.not. ok) return
        call sx_read_token(input, position, token, ok)
        if (.not. ok) return
        if (label == 'origin') then
            select case (trim(token))
            case ('mechanical'); value = fortfront_grammar_origin_mechanical
            case ('search'); value = fortfront_grammar_origin_search
            case ('smt'); value = fortfront_grammar_origin_smt
            case ('llm'); value = fortfront_grammar_origin_llm
            case ('llm-repair'); value = fortfront_grammar_origin_llm_repair
            case ('human'); value = fortfront_grammar_origin_human
            case ('imported'); value = fortfront_grammar_origin_imported
            case ('differential'); value = fortfront_grammar_origin_differential
            case default; ok = .false.; return
            end select
        else
            select case (trim(token))
            case ('resolved'); value = fortfront_grammar_resolution_resolved
            case ('unresolved'); value = fortfront_grammar_resolution_unresolved
            case ('disputed'); value = fortfront_grammar_resolution_disputed
            case default; ok = .false.; return
            end select
        end if
        call sx_expect_character(input, position, ')', ok)
    end subroutine sx_read_pair_enum

    subroutine fortfront_grammar_project_contract_sequence(contract_rule, output, status, &
            message)
        type(fortfront_grammar_contract_rule_t), intent(in) :: contract_rule
        type(fortfront_grammar_rule_t), intent(out) :: output
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_contract_rule_t) :: accepted
        integer :: i, node_index, validation_status
        character(len=256) :: projected_identity
        character(len=256) :: validation_message

        output = fortfront_grammar_rule_t()
        call fortfront_grammar_consume_contract_rule(contract_rule, accepted, status, message)
        if (status /= fortfront_grammar_contract_valid) return
        if (accepted%nodes(accepted%root)%kind /= fortfront_grammar_node_sequence) then
            status = fortfront_grammar_contract_not_projectable
            message = 'contract-root-is-not-a-sequence'
            return
        end if
        if (accepted%nodes(accepted%root)%child_count > fortfront_grammar_rhs_capacity) then
            status = fortfront_grammar_contract_capacity
            message = 'contract-sequence-exceeds-RHS-capacity'
            return
        end if

        projected_identity = projection_identity(accepted%identity, accepted%alternative)
        if (len_trim(projected_identity) > len(output%identity) .or. &
            .not. valid_atom(projected_identity)) then
            status = fortfront_grammar_contract_capacity
            message = 'projected-rule-identity-exceeds-capacity'
            return
        end if
        output%identity = projected_identity
        output%lhs = accepted%lhs
        output%rhs_count = accepted%nodes(accepted%root)%child_count
        do i = 1, output%rhs_count
            node_index = accepted%nodes(accepted%root)%first_child + i - 1
            if (accepted%nodes(node_index)%kind /= fortfront_grammar_node_reference .and. &
                accepted%nodes(node_index)%kind /= fortfront_grammar_node_token) then
                output = fortfront_grammar_rule_t()
                status = fortfront_grammar_contract_not_projectable
                message = 'contract-sequence-contains-a-nonleaf'
                return
            end if
            output%rhs(i)%name = accepted%nodes(node_index)%name
            if (accepted%nodes(node_index)%kind == fortfront_grammar_node_reference) then
                output%rhs(i)%kind = fortfront_grammar_symbol_reference
            else
                output%rhs(i)%kind = fortfront_grammar_symbol_token
            end if
        end do
        output%provenance%document = accepted%source%document
        output%provenance%clause = accepted%source%clause
        output%provenance%rule = accepted%source%rule
        output%provenance%page = accepted%source%page
        output%provenance%source_hash = accepted%source%source_hash
        call fortfront_grammar_validate_rule(output, validation_status, validation_message)
        if (validation_status /= fortfront_grammar_valid) then
            output = fortfront_grammar_rule_t()
            status = fortfront_grammar_contract_malformed
            message = 'projected-rule-failed-existing-boundary-validation'
            return
        end if
        status = fortfront_grammar_contract_valid
        message = 'contract-sequence-projected-to-leaf-RHS'
    end subroutine fortfront_grammar_project_contract_sequence

    subroutine fortfront_grammar_query_lhs(table, lhs, output, output_count, status, &
            message)
        type(fortfront_grammar_table_t), intent(in) :: table
        character(len=*), intent(in) :: lhs
        type(fortfront_grammar_rule_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i, rule_status, matches
        character(len=256) :: rule_message

        output = fortfront_grammar_rule_t()
        output_count = 0
        status = fortfront_grammar_query_empty
        message = ''
        if (.not. valid_atom(lhs)) then
            message = 'grammar-query-lhs-is-empty-or-malformed'
            return
        end if
        if (table%count < 0 .or. table%count > fortfront_grammar_rule_capacity) then
            status = fortfront_grammar_malformed
            message = 'grammar-table-count-is-out-of-range'
            return
        end if
        if (table%count == 0) then
            status = fortfront_grammar_query_table_empty
            message = 'grammar-table-is-empty'
            return
        end if
        matches = 0
        do i = 1, table%count
            call fortfront_grammar_validate_rule(table%rules(i), rule_status, rule_message)
            if (rule_status /= fortfront_grammar_valid) then
                status = fortfront_grammar_malformed
                message = 'grammar-table-contains-invalid-rule'
                return
            end if
            if (trim(table%rules(i)%lhs) == trim(lhs)) matches = matches + 1
        end do
        if (matches == 0) then
            status = fortfront_grammar_query_missing
            message = 'grammar-query-lhs-is-missing'
            return
        end if
        do i = 1, table%count
            if (trim(table%rules(i)%lhs) == trim(lhs)) then
                if (output_count == size(output)) then
                    status = fortfront_grammar_capacity
                    message = 'grammar-query-output-capacity-exhausted'
                    return
                end if
                output_count = output_count + 1
                output(output_count) = table%rules(i)
            end if
        end do
        status = fortfront_grammar_valid
    end subroutine fortfront_grammar_query_lhs

    logical function valid_contract_node_kind(kind)
        integer, intent(in) :: kind

        valid_contract_node_kind = kind >= fortfront_grammar_node_reference .and. &
            kind <= fortfront_grammar_node_repeat
    end function valid_contract_node_kind

    logical function valid_contract_origin(origin)
        integer, intent(in) :: origin

        valid_contract_origin = origin >= fortfront_grammar_origin_mechanical .and. &
            origin <= fortfront_grammar_origin_differential
    end function valid_contract_origin

    logical function valid_contract_resolution(resolution)
        integer, intent(in) :: resolution

        valid_contract_resolution = resolution >= fortfront_grammar_resolution_resolved .and. &
            resolution <= fortfront_grammar_resolution_disputed
    end function valid_contract_resolution

    logical function valid_contract_source(value)
        type(fortfront_grammar_contract_source_t), intent(in) :: value

        valid_contract_source = valid_atom(value%document)
        if (.not. valid_contract_source) return
        valid_contract_source = valid_atom(value%clause)
        if (.not. valid_contract_source) return
        valid_contract_source = valid_atom(value%rule)
        if (.not. valid_contract_source) return
        valid_contract_source = valid_atom(value%source_hash)
        if (.not. valid_contract_source) return
        valid_contract_source = value%page > 0_int64
    end function valid_contract_source

    function projection_identity(identity, alternative) result(value)
        character(len=*), intent(in) :: identity
        integer, intent(in) :: alternative
        character(len=256) :: value
        character(len=32) :: alternative_text

        value = ''
        if (alternative == 0) then
            value = trim(identity)
        else
            write (alternative_text, '(i0)') alternative
            value = trim(identity)//':ALT'//trim(alternative_text)
        end if
    end function projection_identity

    logical function valid_provenance(value)
        type(fortfront_grammar_provenance_t), intent(in) :: value

        valid_provenance = valid_atom(value%document)
        if (.not. valid_provenance) return
        valid_provenance = valid_atom(value%clause)
        if (.not. valid_provenance) return
        valid_provenance = valid_atom(value%rule)
        if (.not. valid_provenance) return
        valid_provenance = valid_atom(value%source_hash)
        if (.not. valid_provenance) return
        valid_provenance = value%page > 0_int64
        if (.not. valid_provenance) return
        valid_provenance = value%start_byte >= 0_int64
        if (.not. valid_provenance) return
        valid_provenance = value%end_byte >= value%start_byte
    end function valid_provenance

    logical function valid_atom(value)
        character(len=*), intent(in) :: value

        integer :: i, code

        valid_atom = len_trim(value) > 0
        if (.not. valid_atom) return
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if (code <= 32 .or. code == 127) then
                valid_atom = .false.
                return
            end if
        end do
    end function valid_atom

    logical function valid_input_symbol(value)
        type(fortfront_grammar_symbol_t), intent(in) :: value

        valid_input_symbol = valid_atom(value%name)
        if (.not. valid_input_symbol) return
        valid_input_symbol = value%kind == fortfront_grammar_symbol_reference .or. &
            value%kind == fortfront_grammar_symbol_token
    end function valid_input_symbol

end module fortfront_grammar
