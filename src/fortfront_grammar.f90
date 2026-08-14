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
