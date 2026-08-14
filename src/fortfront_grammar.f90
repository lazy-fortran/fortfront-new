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

    public :: fortfront_grammar_add
    public :: fortfront_grammar_collect_matches
    public :: fortfront_grammar_match_rule
    public :: fortfront_grammar_query_lhs
    public :: fortfront_grammar_reset
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
