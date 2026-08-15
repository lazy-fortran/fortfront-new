module fortfront_grammar_analysis
    !! Fixed-point nullable and first-symbol analysis for the flat rule table.
    !! Multiple rules with one LHS are choice. An empty RHS is epsilon, so the
    !! table can represent optional and repeat forms without an operator policy.
    !! Ambiguous means overlapping known first symbols, not full parser ambiguity.

    use fortfront_grammar, only: fortfront_grammar_symbol_reference, &
        fortfront_grammar_symbol_t, fortfront_grammar_symbol_token, &
        fortfront_grammar_table_t
    implicit none
    private

    integer, parameter, public :: fortfront_grammar_analysis_valid = 0
    integer, parameter, public :: fortfront_grammar_analysis_malformed = 1
    integer, parameter, public :: fortfront_grammar_analysis_capacity = 2
    integer, parameter, public :: fortfront_grammar_analysis_empty = 3
    integer, parameter, public :: fortfront_grammar_analysis_duplicate_identity = 4
    integer, parameter, public :: fortfront_grammar_analysis_unresolved = 5
    integer, parameter, public :: fortfront_grammar_analysis_ambiguous = 6
    integer, parameter, public :: fortfront_grammar_analysis_nonconvergent = 7

    integer, parameter, public :: fortfront_grammar_analysis_nullable_no = 0
    integer, parameter, public :: fortfront_grammar_analysis_nullable_yes = 1
    integer, parameter, public :: fortfront_grammar_analysis_nullable_unknown = 2

    type, public :: fortfront_grammar_analysis_result_t
        character(len=128) :: lhs = ''
        integer :: nullable_state = fortfront_grammar_analysis_nullable_unknown
        logical :: unresolved = .false.
        logical :: ambiguous = .false.
        integer :: first_count = 0
        type(fortfront_grammar_symbol_t), allocatable :: first(:)
        integer :: status = fortfront_grammar_analysis_valid
    end type fortfront_grammar_analysis_result_t

    public :: fortfront_grammar_analyze

contains

    subroutine fortfront_grammar_analyze(table, output, output_count, status, message)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        character(len=128), allocatable :: lhs_names(:)
        integer, allocatable :: rule_lhs(:), nullable_state(:)
        logical, allocatable :: unresolved(:)
        logical, allocatable :: first_present(:, :)
        character(len=128), allocatable :: first_names(:, :)
        logical, allocatable :: rule_first_present(:, :)
        character(len=128), allocatable :: rule_first_names(:, :)
        integer :: lhs_count, i, j, rule_status, iteration, max_iterations
        integer :: sequence_state, reference_lhs
        integer :: total_rhs, first_capacity
        logical :: sequence_unresolved, prefix_maybe_nullable, changed, converged
        logical :: ambiguous, any_unresolved
        character(len=256) :: rule_message

        output = fortfront_grammar_analysis_result_t()
        output_count = 0
        status = fortfront_grammar_analysis_malformed
        message = ''

        if (table%count < 0) then
            message = 'grammar-analysis-table-count-is-negative'
            return
        end if
        if (table%count > 0 .and. .not. allocated(table%rules)) then
            message = 'grammar-analysis-table-rule-storage-is-unallocated'
            return
        end if
        if (allocated(table%rules)) then
            if (table%count > size(table%rules)) then
                message = 'grammar-analysis-table-count-exceeds-storage'
                return
            end if
        end if
        if (table%count == 0) then
            status = fortfront_grammar_analysis_empty
            message = 'grammar-analysis-table-is-empty'
            return
        end if

        do i = 1, table%count
            call validate_structure(table%rules(i), rule_status, rule_message)
            if (rule_status /= fortfront_grammar_analysis_valid) then
                status = rule_status
                message = rule_message
                return
            end if
            do j = 1, i - 1
                if (trim(table%rules(j)%identity) == trim(table%rules(i)%identity)) then
                    status = fortfront_grammar_analysis_duplicate_identity
                    message = 'grammar-analysis-rule-identity-is-duplicate'
                    return
                end if
            end do
        end do

        allocate(lhs_names(table%count), rule_lhs(table%count), &
            nullable_state(table%count), unresolved(table%count))
        lhs_count = 0
        lhs_names = ''
        rule_lhs = 0
        total_rhs = 0
        do i = 1, table%count
            total_rhs = total_rhs + table%rules(i)%rhs_count
            rule_lhs(i) = find_name(lhs_names, lhs_count, table%rules(i)%lhs)
            if (rule_lhs(i) == 0) then
                lhs_count = lhs_count + 1
                lhs_names(lhs_count) = table%rules(i)%lhs
                rule_lhs(i) = lhs_count
            end if
        end do
        if (lhs_count > size(output)) then
            status = fortfront_grammar_analysis_capacity
            message = 'grammar-analysis-output-capacity-is-exceeded'
            return
        end if

        first_capacity = max(1, total_rhs)
        allocate(first_present(lhs_count, first_capacity))
        allocate(first_names(lhs_count, first_capacity))
        first_present = .false.
        first_names = ''
        nullable_state = fortfront_grammar_analysis_nullable_no
        unresolved = .false.

        max_iterations = max(1, table%count * first_capacity + table%count + 1)
        converged = .false.
        do iteration = 1, max_iterations
            changed = .false.
            do i = 1, table%count
                prefix_maybe_nullable = .true.
                sequence_unresolved = .false.
                do j = 1, table%rules(i)%rhs_count
                    if (.not. prefix_maybe_nullable) exit
                    if (table%rules(i)%rhs(j)%kind == fortfront_grammar_symbol_token) then
                        call add_first_symbol(rule_lhs(i), table%rules(i)%rhs(j)%name, &
                            first_names, first_present, changed)
                        prefix_maybe_nullable = .false.
                    else
                        reference_lhs = find_name(lhs_names, lhs_count, &
                            table%rules(i)%rhs(j)%name)
                        if (reference_lhs == 0) then
                            sequence_unresolved = .true.
                        else
                            if (unresolved(reference_lhs)) sequence_unresolved = .true.
                            call union_first_symbols(rule_lhs(i), reference_lhs, first_names, &
                                first_present, changed)
                            if (nullable_state(reference_lhs) == &
                                fortfront_grammar_analysis_nullable_yes) then
                                cycle
                            end if
                            if (nullable_state(reference_lhs) == &
                                fortfront_grammar_analysis_nullable_unknown) then
                                sequence_unresolved = .true.
                            else
                                prefix_maybe_nullable = .false.
                            end if
                        end if
                    end if
                end do

                if (prefix_maybe_nullable) then
                    if (sequence_unresolved) then
                        sequence_state = fortfront_grammar_analysis_nullable_unknown
                    else
                        sequence_state = fortfront_grammar_analysis_nullable_yes
                    end if
                else
                    sequence_state = fortfront_grammar_analysis_nullable_no
                end if
                if (sequence_state == fortfront_grammar_analysis_nullable_yes) then
                    if (nullable_state(rule_lhs(i)) /= &
                        fortfront_grammar_analysis_nullable_yes) then
                        nullable_state(rule_lhs(i)) = &
                            fortfront_grammar_analysis_nullable_yes
                        changed = .true.
                    end if
                else if (sequence_state == fortfront_grammar_analysis_nullable_unknown) then
                    if (nullable_state(rule_lhs(i)) == &
                        fortfront_grammar_analysis_nullable_no) then
                        nullable_state(rule_lhs(i)) = &
                            fortfront_grammar_analysis_nullable_unknown
                        changed = .true.
                    end if
                end if
                if (sequence_unresolved .and. .not. unresolved(rule_lhs(i))) then
                    unresolved(rule_lhs(i)) = .true.
                    changed = .true.
                end if
            end do
            if (.not. changed) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            status = fortfront_grammar_analysis_nonconvergent
            message = 'grammar-analysis-fixed-point-did-not-converge'
            return
        end if

        allocate(rule_first_present(table%count, first_capacity))
        allocate(rule_first_names(table%count, first_capacity))
        rule_first_present = .false.
        rule_first_names = ''
        do i = 1, table%count
            call compute_rule_first(table%rules(i), lhs_names, lhs_count, nullable_state, &
                first_names, first_present, rule_first_names(i, :), rule_first_present(i, :))
        end do

        do i = 1, lhs_count
            ambiguous = .false.
            do j = 1, table%count
                if (rule_lhs(j) /= i) cycle
                do rule_status = j + 1, table%count
                    if (rule_lhs(rule_status) /= i) cycle
                    if (has_intersection(rule_first_present(j, :), &
                        rule_first_names(j, :), rule_first_present(rule_status, :), &
                        rule_first_names(rule_status, :))) then
                        ambiguous = .true.
                    end if
                end do
            end do
            output(i)%lhs = lhs_names(i)
            output(i)%nullable_state = nullable_state(i)
            output(i)%unresolved = unresolved(i)
            output(i)%ambiguous = ambiguous
            output(i)%first_count = count_first(first_present(i, :))
            allocate(output(i)%first(output(i)%first_count))
            do rule_status = 1, output(i)%first_count
                output(i)%first(rule_status)%name = first_names(i, rule_status)
                output(i)%first(rule_status)%kind = fortfront_grammar_symbol_token
            end do
            if (ambiguous) then
                output(i)%status = fortfront_grammar_analysis_ambiguous
            else if (unresolved(i)) then
                output(i)%status = fortfront_grammar_analysis_unresolved
            else
                output(i)%status = fortfront_grammar_analysis_valid
            end if
        end do
        output_count = lhs_count

        any_unresolved = .false.
        ambiguous = .false.
        do i = 1, lhs_count
            if (output(i)%unresolved) any_unresolved = .true.
            if (output(i)%ambiguous) ambiguous = .true.
        end do
        if (any_unresolved) then
            status = fortfront_grammar_analysis_unresolved
            if (ambiguous) then
                message = 'grammar-analysis-is-unresolved-and-ambiguous'
            else
                message = 'grammar-analysis-contains-unresolved-reference'
            end if
        else if (ambiguous) then
            status = fortfront_grammar_analysis_ambiguous
            message = 'grammar-analysis-has-overlapping-first-symbols'
        else
            status = fortfront_grammar_analysis_valid
            message = 'grammar-analysis-fixed-point-is-exact'
        end if
    end subroutine fortfront_grammar_analyze

    subroutine validate_structure(rule, status, message)
        use fortfront_grammar, only: fortfront_grammar_rule_t
        type(fortfront_grammar_rule_t), intent(in) :: rule
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: i

        status = fortfront_grammar_analysis_valid
        message = ''
        if (.not. valid_atom(rule%identity)) then
            status = fortfront_grammar_analysis_malformed
            message = 'grammar-analysis-rule-identity-is-malformed'
            return
        end if
        if (.not. valid_atom(rule%lhs)) then
            status = fortfront_grammar_analysis_malformed
            message = 'grammar-analysis-rule-lhs-is-malformed'
            return
        end if
        if (rule%rhs_count < 0) then
            status = fortfront_grammar_analysis_malformed
            message = 'grammar-analysis-rule-rhs-count-is-out-of-range'
            return
        end if
        if (rule%rhs_count > 0 .and. .not. allocated(rule%rhs)) then
            status = fortfront_grammar_analysis_malformed
            message = 'grammar-analysis-rule-rhs-storage-is-unallocated'
            return
        end if
        if (allocated(rule%rhs)) then
            if (rule%rhs_count > size(rule%rhs)) then
                status = fortfront_grammar_analysis_malformed
                message = 'grammar-analysis-rule-rhs-count-exceeds-storage'
                return
            end if
        end if
        do i = 1, rule%rhs_count
            if (.not. valid_atom(rule%rhs(i)%name)) then
                status = fortfront_grammar_analysis_malformed
                message = 'grammar-analysis-symbol-name-is-malformed'
                return
            end if
            if (rule%rhs(i)%kind /= fortfront_grammar_symbol_reference .and. &
                rule%rhs(i)%kind /= fortfront_grammar_symbol_token) then
                status = fortfront_grammar_analysis_malformed
                message = 'grammar-analysis-symbol-kind-is-invalid'
                return
            end if
        end do
    end subroutine validate_structure

    subroutine compute_rule_first(rule, lhs_names, lhs_count, nullable_state, &
            first_names, first_present, output_names, output_present)
        use fortfront_grammar, only: fortfront_grammar_rule_t
        type(fortfront_grammar_rule_t), intent(in) :: rule
        character(len=*), intent(in) :: lhs_names(:)
        integer, intent(in) :: lhs_count
        integer, intent(in) :: nullable_state(:)
        character(len=*), intent(in) :: first_names(:, :)
        logical, intent(in) :: first_present(:, :)
        character(len=*), intent(out) :: output_names(:)
        logical, intent(out) :: output_present(:)

        integer :: i, reference_lhs
        logical :: prefix_maybe_nullable

        output_names = ''
        output_present = .false.
        prefix_maybe_nullable = .true.
        do i = 1, rule%rhs_count
            if (.not. prefix_maybe_nullable) exit
            if (rule%rhs(i)%kind == fortfront_grammar_symbol_token) then
                call add_local_symbol(rule%rhs(i)%name, output_names, output_present)
                prefix_maybe_nullable = .false.
            else
                reference_lhs = find_name(lhs_names, lhs_count, rule%rhs(i)%name)
                if (reference_lhs == 0) then
                    cycle
                end if
                call copy_local_symbols(first_names(reference_lhs, :), &
                    first_present(reference_lhs, :), output_names, output_present)
                if (nullable_state(reference_lhs) == &
                    fortfront_grammar_analysis_nullable_yes) cycle
                if (nullable_state(reference_lhs) /= &
                    fortfront_grammar_analysis_nullable_unknown) then
                    prefix_maybe_nullable = .false.
                end if
            end if
        end do
    end subroutine compute_rule_first

    subroutine add_first_symbol(lhs_index, name, first_names, first_present, changed)
        integer, intent(in) :: lhs_index
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: first_names(:, :)
        logical, intent(inout) :: first_present(:, :)
        logical, intent(inout) :: changed

        integer :: i

        do i = 1, size(first_names, 2)
            if (first_present(lhs_index, i)) then
                if (trim(first_names(lhs_index, i)) == trim(name)) return
            else
                first_present(lhs_index, i) = .true.
                first_names(lhs_index, i) = name
                changed = .true.
                return
            end if
        end do
        changed = .true.
    end subroutine add_first_symbol

    subroutine union_first_symbols(lhs_index, reference_lhs, first_names, first_present, changed)
        integer, intent(in) :: lhs_index, reference_lhs
        character(len=*), intent(inout) :: first_names(:, :)
        logical, intent(inout) :: first_present(:, :)
        logical, intent(inout) :: changed

        integer :: i

        do i = 1, size(first_names, 2)
            if (first_present(reference_lhs, i)) then
                call add_first_symbol(lhs_index, first_names(reference_lhs, i), &
                    first_names, first_present, changed)
            end if
        end do
    end subroutine union_first_symbols

    subroutine add_local_symbol(name, names, present)
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: names(:)
        logical, intent(inout) :: present(:)

        integer :: i

        do i = 1, size(names)
            if (present(i)) then
                if (trim(names(i)) == trim(name)) return
            else
                names(i) = name
                present(i) = .true.
                return
            end if
        end do
    end subroutine add_local_symbol

    subroutine copy_local_symbols(source_names, source_present, target_names, target_present)
        character(len=*), intent(in) :: source_names(:)
        logical, intent(in) :: source_present(:)
        character(len=*), intent(inout) :: target_names(:)
        logical, intent(inout) :: target_present(:)

        integer :: i

        do i = 1, size(source_names)
            if (source_present(i)) call add_local_symbol(source_names(i), target_names, &
                target_present)
        end do
    end subroutine copy_local_symbols

    logical function has_intersection(left_present, left_names, right_present, right_names)
        logical, intent(in) :: left_present(:), right_present(:)
        character(len=*), intent(in) :: left_names(:), right_names(:)

        integer :: i, j

        has_intersection = .false.
        do i = 1, size(left_names)
            if (.not. left_present(i)) cycle
            do j = 1, size(right_names)
                if (right_present(j)) then
                    if (trim(left_names(i)) == trim(right_names(j))) then
                        has_intersection = .true.
                        return
                    end if
                end if
            end do
        end do
    end function has_intersection

    integer function count_first(present)
        logical, intent(in) :: present(:)

        integer :: i

        count_first = 0
        do i = 1, size(present)
            if (present(i)) count_first = count_first + 1
        end do
    end function count_first

    integer function find_name(names, count, name)
        character(len=*), intent(in) :: names(:), name
        integer, intent(in) :: count

        integer :: i

        find_name = 0
        do i = 1, count
            if (trim(names(i)) == trim(name)) then
                find_name = i
                return
            end if
        end do
    end function find_name

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

end module fortfront_grammar_analysis
