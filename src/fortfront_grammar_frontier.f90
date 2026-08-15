module fortfront_grammar_frontier
    !! Bounded recognizer for the normalized flat grammar table.
    !!
    !! The input is an abstract sequence of symbol names.  The operation keeps
    !! all finite-input derivations, including epsilon and recursive ones.  It
    !! never selects one rule when more than one root rule remains viable.

    use fortfront_grammar, only: fortfront_grammar_invalid_provenance, &
        fortfront_grammar_rule_t, &
        fortfront_grammar_symbol_token, &
        fortfront_grammar_table_t, fortfront_grammar_validate_rule, &
        fortfront_grammar_provenance_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_ambiguous, &
        fortfront_grammar_analysis_nullable_no, &
        fortfront_grammar_analysis_nullable_unknown, &
        fortfront_grammar_analysis_nullable_yes, &
        fortfront_grammar_analysis_result_t, fortfront_grammar_analysis_unresolved, &
        fortfront_grammar_analysis_valid
    implicit none
    private

    integer, parameter, public :: fortfront_grammar_frontier_accepted = 0
    integer, parameter, public :: fortfront_grammar_frontier_rejected = 1
    integer, parameter, public :: fortfront_grammar_frontier_ambiguous = 2
    integer, parameter, public :: fortfront_grammar_frontier_unresolved = 3
    integer, parameter, public :: fortfront_grammar_frontier_malformed = 4
    integer, parameter, public :: fortfront_grammar_frontier_capacity = 5

    type, public :: fortfront_grammar_frontier_result_t
        character(len=64) :: identity = ''
        character(len=128) :: lhs = ''
        type(fortfront_grammar_provenance_t) :: provenance
        integer :: start_position = 0
        integer :: next_position = 0
        integer :: consumed = 0
    end type fortfront_grammar_frontier_result_t

    public :: fortfront_grammar_advance_frontier

contains

    subroutine fortfront_grammar_advance_frontier(table, facts, fact_count, start_lhs, &
            input, input_count, output, output_count, status, message)
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        character(len=*), intent(in) :: start_lhs
        character(len=*), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        character(len=128), allocatable :: lhs_names(:)
        integer, allocatable :: rule_lhs(:)
        logical, allocatable :: complete(:, :, :)
        logical, allocatable :: uncertain(:, :, :)
        logical, allocatable :: fact_unresolved(:)
        logical :: changed
        logical, allocatable :: known_frontier(:), unknown_frontier(:)
        logical, allocatable :: next_known(:), next_unknown(:)
        integer :: lhs_count, start_index, position_count, iteration, max_iterations
        integer :: i, j, k, end_position, reference_index, fact_index
        integer :: frontier_position
        integer :: rule_status, known_count
        integer :: table_status
        logical :: uncertain_root, valid_facts
        character(len=256) :: rule_message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_frontier_malformed
        message = ''

        if (.not. valid_atom(start_lhs)) then
            message = 'grammar-frontier-start-lhs-is-malformed'
            return
        end if
        if (input_count < 0) then
            message = 'grammar-frontier-input-count-is-negative'
            return
        end if
        if (input_count > size(input)) then
            message = 'grammar-frontier-input-count-is-out-of-range'
            return
        end if
        do i = 1, input_count
            if (.not. valid_atom(input(i))) then
                message = 'grammar-frontier-input-symbol-is-malformed'
                return
            end if
        end do

        allocate(lhs_names(max(1, table%count)), rule_lhs(max(1, table%count)))
        call validate_table(table, lhs_names, rule_lhs, lhs_count, table_status, message)
        if (table_status /= fortfront_grammar_frontier_accepted) then
            return
        end if
        call validate_facts(lhs_names, lhs_count, facts, fact_count, valid_facts, message)
        if (.not. valid_facts) return
        allocate(fact_unresolved(max(1, lhs_count)))

        if (table%count == 0) then
            status = fortfront_grammar_frontier_rejected
            message = 'grammar-frontier-table-has-no-start-candidate'
            return
        end if
        start_index = find_name(lhs_names, lhs_count, start_lhs)
        if (start_index == 0) then
            status = fortfront_grammar_frontier_rejected
            message = 'grammar-frontier-start-lhs-is-missing'
            return
        end if

        position_count = input_count + 1
        allocate(complete(lhs_count, position_count, position_count))
        allocate(uncertain(lhs_count, position_count, position_count))
        allocate(known_frontier(position_count), unknown_frontier(position_count))
        allocate(next_known(position_count), next_unknown(position_count))
        complete = .false.
        uncertain = .false.
        do i = 1, lhs_count
            fact_index = find_fact(facts, fact_count, lhs_names(i))
            fact_unresolved(i) = facts(fact_index)%unresolved
            if (facts(fact_index)%status == fortfront_grammar_analysis_unresolved) then
                fact_unresolved(i) = .true.
            end if
            if (facts(fact_index)%nullable_state == fortfront_grammar_analysis_nullable_unknown) then
                fact_unresolved(i) = .true.
            end if
        end do

        max_iterations = 2 * lhs_count * position_count * position_count + 1
        do iteration = 1, max_iterations
            changed = .false.
            do i = 1, table%count
                do j = 1, position_count
                    known_frontier = .false.
                    unknown_frontier = .false.
                    known_frontier(j) = .true.
                    do k = 1, table%rules(i)%rhs_count
                        next_known = .false.
                        next_unknown = .false.
                        if (table%rules(i)%rhs(k)%kind == fortfront_grammar_symbol_token) then
                            do frontier_position = 1, position_count
                                if (frontier_position > input_count) cycle
                                if (known_frontier(frontier_position)) then
                                    if (trim(input(frontier_position)) == &
                                        trim(table%rules(i)%rhs(k)%name)) then
                                        next_known(frontier_position + 1) = .true.
                                    end if
                                end if
                                if (unknown_frontier(frontier_position)) then
                                    if (trim(input(frontier_position)) == &
                                        trim(table%rules(i)%rhs(k)%name)) then
                                        next_unknown(frontier_position + 1) = .true.
                                    end if
                                end if
                            end do
                        else
                            reference_index = find_name(lhs_names, lhs_count, &
                                table%rules(i)%rhs(k)%name)
                            if (reference_index == 0) then
                                call mark_unknown_spans(known_frontier, unknown_frontier, &
                                    next_unknown, position_count)
                            else if (fact_unresolved(reference_index)) then
                                call mark_unknown_spans(known_frontier, unknown_frontier, &
                                    next_unknown, position_count)
                            else
                                do frontier_position = 1, position_count
                                    if (known_frontier(frontier_position)) then
                                        do end_position = frontier_position, position_count
                                            if (complete(reference_index, frontier_position, &
                                                end_position)) then
                                                next_known(end_position) = .true.
                                            end if
                                            if (uncertain(reference_index, frontier_position, &
                                                end_position)) then
                                                next_unknown(end_position) = .true.
                                            end if
                                        end do
                                    end if
                                    if (unknown_frontier(frontier_position)) then
                                        call mark_unknown_spans_from(frontier_position, &
                                            next_unknown, position_count)
                                    end if
                                end do
                            end if
                        end if
                        known_frontier = next_known
                        unknown_frontier = next_unknown
                    end do
                    do end_position = 1, position_count
                        if (known_frontier(end_position)) then
                            if (.not. complete(rule_lhs(i), j, end_position)) then
                                complete(rule_lhs(i), j, end_position) = .true.
                                changed = .true.
                            end if
                        end if
                        if (unknown_frontier(end_position)) then
                            if (.not. uncertain(rule_lhs(i), j, end_position)) then
                                uncertain(rule_lhs(i), j, end_position) = .true.
                                changed = .true.
                            end if
                        end if
                    end do
                end do
            end do
            if (.not. changed) exit
        end do

        uncertain_root = uncertain(start_index, 1, position_count)
        known_count = 0
        do i = 1, table%count
            if (rule_lhs(i) /= start_index) cycle
            if (.not. rule_can_start(table%rules(i), facts, fact_count, input, input_count)) then
                cycle
            end if
            if (complete(start_index, 1, position_count)) then
                if (rule_derives(table%rules(i), complete, lhs_names, lhs_count, input, &
                    input_count)) then
                    known_count = known_count + 1
                    if (known_count <= size(output)) then
                        call fill_result(output(known_count), table%rules(i), input_count)
                    end if
                end if
            end if
        end do

        if (known_count > size(output)) then
            output = fortfront_grammar_frontier_result_t()
            output_count = 0
            status = fortfront_grammar_frontier_capacity
            message = 'grammar-frontier-output-capacity-exhausted'
            return
        end if
        output_count = known_count
        if (uncertain_root) then
            status = fortfront_grammar_frontier_unresolved
            message = 'grammar-frontier-start-candidate-is-unresolved'
        else if (known_count == 0) then
            status = fortfront_grammar_frontier_rejected
            message = 'grammar-frontier-input-is-rejected'
        else if (known_count == 1) then
            status = fortfront_grammar_frontier_accepted
            message = 'grammar-frontier-input-is-accepted'
        else
            status = fortfront_grammar_frontier_ambiguous
            message = 'grammar-frontier-input-is-ambiguous'
        end if
    end subroutine fortfront_grammar_advance_frontier

    logical function rule_can_start(rule, facts, fact_count, input, input_count)
        type(fortfront_grammar_rule_t), intent(in) :: rule
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count, input_count
        character(len=*), intent(in) :: input(:)

        integer :: i, fact_index

        rule_can_start = .true.
        if (input_count == 0) return
        fact_index = find_fact(facts, fact_count, rule%lhs)
        if (fact_index == 0) return
        if (facts(fact_index)%first_count == 0) return
        rule_can_start = .false.
        do i = 1, facts(fact_index)%first_count
            if (trim(facts(fact_index)%first(i)%name) == trim(input(1))) then
                rule_can_start = .true.
                return
            end if
        end do
    end function rule_can_start

    subroutine validate_table(table, lhs_names, rule_lhs, lhs_count, status, message)
        type(fortfront_grammar_table_t), intent(in) :: table
        character(len=*), intent(out) :: lhs_names(:)
        integer, intent(out) :: rule_lhs(:)
        integer, intent(out) :: lhs_count, status
        character(len=*), intent(out) :: message

        integer :: i, j, rule_status
        character(len=256) :: rule_message

        lhs_names = ''
        rule_lhs = 0
        lhs_count = 0
        status = fortfront_grammar_frontier_malformed
        message = ''
        if (table%count < 0) then
            message = 'grammar-frontier-table-count-is-out-of-range'
            return
        end if
        if (table%count > 0 .and. .not. allocated(table%rules)) then
            message = 'grammar-frontier-table-rule-storage-is-unallocated'
            return
        end if
        if (allocated(table%rules)) then
            if (table%count > size(table%rules)) then
                message = 'grammar-frontier-table-count-exceeds-storage'
                return
            end if
        end if
        do i = 1, table%count
            call fortfront_grammar_validate_rule(table%rules(i), rule_status, rule_message)
            if (rule_status /= 0) then
                if (rule_status == fortfront_grammar_invalid_provenance) then
                    message = 'grammar-frontier-table-provenance-is-invalid'
                else
                    message = 'grammar-frontier-table-contains-invalid-rule'
                end if
                return
            end if
            do j = 1, i - 1
                if (trim(table%rules(j)%identity) == trim(table%rules(i)%identity)) then
                    message = 'grammar-frontier-table-has-duplicate-rule-identity'
                    return
                end if
            end do
            rule_lhs(i) = find_name(lhs_names, lhs_count, table%rules(i)%lhs)
            if (rule_lhs(i) == 0) then
                lhs_count = lhs_count + 1
                lhs_names(lhs_count) = table%rules(i)%lhs
                rule_lhs(i) = lhs_count
            end if
        end do
        status = fortfront_grammar_frontier_accepted
    end subroutine validate_table

    subroutine validate_facts(lhs_names, lhs_count, facts, fact_count, valid, message)
        character(len=*), intent(in) :: lhs_names(:)
        integer, intent(in) :: lhs_count
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        logical, intent(out) :: valid
        character(len=*), intent(out) :: message

        logical, allocatable :: found(:)
        integer :: i, j

        valid = .false.
        message = ''
        allocate(found(max(1, lhs_count)))
        found = .false.
        if (fact_count < 0 .or. fact_count > size(facts)) then
            message = 'grammar-frontier-analysis-fact-count-is-out-of-range'
            return
        end if
        if (fact_count /= lhs_count) then
            message = 'grammar-frontier-analysis-facts-do-not-cover-table'
            return
        end if
        do i = 1, fact_count
            if (.not. valid_atom(facts(i)%lhs)) then
                message = 'grammar-frontier-analysis-fact-lhs-is-malformed'
                return
            end if
            j = find_name(lhs_names, lhs_count, facts(i)%lhs)
            if (j == 0) then
                message = 'grammar-frontier-analysis-fact-lhs-is-not-in-table'
                return
            end if
            if (found(j)) then
                message = 'grammar-frontier-analysis-fact-lhs-is-duplicate'
                return
            end if
            found(j) = .true.
            if (facts(i)%nullable_state /= fortfront_grammar_analysis_nullable_no .and. &
                facts(i)%nullable_state /= fortfront_grammar_analysis_nullable_yes .and. &
                facts(i)%nullable_state /= fortfront_grammar_analysis_nullable_unknown) then
                message = 'grammar-frontier-analysis-fact-nullable-state-is-invalid'
                return
            end if
            if (facts(i)%first_count < 0) then
                message = 'grammar-frontier-analysis-fact-first-count-is-out-of-range'
                return
            end if
            if (facts(i)%first_count > 0 .and. .not. allocated(facts(i)%first)) then
                message = 'grammar-frontier-analysis-fact-first-storage-is-unallocated'
                return
            end if
            if (allocated(facts(i)%first)) then
                if (facts(i)%first_count > size(facts(i)%first)) then
                    message = 'grammar-frontier-analysis-fact-first-count-exceeds-storage'
                    return
                end if
            end if
            do j = 1, facts(i)%first_count
                if (.not. valid_atom(facts(i)%first(j)%name)) then
                    message = 'grammar-frontier-analysis-fact-first-symbol-is-malformed'
                    return
                end if
                if (facts(i)%first(j)%kind /= fortfront_grammar_symbol_token) then
                    message = 'grammar-frontier-analysis-fact-first-symbol-kind-is-invalid'
                    return
                end if
            end do
            if (facts(i)%status /= fortfront_grammar_analysis_valid .and. &
                facts(i)%status /= fortfront_grammar_analysis_ambiguous .and. &
                facts(i)%status /= fortfront_grammar_analysis_unresolved) then
                message = 'grammar-frontier-analysis-fact-status-is-invalid'
                return
            end if
        end do
        valid = .true.
    end subroutine validate_facts

    logical function rule_derives(rule, complete, lhs_names, lhs_count, input, input_count)
        type(fortfront_grammar_rule_t), intent(in) :: rule
        logical, intent(in) :: complete(:, :, :)
        character(len=*), intent(in) :: lhs_names(:)
        integer, intent(in) :: lhs_count, input_count
        character(len=*), intent(in) :: input(:)
        logical, allocatable :: frontier(:), next_frontier(:)
        integer :: i, j, reference_index

        rule_derives = .false.
        allocate(frontier(input_count + 1), next_frontier(input_count + 1))
        frontier = .false.
        next_frontier = .false.
        frontier(1) = .true.
        do i = 1, rule%rhs_count
            next_frontier = .false.
            if (rule%rhs(i)%kind == fortfront_grammar_symbol_token) then
                do j = 1, input_count
                    if (frontier(j)) then
                        if (trim(input(j)) == trim(rule%rhs(i)%name)) then
                            next_frontier(j + 1) = .true.
                        end if
                    end if
                end do
            else
                reference_index = find_name(lhs_names, lhs_count, rule%rhs(i)%name)
                if (reference_index == 0) then
                    next_frontier = .false.
                else
                    do j = 1, input_count + 1
                        if (.not. frontier(j)) cycle
                        call union_spans(complete(reference_index, j, :), next_frontier, &
                            input_count + 1)
                    end do
                end if
            end if
            frontier = next_frontier
        end do
        rule_derives = frontier(input_count + 1)
    end function rule_derives

    subroutine fill_result(output, rule, input_count)
        type(fortfront_grammar_frontier_result_t), intent(out) :: output
        type(fortfront_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: input_count

        output = fortfront_grammar_frontier_result_t()
        output%identity = rule%identity
        output%lhs = rule%lhs
        output%provenance = rule%provenance
        output%start_position = 1
        output%next_position = input_count + 1
        output%consumed = input_count
    end subroutine fill_result

    subroutine mark_unknown_spans(known, unknown, output, position_count)
        logical, intent(in) :: known(:), unknown(:)
        logical, intent(inout) :: output(:)
        integer, intent(in) :: position_count

        integer :: i

        do i = 1, position_count
            if (known(i) .or. unknown(i)) call mark_unknown_spans_from(i, output, position_count)
        end do
    end subroutine mark_unknown_spans

    subroutine mark_unknown_spans_from(start_position, output, position_count)
        integer, intent(in) :: start_position, position_count
        logical, intent(inout) :: output(:)

        integer :: i

        do i = start_position, position_count
            output(i) = .true.
        end do
    end subroutine mark_unknown_spans_from

    subroutine union_spans(source, output, position_count)
        logical, intent(in) :: source(:)
        logical, intent(inout) :: output(:)
        integer, intent(in) :: position_count

        integer :: i

        do i = 1, position_count
            if (source(i)) output(i) = .true.
        end do
    end subroutine union_spans

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

    integer function find_fact(facts, fact_count, lhs)
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        character(len=*), intent(in) :: lhs

        integer :: i

        find_fact = 0
        do i = 1, fact_count
            if (trim(facts(i)%lhs) == trim(lhs)) then
                find_fact = i
                return
            end if
        end do
    end function find_fact

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

end module fortfront_grammar_frontier
