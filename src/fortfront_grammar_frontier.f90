module fortfront_grammar_frontier
    !! Bounded recognizer for the normalized flat grammar table.
    !!
    !! The input is an abstract sequence of symbol names.  The operation keeps
    !! all finite-input derivations, including epsilon and recursive ones.  It
    !! never selects one rule when more than one root rule remains viable.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_grammar, only: fortfront_grammar_invalid_provenance, &
        fortfront_grammar_rule_t, &
        fortfront_grammar_symbol_reference, &
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

    type :: frontier_chart_state_t
        integer :: rule_index = 0
        integer :: dot = 0
        integer :: start_position = 0
        integer :: current_position = 0
        logical :: uncertain = .false.
    end type frontier_chart_state_t

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
        integer, allocatable :: rule_lhs(:), rule_next(:), rule_head(:)
        logical, allocatable :: fact_unresolved(:)
        logical, allocatable :: state_seen(:, :, :, :, :), known_root(:)
        type(frontier_chart_state_t), allocatable :: states(:)
        integer, allocatable :: queue(:), waiting_head(:, :), waiting_next(:)
        integer, allocatable :: complete_head(:, :), complete_next(:)
        integer :: lhs_count, start_index, position_count, max_rhs, max_states
        integer :: i, fact_index, state_count, queue_head
        integer :: known_count, allocation_status
        integer :: table_status
        integer(int64) :: max_states_64, integer_max
        logical :: uncertain_root, valid_facts, chart_failed
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
        max_rhs = 0
        do i = 1, table%count
            max_rhs = max(max_rhs, table%rules(i)%rhs_count)
        end do
        max_states_64 = int(table%count, int64) * int(max_rhs + 1, int64) * &
            int(position_count, int64) * int(position_count, int64) * 2_int64
        integer_max = int(huge(0), int64)
        if (max_states_64 > integer_max) then
            status = fortfront_grammar_frontier_capacity
            message = 'grammar-frontier-chart-capacity-exhausted'
            return
        end if
        max_states = int(max_states_64)
        ! Each rule has at most max_rhs+1 dot positions, each span has two
        ! positions, and certainty has two values.  state_seen is this exact
        ! finite product, so the queue drains after at most max_states items.
        allocate(rule_next(max(1, table%count)), rule_head(max(1, lhs_count)))
        rule_next = 0
        rule_head = 0
        do i = table%count, 1, -1
            rule_next(i) = rule_head(rule_lhs(i))
            rule_head(rule_lhs(i)) = i
        end do
        allocate(state_seen(table%count, max_rhs + 1, position_count, position_count, 2), &
            states(max_states), queue(max_states), waiting_head(lhs_count, position_count), &
            waiting_next(max_states), complete_head(lhs_count, position_count), &
            complete_next(max_states), known_root(table%count), stat=allocation_status)
        if (allocation_status /= 0) then
            status = fortfront_grammar_frontier_capacity
            message = 'grammar-frontier-chart-capacity-exhausted'
            return
        end if
        state_seen = .false.
        waiting_head = 0
        waiting_next = 0
        complete_head = 0
        complete_next = 0
        known_root = .false.
        state_count = 0
        queue_head = 1
        chart_failed = .false.

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

        do i = 1, table%count
            if (rule_lhs(i) == start_index) then
                call chart_add_state(i, 0, 1, 1, .false.)
            end if
        end do
        do while (queue_head <= state_count)
            call chart_process_state(queue(queue_head))
            queue_head = queue_head + 1
            if (chart_failed) exit
        end do
        if (chart_failed) then
            status = fortfront_grammar_frontier_capacity
            message = 'grammar-frontier-chart-capacity-exhausted'
            return
        end if

        uncertain_root = .false.
        do i = 1, state_count
            if (states(i)%dot /= table%rules(states(i)%rule_index)%rhs_count) cycle
            if (states(i)%start_position /= 1 .or. &
                states(i)%current_position /= position_count) cycle
            if (rule_lhs(states(i)%rule_index) /= start_index) cycle
            if (states(i)%uncertain) then
                uncertain_root = .true.
            else
                known_root(states(i)%rule_index) = .true.
            end if
        end do
        known_count = 0
        do i = 1, table%count
            if (rule_lhs(i) /= start_index) cycle
            if (known_root(i)) then
                known_count = known_count + 1
                if (known_count <= size(output)) then
                    call fill_result(output(known_count), table%rules(i), input_count)
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

    contains

        subroutine chart_add_state(rule_index, dot, start_position, current_position, uncertain)
            integer, intent(in) :: rule_index, dot, start_position, current_position
            logical, intent(in) :: uncertain

            integer :: certainty_index, reference_index, lhs_index
            integer :: state_index

            certainty_index = 1
            if (uncertain) certainty_index = 2
            if (state_seen(rule_index, dot + 1, start_position, current_position, &
                certainty_index)) return
            if (state_count >= max_states) then
                chart_failed = .true.
                return
            end if
            state_count = state_count + 1
            state_index = state_count
            state_seen(rule_index, dot + 1, start_position, current_position, &
                certainty_index) = .true.
            states(state_index)%rule_index = rule_index
            states(state_index)%dot = dot
            states(state_index)%start_position = start_position
            states(state_index)%current_position = current_position
            states(state_index)%uncertain = uncertain
            queue(state_index) = state_index

            if (dot == table%rules(rule_index)%rhs_count) then
                lhs_index = rule_lhs(rule_index)
                complete_next(state_index) = complete_head(lhs_index, start_position)
                complete_head(lhs_index, start_position) = state_index
            else if (table%rules(rule_index)%rhs(dot + 1)%kind == &
                    fortfront_grammar_symbol_reference) then
                reference_index = find_name(lhs_names, lhs_count, &
                    table%rules(rule_index)%rhs(dot + 1)%name)
                if (reference_index /= 0) then
                    waiting_next(state_index) = waiting_head(reference_index, current_position)
                    waiting_head(reference_index, current_position) = state_index
                end if
            end if
        end subroutine chart_add_state

        subroutine chart_process_state(state_index)
            integer, intent(in) :: state_index

            integer :: rule_index, dot, start_position, current_position
            integer :: lhs_index, reference_index, waiting_index, completion_index
            integer :: predicted_rule, end_position
            logical :: next_uncertain

            rule_index = states(state_index)%rule_index
            dot = states(state_index)%dot
            start_position = states(state_index)%start_position
            current_position = states(state_index)%current_position
            if (dot == table%rules(rule_index)%rhs_count) then
                lhs_index = rule_lhs(rule_index)
                waiting_index = waiting_head(lhs_index, start_position)
                do while (waiting_index /= 0)
                    next_uncertain = states(waiting_index)%uncertain .or. &
                        states(state_index)%uncertain
                    call chart_add_state(states(waiting_index)%rule_index, &
                        states(waiting_index)%dot + 1, states(waiting_index)%start_position, &
                        current_position, next_uncertain)
                    waiting_index = waiting_next(waiting_index)
                end do
                return
            end if

            if (table%rules(rule_index)%rhs(dot + 1)%kind == fortfront_grammar_symbol_token) then
                if (current_position <= input_count) then
                    if (trim(input(current_position)) == &
                        trim(table%rules(rule_index)%rhs(dot + 1)%name)) then
                        call chart_add_state(rule_index, dot + 1, start_position, &
                            current_position + 1, states(state_index)%uncertain)
                    end if
                end if
                return
            end if

            reference_index = find_name(lhs_names, lhs_count, &
                table%rules(rule_index)%rhs(dot + 1)%name)
            if (states(state_index)%uncertain .or. reference_index == 0) then
                do end_position = current_position, position_count
                    call chart_add_state(rule_index, dot + 1, start_position, end_position, .true.)
                end do
            else if (fact_unresolved(reference_index)) then
                do end_position = current_position, position_count
                    call chart_add_state(rule_index, dot + 1, start_position, end_position, .true.)
                end do
            else
                predicted_rule = rule_head(reference_index)
                do while (predicted_rule /= 0)
                    call chart_add_state(predicted_rule, 0, current_position, current_position, &
                        .false.)
                    predicted_rule = rule_next(predicted_rule)
                end do
            end if
            if (reference_index /= 0) then
                completion_index = complete_head(reference_index, current_position)
                do while (completion_index /= 0)
                    next_uncertain = states(state_index)%uncertain .or. &
                        states(completion_index)%uncertain
                    call chart_add_state(rule_index, dot + 1, start_position, &
                        states(completion_index)%current_position, next_uncertain)
                    completion_index = complete_next(completion_index)
                end do
            end if
        end subroutine chart_process_state

    end subroutine fortfront_grammar_advance_frontier

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
