module fortfront_grammar_runtime
    !! Executable consumer for standardir-grammar-v0 expressions.
    !!
    !! Contract nodes are lowered to the existing flat grammar table.  The
    !! frontier and session remain the only recognizer; this module supplies
    !! the generic contract-to-table bridge and no source-language dispatch.

    use, intrinsic :: iso_fortran_env, only: int64, iostat_end
    use fortfront_grammar, only: fortfront_grammar_add, fortfront_grammar_capacity, &
        fortfront_grammar_consume_contract_rule, fortfront_grammar_contract_rule_t, &
        fortfront_grammar_contract_capacity, &
        fortfront_grammar_contract_valid, fortfront_grammar_node_choice, &
        fortfront_grammar_node_optional, fortfront_grammar_node_reference, &
        fortfront_grammar_node_repeat, fortfront_grammar_node_sequence, &
        fortfront_grammar_node_token, fortfront_grammar_rule_t, &
        fortfront_grammar_resolution_resolved, &
        fortfront_grammar_read_contract_sx, &
        fortfront_grammar_symbol_reference, fortfront_grammar_symbol_t, &
        fortfront_grammar_symbol_token, fortfront_grammar_table_t, &
        fortfront_grammar_validate_contract_rule, fortfront_grammar_valid
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_ambiguous, &
        fortfront_grammar_analysis_capacity, fortfront_grammar_analysis_result_t, &
        fortfront_grammar_analysis_unresolved, fortfront_grammar_analysis_valid, &
        fortfront_grammar_analyze
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_accepted, &
        fortfront_grammar_frontier_ambiguous, fortfront_grammar_frontier_capacity, &
        fortfront_grammar_frontier_malformed, fortfront_grammar_frontier_rejected, &
        fortfront_grammar_frontier_result_t, fortfront_grammar_frontier_unresolved
    use fortfront_grammar_session, only: fortfront_grammar_session_finalize, &
        fortfront_grammar_session_initialize, fortfront_grammar_session_initialized, &
        fortfront_grammar_session_push, fortfront_grammar_session_t
    implicit none
    private

    integer, parameter, public :: fortfront_grammar_runtime_accepted = &
        fortfront_grammar_frontier_accepted
    integer, parameter, public :: fortfront_grammar_runtime_rejected = &
        fortfront_grammar_frontier_rejected
    integer, parameter, public :: fortfront_grammar_runtime_ambiguous = &
        fortfront_grammar_frontier_ambiguous
    integer, parameter, public :: fortfront_grammar_runtime_unresolved = &
        fortfront_grammar_frontier_unresolved
    integer, parameter, public :: fortfront_grammar_runtime_malformed = &
        fortfront_grammar_frontier_malformed
    integer, parameter, public :: fortfront_grammar_runtime_capacity = &
        fortfront_grammar_frontier_capacity
    integer, parameter, public :: fortfront_grammar_runtime_initialized = 6
    integer, parameter :: runtime_line_capacity = 262144

    type, public :: fortfront_grammar_runtime_t
        private
        type(fortfront_grammar_session_t) :: session
        logical :: initialized = .false.
    end type fortfront_grammar_runtime_t

    public :: fortfront_grammar_runtime_finalize
    public :: fortfront_grammar_runtime_initialize
    public :: fortfront_grammar_runtime_load_file
    public :: fortfront_grammar_runtime_push
    public :: fortfront_grammar_runtime_status_name

    type :: runtime_builder_t
        type(fortfront_grammar_table_t) :: table
        integer :: source_rule_index = 0
    end type runtime_builder_t

contains

    subroutine fortfront_grammar_runtime_load_file(runtime, path, start_lhs, rule_count, &
            line_count, status, message)
        type(fortfront_grammar_runtime_t), intent(out) :: runtime
        character(len=*), intent(in) :: path, start_lhs
        integer, intent(out) :: rule_count, line_count, status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_contract_rule_t), allocatable :: rules(:)
        type(fortfront_grammar_contract_rule_t) :: rule
        integer :: unit, io_status, parse_status, allocation_status
        character(len=runtime_line_capacity) :: line
        character(len=256) :: io_message, parse_message

        runtime = fortfront_grammar_runtime_t()
        rule_count = 0
        line_count = 0
        status = fortfront_grammar_runtime_malformed
        message = ''
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=io_status, &
            iomsg=io_message)
        if (io_status /= 0) then
            message = 'grammar-runtime-file-open-failed: '//trim(io_message)
            return
        end if

        allocate(rules(0))
        do
            read (unit, '(A)', iostat=io_status, iomsg=io_message) line
            if (io_status == iostat_end) exit
            if (io_status /= 0) then
                close (unit)
                message = 'grammar-runtime-file-read-failed: '//trim(io_message)
                return
            end if
            line_count = line_count + 1
            call fortfront_grammar_read_contract_sx(trim(line), rule, parse_status, parse_message)
            if (parse_status /= fortfront_grammar_contract_valid) then
                close (unit)
                message = 'grammar-runtime-line-'//trim(integer_text(line_count))// &
                    '-malformed: '//trim(parse_message)
                return
            end if
            call append_rule(rules, rule, allocation_status)
            if (allocation_status /= 0) then
                close (unit)
                status = fortfront_grammar_runtime_capacity
                message = 'grammar-runtime-rule-storage-allocation-failed'
                return
            end if
            rule_count = rule_count + 1
        end do
        close (unit)

        call fortfront_grammar_runtime_initialize(runtime, rules, rule_count, start_lhs, status, &
            message)
    end subroutine fortfront_grammar_runtime_load_file

    pure function fortfront_grammar_runtime_status_name(status) result(name)
        integer, intent(in) :: status
        character(len=16) :: name

        select case (status)
        case (fortfront_grammar_runtime_accepted)
            name = 'accepted'
        case (fortfront_grammar_runtime_rejected)
            name = 'rejected'
        case (fortfront_grammar_runtime_ambiguous)
            name = 'ambiguous'
        case (fortfront_grammar_runtime_unresolved)
            name = 'unresolved'
        case (fortfront_grammar_runtime_malformed)
            name = 'malformed'
        case (fortfront_grammar_runtime_initialized)
            name = 'initialized'
        case (fortfront_grammar_runtime_capacity)
            name = 'capacity'
        case default
            name = 'unknown'
        end select
    end function fortfront_grammar_runtime_status_name

    subroutine fortfront_grammar_runtime_initialize(runtime, rules, rule_count, start_lhs, &
            status, message)
        type(fortfront_grammar_runtime_t), intent(out) :: runtime
        type(fortfront_grammar_contract_rule_t), intent(in) :: rules(:)
        integer, intent(in) :: rule_count
        character(len=*), intent(in) :: start_lhs
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(runtime_builder_t) :: builder
        type(fortfront_grammar_contract_rule_t) :: accepted
        type(fortfront_grammar_symbol_t), allocatable :: alternatives(:, :)
        integer, allocatable :: lengths(:)
        type(fortfront_grammar_analysis_result_t), allocatable :: facts(:)
        integer :: alternative_count, i, j, validation_status, analysis_status
        integer :: fact_count, session_status
        logical :: unresolved
        character(len=256) :: local_message, analysis_message

        runtime = fortfront_grammar_runtime_t()
        status = fortfront_grammar_runtime_malformed
        message = ''
        if (rule_count < 1 .or. rule_count > size(rules)) then
            message = 'grammar-runtime-rule-count-is-out-of-range'
            return
        end if
        builder%table = fortfront_grammar_table_t()
        unresolved = .false.
        do i = 1, rule_count
            call fortfront_grammar_validate_contract_rule(rules(i), validation_status, &
                local_message)
            if (validation_status /= fortfront_grammar_contract_valid) then
                call map_contract_failure(validation_status, status, message, local_message)
                return
            end if
            builder%source_rule_index = i
            if (rules(i)%resolution /= fortfront_grammar_resolution_resolved) then
                unresolved = .true.
                call add_unresolved_rule(builder, rules(i), i, status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
                cycle
            end if
            call fortfront_grammar_consume_contract_rule(rules(i), accepted, validation_status, &
                local_message)
            if (validation_status /= fortfront_grammar_contract_valid) then
                call map_contract_failure(validation_status, status, message, local_message)
                return
            end if
            call validate_node_tree(accepted, accepted%root, status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
            alternative_count = 0
            call lower_node(builder, accepted, accepted%root, alternatives, lengths, &
                alternative_count, status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
            do j = 1, alternative_count
                call add_lowered_rule(builder, accepted, alternatives(j, :), lengths(j), &
                    alternative_count, j, status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
            end do
        end do

        if (builder%table%count == 0) then
            status = fortfront_grammar_runtime_malformed
            message = 'grammar-runtime-produced-no-rules'
            return
        end if
        allocate(facts(max(1, builder%table%count)))
        call fortfront_grammar_analyze(builder%table, facts, fact_count, analysis_status, &
            analysis_message)
        if (analysis_status == fortfront_grammar_analysis_capacity) then
            status = fortfront_grammar_runtime_capacity
            message = 'grammar-runtime-analysis-capacity-exhausted'
            return
        end if
        if (analysis_status /= fortfront_grammar_analysis_valid .and. &
            analysis_status /= fortfront_grammar_analysis_ambiguous .and. &
            analysis_status /= fortfront_grammar_analysis_unresolved) then
            status = fortfront_grammar_runtime_malformed
            message = 'grammar-runtime-analysis-failed: '//trim(analysis_message)
            return
        end if
        call fortfront_grammar_session_initialize(runtime%session, builder%table, facts, &
            fact_count, start_lhs, session_status, message)
        if (session_status /= fortfront_grammar_session_initialized) then
            status = fortfront_grammar_runtime_malformed
            if (len_trim(message) == 0) message = 'grammar-runtime-session-initialization-failed'
            return
        end if
        runtime%initialized = .true.
        status = fortfront_grammar_runtime_initialized
        if (unresolved) then
            message = 'grammar-runtime-initialized-with-unresolved-rules'
        else
            message = 'grammar-runtime-is-initialized'
        end if
    end subroutine fortfront_grammar_runtime_initialize

    subroutine fortfront_grammar_runtime_push(runtime, token, output, output_count, status, &
            message)
        type(fortfront_grammar_runtime_t), intent(inout) :: runtime
        character(len=*), intent(in) :: token
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_runtime_malformed
        message = ''
        if (.not. runtime%initialized) then
            message = 'grammar-runtime-is-not-initialized'
            return
        end if
        call fortfront_grammar_session_push(runtime%session, token, output, output_count, &
            status, message)
    end subroutine fortfront_grammar_runtime_push

    subroutine fortfront_grammar_runtime_finalize(runtime, output, output_count, status, message)
        type(fortfront_grammar_runtime_t), intent(inout) :: runtime
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_runtime_malformed
        message = ''
        if (.not. runtime%initialized) then
            message = 'grammar-runtime-is-not-initialized'
            return
        end if
        call fortfront_grammar_session_finalize(runtime%session, output, output_count, status, &
            message)
    end subroutine fortfront_grammar_runtime_finalize

    subroutine append_rule(rules, rule, allocation_status)
        type(fortfront_grammar_contract_rule_t), allocatable, intent(inout) :: rules(:)
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(out) :: allocation_status

        type(fortfront_grammar_contract_rule_t), allocatable :: replacement(:)
        integer :: old_count, new_count

        allocation_status = 0
        old_count = size(rules)
        new_count = max(1, 2 * old_count)
        allocate(replacement(new_count), stat=allocation_status)
        if (allocation_status /= 0) return
        if (old_count > 0) replacement(1:old_count) = rules
        replacement(old_count + 1) = rule
        call move_alloc(replacement, rules)
    end subroutine append_rule

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function integer_text

    recursive subroutine validate_node_tree(rule, node_index, status, message, seen, visiting)
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(in) :: node_index
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        logical, intent(inout), optional :: seen(:), visiting(:)

        logical, allocatable :: local_seen(:), local_visiting(:)
        integer :: i, child, child_end

        if (.not. present(seen)) then
            allocate(local_seen(rule%node_count), local_visiting(rule%node_count))
            local_seen = .false.
            local_visiting = .false.
            call validate_node_tree(rule, node_index, status, message, local_seen, &
                local_visiting)
            return
        end if
        status = fortfront_grammar_runtime_malformed
        message = ''
        if (node_index < 1 .or. node_index > rule%node_count) then
            message = 'grammar-runtime-node-index-is-out-of-range'
            return
        end if
        if (visiting(node_index)) then
            message = 'grammar-runtime-contract-node-tree-is-cyclic'
            return
        end if
        if (seen(node_index)) then
            message = 'grammar-runtime-contract-node-tree-shares-a-node'
            return
        end if
        visiting(node_index) = .true.
        if (rule%nodes(node_index)%kind == fortfront_grammar_node_reference .or. &
            rule%nodes(node_index)%kind == fortfront_grammar_node_token) then
            if (rule%nodes(node_index)%child_count /= 0 .or. &
                rule%nodes(node_index)%first_child /= 0 .or. &
                rule%nodes(node_index)%minimum /= 1 .or. &
                rule%nodes(node_index)%unbounded) then
                message = 'grammar-runtime-leaf-metadata-is-invalid'
                visiting(node_index) = .false.
                return
            end if
        else if (rule%nodes(node_index)%kind == fortfront_grammar_node_sequence .or. &
                rule%nodes(node_index)%kind == fortfront_grammar_node_choice) then
            if (rule%nodes(node_index)%child_count < 1 .or. &
                rule%nodes(node_index)%first_child < 1 .or. &
                rule%nodes(node_index)%minimum /= 1 .or. &
                rule%nodes(node_index)%unbounded) then
                message = 'grammar-runtime-group-metadata-is-invalid'
                visiting(node_index) = .false.
                return
            end if
        else if (rule%nodes(node_index)%kind == fortfront_grammar_node_optional) then
            if (rule%nodes(node_index)%child_count /= 1 .or. &
                rule%nodes(node_index)%first_child < 1 .or. &
                rule%nodes(node_index)%minimum /= 0 .or. &
                rule%nodes(node_index)%unbounded) then
                message = 'grammar-runtime-optional-metadata-is-invalid'
                visiting(node_index) = .false.
                return
            end if
        else if (rule%nodes(node_index)%kind == fortfront_grammar_node_repeat) then
            if (rule%nodes(node_index)%child_count /= 1 .or. &
                rule%nodes(node_index)%first_child < 1 .or. &
                (rule%nodes(node_index)%minimum /= 0 .and. &
                rule%nodes(node_index)%minimum /= 1) .or. &
                .not. rule%nodes(node_index)%unbounded) then
                message = 'grammar-runtime-repeat-metadata-is-invalid'
                visiting(node_index) = .false.
                return
            end if
        else
            message = 'grammar-runtime-contract-node-kind-is-invalid'
            visiting(node_index) = .false.
            return
        end if
        if (rule%nodes(node_index)%child_count > 0) then
            child = rule%nodes(node_index)%first_child
            do i = 1, rule%nodes(node_index)%child_count
                call validate_node_tree(rule, child, status, message, seen, visiting)
                if (status /= fortfront_grammar_runtime_accepted) then
                    visiting(node_index) = .false.
                    return
                end if
                child_end = subtree_end(rule, child)
                child = child_end + 1
            end do
        end if
        seen(node_index) = .true.
        visiting(node_index) = .false.
        status = fortfront_grammar_runtime_accepted
    end subroutine validate_node_tree

    recursive integer function subtree_end(rule, node_index) result(last_node)
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(in) :: node_index

        integer :: i, child

        last_node = node_index
        if (rule%nodes(node_index)%child_count == 0) return
        child = rule%nodes(node_index)%first_child
        do i = 1, rule%nodes(node_index)%child_count
            last_node = subtree_end(rule, child)
            child = last_node + 1
        end do
    end function subtree_end

    recursive subroutine lower_node(builder, rule, node_index, output, lengths, output_count, &
            status, message)
        type(runtime_builder_t), intent(inout) :: builder
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(in) :: node_index
        type(fortfront_grammar_symbol_t), allocatable, intent(out) :: output(:, :)
        integer, allocatable, intent(out) :: lengths(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_symbol_t), allocatable :: child_output(:, :)
        type(fortfront_grammar_symbol_t), allocatable :: combined(:, :)
        integer, allocatable :: child_lengths(:), combined_lengths(:)
        integer :: child, child_end, i, j, k, child_count, combined_count
        integer :: initial_rhs_capacity

        initial_rhs_capacity = max(1, rule%node_count + 1)
        allocate(output(max(1, rule%node_count), initial_rhs_capacity), &
            lengths(max(1, rule%node_count)))
        output = fortfront_grammar_symbol_t()
        lengths = 0
        output_count = 0
        status = fortfront_grammar_runtime_malformed
        message = ''
        select case (rule%nodes(node_index)%kind)
        case (fortfront_grammar_node_reference, fortfront_grammar_node_token)
            output_count = 1
            lengths(1) = 1
            output(1, 1)%name = rule%nodes(node_index)%name
            if (rule%nodes(node_index)%kind == fortfront_grammar_node_reference) then
                output(1, 1)%kind = fortfront_grammar_symbol_reference
            else
                output(1, 1)%kind = fortfront_grammar_symbol_token
            end if
        case (fortfront_grammar_node_choice)
            child = rule%nodes(node_index)%first_child
            do i = 1, rule%nodes(node_index)%child_count
                call lower_node(builder, rule, child, child_output, child_lengths, &
                    child_count, status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
                do j = 1, child_count
                    call append_alternative(output, lengths, output_count, child_output(j, :), &
                        child_lengths(j), status, message)
                    if (status /= fortfront_grammar_runtime_accepted) return
                end do
                child_end = subtree_end(rule, child)
                child = child_end + 1
            end do
        case (fortfront_grammar_node_optional)
            output_count = 1
            lengths(1) = 0
            child = rule%nodes(node_index)%first_child
            call lower_node(builder, rule, child, child_output, child_lengths, child_count, &
                status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
            do j = 1, child_count
                call append_alternative(output, lengths, output_count, child_output(j, :), &
                    child_lengths(j), status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
            end do
        case (fortfront_grammar_node_sequence)
            output_count = 1
            lengths(1) = 0
            child = rule%nodes(node_index)%first_child
            do i = 1, rule%nodes(node_index)%child_count
                call lower_node(builder, rule, child, child_output, child_lengths, &
                    child_count, status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
                call concatenate(output, lengths, output_count, child_output, child_lengths, &
                    child_count, combined, combined_lengths, combined_count, status, message)
                if (status /= fortfront_grammar_runtime_accepted) return
                output = combined
                lengths = combined_lengths
                output_count = combined_count
                child_end = subtree_end(rule, child)
                child = child_end + 1
            end do
        case (fortfront_grammar_node_repeat)
            child = rule%nodes(node_index)%first_child
            call lower_node(builder, rule, child, child_output, child_lengths, child_count, &
                status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
            call add_repeat_rules(builder, rule, node_index, child_output, child_lengths, &
                child_count, status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
            if (rule%nodes(node_index)%minimum == 0) then
                output_count = 1
                lengths(1) = 1
                output(1, 1)%name = repeat_name(builder%source_rule_index, node_index)
                output(1, 1)%kind = fortfront_grammar_symbol_reference
            else
                deallocate(output, lengths)
                allocate(output(max(1, child_count), max(1, maxval(child_lengths) + 1)), &
                    lengths(max(1, child_count)))
                output = fortfront_grammar_symbol_t()
                lengths = 0
                output_count = child_count
                do k = 1, child_count
                    lengths(k) = child_lengths(k) + 1
                    if (child_lengths(k) > 0) output(k, 1:child_lengths(k)) = &
                        child_output(k, 1:child_lengths(k))
                    output(k, lengths(k))%name = repeat_name(builder%source_rule_index, &
                        node_index)
                    output(k, lengths(k))%kind = fortfront_grammar_symbol_reference
                end do
            end if
        case default
            message = 'grammar-runtime-cannot-lower-contract-node'
            return
        end select
        status = fortfront_grammar_runtime_accepted
    end subroutine lower_node

    subroutine concatenate(left, left_lengths, left_count, right, right_lengths, right_count, &
            output, output_lengths, output_count, status, message)
        type(fortfront_grammar_symbol_t), intent(in) :: left(:, :), right(:, :)
        integer, intent(in) :: left_lengths(:), left_count, right_lengths(:), right_count
        type(fortfront_grammar_symbol_t), allocatable, intent(out) :: output(:, :)
        integer, allocatable, intent(out) :: output_lengths(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        integer :: i, j, result_count, rhs_capacity

        rhs_capacity = max(1, max(maxval(left_lengths(1:max(1, left_count))), &
            maxval(right_lengths(1:max(1, right_count)))))
        allocate(output(max(1, left_count * right_count), rhs_capacity), &
            output_lengths(max(1, left_count * right_count)))
        output = fortfront_grammar_symbol_t()
        output_lengths = 0
        status = fortfront_grammar_runtime_accepted
        message = ''
        result_count = 0
        do i = 1, left_count
            do j = 1, right_count
                call append_alternative(output, output_lengths, result_count, right(j, :), &
                    right_lengths(j), status, message, left(i, :), left_lengths(i))
                if (status /= fortfront_grammar_runtime_accepted) return
                output_lengths(result_count) = left_lengths(i) + right_lengths(j)
            end do
        end do
        output_count = result_count
    end subroutine concatenate

    subroutine append_alternative(output, lengths, output_count, right, right_count, status, &
            message, left, left_count)
        type(fortfront_grammar_symbol_t), allocatable, intent(inout) :: output(:, :)
        integer, allocatable, intent(inout) :: lengths(:)
        integer, intent(inout) :: output_count
        type(fortfront_grammar_symbol_t), intent(in) :: right(:)
        integer, intent(in) :: right_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        type(fortfront_grammar_symbol_t), intent(in), optional :: left(:)
        integer, intent(in), optional :: left_count

        integer :: local_left_count, required_rhs, cursor

        status = fortfront_grammar_runtime_accepted
        message = ''
        local_left_count = 0
        if (present(left)) local_left_count = left_count
        required_rhs = local_left_count + right_count
        call reserve_alternatives(output, lengths, output_count + 1, required_rhs, status, &
            message)
        if (status /= fortfront_grammar_runtime_accepted) return
        output_count = output_count + 1
        lengths(output_count) = required_rhs
        cursor = 0
        if (local_left_count > 0) then
            output(output_count, 1:local_left_count) = left(1:local_left_count)
            cursor = local_left_count
        end if
        if (right_count > 0) output(output_count, cursor + 1:required_rhs) = &
            right(1:right_count)
    end subroutine append_alternative

    subroutine reserve_alternatives(output, lengths, required_count, required_rhs, status, &
            message)
        type(fortfront_grammar_symbol_t), allocatable, intent(inout) :: output(:, :)
        integer, allocatable, intent(inout) :: lengths(:)
        integer, intent(in) :: required_count, required_rhs
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_symbol_t), allocatable :: replacement(:, :)
        integer, allocatable :: replacement_lengths(:)
        integer :: alternative_capacity, rhs_capacity, allocation_status

        status = fortfront_grammar_runtime_accepted
        message = ''
        alternative_capacity = max(required_count, max(1, 2 * size(lengths)))
        rhs_capacity = max(required_rhs, max(1, 2 * size(output, 2)))
        if (required_count <= size(lengths) .and. required_rhs <= size(output, 2)) return
        allocate(replacement(alternative_capacity, rhs_capacity), &
            replacement_lengths(alternative_capacity), stat=allocation_status)
        if (allocation_status /= 0) then
            status = fortfront_grammar_runtime_capacity
            message = 'grammar-runtime-alternative-storage-allocation-failed'
            return
        end if
        replacement = fortfront_grammar_symbol_t()
        replacement_lengths = 0
        if (size(lengths) > 0) then
            replacement_lengths(1:size(lengths)) = lengths
            replacement(:, 1:size(output, 2)) = output
        end if
        call move_alloc(replacement, output)
        call move_alloc(replacement_lengths, lengths)
    end subroutine reserve_alternatives

    subroutine add_repeat_rules(builder, rule, node_index, child, child_lengths, child_count, &
            status, message)
        type(runtime_builder_t), intent(inout) :: builder
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(in) :: node_index, child_count
        type(fortfront_grammar_symbol_t), intent(in) :: child(:, :)
        integer, intent(in) :: child_lengths(:)
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_symbol_t), allocatable :: sequence(:)
        integer :: i, length
        character(len=128) :: lhs

        lhs = repeat_name(builder%source_rule_index, node_index)
        allocate(sequence(0))
        call add_flat_rule(builder, rule, repeat_identity(builder%source_rule_index, node_index, &
            0), lhs, sequence, 0, status, message)
        if (status /= fortfront_grammar_runtime_accepted) return
        do i = 1, child_count
            length = child_lengths(i) + 1
            deallocate(sequence)
            allocate(sequence(length))
            sequence = fortfront_grammar_symbol_t()
            if (child_lengths(i) > 0) sequence(1:child_lengths(i)) = child(i, &
                1:child_lengths(i))
            sequence(length)%name = lhs
            sequence(length)%kind = fortfront_grammar_symbol_reference
            call add_flat_rule(builder, rule, repeat_identity(builder%source_rule_index, &
                node_index, i), lhs, sequence, length, status, message)
            if (status /= fortfront_grammar_runtime_accepted) return
        end do
    end subroutine add_repeat_rules

    subroutine add_lowered_rule(builder, rule, symbols, length, alternative_count, alternative, &
            status, message)
        type(runtime_builder_t), intent(inout) :: builder
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        type(fortfront_grammar_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: length, alternative_count, alternative
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        character(len=128) :: identity

        identity = base_identity(rule%identity, rule%alternative)
        if (alternative_count > 1) identity = identity_with_suffix(identity, 'BR', alternative)
        call add_flat_rule(builder, rule, identity, rule%lhs, symbols, length, status, message)
    end subroutine add_lowered_rule

    subroutine add_unresolved_rule(builder, rule, rule_index, status, message)
        type(runtime_builder_t), intent(inout) :: builder
        type(fortfront_grammar_contract_rule_t), intent(in) :: rule
        integer, intent(in) :: rule_index
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_symbol_t) :: symbols(1)
        character(len=128) :: missing_name, identity

        symbols = fortfront_grammar_symbol_t()
        missing_name = unresolved_name(rule_index)
        symbols(1)%name = missing_name
        symbols(1)%kind = fortfront_grammar_symbol_reference
        identity = base_identity(rule%identity, rule%alternative)
        identity = identity_with_suffix(identity, 'UNRESOLVED', rule_index)
        call add_flat_rule(builder, rule, identity, rule%lhs, symbols, 1, status, message)
    end subroutine add_unresolved_rule

    subroutine add_flat_rule(builder, contract_rule, identity, lhs, symbols, length, status, &
            message)
        type(runtime_builder_t), intent(inout) :: builder
        type(fortfront_grammar_contract_rule_t), intent(in) :: contract_rule
        character(len=*), intent(in) :: identity, lhs
        type(fortfront_grammar_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: length
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_rule_t) :: rule
        integer :: add_status

        status = fortfront_grammar_runtime_malformed
        message = ''
        if (len_trim(identity) > len(rule%identity) .or. len_trim(lhs) > len(rule%lhs)) then
            status = fortfront_grammar_runtime_capacity
            message = 'grammar-runtime-generated-name-capacity-exhausted'
            return
        end if
        if (length < 0 .or. length > size(symbols)) then
            status = fortfront_grammar_runtime_capacity
            message = 'grammar-runtime-generated-RHS-capacity-exhausted'
            return
        end if
        rule = fortfront_grammar_rule_t()
        rule%identity = identity
        rule%lhs = lhs
        rule%rhs_count = length
        allocate(rule%rhs(length))
        if (length > 0) rule%rhs = symbols(1:length)
        rule%provenance%document = contract_rule%source%document
        rule%provenance%clause = contract_rule%source%clause
        rule%provenance%rule = contract_rule%source%rule
        rule%provenance%page = int(contract_rule%source%page, int64)
        rule%provenance%source_hash = contract_rule%source%source_hash
        call fortfront_grammar_add(builder%table, rule, add_status, message)
        if (add_status == fortfront_grammar_valid) then
            status = fortfront_grammar_runtime_accepted
        else if (add_status == fortfront_grammar_capacity) then
            status = fortfront_grammar_runtime_capacity
        else
            status = fortfront_grammar_runtime_malformed
            message = 'grammar-runtime-generated-rule-rejected: '//trim(message)
        end if
    end subroutine add_flat_rule

    subroutine map_contract_failure(contract_status, status, message, detail)
        integer, intent(in) :: contract_status
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        character(len=*), intent(in) :: detail

        if (contract_status == fortfront_grammar_contract_capacity) then
            status = fortfront_grammar_runtime_capacity
        else
            status = fortfront_grammar_runtime_malformed
        end if
        message = 'grammar-runtime-contract-rejected: '//trim(detail)
    end subroutine map_contract_failure

    function base_identity(identity, alternative) result(value)
        character(len=*), intent(in) :: identity
        integer, intent(in) :: alternative
        character(len=128) :: value
        character(len=32) :: number

        value = trim(identity)
        if (alternative /= 0) then
            write (number, '(i0)') alternative
            value = trim(value)//':ALT'//trim(number)
        end if
    end function base_identity

    function identity_with_suffix(identity, label, number_value) result(value)
        character(len=*), intent(in) :: identity, label
        integer, intent(in) :: number_value
        character(len=128) :: value
        character(len=32) :: number

        write (number, '(i0)') number_value
        value = trim(identity)//':'//trim(label)//trim(number)
    end function identity_with_suffix

    function repeat_name(rule_index, node_index) result(value)
        integer, intent(in) :: rule_index, node_index
        character(len=128) :: value

        write (value, '("__runtime_repeat_",i0,"_",i0)') rule_index, node_index
    end function repeat_name

    function repeat_identity(rule_index, node_index, alternative) result(value)
        integer, intent(in) :: rule_index, node_index, alternative
        character(len=128) :: value

        value = 'RUNTIME-REPEAT-'//trim(adjustl(repeat_name(rule_index, node_index)))
        if (alternative > 0) value = identity_with_suffix(value, 'ALT', alternative)
    end function repeat_identity

    function unresolved_name(rule_index) result(value)
        integer, intent(in) :: rule_index
        character(len=128) :: value

        write (value, '("__runtime_unresolved_",i0)') rule_index
    end function unresolved_name

end module fortfront_grammar_runtime
