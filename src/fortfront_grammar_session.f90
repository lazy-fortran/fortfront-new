module fortfront_grammar_session
    !! Fixed-capacity incremental wrapper around the generic grammar frontier.
    !! Tokens are caller-supplied names; this module performs no tokenization.

    use fortfront_grammar, only: fortfront_grammar_rule_capacity, &
        fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t
    use fortfront_grammar_frontier, only: fortfront_grammar_advance_frontier, &
        fortfront_grammar_frontier_accepted, fortfront_grammar_frontier_ambiguous, &
        fortfront_grammar_frontier_capacity, fortfront_grammar_frontier_malformed, &
        fortfront_grammar_frontier_rejected, fortfront_grammar_frontier_result_t, &
        fortfront_grammar_frontier_unresolved
    implicit none
    private

    integer, parameter, public :: fortfront_grammar_session_token_capacity = 16
    integer, parameter, public :: fortfront_grammar_session_accepted = &
        fortfront_grammar_frontier_accepted
    integer, parameter, public :: fortfront_grammar_session_rejected = &
        fortfront_grammar_frontier_rejected
    integer, parameter, public :: fortfront_grammar_session_ambiguous = &
        fortfront_grammar_frontier_ambiguous
    integer, parameter, public :: fortfront_grammar_session_unresolved = &
        fortfront_grammar_frontier_unresolved
    integer, parameter, public :: fortfront_grammar_session_malformed = &
        fortfront_grammar_frontier_malformed
    integer, parameter, public :: fortfront_grammar_session_capacity = &
        fortfront_grammar_frontier_capacity
    integer, parameter, public :: fortfront_grammar_session_finalization = 6
    integer, parameter, public :: fortfront_grammar_session_initialized = 7

    type, public :: fortfront_grammar_session_t
        private
        type(fortfront_grammar_table_t) :: table
        type(fortfront_grammar_analysis_result_t) :: facts(fortfront_grammar_rule_capacity)
        integer :: fact_count = 0
        character(len=128) :: start_lhs = ''
        character(len=128) :: input(fortfront_grammar_session_token_capacity) = ''
        integer :: input_count = 0
        logical :: initialized = .false.
        logical :: finalized = .false.
    end type fortfront_grammar_session_t

    public :: fortfront_grammar_session_finalize
    public :: fortfront_grammar_session_initialize
    public :: fortfront_grammar_session_push

contains

    subroutine fortfront_grammar_session_initialize(session, table, facts, fact_count, &
            start_lhs, status, message)
        type(fortfront_grammar_session_t), intent(out) :: session
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        character(len=*), intent(in) :: start_lhs
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_frontier_result_t) :: probe(fortfront_grammar_rule_capacity)
        integer :: probe_count, frontier_status
        character(len=256) :: frontier_message

        session = fortfront_grammar_session_t()
        status = fortfront_grammar_session_malformed
        message = ''
        if (fact_count < 0 .or. fact_count > size(session%facts) .or. &
            fact_count > size(facts)) then
            message = 'grammar-session-fact-count-is-out-of-range'
            return
        end if
        session%table = table
        session%facts(1:fact_count) = facts(1:fact_count)
        session%fact_count = fact_count
        session%start_lhs = start_lhs
        call fortfront_grammar_advance_frontier(session%table, session%facts, &
            session%fact_count, session%start_lhs, session%input, 0, probe, probe_count, &
            frontier_status, frontier_message)
        if (frontier_status == fortfront_grammar_frontier_malformed) then
            message = 'grammar-session-configuration-is-malformed: '//trim(frontier_message)
            return
        end if
        session%initialized = .true.
        status = fortfront_grammar_session_initialized
        message = 'grammar-session-is-initialized'
    end subroutine fortfront_grammar_session_initialize

    subroutine fortfront_grammar_session_push(session, token, output, output_count, status, &
            message)
        type(fortfront_grammar_session_t), intent(inout) :: session
        character(len=*), intent(in) :: token
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        character(len=128) :: candidate(fortfront_grammar_session_token_capacity)
        type(fortfront_grammar_frontier_result_t) :: candidate_output(&
            fortfront_grammar_rule_capacity)
        integer :: candidate_count, frontier_status
        character(len=256) :: frontier_message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_session_malformed
        message = ''
        if (.not. session%initialized) then
            message = 'grammar-session-is-not-initialized'
            return
        end if
        if (session%finalized) then
            status = fortfront_grammar_session_finalization
            message = 'grammar-session-cannot-push-after-finalization'
            return
        end if
        if (session%input_count == fortfront_grammar_session_token_capacity) then
            status = fortfront_grammar_session_capacity
            message = 'grammar-session-token-capacity-exhausted'
            return
        end if
        candidate = session%input
        candidate(session%input_count + 1) = token
        call evaluate(session, candidate, session%input_count + 1, candidate_output, &
            candidate_count, frontier_status, frontier_message)
        if (frontier_status == fortfront_grammar_frontier_malformed) then
            message = 'grammar-session-token-is-malformed: '//trim(frontier_message)
            return
        end if
        session%input = candidate
        session%input_count = session%input_count + 1
        call copy_results(candidate_output, candidate_count, frontier_status, output, output_count, &
            status, message)
        if (status == fortfront_grammar_session_capacity) return
        if (len_trim(message) == 0) message = frontier_message
    end subroutine fortfront_grammar_session_push

    subroutine fortfront_grammar_session_finalize(session, output, output_count, status, message)
        type(fortfront_grammar_session_t), intent(inout) :: session
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_frontier_result_t) :: candidate_output(&
            fortfront_grammar_rule_capacity)
        integer :: candidate_count, frontier_status
        character(len=256) :: frontier_message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_session_malformed
        message = ''
        if (.not. session%initialized) then
            message = 'grammar-session-is-not-initialized'
            return
        end if
        if (session%finalized) then
            status = fortfront_grammar_session_finalization
            message = 'grammar-session-is-already-finalized'
            return
        end if
        call evaluate(session, session%input, session%input_count, candidate_output, candidate_count, &
            frontier_status, frontier_message)
        session%finalized = .true.
        call copy_results(candidate_output, candidate_count, frontier_status, output, output_count, &
            status, message)
        if (status == fortfront_grammar_session_capacity) return
        if (len_trim(message) == 0) message = 'grammar-session-finalization-'//trim(frontier_message)
    end subroutine fortfront_grammar_session_finalize

    subroutine evaluate(session, input, input_count, output, output_count, status, message)
        type(fortfront_grammar_session_t), intent(in) :: session
        character(len=*), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        call fortfront_grammar_advance_frontier(session%table, session%facts, &
            session%fact_count, session%start_lhs, input, input_count, output, output_count, &
            status, message)
    end subroutine evaluate

    subroutine copy_results(source, source_count, source_status, output, output_count, status, &
            message)
        type(fortfront_grammar_frontier_result_t), intent(in) :: source(:)
        integer, intent(in) :: source_count, source_status
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(inout) :: message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_grammar_session_malformed
        if (source_count > size(output)) then
            status = fortfront_grammar_session_capacity
            message = 'grammar-session-output-capacity-exhausted'
            return
        end if
        if (source_count > 0) output(1:source_count) = source(1:source_count)
        output_count = source_count
        status = source_status
    end subroutine copy_results

end module fortfront_grammar_session
