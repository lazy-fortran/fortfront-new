module fortfront_lexical_grammar_session
    !! Source-preserving lexical cursor composed with the generic grammar session.

    use fortfront_grammar, only: fortfront_grammar_table_t
    use fortfront_grammar_analysis, only: fortfront_grammar_analysis_result_t
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_grammar_session, only: fortfront_grammar_session_accepted, &
        fortfront_grammar_session_ambiguous, fortfront_grammar_session_capacity, &
        fortfront_grammar_session_finalize, fortfront_grammar_session_initialized, &
        fortfront_grammar_session_malformed, fortfront_grammar_session_push, &
        fortfront_grammar_session_rejected, fortfront_grammar_session_t, &
        fortfront_grammar_session_unresolved, fortfront_grammar_session_initialize
    use fortfront_lexical_token_cursor, only: &
        fortfront_lexical_token_cursor_advance, fortfront_lexical_token_cursor_end_of_stream, &
        fortfront_lexical_token_cursor_initialize, fortfront_lexical_token_cursor_malformed, &
        fortfront_lexical_token_cursor_peek, fortfront_lexical_token_cursor_t, &
        fortfront_lexical_token_cursor_ok
    use fortfront_lexical, only: fortfront_lexical_facts_t, &
        fortfront_lexical_validate
    use fortfront_lexical_tokens, only: fortfront_lexical_token_ambiguous, &
        fortfront_lexical_token_capacity, fortfront_lexical_token_match, &
        fortfront_lexical_token_malformed, fortfront_lexical_token_no_match, &
        fortfront_lexical_token_t, fortfront_lexical_token_unsupported
    implicit none
    private

    integer, parameter, public :: fortfront_lexical_grammar_session_accepted = &
        fortfront_grammar_session_accepted
    integer, parameter, public :: fortfront_lexical_grammar_session_rejected = &
        fortfront_grammar_session_rejected
    integer, parameter, public :: fortfront_lexical_grammar_session_ambiguous = &
        fortfront_grammar_session_ambiguous
    integer, parameter, public :: fortfront_lexical_grammar_session_unresolved = &
        fortfront_grammar_session_unresolved
    integer, parameter, public :: fortfront_lexical_grammar_session_malformed = &
        fortfront_grammar_session_malformed
    integer, parameter, public :: fortfront_lexical_grammar_session_capacity = &
        fortfront_grammar_session_capacity
    integer, parameter, public :: fortfront_lexical_grammar_session_no_match = 10
    integer, parameter, public :: fortfront_lexical_grammar_session_unsupported = 11
    integer, parameter, public :: fortfront_lexical_grammar_session_token_ambiguous = 12
    integer, parameter, public :: fortfront_lexical_grammar_session_token_malformed = 13
    integer, parameter, public :: fortfront_lexical_grammar_session_token_capacity = 14
    integer, parameter, public :: fortfront_lexical_grammar_session_end_of_stream = 15
    integer, parameter, public :: fortfront_lexical_grammar_session_initialized = 16

    type, public :: fortfront_lexical_grammar_session_t
        private
        type(fortfront_lexical_token_cursor_t) :: cursor
        type(fortfront_grammar_session_t) :: grammar
        logical :: initialized = .false.
    end type fortfront_lexical_grammar_session_t

    public :: fortfront_lexical_grammar_session_initialize
    public :: fortfront_lexical_grammar_session_advance
    public :: fortfront_lexical_grammar_session_finalize

contains

    subroutine fortfront_lexical_grammar_session_initialize(session, table, facts, fact_count, &
            start_lhs, tokens, token_count, status, message)
        type(fortfront_lexical_grammar_session_t), intent(out) :: session
        type(fortfront_grammar_table_t), intent(in) :: table
        type(fortfront_grammar_analysis_result_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        character(len=*), intent(in) :: start_lhs
        type(fortfront_lexical_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: token_count
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        integer :: cursor_status, grammar_status
        integer :: i
        character(len=256) :: cursor_message, grammar_message
        character(len=256) :: facts_message

        session = fortfront_lexical_grammar_session_t()
        status = fortfront_lexical_grammar_session_malformed
        message = ''
        call fortfront_lexical_token_cursor_initialize(session%cursor, tokens, token_count, &
            cursor_status, cursor_message)
        if (cursor_status /= fortfront_lexical_token_cursor_ok) then
            message = 'lexical-grammar-session-cursor-is-malformed: '//trim(cursor_message)
            return
        end if
        do i = 1, token_count
            if (.not. token_provenance_is_valid(tokens(i), facts_message)) then
                session = fortfront_lexical_grammar_session_t()
                message = 'lexical-grammar-session-token-provenance-is-malformed: '// &
                    trim(facts_message)
                return
            end if
        end do
        call fortfront_grammar_session_initialize(session%grammar, table, facts, fact_count, &
            start_lhs, grammar_status, grammar_message)
        if (grammar_status /= fortfront_grammar_session_initialized) then
            message = 'lexical-grammar-session-grammar-is-malformed: '//trim(grammar_message)
            return
        end if
        session%initialized = .true.
        status = fortfront_lexical_grammar_session_initialized
        message = 'lexical grammar session is initialized'
    end subroutine fortfront_lexical_grammar_session_initialize

    subroutine fortfront_lexical_grammar_session_advance(session, token, output, output_count, &
            status, message)
        type(fortfront_lexical_grammar_session_t), intent(inout) :: session
        type(fortfront_lexical_token_t), intent(out) :: token
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        type(fortfront_lexical_token_t) :: candidate
        integer :: cursor_status, grammar_status
        character(len=256) :: cursor_message, grammar_message

        token = fortfront_lexical_token_t()
        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_lexical_grammar_session_malformed
        message = ''
        if (.not. session%initialized) then
            message = 'lexical-grammar-session-is-not-initialized'
            return
        end if
        call fortfront_lexical_token_cursor_peek(session%cursor, candidate, cursor_status, &
            cursor_message)
        if (cursor_status == fortfront_lexical_token_cursor_end_of_stream) then
            status = fortfront_lexical_grammar_session_end_of_stream
            message = cursor_message
            return
        end if
        if (cursor_status /= fortfront_lexical_token_cursor_ok) then
            message = 'lexical-grammar-session-cursor-is-malformed: '//trim(cursor_message)
            return
        end if
        if (candidate%status /= fortfront_lexical_token_match) then
            token = candidate
            status = map_token_status(candidate%status)
            message = candidate%message
            if (len_trim(message) == 0) message = lexical_status_message(candidate%status)
            return
        end if
        call fortfront_grammar_session_push(session%grammar, candidate%symbol, output, output_count, &
            grammar_status, grammar_message)
        status = grammar_status
        message = grammar_message
        if (grammar_status == fortfront_lexical_grammar_session_malformed .or. &
            grammar_status == fortfront_lexical_grammar_session_capacity) then
            token = candidate
            return
        end if
        call fortfront_lexical_token_cursor_advance(session%cursor, token, cursor_status, &
            cursor_message)
        if (cursor_status /= fortfront_lexical_token_cursor_ok) then
            token = fortfront_lexical_token_t()
            message = 'lexical-grammar-session-cursor-advance-failed: '//trim(cursor_message)
            return
        end if
    end subroutine fortfront_lexical_grammar_session_advance

    subroutine fortfront_lexical_grammar_session_finalize(session, output, output_count, status, &
            message)
        type(fortfront_lexical_grammar_session_t), intent(inout) :: session
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        integer, intent(out) :: output_count, status
        character(len=*), intent(out) :: message

        type(fortfront_lexical_token_t) :: candidate
        integer :: cursor_status
        character(len=256) :: cursor_message

        output = fortfront_grammar_frontier_result_t()
        output_count = 0
        status = fortfront_lexical_grammar_session_malformed
        message = ''
        if (.not. session%initialized) then
            message = 'lexical-grammar-session-is-not-initialized'
            return
        end if
        call fortfront_lexical_token_cursor_peek(session%cursor, candidate, cursor_status, &
            cursor_message)
        if (cursor_status == fortfront_lexical_token_cursor_end_of_stream) then
            call fortfront_grammar_session_finalize(session%grammar, output, output_count, status, &
                message)
            return
        end if
        if (cursor_status /= fortfront_lexical_token_cursor_ok) then
            message = 'lexical-grammar-session-cursor-is-malformed: '//trim(cursor_message)
            return
        end if
        if (candidate%status /= fortfront_lexical_token_match) then
            status = map_token_status(candidate%status)
            message = candidate%message
            if (len_trim(message) == 0) message = lexical_status_message(candidate%status)
            return
        end if
        message = 'lexical-grammar-session-has-unconsumed-matched-token'
        return
    end subroutine fortfront_lexical_grammar_session_finalize

    integer function map_token_status(token_status)
        integer, intent(in) :: token_status

        select case (token_status)
        case (fortfront_lexical_token_no_match)
            map_token_status = fortfront_lexical_grammar_session_no_match
        case (fortfront_lexical_token_unsupported)
            map_token_status = fortfront_lexical_grammar_session_unsupported
        case (fortfront_lexical_token_ambiguous)
            map_token_status = fortfront_lexical_grammar_session_token_ambiguous
        case (fortfront_lexical_token_malformed)
            map_token_status = fortfront_lexical_grammar_session_token_malformed
        case (fortfront_lexical_token_capacity)
            map_token_status = fortfront_lexical_grammar_session_token_capacity
        case default
            map_token_status = fortfront_lexical_grammar_session_token_malformed
        end select
    end function map_token_status

    logical function token_provenance_is_valid(token, message)
        type(fortfront_lexical_token_t), intent(in) :: token
        character(len=*), intent(out) :: message

        type(fortfront_lexical_facts_t) :: facts
        logical :: facts_ok

        facts = fortfront_lexical_facts_t()
        facts%count = 1
        facts%facts(1) = token%fact
        call fortfront_lexical_validate(facts, facts_ok, message)
        token_provenance_is_valid = facts_ok
    end function token_provenance_is_valid

    function lexical_status_message(token_status) result(message)
        integer, intent(in) :: token_status
        character(len=64) :: message

        select case (token_status)
        case (fortfront_lexical_token_no_match)
            message = 'lexical token has no match'
        case (fortfront_lexical_token_unsupported)
            message = 'lexical token is unsupported'
        case (fortfront_lexical_token_ambiguous)
            message = 'lexical token is ambiguous'
        case (fortfront_lexical_token_capacity)
            message = 'lexical token capacity was exhausted'
        case default
            message = 'lexical token is malformed'
        end select
    end function lexical_status_message

end module fortfront_lexical_grammar_session
