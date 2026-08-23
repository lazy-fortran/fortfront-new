program test_fortfront_lexical_token_cursor
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_lexical, only: fortfront_lexical_fact_t
    use fortfront_lexical_tokens, only: fortfront_lexical_token_ambiguous, &
        fortfront_lexical_token_match, fortfront_lexical_token_no_match, &
        fortfront_lexical_token_t, fortfront_lexical_token_unsupported
    use fortfront_lexical_token_cursor, only: advance, initialize, peek, &
        fortfront_lexical_token_cursor_end_of_stream, fortfront_lexical_token_cursor_malformed, &
        fortfront_lexical_token_cursor_ok, fortfront_lexical_token_cursor_t
    implicit none

    type(fortfront_lexical_token_cursor_t) :: cursor
    type(fortfront_lexical_token_t) :: tokens(3), token
    character(len=256) :: message
    integer :: status

    call make_tokens(tokens)
    call initialize(cursor, tokens, 3, status, message)
    call require(status == fortfront_lexical_token_cursor_ok, 'initialization failed')
    call peek(cursor, token, status, message)
    call require(status == fortfront_lexical_token_cursor_ok .and. token_equal(token, tokens(1)), &
        'first peek differed')
    call peek(cursor, token, status, message)
    call require(token_equal(token, tokens(1)), 'repeated peek advanced the cursor')
    call advance(cursor, token, status, message)
    call require(token_equal(token, tokens(1)), 'first advance differed')
    call advance(cursor, token, status, message)
    call require(token_equal(token, tokens(2)), 'second advance differed')
    call advance(cursor, token, status, message)
    call require(token_equal(token, tokens(3)), 'third advance differed')
    call peek(cursor, token, status, message)
    call require(status == fortfront_lexical_token_cursor_end_of_stream .and. &
        len_trim(token%symbol) == 0, 'peek did not report EOS and clear output')
    call advance(cursor, token, status, message)
    call require(status == fortfront_lexical_token_cursor_end_of_stream .and. &
        len_trim(token%symbol) == 0, 'advance did not report EOS and clear output')

    call initialize(cursor, tokens, 0, status, message)
    call require(status == fortfront_lexical_token_cursor_ok, 'empty initialization failed')
    token%symbol = 'stale'
    call peek(cursor, token, status, message)
    call require(status == fortfront_lexical_token_cursor_end_of_stream .and. &
        len_trim(token%symbol) == 0, 'empty stream was not EOS')

    tokens(1)%symbol = ''
    call initialize(cursor, tokens, 3, status, message)
    call require(status == fortfront_lexical_token_cursor_malformed .and. .not. &
        cursor%initialized, 'malformed symbol was accepted transactionally')
    call make_tokens(tokens)
    tokens(1)%status = 99
    call initialize(cursor, tokens, 3, status, message)
    call require(status == fortfront_lexical_token_cursor_malformed, 'bad status was accepted')
    call make_tokens(tokens)
    tokens(1)%start_byte = -1_int64
    call initialize(cursor, tokens, 3, status, message)
    call require(status == fortfront_lexical_token_cursor_malformed, 'bad span was accepted')
    call make_tokens(tokens)
    call initialize(cursor, tokens, -1, status, message)
    call require(status == fortfront_lexical_token_cursor_malformed .and. .not. &
        cursor%initialized, 'bad count was accepted')
    print '(a)', 'fortfront lexical token cursor behavioral checks: ok'

contains

    subroutine make_tokens(tokens)
        type(fortfront_lexical_token_t), intent(out) :: tokens(:)
        integer :: i

        tokens = fortfront_lexical_token_t()
        do i = 1, size(tokens)
            write (tokens(i)%symbol, '(a,i1)') 'TOKEN-', i
            tokens(i)%start_byte = int(i - 1, int64) * 3_int64
            tokens(i)%end_byte = tokens(i)%start_byte + 2_int64
            tokens(i)%scalar_count = 1
            tokens(i)%status = fortfront_lexical_token_match
            tokens(i)%fact%target_name = 'FACT-'//char(iachar('A') + i - 1)
            tokens(i)%fact%source_hash = repeat('b', 64)
            tokens(i)%message = 'preserve this message'
        end do
        tokens(2)%status = fortfront_lexical_token_no_match
        tokens(3)%status = fortfront_lexical_token_unsupported
        tokens(3)%start_byte = 100_int64
        tokens(3)%end_byte = 101_int64
        tokens(3)%scalar_count = 1
        tokens(3)%fact%codepoint = 'UTF-8-α'
        tokens(3)%fact%document = 'provenance-document'
    end subroutine make_tokens

    logical function token_equal(left, right)
        type(fortfront_lexical_token_t), intent(in) :: left, right

        token_equal = left%symbol == right%symbol .and. left%start_byte == right%start_byte .and. &
            left%end_byte == right%end_byte .and. left%scalar_count == right%scalar_count .and. &
            left%status == right%status .and. left%message == right%message .and. &
            left%fact%target_name == right%fact%target_name .and. &
            left%fact%source_hash == right%fact%source_hash .and. &
            left%fact%codepoint == right%fact%codepoint .and. &
            left%fact%document == right%fact%document
    end function token_equal

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_lexical_token_cursor
