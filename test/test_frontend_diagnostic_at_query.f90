program test_frontend_diagnostic_at_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: diagnostic_t, frontend_query_diagnostic_at, &
        frontend_result_from_sx, frontend_result_t
    implicit none

    type(frontend_result_t) :: result
    type(diagnostic_t) :: diagnostic
    character(len=128) :: message
    logical :: ok

    call parse_rejected_result(result)
    ok = frontend_query_diagnostic_at(result, 2_int64, 'unit.f90', &
        'source-hash', diagnostic, message)
    call assert_true(ok, 'valid indexed diagnostic query was rejected')
    call assert_equal(diagnostic%message, 'second-error', &
        'indexed diagnostic query returned the wrong diagnostic')
    call assert_equal(diagnostic%span%file, 'unit.f90', &
        'indexed diagnostic query changed source file')
    call assert_equal(diagnostic%span%source_hash, 'source-hash', &
        'indexed diagnostic query changed source hash')

    call assert_rejected(result, -1_int64, 'negative-diagnostic-index')
    call assert_rejected(result, 0_int64, 'diagnostic-index-out-of-range')
    call assert_rejected(result, 3_int64, 'diagnostic-index-out-of-range')

    call frontend_result_from_sx( &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))', result, ok, message)
    call assert_true(ok, 'accepted result fixture was not parsed')
    call assert_rejected(result, 1_int64, 'accepted-frontend-result')

    result = frontend_result_t()
    call assert_rejected(result, 1_int64, 'invalid-rejected-result')

    call parse_rejected_result(result)
    result%diagnostics(2)%message = ''
    call assert_rejected(result, 1_int64, 'missing-diagnostic-message')

    call parse_rejected_result(result)
    call assert_rejected_with_source(result, 1_int64, 'other.f90', &
        'source-hash', 'diagnostic-source-file-mismatch')
    call assert_rejected_with_source(result, 1_int64, 'unit.f90', &
        'other-hash', 'diagnostic-source-hash-mismatch')

    write (*, '(a)') 'frontend diagnostic-at query behavioral checks: ok'

contains

    subroutine parse_rejected_result(value)
        type(frontend_result_t), intent(out) :: value

        logical :: parsed

        call frontend_result_from_sx( &
            '(frontend-result (status rejected) (root-kind none) '// &
            '(diagnostic-count 2) (diagnostics (diagnostic (status rejected) '// &
            '(severity error) (message first-error) (span (file unit.f90) '// &
            '(start-byte 4) (end-byte 10) (source-hash source-hash))) '// &
            '(diagnostic (status rejected) (severity warning) '// &
            '(message second-error) (span (file unit.f90) (start-byte 12) '// &
            '(end-byte 18) (source-hash source-hash)))))', value, parsed, message)
        call assert_true(parsed, 'rejected result fixture was not parsed')
    end subroutine parse_rejected_result

    subroutine assert_rejected(value, index, expected_message)
        type(frontend_result_t), intent(in) :: value
        integer(int64), intent(in) :: index
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_diagnostic_at(value, index, 'unit.f90', &
            'source-hash', diagnostic, message)
        call assert_true(.not. valid, 'invalid indexed diagnostic was accepted')
        call assert_equal(message, expected_message, &
            'indexed diagnostic query reported the wrong failure')
    end subroutine assert_rejected

    subroutine assert_rejected_with_source(value, index, expected_file, &
            expected_hash, expected_message)
        type(frontend_result_t), intent(in) :: value
        integer(int64), intent(in) :: index
        character(len=*), intent(in) :: expected_file, expected_hash
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_diagnostic_at(value, index, expected_file, &
            expected_hash, diagnostic, message)
        call assert_true(.not. valid, 'wrong diagnostic source was accepted')
        call assert_equal(message, expected_message, &
            'indexed diagnostic source query reported the wrong failure')
    end subroutine assert_rejected_with_source

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_true(value, failure)
        logical, intent(in) :: value
        character(len=*), intent(in) :: failure

        if (.not. value) error stop failure
    end subroutine assert_true

end program test_frontend_diagnostic_at_query
