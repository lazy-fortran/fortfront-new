program test_frontend_diagnostic_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: diagnostic_t, frontend_accepted, &
        frontend_query_diagnostic, frontend_rejected, frontend_result_from_sx, &
        frontend_result_t, root_kind_none
    implicit none

    type(frontend_result_t) :: result
    type(diagnostic_t) :: diagnostic
    character(len=4096) :: serialized
    character(len=128) :: message
    logical :: ok

    serialized = '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 1) (diagnostics (diagnostic (status rejected) '// &
        '(severity error) (message invalid-program) (span (file unit.f90) '// &
        '(start-byte 4) (end-byte 10) (source-hash source-hash)))))'
    call frontend_result_from_sx(serialized, result, ok, message)
    call assert_true(ok, 'rejected result fixture was not parsed')
    call assert_true(trim(result%status) == frontend_rejected, &
        'rejected result fixture changed status')
    ok = frontend_query_diagnostic(result, 'unit.f90', 'source-hash', diagnostic, message)
    call assert_true(ok, 'source-linked diagnostic query was rejected')
    call assert_equal(diagnostic%status, frontend_rejected, &
        'diagnostic query changed rejected status')
    call assert_equal(diagnostic%message, 'invalid-program', &
        'diagnostic query changed message')
    call assert_equal(diagnostic%span%file, 'unit.f90', &
        'diagnostic query changed source file')
    call assert_equal(diagnostic%span%source_hash, 'source-hash', &
        'diagnostic query changed source hash')
    call assert_equal_integer(diagnostic%span%start_byte, 4_int64, &
        'diagnostic query changed start span')
    call assert_equal_integer(diagnostic%span%end_byte, 10_int64, &
        'diagnostic query changed end span')

    call assert_rejected(result, 'other.f90', 'source-hash', &
        'diagnostic-source-file-mismatch')
    call assert_rejected(result, 'unit.f90', 'other-hash', &
        'diagnostic-source-hash-mismatch')
    call assert_rejected(result, '', 'source-hash', 'missing-expected-source-file')
    call assert_rejected(result, 'unit.f90', '', 'missing-expected-source-hash')

    call frontend_result_from_sx( &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))', result, ok, message)
    call assert_true(ok, 'accepted result fixture was not parsed')
    ok = frontend_query_diagnostic(result, 'unit.f90', 'source-hash', diagnostic, message)
    call assert_true(.not. ok, 'accepted result produced a diagnostic')
    call assert_equal(message, 'accepted-frontend-result', &
        'accepted result changed query diagnostic')

    result = frontend_result_t()
    result%status = frontend_accepted
    result%root_kind = root_kind_none
    ok = frontend_query_diagnostic(result, 'unit.f90', 'source-hash', diagnostic, message)
    call assert_true(.not. ok, 'invalid accepted result produced a diagnostic')
    call assert_equal(message, 'invalid-accepted-result', &
        'invalid accepted result changed query diagnostic')

    call frontend_result_from_sx( &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 1) (diagnostics (diagnostic (status accepted) '// &
        '(severity error) (message invalid-program) (span (file unit.f90) '// &
        '(start-byte 4) (end-byte 10) (source-hash source-hash)))))', &
        result, ok, message)
    call assert_true(ok, 'status mismatch fixture was not parsed')
    ok = frontend_query_diagnostic(result, 'unit.f90', 'source-hash', diagnostic, message)
    call assert_true(.not. ok, 'status mismatch diagnostic was accepted')
    call assert_equal(message, 'diagnostic-result-status-mismatch', &
        'status mismatch reported the wrong query diagnostic')

    write (*, '(a)') 'frontend diagnostic query behavioral checks: ok'

contains

    subroutine assert_rejected(value, expected_file, expected_hash, expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_file, expected_hash, expected_message

        logical :: valid

        valid = frontend_query_diagnostic(value, expected_file, expected_hash, &
            diagnostic, message)
        call assert_true(.not. valid, 'invalid diagnostic query was accepted')
        call assert_equal(message, expected_message, &
            'diagnostic query reported the wrong failure')
    end subroutine assert_rejected

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, failure)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: failure

        if (actual /= expected) error stop failure
    end subroutine assert_equal_integer

    subroutine assert_true(value, failure)
        logical, intent(in) :: value
        character(len=*), intent(in) :: failure

        if (.not. value) error stop failure
    end subroutine assert_true

end program test_frontend_diagnostic_query
