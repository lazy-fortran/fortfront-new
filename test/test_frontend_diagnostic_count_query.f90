program test_frontend_diagnostic_count_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_parse, &
        frontend_query_diagnostic_count, frontend_result_from_sx, &
        frontend_result_t, root_kind_none, &
        standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    integer(int64) :: diagnostic_count
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    ok = frontend_query_diagnostic_count(result, 'unit.f90', 'hash-positive', &
        diagnostic_count, message)
    call assert_true(ok, 'accepted diagnostic count query was rejected')
    call assert_equal_integer(diagnostic_count, 0_int64, &
        'accepted result did not return zero diagnostics')

    call assert_rejected(result, 'other.f90', 'hash-positive', &
        'frontend-result-source-file-mismatch')
    call assert_rejected(result, 'unit.f90', 'other-hash', &
        'frontend-result-source-hash-mismatch')

    call frontend_result_from_sx( &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 2) (diagnostics (diagnostic (status rejected) '// &
        '(severity error) (message first-error) (span (file unit.f90) '// &
        '(start-byte 4) (end-byte 10) (source-hash source-hash))) '// &
        '(diagnostic (status rejected) (severity warning) '// &
        '(message second-error) (span (file unit.f90) (start-byte 12) '// &
        '(end-byte 18) (source-hash source-hash)))))', result, ok, message)
    call assert_true(ok, 'rejected result fixture was not parsed')
    ok = frontend_query_diagnostic_count(result, 'unit.f90', 'source-hash', &
        diagnostic_count, message)
    call assert_true(ok, 'rejected diagnostic count query was rejected')
    call assert_equal_integer(diagnostic_count, 2_int64, &
        'rejected result returned the wrong diagnostic count')

    call assert_rejected(result, 'other.f90', 'source-hash', &
        'diagnostic-source-file-mismatch')
    call assert_rejected(result, 'unit.f90', 'other-hash', &
        'diagnostic-source-hash-mismatch')
    result%diagnostics(2)%span%file = 'other.f90'
    call assert_rejected(result, 'unit.f90', 'source-hash', &
        'diagnostic-source-file-mismatch')
    result%diagnostics(2)%span%file = 'unit.f90'
    result%diagnostics(2)%span%source_hash = 'other-hash'
    call assert_rejected(result, 'unit.f90', 'source-hash', &
        'diagnostic-source-hash-mismatch')
    result%diagnostics(2)%status = frontend_accepted
    call assert_rejected(result, 'unit.f90', 'source-hash', &
        'diagnostic-result-status-mismatch')

    result = frontend_result_t()
    ok = frontend_query_diagnostic_count(result, 'unit.f90', 'source-hash', &
        diagnostic_count, message)
    call assert_true(.not. ok, 'invalid rejected result was accepted')
    call assert_equal(message, 'invalid-rejected-result', &
        'invalid rejected result changed query failure')

    result = frontend_result_t()
    result%status = frontend_accepted
    result%root_kind = root_kind_none
    ok = frontend_query_diagnostic_count(result, 'unit.f90', 'source-hash', &
        diagnostic_count, message)
    call assert_true(.not. ok, 'invalid accepted result was accepted')
    call assert_equal(message, 'invalid-accepted-result', &
        'invalid accepted result changed query failure')

    write (*, '(a)') 'frontend diagnostic count query behavioral checks: ok'

contains

    subroutine set_program_witness(value)
        type(standardir_syntax_item_t), intent(out) :: value

        value%id = 'R501'
        value%lhs = 'program'
        value%origin = 'mechanical'
        value%resolution = 'resolved'
        value%source%document = 'J3-24-007'
        value%source%clause = '1'
        value%source%rule = 'R501'
        value%source%page = 45_int64
        value%source%source_hash = 'fixture'
    end subroutine set_program_witness

    subroutine assert_rejected(value, expected_file, expected_hash, &
            expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_file, expected_hash
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_diagnostic_count(value, expected_file, &
            expected_hash, diagnostic_count, message)
        call assert_true(.not. valid, 'invalid diagnostic count query was accepted')
        call assert_equal(message, expected_message, &
            'diagnostic count query reported the wrong failure')
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

end program test_frontend_diagnostic_count_query
