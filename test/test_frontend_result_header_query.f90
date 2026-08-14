program test_frontend_result_header_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_parse, &
        frontend_query_result_header, frontend_rejected, frontend_result_from_sx, &
        frontend_result_header_t, frontend_result_t, root_kind_none, &
        root_kind_program, standardir_syntax_item_t
    implicit none

    type(frontend_result_header_t) :: header
    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    ok = frontend_query_result_header(result, 'unit.f90', 'hash-positive', &
        header, message)
    call assert_true(ok, 'accepted result header query was rejected')
    call assert_equal(header%status, frontend_accepted, &
        'accepted result header changed status')
    call assert_equal(header%root_kind, root_kind_program, &
        'accepted result header changed root kind')
    call assert_equal(header%root_name, 'unit', &
        'accepted result header changed root name')
    call assert_equal_integer(header%diagnostic_count, 0_int64, &
        'accepted result header changed diagnostic count')
    call assert_span(header, 'unit.f90', 'hash-positive', 0_int64, 16_int64, &
        'accepted result header changed source span')

    ok = frontend_query_result_header(result, 'other.f90', 'hash-positive', &
        header, message)
    call assert_true(.not. ok, 'wrong accepted result file was accepted')
    call assert_equal(message, 'frontend-result-source-file-mismatch', &
        'wrong accepted result file changed failure')
    ok = frontend_query_result_header(result, 'unit.f90', 'other-hash', &
        header, message)
    call assert_true(.not. ok, 'wrong accepted result hash was accepted')
    call assert_equal(message, 'frontend-result-source-hash-mismatch', &
        'wrong accepted result hash changed failure')

    call frontend_result_from_sx( &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 1) (diagnostics (diagnostic (status rejected) '// &
        '(severity error) (message invalid-program) (span (file unit.f90) '// &
        '(start-byte 4) (end-byte 10) (source-hash source-hash)))))', &
        result, ok, message)
    call assert_true(ok, 'rejected result fixture was not parsed')
    ok = frontend_query_result_header(result, 'unit.f90', 'source-hash', &
        header, message)
    call assert_true(ok, 'rejected result header query was rejected')
    call assert_equal(header%status, frontend_rejected, &
        'rejected result header changed status')
    call assert_equal(header%root_kind, root_kind_none, &
        'rejected result header changed root kind')
    call assert_equal(header%root_name, '', &
        'rejected result header changed root name')
    call assert_equal_integer(header%diagnostic_count, 1_int64, &
        'rejected result header changed diagnostic count')
    call assert_span(header, 'unit.f90', 'source-hash', 4_int64, 10_int64, &
        'rejected result header changed diagnostic span')

    ok = frontend_query_result_header(result, '', 'source-hash', header, message)
    call assert_true(.not. ok, 'missing result file was accepted')
    call assert_equal(message, 'missing-expected-source-file', &
        'missing result file changed failure')
    ok = frontend_query_result_header(result, 'unit.f90', '', header, message)
    call assert_true(.not. ok, 'missing result hash was accepted')
    call assert_equal(message, 'missing-expected-source-hash', &
        'missing result hash changed failure')

    result%diagnostics(1)%status = frontend_accepted
    ok = frontend_query_result_header(result, 'unit.f90', 'source-hash', &
        header, message)
    call assert_true(.not. ok, 'status-mismatched result header was accepted')
    call assert_equal(message, 'diagnostic-result-status-mismatch', &
        'status-mismatched result changed failure')

    write (*, '(a)') 'frontend result header query behavioral checks: ok'

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

    subroutine assert_span(value, expected_file, expected_hash, expected_start, &
            expected_end, failure)
        type(frontend_result_header_t), intent(in) :: value
        character(len=*), intent(in) :: expected_file, expected_hash, failure
        integer(int64), intent(in) :: expected_start, expected_end

        call assert_equal(value%span%file, expected_file, failure)
        call assert_equal(value%span%source_hash, expected_hash, failure)
        call assert_equal_integer(value%span%start_byte, expected_start, failure)
        call assert_equal_integer(value%span%end_byte, expected_end, failure)
    end subroutine assert_span

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

end program test_frontend_result_header_query
