program test_dispatch_l2_frontend_trace
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: diagnostic_t, frontend_accepted, &
        frontend_parse, frontend_rejected, frontend_result_from_sx, &
        frontend_result_t, frontend_result_to_sx, root_kind_none, &
        root_kind_program, severity_error, standardir_syntax_item_t
    implicit none

    character(len=*), parameter :: valid_fixture = &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))'
    character(len=*), parameter :: invalid_fixture = &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 1))'
    character(len=*), parameter :: valid_source = &
        'program witness'//new_line('a')//'end program witness'
    character(len=*), parameter :: invalid_source = &
        'program witness'//new_line('a')//'end program other'

    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: syntax_item
    character(len=4096) :: serialized
    character(len=128) :: message
    logical :: ok

    call frontend_result_from_sx(valid_fixture, result, ok, message)
    call assert_true(ok, 'valid frontend-v0 witness was rejected')
    call assert_equal(result%status, frontend_accepted, &
        'valid frontend-v0 witness changed status')
    call assert_equal(result%root_kind, root_kind_program, &
        'valid frontend-v0 witness changed root kind')
    call assert_equal_integer(result%diagnostic_count, 0_int64, &
        'valid frontend-v0 witness changed diagnostic count')
    call frontend_result_to_sx(result, serialized, ok, message)
    call assert_true(ok, 'valid frontend-v0 witness failed canonical SX output')
    call assert_equal(trim(serialized), valid_fixture, &
        'valid frontend-v0 witness canonical SX changed')

    call frontend_result_from_sx(invalid_fixture, result, ok, message)
    call assert_true(.not. ok, 'invalid frontend-v0 neighbor was accepted')
    call assert_equal(message, 'malformed-sx-diagnostics', &
        'invalid frontend-v0 neighbor changed diagnostic class')

    call set_program_witness(syntax_item)
    call frontend_parse('witness.f90', valid_source, 'fixture', syntax_item, result)
    call assert_true(trim(result%status) == frontend_accepted, &
        'valid source witness was rejected')
    call assert_equal(result%root_kind, root_kind_program, &
        'valid source witness changed root kind')
    call assert_equal(result%root%span%file, 'witness.f90', &
        'valid source witness lost its file span')
    call assert_equal_integer(result%root%span%start_byte, 0_int64, &
        'valid source witness changed span start')
    call assert_equal_integer(result%root%span%end_byte, &
        int(len(valid_source), int64), 'valid source witness changed span end')
    call assert_equal(result%root%span%source_hash, 'fixture', &
        'valid source witness lost its source hash')

    call frontend_parse('witness.f90', invalid_source, 'fixture', syntax_item, result)
    call assert_equal(result%status, frontend_rejected, &
        'invalid source neighbor was accepted')
    call assert_equal(result%root_kind, root_kind_none, &
        'invalid source neighbor changed rejected root kind')
    call assert_equal_integer(result%diagnostic_count, 1_int64, &
        'invalid source neighbor changed diagnostic count')
    call assert_diagnostic(result%diagnostics(1), 'invalid-program', &
        'witness.f90', 0_int64, int(len(invalid_source), int64), 'fixture')

    write (*, '(a)') 'L2 frontend-v0 trace behavioral checks: ok'

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

    subroutine assert_diagnostic(value, expected_message, expected_file, &
            expected_start, expected_end, expected_hash)
        type(diagnostic_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message, expected_file
        character(len=*), intent(in) :: expected_hash
        integer(int64), intent(in) :: expected_start, expected_end

        call assert_equal(value%status, frontend_rejected, &
            'invalid source diagnostic changed status')
        call assert_equal(value%severity, severity_error, &
            'invalid source diagnostic changed severity')
        call assert_equal(value%message, expected_message, &
            'invalid source diagnostic changed class')
        call assert_equal(value%span%file, expected_file, &
            'invalid source diagnostic lost its file span')
        call assert_equal_integer(value%span%start_byte, expected_start, &
            'invalid source diagnostic changed span start')
        call assert_equal_integer(value%span%end_byte, expected_end, &
            'invalid source diagnostic changed span end')
        call assert_equal(value%span%source_hash, expected_hash, &
            'invalid source diagnostic lost its source hash')
    end subroutine assert_diagnostic

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

end program test_dispatch_l2_frontend_trace
