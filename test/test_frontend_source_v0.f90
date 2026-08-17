program test_frontend_source_v0
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse, frontend_result_t, &
        frontend_result_to_sx, frontend_rejected, frontend_accepted, &
        root_kind_none, root_kind_program, standardir_syntax_item_t
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-v0'
    character(len=*), parameter :: positive = 'program p'//new_line('a')//'end program p'
    character(len=*), parameter :: negative = 'program p'//new_line('a')//'end program q'
    character(len=*), parameter :: declaration_positive = &
        'program p'//new_line('a')//'  integer :: x'//new_line('a')// &
        'end program p'
    character(len=*), parameter :: declaration_negative = &
        'program p'//new_line('a')//'  integer ::'//new_line('a')// &
        'end program p'
    character(len=32768) :: first_sx, second_sx
    character(len=128) :: message
    logical :: ok
    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: witness

    call set_r501_program_witness(witness)
    call frontend_parse('fixture.f90', positive, source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_accepted, 'positive source rejected')
    call assert_equal(trim(result%root_kind), root_kind_program, &
        'positive source root kind changed')
    call assert_equal_integer(result%diagnostic_count, 0_int64, &
        'positive source produced diagnostics')
    call frontend_result_to_sx(result, first_sx, ok, message)
    call assert_true(ok, 'positive result did not serialize')
    call assert_equal(trim(first_sx), &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))', 'positive SX oracle changed')

    call frontend_parse('fixture.f90', negative, source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_rejected, &
        'mismatched end was accepted')
    call assert_equal(trim(result%root_kind), root_kind_none, &
        'rejected root kind changed')
    call assert_equal_integer(result%diagnostic_count, 1_int64, &
        'mismatched end diagnostic count changed')
    call frontend_result_to_sx(result, first_sx, ok, message)
    call assert_true(ok, 'rejected result did not serialize')
    call assert_equal(trim(first_sx), &
        '(frontend-result (status rejected) (root-kind none) (diagnostic-count 1) '// &
        '(diagnostics (diagnostic (status rejected) (severity error) '// &
        '(message invalid-program) (span (file fixture.f90) (start-byte 0) '// &
        '(end-byte 23) (source-hash l3-raw-program-v0)))))', &
        'negative SX oracle changed')

    call frontend_parse('empty.f90', '', source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_rejected, 'empty source accepted')
    call assert_diagnostic(result, 'empty-source', 'empty.f90', 0_int64, 0_int64)

    call frontend_parse('malformed.f90', 'program', source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_rejected, &
        'malformed source accepted')
    call assert_diagnostic(result, 'invalid-program', 'malformed.f90', 0_int64, 7_int64)

    call frontend_parse('fixture.f90', positive, source_hash, witness, result)
    call frontend_result_to_sx(result, first_sx, ok, message)
    call assert_true(ok, 'first repeat serialization failed')
    call frontend_parse('fixture.f90', positive, source_hash, witness, result)
    call frontend_result_to_sx(result, second_sx, ok, message)
    call assert_true(ok, 'second repeat serialization failed')
    call assert_equal(trim(first_sx), trim(second_sx), 'repeat output was nondeterministic')

    call frontend_parse('fixture.f90', declaration_positive, source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_accepted, &
        'declaration source rejected')
    call assert_equal_integer(result%diagnostic_count, 0_int64, &
        'declaration source produced diagnostics')

    call frontend_parse('fixture.f90', declaration_negative, source_hash, witness, result)
    call assert_equal(trim(result%status), frontend_rejected, &
        'missing declaration entity was accepted')
    call assert_equal_integer(result%diagnostic_count, 1_int64, &
        'missing declaration entity diagnostic count changed')
    call assert_diagnostic(result, 'invalid-program', 'fixture.f90', 0_int64, &
        int(len(declaration_negative), int64))
    write (*, '(a)') 'frontend source-v0 behavioral checks: ok'

contains

    subroutine set_r501_program_witness(value)
        type(standardir_syntax_item_t), intent(out) :: value

        value%id = 'R501'
        value%lhs = 'program'
        value%origin = 'mechanical'
        value%resolution = 'resolved'
        value%source%document = 'J3-24-007'
        value%source%clause = '5'
        value%source%rule = 'R501'
        value%source%page = 53_int64
        value%source%source_hash = &
            '1cf538329c57e4f617adb36f2c7cd91a5a5561c78bcce16ec96f7ff1a9979f9'
    end subroutine set_r501_program_witness

    subroutine assert_diagnostic(value, expected_message, expected_file, start_byte, end_byte)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message, expected_file
        integer(int64), intent(in) :: start_byte, end_byte

        call assert_equal(trim(value%diagnostics(1)%message), expected_message, &
            'diagnostic class changed')
        call assert_equal(trim(value%diagnostics(1)%span%file), expected_file, &
            'diagnostic source file changed')
        call assert_equal(trim(value%diagnostics(1)%span%source_hash), source_hash, &
            'diagnostic source hash changed')
        call assert_equal_integer(value%diagnostics(1)%span%start_byte, start_byte, &
            'diagnostic start span changed')
        call assert_equal_integer(value%diagnostics(1)%span%end_byte, end_byte, &
            'diagnostic end span changed')
    end subroutine assert_diagnostic

    subroutine assert_true(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine assert_true

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, failure)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: failure

        if (actual /= expected) error stop failure
    end subroutine assert_equal_integer

end program test_frontend_source_v0
