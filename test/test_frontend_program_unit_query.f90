program test_frontend_program_unit_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_query_program_unit, &
        frontend_parse, frontend_read, frontend_rejected, frontend_result_t, &
        program_unit_t, &
        standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(program_unit_t) :: unit
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    ok = frontend_query_program_unit(result, 'unit.f90', 'hash-positive', unit, message)
    call assert_true(ok, 'accepted typed unit query was rejected')
    call assert_equal(unit%root%name, 'unit', 'typed unit query lost root name')
    call assert_equal(unit%root%span%file, 'unit.f90', &
        'typed unit query lost source file')
    call assert_equal(unit%root%span%source_hash, 'hash-positive', &
        'typed unit query lost source hash')

    call assert_rejected(result, 'other.f90', 'hash-positive', &
        'program-unit-source-file-mismatch')
    call assert_rejected(result, 'unit.f90', 'other-hash', &
        'program-unit-source-hash-mismatch')
    call assert_rejected(result, '', 'hash-positive', 'missing-expected-source-file')
    call assert_rejected(result, 'unit.f90', '', 'missing-expected-source-hash')

    call frontend_read('rejected.f90', '', 'hash-rejected', result)
    call assert_true(trim(result%status) == frontend_rejected, &
        'rejection fixture did not remain rejected')
    ok = frontend_query_program_unit(result, 'rejected.f90', 'hash-rejected', unit, message)
    call assert_true(.not. ok, 'rejected frontend result became a typed unit')
    call assert_equal(message, 'rejected-frontend-result', &
        'rejected frontend result changed query diagnostic')

    result = frontend_result_t()
    result%status = frontend_accepted
    ok = frontend_query_program_unit(result, 'unit.f90', 'hash-positive', unit, message)
    call assert_true(.not. ok, 'invalid accepted result became a typed unit')
    call assert_equal(message, 'invalid-accepted-result', &
        'invalid accepted result changed query diagnostic')

    write (*, '(a)') 'frontend program-unit query behavioral checks: ok'

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

    subroutine assert_rejected(value, expected_file, expected_hash, expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_file, expected_hash, expected_message

        logical :: valid

        valid = frontend_query_program_unit(value, expected_file, expected_hash, unit, message)
        call assert_true(.not. valid, 'invalid typed unit query was accepted')
        call assert_equal(message, expected_message, &
            'typed unit query reported the wrong failure')
    end subroutine assert_rejected

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_true(value, failure)
        logical, intent(in) :: value
        character(len=*), intent(in) :: failure

        if (.not. value) error stop failure
    end subroutine assert_true

end program test_frontend_program_unit_query
