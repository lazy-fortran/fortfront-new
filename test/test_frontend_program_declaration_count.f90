program test_frontend_program_declaration_count
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse, &
        frontend_query_program_declaration_count, frontend_result_t, &
        standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    integer(int64) :: declaration_count
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    declaration_count = -1_int64
    ok = frontend_query_program_declaration_count(result, 'unit.f90', &
        'hash-positive', declaration_count, message)
    call assert_true(ok, 'valid program declaration count query was rejected')
    call assert_equal_integer(declaration_count, 0_int64, &
        'valid program declaration count changed')

    call assert_rejected(result, 'other.f90', 'hash-positive', &
        'program-unit-source-file-mismatch')
    call assert_rejected(result, 'unit.f90', 'other-hash', &
        'program-unit-source-hash-mismatch')

    result = frontend_result_t()
    declaration_count = -1_int64
    ok = frontend_query_program_declaration_count(result, 'unit.f90', &
        'hash-positive', declaration_count, message)
    call assert_true(.not. ok, 'malformed frontend result was accepted')
    call assert_equal(message, 'invalid-rejected-result', &
        'malformed frontend result changed query diagnostic')
    call assert_equal_integer(declaration_count, 0_int64, &
        'malformed frontend result produced an invalid output')

    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    result%root%span%end_byte = -1_int64
    declaration_count = -1_int64
    ok = frontend_query_program_declaration_count(result, 'unit.f90', &
        'hash-positive', declaration_count, message)
    call assert_true(.not. ok, 'invalid program-unit output was accepted')
    call assert_equal(message, 'invalid-program-root-span', &
        'invalid program-unit output changed query diagnostic')
    call assert_equal_integer(declaration_count, 0_int64, &
        'invalid program-unit output was returned')

    write (*, '(a)') 'frontend program-declaration-count behavioral checks: ok'

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

        declaration_count = -1_int64
        valid = frontend_query_program_declaration_count(value, expected_file, &
            expected_hash, declaration_count, message)
        call assert_true(.not. valid, 'invalid program declaration count was accepted')
        call assert_equal(message, expected_message, &
            'program declaration count reported the wrong failure')
        call assert_equal_integer(declaration_count, 0_int64, &
            'rejected program declaration count produced an invalid output')
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

end program test_frontend_program_declaration_count
