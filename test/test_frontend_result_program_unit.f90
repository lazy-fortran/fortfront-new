program test_frontend_result_program_unit
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_parse, &
        frontend_read, frontend_rejected, frontend_result_to_program_unit, &
        frontend_result_t, frontend_validate_program_unit_handoff, &
        program_unit_t, program_unit_to_sx, program_unit_validate, root_kind_none, &
        root_kind_source, &
        standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(program_unit_t) :: unit
    type(standardir_syntax_item_t) :: syntax_item
    character(len=16384) :: serialized
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    call assert_equal(result%status, frontend_accepted, &
        'fixture source was not accepted')
    call frontend_result_to_program_unit(result, unit, ok, message)
    call assert_true(ok, 'accepted result was not converted to program unit')
    call assert_true(program_unit_validate(unit, message), &
        'converted program unit was not valid')
    call assert_true(frontend_validate_program_unit_handoff(result, unit, message), &
        'converted program unit failed handoff validation')
    call assert_equal(unit%root%name, 'unit', &
        'converted program unit lost the root name')
    call assert_equal(unit%root%span%file, 'unit.f90', &
        'converted program unit lost the source file')
    call assert_equal(unit%root%span%source_hash, 'hash-positive', &
        'converted program unit lost the source hash')
    call assert_equal_integer(unit%root%span%start_byte, 0_int64, &
        'converted program unit changed the root start')
    call assert_equal_integer(unit%root%span%end_byte, 16_int64, &
        'converted program unit changed the root end')
    call assert_equal_integer(unit%declaration_count, 0_int64, &
        'bounded consumer fabricated declarations')
    call program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'converted program unit failed SX writing')
    call assert_equal(trim(serialized), &
        '(program-unit (root (program-root (name unit) (span (file unit.f90) '// &
        '(start-byte 0) (end-byte 16) (source-hash hash-positive)))) '// &
        '(declaration-count 0) (declarations))', &
        'converted program unit SX changed')

    unit%root%name = 'other'
    call assert_invalid_handoff(result, unit, 'program-unit-root-name-mismatch')
    unit%root%name = 'unit'
    unit%root%span%source_hash = 'other-hash'
    call assert_invalid_handoff(result, unit, &
        'program-unit-root-source-hash-mismatch')
    unit%root%span%source_hash = 'hash-positive'
    unit%root%span%end_byte = 15_int64
    call assert_invalid_handoff(result, unit, 'program-unit-root-span-mismatch')
    unit%root%span%end_byte = 16_int64
    unit%root%span%file = 'other.f90'
    call assert_invalid_handoff(result, unit, 'program-unit-root-file-mismatch')
    unit%root%span%file = 'unit.f90'
    unit%root%name = ''
    call assert_invalid_handoff(result, unit, 'missing-program-root-name')

    call frontend_read('rejected.f90', '', 'hash-rejected', result)
    call frontend_result_to_program_unit(result, unit, ok, message)
    call assert_true(.not. ok, 'rejected result became a program unit')
    call assert_equal(message, 'rejected-frontend-result', &
        'rejected result reported the wrong conversion failure')

    result = frontend_result_t()
    result%status = frontend_accepted
    result%root_kind = root_kind_source
    result%root%kind = root_kind_source
    call frontend_result_to_program_unit(result, unit, ok, message)
    call assert_true(.not. ok, 'non-program result became a program unit')
    call assert_equal(message, 'non-program-root', &
        'non-program result reported the wrong conversion failure')

    result = frontend_result_t()
    result%status = frontend_rejected
    result%root_kind = root_kind_none
    result%diagnostic_count = 0_int64
    call frontend_result_to_program_unit(result, unit, ok, message)
    call assert_true(.not. ok, 'invalid rejected result became a program unit')
    call assert_equal(message, 'invalid-rejected-result', &
        'invalid rejected result reported the wrong conversion failure')
    call assert_invalid_handoff(result, unit, 'invalid-rejected-result')

    write (*, '(a)') 'frontend result program-unit behavioral checks: ok'

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

    subroutine assert_invalid_handoff(result, unit, expected_message)
        type(frontend_result_t), intent(in) :: result
        type(program_unit_t), intent(in) :: unit
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_validate_program_unit_handoff(result, unit, message)
        call assert_true(.not. valid, 'invalid program-unit handoff was accepted')
        call assert_equal(message, expected_message, &
            'invalid program-unit handoff reported the wrong failure')
    end subroutine assert_invalid_handoff

end program test_frontend_result_program_unit
