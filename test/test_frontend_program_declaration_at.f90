program test_frontend_program_declaration_at
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: declaration_kind_program, &
        frontend_parse, frontend_query_program_declaration_at, frontend_read, &
        frontend_result_t, program_declaration_t, root_kind_source, &
        standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(program_declaration_t) :: declaration
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    ok = frontend_query_program_declaration_at(result, 1_int64, 'unit.f90', &
        'hash-positive', declaration, message)
    call assert_true(ok, 'valid program declaration query was rejected')
    call assert_equal(declaration%declaration_kind, declaration_kind_program, &
        'valid query changed declaration kind')
    call assert_equal(declaration%name, 'unit', &
        'valid query changed declaration name')
    call assert_equal(declaration%span%file, 'unit.f90', &
        'valid query changed declaration source file')
    call assert_equal(declaration%span%source_hash, 'hash-positive', &
        'valid query changed declaration source hash')
    call assert_equal_integer(declaration%span%end_byte, 16_int64, &
        'valid query changed declaration span')

    call assert_rejected(result, -1_int64, 'negative-program-declaration-index')
    call assert_rejected(result, 0_int64, 'program-declaration-index-out-of-range')
    call assert_rejected(result, 2_int64, 'program-declaration-index-out-of-range')

    call frontend_read('rejected.f90', '', 'hash-rejected', result)
    call assert_rejected(result, 1_int64, 'rejected-frontend-result')

    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    result%root_kind = root_kind_source
    result%root%kind = root_kind_source
    call assert_rejected(result, 1_int64, 'non-program-root')

    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    result%root%name = ''
    call assert_rejected(result, 1_int64, 'missing-program-root-name')

    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    call assert_rejected_with_source(result, 'other.f90', 'hash-positive', &
        'program-declaration-source-file-mismatch')
    call assert_rejected_with_source(result, 'unit.f90', 'other-hash', &
        'program-declaration-source-hash-mismatch')

    write (*, '(a)') 'frontend program-declaration-at behavioral checks: ok'

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

    subroutine assert_rejected(value, index, expected_message)
        type(frontend_result_t), intent(in) :: value
        integer(int64), intent(in) :: index
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_program_declaration_at(value, index, 'unit.f90', &
            'hash-positive', declaration, message)
        call assert_true(.not. valid, 'invalid program declaration was accepted')
        call assert_equal(message, expected_message, &
            'program declaration query reported the wrong failure')
    end subroutine assert_rejected

    subroutine assert_rejected_with_source(value, expected_file, expected_hash, &
            expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_file, expected_hash
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_program_declaration_at(value, 1_int64, &
            expected_file, expected_hash, declaration, message)
        call assert_true(.not. valid, 'wrong declaration source was accepted')
        call assert_equal(message, expected_message, &
            'program declaration source query reported the wrong failure')
    end subroutine assert_rejected_with_source

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

end program test_frontend_program_declaration_at
