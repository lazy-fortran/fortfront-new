program test_frontend_generated_program_unit
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: declaration_kind_program, &
        frontend_generated_program_unit_to_sx, &
        frontend_parse_generated_program_unit, &
        frontend_validate_generated_program_unit, generated_program_unit_t, &
        standardir_syntax_item_t
    implicit none

    type(generated_program_unit_t) :: unit
    type(standardir_syntax_item_t) :: syntax_item
    character(len=16384) :: serialized
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse_generated_program_unit('unit.f90', &
        'program unit'//new_line('a')//'end', 'hash-positive', syntax_item, &
        unit, ok, message)
    call assert_true(ok, 'minimal program witness was rejected')
    call assert_equal(message, '', 'accepted witness returned a diagnostic')
    call assert_true(frontend_validate_generated_program_unit(unit, message), &
        'generated program unit was not valid')
    call assert_equal(unit%root%name, 'unit', 'generated root name changed')
    call assert_equal(unit%root%span%file, 'unit.f90', &
        'generated root source file changed')
    call assert_equal(unit%root%span%source_hash, 'hash-positive', &
        'generated root source hash changed')
    call assert_equal_integer(unit%root%span%start_byte, 0_int64, &
        'generated root start span changed')
    call assert_equal_integer(unit%root%span%end_byte, 16_int64, &
        'generated root end span changed')
    call assert_equal_integer(unit%declaration_count, 1_int64, &
        'generated declaration count changed')
    call assert_equal(unit%declaration%declaration_kind, declaration_kind_program, &
        'generated declaration kind changed')
    call assert_equal(unit%declaration%name, 'unit', &
        'generated declaration name changed')
    call assert_equal(unit%declaration%span%file, 'unit.f90', &
        'generated declaration source file changed')
    call assert_equal(unit%declaration%span%source_hash, 'hash-positive', &
        'generated declaration source hash changed')
    call assert_equal_integer(unit%declaration%span%start_byte, 0_int64, &
        'generated declaration start span changed')
    call assert_equal_integer(unit%declaration%span%end_byte, 16_int64, &
        'generated declaration end span changed')
    call frontend_generated_program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'generated program unit failed SX serialization')
    call assert_equal(trim(serialized), &
        '(program-unit (root (program-root (name unit) (span '// &
        '(source-span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive))))) (declaration-count 1) '// &
        '(declaration (program-declaration (declaration-kind program) '// &
        '(name unit) (span (source-span (file unit.f90) (start-byte 0) '// &
        '(end-byte 16) (source-hash hash-positive))))))', &
        'generated program-unit SX oracle changed')

    call assert_rejected('empty.f90', '', 'hash-empty', syntax_item, &
        'empty-source')
    call assert_rejected('broken.f90', 'program'//new_line('a')//'end', &
        'hash-broken', syntax_item, 'invalid-program')
    call assert_rejected('module.f90', 'module unit'//new_line('a')//'end', &
        'hash-module', syntax_item, 'unsupported-syntax')

    call assert_rejected('', 'program unit'//new_line('a')//'end', &
        'hash-invalid-file', syntax_item, 'invalid-source-span-file')
    call assert_rejected('unit.f90', 'program unit'//new_line('a')//'end', &
        '', syntax_item, 'invalid-source-span-source-hash')

    syntax_item%source%page = 0_int64
    call assert_rejected('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-invalid-witness', syntax_item, 'invalid-syntax-provenance')

    write (*, '(a)') 'frontend generated program-unit behavioral checks: ok'

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

    subroutine assert_rejected(file_name, source, source_hash, witness, expected)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(standardir_syntax_item_t), intent(in) :: witness
        character(len=*), intent(in) :: expected

        call frontend_parse_generated_program_unit(file_name, source, source_hash, &
            witness, unit, ok, message)
        call assert_true(.not. ok, 'invalid witness source was accepted')
        call assert_equal(message, expected, 'wrong generated parser diagnostic')
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

end program test_frontend_generated_program_unit
