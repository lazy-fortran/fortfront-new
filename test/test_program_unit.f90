program test_program_unit
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: declaration_kind_program, &
        program_declaration_t, program_root_t, program_unit_declaration_capacity, &
        program_declaration_to_sx, program_root_to_sx, program_unit_from_sx, &
        program_unit_t, program_unit_to_sx, program_unit_validate
    implicit none

    type(program_unit_t) :: unit, parsed
    character(len=16384) :: serialized, reread
    character(len=32768) :: overflow
    character(len=2048) :: root_sx, declaration_sx
    character(len=128) :: message
    logical :: ok

    call set_root(unit%root)
    call assert_true(program_unit_validate(unit, message), &
        'empty program unit was rejected')
    call program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'empty program unit failed SX writing')
    call assert_equal(trim(serialized), &
        '(program-unit (root (program-root (name unit) (span (file unit.f90) '// &
        '(start-byte 0) (end-byte 24) (source-hash hash-positive)))) '// &
        '(declaration-count 0) (declarations))', 'empty SX changed')
    call program_unit_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'empty program unit SX was rejected')
    call assert_equal_integer(parsed%declaration_count, 0_int64, &
        'empty unit count changed')

    call set_declaration(unit%declarations(1), 'first', 25_int64, 30_int64)
    unit%declaration_count = 1
    call program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'one-declaration unit failed SX writing')
    call program_unit_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'one-declaration unit SX was rejected')
    call assert_equal(parsed%declarations(1)%name, 'first', &
        'one declaration was not retained')
    call assert_equal_integer(parsed%declarations(1)%span%start_byte, 25_int64, &
        'declaration span was not retained')

    call set_declaration(unit%declarations(2), 'second', 31_int64, 38_int64)
    call set_declaration(unit%declarations(3), 'third', 39_int64, 46_int64)
    unit%declaration_count = 3
    call program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'multiple-declaration unit failed SX writing')
    call program_unit_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'multiple-declaration unit SX was rejected')
    call assert_equal_integer(parsed%declaration_count, 3_int64, &
        'multiple declaration count changed')
    call assert_equal(parsed%declarations(1)%name, 'first', 'order changed at one')
    call assert_equal(parsed%declarations(2)%name, 'second', 'order changed at two')
    call assert_equal(parsed%declarations(3)%name, 'third', 'order changed at three')
    call program_unit_to_sx(parsed, reread, ok, message)
    call assert_true(ok, 'round-tripped unit failed SX writing')
    call assert_equal(trim(reread), trim(serialized), 'unit SX did not round-trip')

    unit%declaration_count = -1
    call assert_invalid(unit, 'negative-program-unit-declaration-count')
    unit%declaration_count = program_unit_declaration_capacity + 1
    call assert_invalid(unit, 'program-unit-declaration-capacity-exceeded')
    unit%declaration_count = 1
    unit%declarations(1)%name = ''
    call assert_invalid(unit, 'missing-program-declaration-name')

    call assert_invalid_sx(trim(serialized)//' trailing', 'malformed-program-unit')
    call assert_invalid_sx('(program-unit (root (program-root (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 24) '// &
        '(source-hash hash-positive)))) (declaration-count 2) '// &
        '(declarations))', 'program-unit-declaration-count-mismatch')
    call assert_invalid_sx('(program-unit (root (program-root (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 24) '// &
        '(source-hash hash-positive)))) (declaration-count 0) '// &
        '(declarations (program-declaration (declaration-kind program) '// &
        '(name first) (span (file unit.f90) (start-byte 25) (end-byte 30) '// &
        '(source-hash hash-positive)))))', &
        'program-unit-declaration-count-mismatch')

    call program_root_to_local_sx(root_sx)
    call set_declaration(unit%declarations(1), 'first', 25_int64, 30_int64)
    call program_declaration_to_local_sx(unit%declarations(1), declaration_sx)
    overflow = '(program-unit (root '//trim(root_sx)//') '// &
        '(declaration-count 17) (declarations '// &
        trim(repeat(trim(declaration_sx)//' ', 17))//'))'
    call assert_invalid_sx(trim(overflow), &
        'program-unit-declaration-capacity-exceeded')

    write (*, '(a)') 'program unit behavioral checks: ok'

contains

    subroutine set_root(root)
        type(program_root_t), intent(out) :: root

        root%name = 'unit'
        root%span%file = 'unit.f90'
        root%span%start_byte = 0_int64
        root%span%end_byte = 24_int64
        root%span%source_hash = 'hash-positive'
    end subroutine set_root

    subroutine set_declaration(declaration, name, start_byte, end_byte)
        type(program_declaration_t), intent(out) :: declaration
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: start_byte, end_byte

        declaration%declaration_kind = declaration_kind_program
        declaration%name = name
        declaration%span%file = 'unit.f90'
        declaration%span%start_byte = start_byte
        declaration%span%end_byte = end_byte
        declaration%span%source_hash = 'hash-positive'
    end subroutine set_declaration

    subroutine program_root_to_local_sx(output)
        character(len=*), intent(out) :: output
        logical :: valid

        call program_root_to_sx(unit%root, output, valid, message)
        call assert_true(valid, 'test root failed SX writing')
    end subroutine program_root_to_local_sx

    subroutine program_declaration_to_local_sx(declaration, output)
        type(program_declaration_t), intent(in) :: declaration
        character(len=*), intent(out) :: output
        logical :: valid

        call program_declaration_to_sx(declaration, output, valid, message)
        call assert_true(valid, 'test declaration failed SX writing')
    end subroutine program_declaration_to_local_sx

    subroutine assert_invalid(value, expected_message)
        type(program_unit_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message

        character(len=16384) :: output
        logical :: valid

        valid = program_unit_validate(value, message)
        call assert_true(.not. valid, 'invalid program unit was accepted')
        call assert_equal(message, expected_message, &
            'invalid program unit reported the wrong failure')
        call program_unit_to_sx(value, output, valid, message)
        call assert_true(.not. valid, 'invalid program unit was serialized')
    end subroutine assert_invalid

    subroutine assert_invalid_sx(value, expected_message)
        character(len=*), intent(in) :: value, expected_message

        logical :: valid

        call program_unit_from_sx(value, parsed, valid, message)
        call assert_true(.not. valid, 'invalid program unit SX was accepted')
        call assert_equal(message, expected_message, &
            'invalid program unit SX reported the wrong failure')
    end subroutine assert_invalid_sx

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

end program test_program_unit
