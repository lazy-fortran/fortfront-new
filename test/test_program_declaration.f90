program test_program_declaration
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: declaration_kind_module, declaration_kind_program, &
        program_declaration_from_sx, program_declaration_t, &
        program_declaration_to_sx, program_declaration_validate
    implicit none

    type(program_declaration_t) :: declaration, parsed
    character(len=512) :: serialized, reread
    character(len=128) :: message
    logical :: ok

    declaration%declaration_kind = declaration_kind_program
    declaration%name = 'unit'
    declaration%span%file = 'unit.f90'
    declaration%span%start_byte = 0_int64
    declaration%span%end_byte = 16_int64
    declaration%span%source_hash = 'hash-positive'

    ok = program_declaration_validate(declaration, message)
    call assert_true(ok, 'valid program declaration was rejected')
    call program_declaration_to_sx(declaration, serialized, ok, message)
    call assert_true(ok, 'valid program declaration failed SX writing')
    call assert_equal(trim(serialized), &
        '(program-declaration (declaration-kind program) (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive)))', 'canonical SX changed')

    call program_declaration_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'canonical SX was rejected')
    call assert_equal(parsed%declaration_kind, declaration_kind_program, &
        'reader lost declaration kind')
    call assert_equal(parsed%name, 'unit', 'reader lost declaration name')
    call assert_equal(parsed%span%file, 'unit.f90', 'reader lost source file')
    call assert_equal(parsed%span%source_hash, 'hash-positive', &
        'reader lost source hash')
    call assert_equal_integer(parsed%span%start_byte, 0_int64, &
        'reader lost start byte')
    call assert_equal_integer(parsed%span%end_byte, 16_int64, &
        'reader lost end byte')
    call program_declaration_to_sx(parsed, reread, ok, message)
    call assert_true(ok, 'reread declaration failed SX writing')
    call assert_equal(trim(reread), trim(serialized), 'SX did not round-trip')

    declaration%name = ''
    call assert_invalid(declaration, 'missing-program-declaration-name')
    declaration%name = 'unit'
    declaration%declaration_kind = ''
    call assert_invalid(declaration, 'missing-program-declaration-kind')
    declaration%declaration_kind = declaration_kind_module
    call program_declaration_to_sx(declaration, serialized, ok, message)
    call assert_true(ok, 'module declaration was rejected')
    declaration%declaration_kind = declaration_kind_program
    declaration%span%source_hash = ''
    call assert_invalid(declaration, 'missing-program-declaration-source-hash')

    call assert_invalid_sx(&
        '(program-declaration (declaration-kind unknown) (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive)))', 'invalid-program-declaration-kind')
    call assert_invalid_sx(&
        '(program-declaration (declaration-kind program) (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive)) (extra x))', 'malformed-program-declaration')
    call assert_invalid_sx(&
        '(program-declaration (declaration-kind program) (name unit) '// &
        '(span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive))) trailing', 'malformed-program-declaration')
    call assert_invalid_sx('(program-declaration (declaration-kind program) '// &
        '(name unit)', 'malformed-program-declaration-span')

    write (*, '(a)') 'program declaration behavioral checks: ok'

contains

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

    subroutine assert_invalid(value, expected_message)
        type(program_declaration_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message

        character(len=512) :: output
        logical :: valid

        valid = program_declaration_validate(value, message)
        call assert_true(.not. valid, 'invalid declaration was accepted')
        call assert_equal(message, expected_message, &
            'invalid declaration reported the wrong failure')
        call program_declaration_to_sx(value, output, valid, message)
        call assert_true(.not. valid, 'invalid declaration was serialized')
    end subroutine assert_invalid

    subroutine assert_invalid_sx(value, expected_message)
        character(len=*), intent(in) :: value, expected_message

        logical :: valid

        call program_declaration_from_sx(value, parsed, valid, message)
        call assert_true(.not. valid, 'invalid declaration SX was accepted')
        call assert_equal(message, expected_message, &
            'invalid declaration SX reported the wrong failure')
    end subroutine assert_invalid_sx

end program test_program_declaration
