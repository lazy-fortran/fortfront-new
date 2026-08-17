program test_frontend_typed_declaration_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, frontend_validate_typed_program_unit, &
        typed_program_unit_t
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-v0'
    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  integer :: x'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: malformed = 'program p'//new_line('a')// &
        '  integer ::'//new_line('a')//'end program p'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    call frontend_parse_typed_program_unit('tests/fixtures/l3-declaration-v0.f90', &
        source, source_hash, unit, ok, message)
    call assert_true(ok, 'typed declaration witness was rejected')
    call assert_true(frontend_validate_typed_program_unit(unit, message), &
        'typed declaration witness was invalid')
    call assert_equal_integer(unit%declaration_count, 1_int64, &
        'program declaration count changed')
    call assert_equal_integer(unit%variable_count, 1_int64, &
        'variable declaration count changed')
    call assert_equal(trim(unit%declaration%declaration_kind), 'program', &
        'program declaration kind changed')
    call assert_equal(trim(unit%declaration%name), 'p', 'program name changed')
    call assert_equal(trim(unit%variable%type_spec), 'integer', 'type spec changed')
    call assert_equal(trim(unit%variable%name), 'x', 'variable name changed')
    call assert_equal_integer(unit%variable%span%start_byte, 10_int64, &
        'variable span start changed')
    call assert_equal_integer(unit%variable%span%end_byte, 24_int64, &
        'variable span end changed')
    call assert_equal(trim(unit%variable%span%source_hash), source_hash, &
        'variable source hash changed')

    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'typed AST serialization failed')
    call assert_equal(trim(serialized), &
        '(program-unit (root (program-root (name p) (span (source-span (file '// &
        'tests/fixtures/l3-declaration-v0.f90) (start-byte 0) (end-byte 38) '// &
        '(source-hash l3-raw-program-v0))))) (declaration-count 1) '// &
        '(declaration (program-declaration (declaration-kind program) (name p) '// &
        '(span (source-span (file tests/fixtures/l3-declaration-v0.f90) '// &
        '(start-byte 0) (end-byte 9) (source-hash l3-raw-program-v0))))) '// &
        '(variable-count 1) (variable (variable-declaration (type-spec integer) '// &
        '(name x) (span (source-span '// &
        '(file tests/fixtures/l3-declaration-v0.f90) (start-byte 10) '// &
        '(end-byte 24) (source-hash l3-raw-program-v0))))))', &
        'canonical v1 SX changed')

    call frontend_parse_typed_program_unit('bad.f90', malformed, source_hash, &
        unit, ok, message)
    call assert_true(.not. ok, 'malformed declaration was accepted')
    call assert_equal(trim(message), 'unsupported-typed-program-unit', &
        'malformed declaration diagnostic changed')
    write (*, '(a)') 'frontend typed declaration v1 checks: ok'

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

end program test_frontend_typed_declaration_v1
