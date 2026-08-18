program test_frontend_typed_variable_name_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, frontend_validate_typed_program_unit, &
        typed_program_unit_t
    implicit none

    character(len=*), parameter :: source_hash = &
        'l3-raw-program-variable-name-v1'
    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  integer :: y'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: malformed = 'program p'//new_line('a')// &
        '  integer ::'//new_line('a')//'end program p'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    call frontend_parse_typed_program_unit( &
        'tests/fixtures/l3-ast-variable-name-v1.f90', source, source_hash, &
        unit, ok, message)
    call assert_true(ok, 'typed variable-name witness was rejected')
    call assert_true(frontend_validate_typed_program_unit(unit, message), &
        'typed variable-name witness was invalid')
    call assert_equal(trim(unit%root%name), 'p', 'program name changed')
    call assert_equal_integer(unit%declaration_count, 1_int64, &
        'program declaration count changed')
    call assert_equal_integer(unit%variable_count, 1_int64, &
        'variable declaration count changed')
    call assert_equal(trim(unit%variable%type_spec), 'integer', &
        'type spec changed')
    call assert_equal(trim(unit%variable%name), 'y', 'variable name changed')
    call assert_equal_integer(unit%variable%span%start_byte, 10_int64, &
        'variable span start changed')
    call assert_equal_integer(unit%variable%span%end_byte, 24_int64, &
        'variable span end changed')
    call assert_equal(trim(unit%variable%span%source_hash), source_hash, &
        'variable source hash changed')

    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'typed AST serialization failed')
    call assert_true(index(trim(serialized), '(name y)') > 0, &
        'canonical SX omitted variable name')

    call frontend_parse_typed_program_unit('bad.f90', malformed, source_hash, &
        unit, ok, message)
    call assert_true(.not. ok, 'malformed declaration was accepted')
    call assert_equal(trim(message), 'unsupported-typed-program-unit', &
        'malformed declaration diagnostic changed')
    write (*, '(a)') 'frontend typed variable name v1 checks: ok'

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

end program test_frontend_typed_variable_name_v1
