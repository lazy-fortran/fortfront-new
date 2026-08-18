program test_frontend_typed_variable_name_multichar_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_validate_typed_program_unit, typed_program_unit_t
    implicit none

    character(len=*), parameter :: source_hash = &
        'l3-raw-program-variable-name-alpha-v1'
    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  integer :: alpha'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: malformed = 'program p'//new_line('a')// &
        '  integer ::'//new_line('a')//'end program p'//new_line('a')
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    call frontend_parse_typed_program_unit('alpha.f90', source, source_hash, &
        unit, ok, message)
    call assert_true(ok, 'alpha typed variable-name witness was rejected')
    call assert_true(frontend_validate_typed_program_unit(unit, message), &
        'alpha typed variable-name witness was invalid')
    call assert_equal(trim(unit%variable%name), 'alpha', &
        'multi-character variable name was not preserved')
    call assert_equal(trim(unit%variable%type_spec), 'integer', &
        'alpha variable type was not preserved')
    call assert_equal_integer(unit%variable%span%start_byte, 10_int64, &
        'alpha declaration span start changed')
    call assert_equal_integer(unit%variable%span%end_byte, 28_int64, &
        'alpha declaration span end changed')
    call assert_equal(trim(unit%variable%span%source_hash), source_hash, &
        'alpha source hash changed')

    call frontend_parse_typed_program_unit('malformed.f90', malformed, &
        source_hash, unit, ok, message)
    call assert_true(.not. ok, 'malformed integer declaration was accepted')
    call assert_equal(trim(message), 'unsupported-typed-program-unit', &
        'malformed integer declaration diagnostic changed')
    write (*, '(a)') 'frontend typed variable name multichar v1 checks: ok'

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

end program test_frontend_typed_variable_name_multichar_v1
