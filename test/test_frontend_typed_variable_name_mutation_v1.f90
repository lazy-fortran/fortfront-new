program test_frontend_typed_variable_name_mutation_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    implicit none

    character(len=*), parameter :: source_prefix = 'program p'//new_line('a')// &
        '  integer :: '
    character(len=*), parameter :: source_suffix = new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: malformed = source_prefix//source_suffix
    character(len=256) :: message
    character(len=128) :: source_hash
    character(len=:), allocatable :: source
    logical :: ok
    type(typed_program_unit_t) :: unit

    call check_name('x', 'l3-raw-program-v0')
    call check_name('y', 'l3-raw-program-variable-name-v1')
    call check_name('z', 'l3-raw-program-variable-name-z-v1')

    call frontend_parse_typed_program_unit('bad.f90', malformed, &
        'l3-raw-program-variable-name-z-v1', unit, ok, message)
    call assert_true(.not. ok, 'malformed integer declaration was accepted')
    call assert_equal(trim(message), 'unsupported-typed-program-unit', &
        'malformed integer declaration diagnostic changed')
    write (*, '(a)') 'frontend typed variable name mutation v1 checks: ok'

contains

    subroutine check_name(name, expected_hash)
        character(len=*), intent(in) :: name, expected_hash

        source = source_prefix//name//source_suffix
        source_hash = expected_hash
        call frontend_parse_typed_program_unit('mutation.f90', source, &
            source_hash, unit, ok, message)
        call assert_true(ok, 'typed variable-name mutation was rejected')
        call assert_equal(trim(unit%variable%name), name, &
            'variable name was not preserved')
        call assert_equal(trim(unit%variable%type_spec), 'integer', &
            'variable type was not preserved')
        call assert_equal_integer(unit%variable%span%start_byte, 10_int64, &
            'variable span start changed')
        call assert_equal_integer(unit%variable%span%end_byte, 24_int64, &
            'variable span end changed')
        call assert_equal(trim(unit%variable%span%source_hash), expected_hash, &
            'variable source hash changed')
    end subroutine check_name

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

end program test_frontend_typed_variable_name_mutation_v1
