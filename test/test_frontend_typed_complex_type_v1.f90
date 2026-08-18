program test_frontend_typed_complex_type_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, frontend_validate_typed_program_unit, &
        typed_program_unit_t
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-complex-type-v1'
    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  complex :: x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: malformed = 'program main'//new_line('a')// &
        '  complex ::'//new_line('a')//'end program main'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    call frontend_parse_typed_program_unit('complex.f90', source, source_hash, &
        unit, ok, message)
    call assert_true(ok, 'complex declaration witness was rejected')
    call assert_true(frontend_validate_typed_program_unit(unit, message), &
        'complex declaration witness was invalid')
    call assert_equal(trim(unit%root%name), 'main', 'program name changed')
    call assert_equal(trim(unit%variable%type_spec), 'complex', &
        'complex type spec was not preserved')
    call assert_equal(trim(unit%variable%name), 'x', 'variable name changed')
    call assert_equal_integer(unit%variable%span%start_byte, 13_int64, &
        'variable span start changed')
    call assert_equal_integer(unit%variable%span%end_byte, 27_int64, &
        'variable span end changed')
    call assert_equal(trim(unit%variable%span%source_hash), source_hash, &
        'variable source hash changed')

    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'complex typed AST serialization failed')
    call assert_true(index(trim(serialized), '(type-spec complex)') > 0, &
        'canonical SX omitted complex type')

    call frontend_parse_typed_program_unit('bad.f90', malformed, source_hash, &
        unit, ok, message)
    call assert_true(.not. ok, 'malformed complex declaration was accepted')
    call assert_equal(trim(message), 'unsupported-typed-program-unit', &
        'malformed complex declaration diagnostic changed')
    write (*, '(a)') 'frontend typed complex type v1 checks: ok'

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

end program test_frontend_typed_complex_type_v1
