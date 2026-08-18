program test_frontend_typed_intrinsic_type_specs_v1
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    implicit none

    type(typed_program_unit_t) :: unit
    character(len=256) :: message
    logical :: ok

    call check_type('program p'//new_line('a')//'  integer :: y'//new_line('a')// &
        'end program p'//new_line('a'), 'integer', 'y')
    call check_type('program p'//new_line('a')//'  real :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'real', 'x')
    call check_type('program p'//new_line('a')//'  double precision :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'double-precision', 'x')
    call check_type('program p'//new_line('a')//'  complex :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'complex', 'x')

    call frontend_parse_typed_program_unit('bad.f90', &
        'program p'//new_line('a')//'  real :: y'//new_line('a')// &
        'end program p'//new_line('a'), 'type-spec-test', unit, ok, message)
    if (ok .or. trim(message) /= 'unsupported-typed-program-unit') &
        error stop 'non-permitted intrinsic variable name was accepted'

    call frontend_parse_typed_program_unit('bad.f90', &
        'program p'//new_line('a')//'  logical :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'type-spec-test', unit, ok, message)
    if (ok .or. trim(message) /= 'unsupported-typed-program-unit') &
        error stop 'unsupported intrinsic type was accepted'
    write (*, '(a)') 'frontend typed intrinsic type-spec checks: ok'

contains

    subroutine check_type(source, expected, name)
        character(len=*), intent(in) :: source, expected, name

        call frontend_parse_typed_program_unit('type.f90', source, 'type-spec-test', &
            unit, ok, message)
        if (.not. ok) error stop 'generated type-spec lookup rejected '//trim(expected)//'/'//trim(name)//': '//trim(message)
        if (trim(unit%variable%type_spec) /= trim(expected)) &
            error stop 'generated type-spec canonical value changed'
        if (trim(unit%variable%name) /= trim(name)) &
            error stop 'generated type-spec variable extraction changed'
    end subroutine check_type

end program test_frontend_typed_intrinsic_type_specs_v1
