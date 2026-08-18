program test_frontend_generated_typed_declaration_policy_v1
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    implicit none

    type(typed_program_unit_t) :: unit
    character(len=256) :: message
    logical :: ok

    call check_accepted('program p'//new_line('a')//'  integer :: x'// &
        new_line('a')//'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  integer ::'// &
        new_line('a')//'end program p'//new_line('a'))
    write (*, '(a)') 'frontend generated typed declaration policy checks: ok'

contains

    subroutine check_accepted(source)
        character(len=*), intent(in) :: source

        call frontend_parse_typed_program_unit('declaration-policy.f90', source, &
            'declaration-policy-test', unit, ok, message)
        if (.not. ok) error stop 'generated typed declaration policy rejected valid source'
    end subroutine check_accepted

    subroutine check_rejected(source)
        character(len=*), intent(in) :: source

        call frontend_parse_typed_program_unit('declaration-policy.f90', source, &
            'declaration-policy-test', unit, ok, message)
        if (ok .or. trim(message) /= 'unsupported-typed-program-unit') &
            error stop 'generated typed declaration policy accepted mutated source'
    end subroutine check_rejected

end program test_frontend_generated_typed_declaration_policy_v1
