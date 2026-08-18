program test_frontend_typed_cardinality_policy_v1
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    implicit none

    type(typed_program_unit_t) :: unit
    character(len=256) :: message
    logical :: ok

    call check_accepted('integer', 'x')
    call check_accepted('real', 'x')
    call check_accepted('double precision', 'x')
    call check_accepted('complex', 'x')
    call check_rejected('program p'//new_line('a')//'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  integer ::'// &
        new_line('a')//'end program p'//new_line('a'))
    write (*, '(a)') 'frontend typed cardinality policy checks: ok'

contains

    subroutine check_accepted(type_spec, name)
        character(len=*), intent(in) :: type_spec
        character(len=*), intent(in) :: name
        character(len=256) :: source

        source = 'program p'//new_line('a')//'  '//trim(type_spec)//' :: '// &
            trim(name)//new_line('a')//'end program p'//new_line('a')
        call frontend_parse_typed_program_unit('cardinality.f90', trim(source), &
            'cardinality-test', unit, ok, message)
        if (.not. ok .or. unit%declaration_count /= 1 .or. &
            unit%variable_count /= 1) &
            error stop 'typed cardinality policy rejected valid source'
    end subroutine check_accepted

    subroutine check_rejected(source)
        character(len=*), intent(in) :: source

        call frontend_parse_typed_program_unit('cardinality.f90', source, &
            'cardinality-test', unit, ok, message)
        if (ok) error stop 'typed cardinality policy accepted invalid source'
    end subroutine check_rejected

end program test_frontend_typed_cardinality_policy_v1
