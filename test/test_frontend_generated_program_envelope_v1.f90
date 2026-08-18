program test_frontend_generated_program_envelope_v1
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    implicit none

    type(typed_program_unit_t) :: unit
    character(len=256) :: message
    logical :: ok

    call check_accepted('program p'//new_line('a')//'  integer :: x'// &
        new_line('a')//'end program p'//new_line('a'))
    call check_accepted('program main'//new_line('a')//'  real :: x'// &
        new_line('a')//'end program'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  integer :: x'// &
        new_line('a')//'end module p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  integer :: x'// &
        new_line('a')//'end program q'//new_line('a'))
    write (*, '(a)') 'frontend generated program envelope v1 checks: ok'

contains

    subroutine check_accepted(source)
        character(len=*), intent(in) :: source

        call frontend_parse_typed_program_unit('envelope.f90', source, &
            'program-envelope-test', unit, ok, message)
        if (.not. ok) error stop 'generated program envelope rejected valid source: '//trim(message)
    end subroutine check_accepted

    subroutine check_rejected(source)
        character(len=*), intent(in) :: source

        call frontend_parse_typed_program_unit('envelope.f90', source, &
            'program-envelope-test', unit, ok, message)
        if (ok .or. trim(message) /= 'invalid-program') &
            error stop 'generated program envelope accepted invalid source'
    end subroutine check_rejected

end program test_frontend_generated_program_envelope_v1
