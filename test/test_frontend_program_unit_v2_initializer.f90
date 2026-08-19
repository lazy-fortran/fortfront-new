program test_frontend_program_unit_v2_initializer
    use fortfront_program_unit_v2, only: program_unit_v2_t, &
        frontend_parse_program_unit_v2, frontend_program_unit_v2_to_sx
    implicit none

    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok

    call assert_accepted('42', '42')
    call assert_accepted('-42', '-42')
    call assert_rejected('2048'//new_line('a'))
    call assert_rejected('-101'//new_line('a'))
    call assert_rejected('42.0'//new_line('a'))
    call assert_rejected('42'//new_line('a')//'  print *, y'//new_line('a'))
    write (*, '(a)') 'frontend program-unit-v2 generic initializer checks: ok'

contains

    subroutine assert_accepted(value, expected)
        character(len=*), intent(in) :: value, expected
        character(len=256) :: source

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(value)//new_line('a')//'  print *, x'//new_line('a')// &
            'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initializer.f90', trim(source), 'initializer-input', &
            unit, ok, message)
        if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 1 .or. &
            trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= trim(expected) .or. &
            trim(unit%root%span%source_hash) /= 'initializer-input') then
            error stop 'generic initializer acceptance changed'
        end if
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(trim(serialized), '(assignment-count 1)') == 0 .or. &
            index(trim(serialized), '(source-hash initializer-input)') == 0) then
            error stop 'generic initializer transport changed'
        end if
    end subroutine assert_accepted

    subroutine assert_rejected(initializer)
        character(len=*), intent(in) :: initializer
        character(len=256) :: source

        if (index(initializer, 'print *,') > 0) then
            source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 42'//new_line('a')//initializer//'end program main'//new_line('a')
        else
            source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = '//initializer//'  print *, x'//new_line('a')// &
                'end program main'//new_line('a')
        end if
        call frontend_parse_program_unit_v2('initializer-negative.f90', trim(source), 'initializer-input', &
            unit, ok, message)
        if (ok) error stop 'invalid generic initializer was accepted'
    end subroutine assert_rejected

end program test_frontend_program_unit_v2_initializer
