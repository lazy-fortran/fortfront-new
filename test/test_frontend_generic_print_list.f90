program test_frontend_generic_print_list
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x'//new_line('a')//'end program main'//new_line('a'), &
        3, '(output-item (kind variable) (name x) (rule R901))', &
        '(output-item (kind integer-literal) (value 7) (rule R1217))')
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x, 8, x'//new_line('a')// &
        'end program main'//new_line('a'), 5, &
        '(output-item (kind integer-literal) (value 8) (rule R1217))', &
        '(source-identity l3-raw-program-generic-print-list-v0)')

    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *,'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x,'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x, 7, x'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, y'//new_line('a')//'end program main'//new_line('a'))

contains

    subroutine check_positive(source, expected_count, expected_item, expected_extra)
        character(len=*), intent(in) :: source, expected_item, expected_extra
        integer, intent(in) :: expected_count
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print.f90', source, &
            'generic-print-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= expected_count) &
            error stop 'generic PRINT positive was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, '(output-items ') == 0 .or. &
            index(serialized, expected_item) == 0 .or. index(serialized, expected_extra) == 0) &
            error stop 'generic PRINT AST list shape mismatch'
    end subroutine check_positive

    subroutine check_rejected(source)
        character(len=*), intent(in) :: source
        type(program_unit_v2_t) :: unit
        character(len=128) :: message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-negative.f90', source, &
            'generic-print-test', unit, ok, message)
        if (ok) error stop 'invalid generic PRINT source was accepted'
    end subroutine check_rejected

end program test_frontend_generic_print_list
