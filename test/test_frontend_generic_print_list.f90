program test_frontend_generic_print_list
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    use frontend_print_policy_generated, only: print_stmt_validate
    implicit none

    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x'//new_line('a')//'end program main'//new_line('a'), &
        3, '(output-item (kind variable) (name x) (rule R901) (clause 12.6.3) (page 248))', &
        '(output-item (kind integer-literal) (value 7) (rule R1217) (clause 12.6.3) (page 248))')
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x, 8, x'//new_line('a')// &
        'end program main'//new_line('a'), 5, &
        '(output-item (kind integer-literal) (value 8) (rule R1217) (clause 12.6.3) (page 248))', &
        '(source-identity l3-raw-program-generic-print-list-v0)')
    call check_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 1, 7'//new_line('a')//'end program main'//new_line('a'), 2)
    call check_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x + 1, x'//new_line('a')//'end program main'//new_line('a'), 3)
    call check_provenance_mutations('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x'//new_line('a')//'end program main'//new_line('a'))

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
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 1,'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x - 1, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x + 1, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, y + 1, 7'//new_line('a')//'end program main'//new_line('a'))

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

    subroutine check_expression(source, expected_count)
        character(len=*), intent(in) :: source
        integer, intent(in) :: expected_count
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= expected_count) &
            error stop 'generic PRINT expression positive was rejected'
        if (trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT expression provenance changed'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator +) (left x) (right 1) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0) &
            error stop 'generic PRINT expression serialization changed'
    end subroutine check_expression

    subroutine check_provenance_mutations(source)
        character(len=*), intent(in) :: source
        type(program_unit_v2_t) :: unit
        character(len=128) :: message
        logical :: ok, valid
        integer :: mutation

        do mutation = 1, 9
            call frontend_parse_program_unit_v2('generic-print-mutation.f90', source, &
                'generic-print-test', unit, ok, message)
            if (.not. ok) error stop 'generic PRINT mutation fixture was rejected'
            select case (mutation)
            case (1)
                unit%execution_part%print%output_items(1)%clause = 'wrong'
            case (2)
                unit%execution_part%print%output_items(1)%page = 249
            case (3)
                unit%execution_part%print%statement_clause = 'wrong'
            case (4)
                unit%execution_part%print%format_clause = 'wrong'
            case (5)
                unit%execution_part%print%output_clause = 'wrong'
            case (6)
                unit%execution_part%print%statement_page = 243
            case (7)
                unit%execution_part%print%format_page = 245
            case (8)
                unit%execution_part%print%output_page = 249
            case (9)
                unit%execution_part%print%source_hash = 'wrong'
            end select
            valid = print_stmt_validate(unit%execution_part%print, message)
            if (valid) error stop 'generic PRINT provenance mutation was accepted'
        end do
    end subroutine check_provenance_mutations

end program test_frontend_generic_print_list
