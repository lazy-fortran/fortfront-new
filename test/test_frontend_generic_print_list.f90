program test_frontend_generic_print_list
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    use frontend_print_policy_generated, only: print_stmt_validate
    implicit none

    call check_pure_literal('program p'//new_line('a')// &
        '  print *, 31, 47, 59, 71'//new_line('a')//'end program p'//new_line('a'), &
        [31_int64, 47_int64, 59_int64, 71_int64])
    call check_pure_literal('program p'//new_line('a')// &
        '  print *, 7'//new_line('a')//'end program p'//new_line('a'), [7_int64])
    call check_pure_literal('program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16'//new_line('a')// &
        'end program p'//new_line('a'), [7_int64, 8_int64, 9_int64, 10_int64, &
        11_int64, 12_int64, 13_int64, 14_int64, 15_int64, 16_int64])
    call check_rejected('program p'//new_line('a')//'  print *,'//new_line('a')// &
        'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  print *, 31,'//new_line('a')// &
        'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  print *, 31.0'//new_line('a')// &
        'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  write *, 31'//new_line('a')// &
        'end program p'//new_line('a'))
    call check_rejected('program p'//new_line('a')// &
        '  print *, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11'//new_line('a')// &
        'end program p'//new_line('a'))
    call check_rejected('module p'//new_line('a')//'  print *, 31'//new_line('a')// &
        'end module p'//new_line('a'))
    call check_rejected('program p'//new_line('a')//'  print *, 31'//new_line('a')// &
        'end program q'//new_line('a'))

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
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a'), 1, &
        '(output-item (kind variable) (name x) (rule R901) (clause 12.6.3) (page 248))', &
        '(source-identity l3-raw-program-generic-print-list-v0)')
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x, 8'//new_line('a')//'end program main'//new_line('a'), 4, &
        '(output-item (kind integer-literal) (value 8) (rule R1217) (clause 12.6.3) (page 248))', &
        '(source-identity l3-raw-program-generic-print-list-v0)')
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x, 8, x, 9, x, 10, x, 11'//new_line('a')// &
        'end program main'//new_line('a'), 10, &
        '(output-item (kind integer-literal) (value 11) (rule R1217) (clause 12.6.3) (page 248))', &
        '(source-identity l3-raw-program-generic-print-list-v0)')
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 37'//new_line('a')// &
        '  print *, x, 11, x'//new_line('a')//'end program main'//new_line('a'), 3, &
        '(output-item (kind variable) (name x) (rule R901) (clause 12.6.3) (page 248))', &
        '(output-item (kind integer-literal) (value 11) (rule R1217) (clause 12.6.3) (page 248))')
    call check_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 1, 7'//new_line('a')//'end program main'//new_line('a'), 2)
    call check_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x + 1, x'//new_line('a')//'end program main'//new_line('a'), 3)
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 2, 7'//new_line('a')//'end program main'//new_line('a'), 1, '2')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x + 2, x'//new_line('a')//'end program main'//new_line('a'), 2, '2')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 3, 7'//new_line('a')//'end program main'//new_line('a'), 1, '3')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x + 4, x'//new_line('a')//'end program main'//new_line('a'), 2, '4')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x + 0, x - 0, 7'//new_line('a')//'end program main'//new_line('a'), 1, '0')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x + 10, x - 10'//new_line('a')//'end program main'//new_line('a'), 2, '10')
    call check_add_constant_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x + 100, 7'//new_line('a')//'end program main'//new_line('a'), 1, '100')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x - 0, 7'//new_line('a')//'end program main'//new_line('a'), 1, 2, '-', '0')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x - 10, x'//new_line('a')//'end program main'//new_line('a'), 2, 3, '-', '10')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x - 100, x'//new_line('a')//'end program main'//new_line('a'), 2, 3, '-', '100')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x – 2, 7'//new_line('a')//'end program main'//new_line('a'), 1, 2, '–', '2')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x – 2, x'//new_line('a')//'end program main'//new_line('a'), 2, 3, '–', '2')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x - 2, 7'//new_line('a')//'end program main'//new_line('a'), 1, 2, '-', '2')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x - 2, x'//new_line('a')//'end program main'//new_line('a'), 2, 3, '-', '2')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x - 3, 7'//new_line('a')//'end program main'//new_line('a'), 1, 2, '-', '3')
    call check_subtract_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, 7, x - 4, x'//new_line('a')//'end program main'//new_line('a'), 2, 3, '-', '4')
    call check_multiply_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x * 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_divide_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x / 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_positive('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x * 2, x ** 10, x - 4'//new_line('a')// &
        'end program main'//new_line('a'), 3, &
        '(output-item (kind integer-expression) (operator **) (left x) (right 10) '// &
        '(rule R1217) (clause 12.6.3) (page 248))', &
        '(source-identity l3-raw-program-generic-print-expression-v0)')
    call check_power_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_power_expression_three('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** 3, 7'//new_line('a')//'end program main'//new_line('a'), 2)
    call check_power_expression_three('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x ** 3, x'//new_line('a')//'end program main'//new_line('a'), 3)
    call check_power_expression_four('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** 4, 7'//new_line('a')//'end program main'//new_line('a'), 2)
    call check_power_expression_four('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x ** 4, x'//new_line('a')//'end program main'//new_line('a'), 3)
    call check_dynamic_power_expression('x ** 5', 243_int64)
    call check_dynamic_power_expression('x ** 7', 2187_int64)
    call check_dynamic_power_expression('x ** 10', 59049_int64)
    call check_variable_power_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** x, 7'//new_line('a')//'end program main'//new_line('a'), 27_int64)
    call check_variable_power_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, 7, x ** x, x'//new_line('a')//'end program main'//new_line('a'), 27_int64)
    call check_variable_power_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 4'//new_line('a')// &
        '  print *, x ** x, 7'//new_line('a')//'end program main'//new_line('a'), 256_int64)
    call check_variable_power_expression('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 4'//new_line('a')// &
        '  print *, 7, x ** x, x'//new_line('a')//'end program main'//new_line('a'), 256_int64)
    call check_provenance_mutations('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x, 7, x'//new_line('a')//'end program main'//new_line('a'))
    call check_variable_identifier_and_value()

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
        '  print *, x, 7, x, 8, x, 9, x, 10, x, 11, x'//new_line('a')// &
        'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x - 101, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
        '  print *, x + 0.0, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x + 101, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x + 1, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, y + 1, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x * 3, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x * 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, y * 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x / 3, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x - 101, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x / 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, y / 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  write *, x ** 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, y ** 2, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** y, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** -1, 7'//new_line('a')//'end program main'//new_line('a'))
    call check_rejected('program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
        '  print *, x ** 2.0, 7'//new_line('a')//'end program main'//new_line('a'))

contains

    subroutine check_pure_literal(source, expected_values)
        character(len=*), intent(in) :: source
        integer(int64), intent(in) :: expected_values(:)
        type(program_unit_v2_t) :: unit
        character(len=128) :: message
        character(len=64) :: source_hash
        logical :: ok
        integer :: item, print_start, line_end

        source_hash = 'pure-print-test-source'
        call frontend_parse_program_unit_v2('pure-print.f90', source, source_hash, unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= size(expected_values)) &
            error stop 'pure integer PRINT was rejected'
        if (trim(unit%root%span%source_hash) /= trim(source_hash) .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-list-v0') &
            error stop 'pure integer PRINT provenance changed'
        print_start = index(source, '  print *,')
        line_end = index(source(print_start:), new_line('a')) + print_start - 1
        if (unit%execution_part%print%span%start_byte /= int(print_start - 1, int64) .or. &
            unit%execution_part%print%span%end_byte /= int(line_end - 1, int64)) &
            error stop 'pure integer PRINT span changed'
        do item = 1, size(expected_values)
            if (trim(unit%execution_part%print%output_items(item)%kind) /= 'integer-literal' .or. &
                unit%execution_part%print%output_items(item)%value /= expected_values(item) .or. &
                trim(unit%execution_part%print%output_items(item)%rule) /= 'R1217' .or. &
                trim(unit%execution_part%print%output_items(item)%clause) /= '12.6.3' .or. &
                unit%execution_part%print%output_items(item)%page /= 248_int64) &
                error stop 'pure integer PRINT output item changed'
        end do
    end subroutine check_pure_literal

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

    subroutine check_add_constant_expression(source, expression_index, expected_right)
        character(len=*), intent(in) :: source, expected_right
        integer, intent(in) :: expression_index
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-add-constant.f90', &
            source, 'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_items(expression_index)%kind /= &
            'integer-expression' .or. &
            trim(unit%execution_part%print%output_items(expression_index)%operator) /= '+' .or. &
            trim(unit%execution_part%print%output_items(expression_index)%left) /= 'x' .or. &
            trim(unit%execution_part%print%output_items(expression_index)%right) /= expected_right .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0') then
            error stop 'generic PRINT x + 2 expression was rejected'
        end if
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator +) (left x) (right '//expected_right//') '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0) then
            error stop 'generic PRINT x + 2 expression serialization changed'
        end if
    end subroutine check_add_constant_expression

    subroutine check_subtract_expression(source, expression_index, expected_count, expected_operator, expected_right)
        character(len=*), intent(in) :: source, expected_operator, expected_right
        integer, intent(in) :: expression_index, expected_count
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-subtract-expression.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= expected_count .or. &
            trim(unit%execution_part%print%output_items(expression_index)%kind) /= &
            'integer-expression' .or. &
            trim(unit%execution_part%print%output_items(expression_index)%operator) /= expected_operator .or. &
            trim(unit%execution_part%print%output_items(expression_index)%left) /= 'x' .or. &
            trim(unit%execution_part%print%output_items(expression_index)%right) /= expected_right .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0') then
            error stop 'generic PRINT subtraction expression was rejected'
        end if
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, '(operator '//expected_operator//')') == 0 .or. &
            index(serialized, '(left x)') == 0 .or. index(serialized, '(right '//expected_right//')') == 0) then
            error stop 'generic PRINT subtraction expression serialization changed'
        end if
    end subroutine check_subtract_expression

    subroutine check_multiply_expression(source)
        character(len=*), intent(in) :: source
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-multiply.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= 2) &
            error stop 'generic PRINT multiply expression was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator *) (left x) (right 2) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0 .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT multiply expression shape or provenance changed'
    end subroutine check_multiply_expression

    subroutine check_divide_expression(source)
        character(len=*), intent(in) :: source
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-divide.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= 2) &
            error stop 'generic PRINT divide expression was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator /) (left x) (right 2) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0 .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT divide expression shape or provenance changed'
    end subroutine check_divide_expression

    subroutine check_power_expression(source)
        character(len=*), intent(in) :: source
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-power.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= 2) &
            error stop 'generic PRINT power expression was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator **) (left x) (right 2) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0 .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT power expression shape or provenance changed'
    end subroutine check_power_expression

    subroutine check_power_expression_three(source, expected_count)
        character(len=*), intent(in) :: source
        integer, intent(in) :: expected_count
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-power-three.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= expected_count) &
            error stop 'generic PRINT power-three expression was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator **) (left x) (right 3) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0 .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT power-three expression shape or provenance changed'
    end subroutine check_power_expression_three

    subroutine check_power_expression_four(source, expected_count)
        character(len=*), intent(in) :: source
        integer, intent(in) :: expected_count
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        call frontend_parse_program_unit_v2('generic-print-expression-power-four.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= expected_count) &
            error stop 'generic PRINT power-four expression was rejected'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator **) (left x) (right 4) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0 .or. &
            trim(unit%root%span%source_hash) /= &
            'l3-raw-program-generic-print-expression-v0' .or. &
            trim(unit%execution_part%print%source_identity) /= &
            'l3-raw-program-generic-print-expression-v0') &
            error stop 'generic PRINT power-four expression shape or provenance changed'
    end subroutine check_power_expression_four

    subroutine check_dynamic_power_expression(expression, expected_value)
        character(len=*), intent(in) :: expression
        integer(int64), intent(in) :: expected_value
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        character(len=256) :: expected_item
        logical :: ok

        if ((trim(expression) == 'x ** 5' .and. expected_value /= 243_int64) .or. &
            (trim(expression) == 'x ** 7' .and. expected_value /= 2187_int64) .or. &
            (trim(expression) == 'x ** 10' .and. expected_value /= 59049_int64)) &
            error stop 'generic PRINT dynamic power oracle value changed'

        call frontend_parse_program_unit_v2('generic-print-expression-power-dynamic.f90', &
            'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = 3'//new_line('a')//'  print *, '//trim(expression)//', 7'// &
            new_line('a')//'end program main'//new_line('a'), &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count /= 2_int64) &
            error stop 'generic PRINT dynamic power expression was rejected'
        write (expected_item, '(a)') '(output-item (kind integer-expression) (operator **) '// &
            '(left x) (right '//trim(expression(index(expression, '**') + 3:))// &
            ') (rule R1217) (clause 12.6.3) (page 248))'
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, trim(expected_item)) == 0 .or. &
            trim(unit%execution_part%print%output_items(1)%right) /= &
            trim(expression(index(expression, '**') + 3:))) &
            error stop 'generic PRINT dynamic power expression shape changed'
    end subroutine check_dynamic_power_expression

    subroutine check_variable_power_expression(source, expected_value)
        character(len=*), intent(in) :: source
        integer(int64), intent(in) :: expected_value
        type(program_unit_v2_t) :: unit
        character(len=65536) :: serialized, message
        logical :: ok

        if (expected_value /= 27_int64 .and. expected_value /= 256_int64) &
            error stop 'variable power oracle value changed'
        call frontend_parse_program_unit_v2('l3_generic_print_expression_power.f90', source, &
            'generic-print-expression-test', unit, ok, message)
        if (.not. ok .or. unit%execution_part%print%output_count < 2_int64) &
            error stop 'generic PRINT variable power expression was rejected'
        if (trim(unit%execution_part%print%output_items(1)%operator) /= '**' .and. &
            trim(unit%execution_part%print%output_items(2)%operator) /= '**') &
            error stop 'generic PRINT variable power operator missing'
        if (trim(unit%execution_part%print%output_items(1)%right) == 'x') then
            if (trim(unit%execution_part%print%output_items(1)%left) /= 'x') &
                error stop 'generic PRINT variable power left operand changed'
        else if (trim(unit%execution_part%print%output_items(2)%right) == 'x') then
            if (trim(unit%execution_part%print%output_items(2)%left) /= 'x') &
                error stop 'generic PRINT variable power left operand changed'
        else
            error stop 'generic PRINT variable power right operand changed'
        end if
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(serialized, &
            '(output-item (kind integer-expression) (operator **) (left x) (right x) '// &
            '(rule R1217) (clause 12.6.3) (page 248))') == 0) &
            error stop 'generic PRINT variable power AST shape changed'
    end subroutine check_variable_power_expression

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

    subroutine check_variable_identifier_and_value()
        character(len=256) :: message
        logical :: ok, valid
        type(program_unit_v2_t) :: unit

        call frontend_parse_program_unit_v2('generic-print-variable.f90', &
            'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = 3'//new_line('a')//'  print *, x'//new_line('a')// &
            'end program main'//new_line('a'), 'generic-print-test', unit, ok, message)
        if (.not. ok) error stop 'generic PRINT variable fixture was rejected'
        unit%execution_part%print%output_items(1)%name = 'counter_2'
        unit%execution_part%print%output_items(1)%value = -100_int64
        unit%execution_part%print%output_value = -100_int64
        valid = print_stmt_validate(unit%execution_part%print, message)
        if (.not. valid) error stop 'legal PRINT variable name/value was rejected'

        unit%execution_part%print%output_items(1)%name = '2counter'
        valid = print_stmt_validate(unit%execution_part%print, message)
        if (valid) error stop 'malformed PRINT variable name was accepted'

        unit%execution_part%print%output_items(1)%name = 'counter_2'
        unit%execution_part%print%output_items(1)%value = -101_int64
        unit%execution_part%print%output_value = -101_int64
        valid = print_stmt_validate(unit%execution_part%print, message)
        if (valid) error stop 'out-of-range PRINT variable value was accepted'
    end subroutine check_variable_identifier_and_value

end program test_frontend_generic_print_list
