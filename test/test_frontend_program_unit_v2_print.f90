program test_frontend_program_unit_v2_print
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t, print_stmt_validate
    use frontend_print_policy_generated, only: print_policy_output_value, &
        print_policy_output_2_value, print_policy_output_2_rule, &
        print_policy_output_3_value, print_policy_output_3_rule, &
        print_policy_output_4_value, print_policy_output_4_rule, &
        print_policy_statement_rule, print_policy_format_rule, print_policy_output_rule, &
        print_policy_statement_clause, print_policy_format_clause, print_policy_output_clause, &
        print_policy_statement_page, print_policy_format_page, print_policy_output_page, &
        print_policy_source_hash
    implicit none

    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  print *, 7'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: print_eight = 'program p'//new_line('a')// &
        '  print *, 8'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: two_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: three_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: four_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_second = 'program p'//new_line('a')// &
        '  print *, 7,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_second = 'program p'//new_line('a')// &
        '  print *, 7, 9'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_two_items = 'program p'//new_line('a')// &
        '  write *, 7, 8'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: trailing_three_items = 'program p'//new_line('a')// &
        '  print *, 7, 8,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_third = 'program p'//new_line('a')// &
        '  print *, 7, 8, 10'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_three_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: trailing_four_items = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_fourth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 11'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_four_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_seven = 'program p'//new_line('a')// &
        '  write *, 7'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_item = 'program p'//new_line('a')// &
        '  print *,'//new_line('a')//'end program p'//new_line('a')
    character(len=256) :: message
    character(len=65536) :: serialized
    logical :: ok
    type(program_unit_v2_t) :: unit

    call frontend_parse_program_unit_v2('print.f90', source, 'print-input', unit, ok, message)
    if (.not. ok) error stop 'bounded PRINT *, 7 source was rejected'
    if (unit%execution_part%print_count /= 1 .or. &
        unit%execution_part%print%output_value /= print_policy_output_value .or. &
        trim(unit%execution_part%print%statement_rule) /= print_policy_statement_rule .or. &
        trim(unit%execution_part%print%format_rule) /= print_policy_format_rule .or. &
        trim(unit%execution_part%print%output_rule) /= print_policy_output_rule .or. &
        trim(unit%execution_part%print%statement_clause) /= print_policy_statement_clause .or. &
        trim(unit%execution_part%print%format_clause) /= print_policy_format_clause .or. &
        trim(unit%execution_part%print%output_clause) /= print_policy_output_clause .or. &
        unit%execution_part%print%statement_page /= print_policy_statement_page .or. &
        unit%execution_part%print%format_page /= print_policy_format_page .or. &
        unit%execution_part%print%output_page /= print_policy_output_page .or. &
        trim(unit%execution_part%print%source_hash) /= print_policy_source_hash) then
        error stop 'PRINT typed provenance changed'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), 'R1212') == 0 .or. &
        index(trim(serialized), 'R1215') == 0 .or. index(trim(serialized), 'R1217') == 0 .or. &
        index(trim(serialized), '(output-value 7)') == 0) then
        error stop 'PRINT serialization changed'
    end if
    unit%execution_part%print%output_value = 8
    if (print_stmt_validate(unit%execution_part%print, message)) then
        error stop 'mutated PRINT value passed validation'
    end if

    call frontend_parse_program_unit_v2('print-two.f90', two_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 2 .or. &
        unit%execution_part%print%output_2_value /= print_policy_output_2_value .or. &
        trim(unit%execution_part%print%output_2_rule) /= print_policy_output_2_rule) then
        error stop 'bounded PRINT *, 7, 8 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 2)') == 0 .or. &
        index(trim(serialized), '(output-value-2 8)') == 0 .or. &
        index(trim(serialized), '(output-rule-2 R1217)') == 0) then
        error stop 'PRINT two-item serialization changed'
    end if

    call frontend_parse_program_unit_v2('print-three.f90', three_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        unit%execution_part%print%output_3_value /= print_policy_output_3_value .or. &
        trim(unit%execution_part%print%output_3_rule) /= print_policy_output_3_rule) then
        error stop 'bounded PRINT *, 7, 8, 9 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 3)') == 0 .or. &
        index(trim(serialized), '(output-value-3 9)') == 0 .or. &
        index(trim(serialized), '(output-rule-2 R1217)') == 0 .or. &
        index(trim(serialized), '(output-rule-3 R1217)') == 0) then
        error stop 'PRINT three-item serialization changed'
    end if

    call frontend_parse_program_unit_v2('print-four.f90', four_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 4 .or. &
        unit%execution_part%print%output_4_value /= print_policy_output_4_value .or. &
        trim(unit%execution_part%print%output_4_rule) /= print_policy_output_4_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 4)') == 0 .or. &
        index(trim(serialized), '(output-value-4 10)') == 0 .or. &
        index(trim(serialized), '(output-rule-4 R1217)') == 0) then
        error stop 'PRINT four-item serialization changed'
    end if

    call assert_rejected(print_eight)
    call assert_rejected(write_seven)
    call assert_rejected(missing_item)
    call assert_rejected(missing_second)
    call assert_rejected(wrong_second)
    call assert_rejected(write_two_items)
    call assert_rejected(trailing_three_items)
    call assert_rejected(wrong_third)
    call assert_rejected(write_three_items)
    call assert_rejected(trailing_four_items)
    call assert_rejected(wrong_fourth)
    call assert_rejected(write_four_items)
    write (*, '(a)') 'frontend program-unit-v2 PRINT *, 7[, 8[, 9[, 10]]] checks: ok'

contains

    subroutine assert_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_program_unit_v2('negative-print.f90', value, 'print-input', &
            unit, ok, message)
        if (ok) error stop 'PRINT mutation was accepted'
    end subroutine assert_rejected

end program test_frontend_program_unit_v2_print
