program test_frontend_program_unit_v2_print
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t, print_stmt_validate
    use frontend_print_policy_generated, only: print_policy_output_value, &
        print_policy_output_2_value, print_policy_output_2_rule, &
        print_policy_output_3_value, print_policy_output_3_rule, &
        print_policy_output_4_value, print_policy_output_4_rule, &
        print_policy_output_5_value, print_policy_output_5_rule, &
        print_policy_output_6_value, print_policy_output_6_rule, &
        print_policy_output_7_value, print_policy_output_7_rule, &
        print_policy_output_8_value, print_policy_output_8_rule, &
        print_policy_output_9_value, print_policy_output_9_rule, &
        print_policy_output_10_value, print_policy_output_10_rule, &
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
    character(len=*), parameter :: five_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: six_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: seven_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: eight_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: nine_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: ten_item_source = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_item_source = 'program p'//new_line('a')// &
        '  print *, 17, 18, 19'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_missing_third = 'program p'//new_line('a')// &
        '  print *, 17, 18,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_wrong_third = 'program p'//new_line('a')// &
        '  print *, 17, 18, 20'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_write = 'program p'//new_line('a')// &
        '  write *, 17, 18, 19'//new_line('a')//'end program p'//new_line('a')
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
    character(len=*), parameter :: trailing_five_items = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_fifth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 12'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_five_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_sixth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_sixth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 13'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_six_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11, 12'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_seventh = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_seventh = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 14'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_seven_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11, 12, 13'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_eighth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_eighth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 15'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_eight_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11, 12, 13, 14'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_ninth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_ninth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 16'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_nine_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11, 12, 13, 14, 15'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_tenth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: wrong_tenth = 'program p'//new_line('a')// &
        '  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: write_ten_items = 'program p'//new_line('a')// &
        '  write *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16'//new_line('a')//'end program p'//new_line('a')
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
    call frontend_parse_program_unit_v2('print-five.f90', five_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 5 .or. &
        unit%execution_part%print%output_5_value /= print_policy_output_5_value .or. &
        trim(unit%execution_part%print%output_5_rule) /= print_policy_output_5_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 5)') == 0 .or. &
        index(trim(serialized), '(output-value-5 11)') == 0 .or. &
        index(trim(serialized), '(output-rule-5 R1217)') == 0) then
        error stop 'PRINT five-item serialization changed'
    end if
    call assert_rejected(trailing_five_items)
    call assert_rejected(wrong_fifth)
    call assert_rejected(write_five_items)
    call frontend_parse_program_unit_v2('print-six.f90', six_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 6 .or. &
        unit%execution_part%print%output_6_value /= print_policy_output_6_value .or. &
        trim(unit%execution_part%print%output_6_rule) /= print_policy_output_6_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11, 12 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 6)') == 0 .or. &
        index(trim(serialized), '(output-value-6 12)') == 0 .or. &
        index(trim(serialized), '(output-rule-6 R1217)') == 0) then
        error stop 'PRINT six-item serialization changed'
    end if
    call assert_rejected(missing_sixth)
    call assert_rejected(wrong_sixth)
    call assert_rejected(write_six_items)
    call frontend_parse_program_unit_v2('print-seven.f90', seven_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 7 .or. &
        unit%execution_part%print%output_7_value /= print_policy_output_7_value .or. &
        trim(unit%execution_part%print%output_7_rule) /= print_policy_output_7_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11, 12, 13 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 7)') == 0 .or. &
        index(trim(serialized), '(output-value-7 13)') == 0 .or. &
        index(trim(serialized), '(output-rule-7 R1217)') == 0) then
        error stop 'PRINT seven-item serialization changed'
    end if
    call assert_rejected(missing_seventh)
    call assert_rejected(wrong_seventh)
    call assert_rejected(write_seven_items)
    call frontend_parse_program_unit_v2('print-eight.f90', eight_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 8 .or. &
        unit%execution_part%print%output_8_value /= print_policy_output_8_value .or. &
        trim(unit%execution_part%print%output_8_rule) /= print_policy_output_8_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11, 12, 13, 14 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 8)') == 0 .or. &
        index(trim(serialized), '(output-value-8 14)') == 0 .or. &
        index(trim(serialized), '(output-rule-8 R1217)') == 0) then
        error stop 'PRINT eight-item serialization changed'
    end if
    call assert_rejected(missing_eighth)
    call assert_rejected(wrong_eighth)
    call assert_rejected(write_eight_items)
    call frontend_parse_program_unit_v2('print-nine.f90', nine_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 9 .or. &
        unit%execution_part%print%output_9_value /= print_policy_output_9_value .or. &
        trim(unit%execution_part%print%output_9_rule) /= print_policy_output_9_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11, 12, 13, 14, 15 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 9)') == 0 .or. &
        index(trim(serialized), '(output-value-9 15)') == 0 .or. &
        index(trim(serialized), '(output-rule-9 R1217)') == 0) then
        error stop 'PRINT nine-item serialization changed'
    end if
    call assert_rejected(missing_ninth)
    call assert_rejected(wrong_ninth)
    call assert_rejected(write_nine_items)
    call frontend_parse_program_unit_v2('print-ten.f90', ten_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 10 .or. &
        unit%execution_part%print%output_10_value /= print_policy_output_10_value .or. &
        trim(unit%execution_part%print%output_10_rule) /= print_policy_output_10_rule) then
        error stop 'bounded PRINT *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 10)') == 0 .or. &
        index(trim(serialized), '(output-value-10 16)') == 0 .or. &
        index(trim(serialized), '(output-rule-10 R1217)') == 0) then
        error stop 'PRINT ten-item serialization changed'
    end if
    call assert_rejected(missing_tenth)
    call assert_rejected(wrong_tenth)
    call assert_rejected(write_ten_items)
    call frontend_parse_program_unit_v2('print-generic.f90', generic_item_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        unit%execution_part%print%output_value /= 17 .or. &
        unit%execution_part%print%output_2_value /= 18 .or. &
        unit%execution_part%print%output_3_value /= 19 .or. &
        trim(unit%execution_part%print%output_2_rule) /= print_policy_output_rule .or. &
        trim(unit%execution_part%print%output_3_rule) /= print_policy_output_rule) then
        error stop 'generic PRINT *, 17, 18, 19 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-value 17)') == 0 .or. &
        index(trim(serialized), '(output-value-2 18)') == 0 .or. &
        index(trim(serialized), '(output-value-3 19)') == 0 .or. &
        index(trim(serialized), '(output-rule-2 R1217)') == 0 .or. &
        index(trim(serialized), '(output-rule-3 R1217)') == 0) then
        error stop 'generic PRINT serialization changed'
    end if
    unit%execution_part%print%output_3_value = 20
    if (print_stmt_validate(unit%execution_part%print, message)) then
        error stop 'mutated generic PRINT value passed validation'
    end if
    call frontend_parse_program_unit_v2('print-generic.f90', generic_item_source, 'print-input', &
        unit, ok, message)
    unit%execution_part%print%output_count = 2
    if (print_stmt_validate(unit%execution_part%print, message)) then
        error stop 'mutated generic PRINT cardinality passed validation'
    end if
    call assert_rejected(generic_missing_third)
    call assert_rejected(generic_wrong_third)
    call assert_rejected(generic_write)
    write (*, '(a)') 'frontend program-unit-v2 PRINT repeated-item checks: ok'

contains

    subroutine assert_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_program_unit_v2('negative-print.f90', value, 'print-input', &
            unit, ok, message)
        if (ok) error stop 'PRINT mutation was accepted'
    end subroutine assert_rejected

end program test_frontend_program_unit_v2_print
