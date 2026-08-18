program test_frontend_program_unit_v2_print
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t, print_stmt_validate
    use frontend_print_policy_generated, only: print_policy_output_value, &
        print_policy_statement_rule, print_policy_format_rule, print_policy_output_rule, &
        print_policy_statement_clause, print_policy_format_clause, print_policy_output_clause, &
        print_policy_statement_page, print_policy_format_page, print_policy_output_page, &
        print_policy_source_hash
    implicit none

    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  print *, 7'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: print_eight = 'program p'//new_line('a')// &
        '  print *, 8'//new_line('a')//'end program p'//new_line('a')
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

    call assert_rejected(print_eight)
    call assert_rejected(write_seven)
    call assert_rejected(missing_item)
    write (*, '(a)') 'frontend program-unit-v2 PRINT *, 7 checks: ok'

contains

    subroutine assert_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_program_unit_v2('negative-print.f90', value, 'print-input', &
            unit, ok, message)
        if (ok) error stop 'PRINT mutation was accepted'
    end subroutine assert_rejected

end program test_frontend_program_unit_v2_print
