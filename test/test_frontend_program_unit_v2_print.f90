program test_frontend_program_unit_v2_print
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t, print_stmt_validate
    use fortfront_assignment_sequence, only: assignment_sequence_source_hash
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
        print_policy_variable_output_kind, print_policy_variable_output_name, &
        print_policy_variable_output_rule, &
        print_policy_statement_rule, print_policy_format_rule, print_policy_output_rule, &
        print_policy_statement_clause, print_policy_format_clause, print_policy_output_clause, &
        print_policy_statement_page, print_policy_format_page, print_policy_output_page, &
        print_policy_source_hash, print_policy_expression_kind, &
        print_policy_expression_operator, print_policy_expression_left, &
        print_policy_expression_right, print_policy_expression_2_right
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
    character(len=*), parameter :: generic_twenty_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, 20, 21, 22'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_hundred_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, 100, 200, 300, 400, 500'// &
        new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: generic_trailing_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, 20, 21, 22,'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_real_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, 20.0, 21, 22'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_undeclared_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, 20, y, 22'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_variable_expression_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, x + x, x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_subtract_expression_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 5'//new_line('a')//'  print *, x – 2, 7'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_subtract_expression_list_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 5'//new_line('a')//'  print *, 7, x – 2, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_ascii_subtract_expression_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 5'//new_line('a')//'  print *, x - 2, 7'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_variable_expression_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, x * x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_variable_expression_wrong_name = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, y + x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_variable_expression_missing_second = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, x + x,'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: generic_missing_third = 'program p'//new_line('a')// &
        '  print *, 17, 18,'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_wrong_third = 'program p'//new_line('a')// &
        '  print *, 17, 18, 20'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: generic_write = 'program p'//new_line('a')// &
        '  write *, 17, 18, 19'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: variable_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 17'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_23_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 23'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_expression_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_multiply_expression_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x * 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_subtract_expression_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x – 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_divide_expression_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')//'  x = x / 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_24_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 24'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_missing_assignment = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_wrong_name = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 17'//new_line('a')// &
        '  print *, y'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_write = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 17'//new_line('a')// &
        '  write *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_expression_missing_second = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')//'  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')// &
        '  x = x ** 3'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')//'  x = x * 3'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_wrong_name = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')//'  y = x ** 3'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')//'  x = x ** 3'//new_line('a')// &
        '  write *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_two_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_three_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_four_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_five_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_six_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_seven_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_eight_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_nine_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_ten_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_eleven_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_twenty_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_four_item_wrong_fourth = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, y'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_four_item_malformed = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x,'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_four_item_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  write *, x, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_three_item_wrong_second = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, y, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_three_item_wrong_third = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, y'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_three_item_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  write *, x, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_three_item_malformed = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x,'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_two_item_malformed = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x,'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_two_item_wrong_second = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, y'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_two_item_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  write *, x, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_malformed = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x * 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_wrong_name = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  y = x ** 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_power_value_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  write *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_expression_wrong_assignment = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x * 1'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_multiply_expression_missing_second = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')//'  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: variable_multiply_expression_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x + 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_multiply_expression_wrong_name = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  y = x * 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_multiply_expression_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x * 2'//new_line('a')// &
        '  write *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_subtract_expression_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x + 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_divide_expression_wrong_operator = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')//'  x = x * 2'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_expression_wrong_variable = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  y = x + 1'//new_line('a')// &
        '  print *, x'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: variable_expression_write = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  write *, x'//new_line('a')//'end program main'//new_line('a')
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
    call frontend_parse_program_unit_v2('print-generic-twenty.f90', generic_twenty_source, &
        'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        unit%execution_part%print%output_items(1)%value /= 20 .or. &
        unit%execution_part%print%output_items(3)%value /= 22 .or. &
        trim(unit%root%span%source_hash) /= 'l3-raw-program-generic-print-list-v0') then
        error stop 'generic PRINT *, 20, 21, 22 witness was rejected'
    end if
    call frontend_parse_program_unit_v2('print-generic-hundred.f90', generic_hundred_source, &
        'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 5 .or. &
        unit%execution_part%print%output_items(1)%value /= 100 .or. &
        unit%execution_part%print%output_items(5)%value /= 500 .or. &
        trim(unit%root%span%source_hash) /= 'l3-raw-program-generic-print-list-v0') then
        error stop 'generic PRINT five-item literal witness was rejected'
    end if
    call assert_rejected(generic_trailing_source)
    call assert_rejected(generic_real_source)
    call assert_rejected(generic_undeclared_source)
    call assert_rejected(generic_missing_third)
    call assert_rejected(generic_wrong_third)
    call assert_rejected(generic_write)
    call frontend_parse_program_unit_v2('print-generic-expression.f90', &
        generic_variable_expression_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 2 .or. &
        trim(unit%execution_part%print%output_items(1)%kind) /= print_policy_expression_kind .or. &
        trim(unit%execution_part%print%output_items(1)%operator) /= print_policy_expression_operator .or. &
        trim(unit%execution_part%print%output_items(1)%left) /= print_policy_expression_left .or. &
        trim(unit%execution_part%print%output_items(1)%right) /= print_policy_expression_2_right .or. &
        trim(unit%execution_part%print%output_items(2)%right) /= print_policy_expression_right .or. &
        trim(unit%root%span%source_hash) /= 'l3-raw-program-generic-print-expression-v0') then
        error stop 'generic PRINT variable-expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator +)') == 0 .or. &
        index(trim(serialized), '(right x)') == 0 .or. &
        index(trim(serialized), '(source-identity l3-raw-program-generic-print-expression-v0)') == 0) then
        error stop 'generic PRINT variable-expression serialization changed'
    end if
    call frontend_parse_program_unit_v2('print-generic-subtract-expression.f90', &
        generic_subtract_expression_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 2 .or. &
        trim(unit%execution_part%print%output_items(1)%kind) /= print_policy_expression_kind .or. &
        trim(unit%execution_part%print%output_items(1)%operator) /= '–' .or. &
        trim(unit%execution_part%print%output_items(1)%left) /= 'x' .or. &
        trim(unit%execution_part%print%output_items(1)%right) /= '2' .or. &
        unit%execution_part%print%output_items(2)%value /= 7) then
        error stop 'generic PRINT subtraction expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator –)') == 0 .or. &
        index(trim(serialized), '(value 7)') == 0 .or. &
        index(trim(serialized), '(source-identity l3-raw-program-generic-print-expression-v0)') == 0) then
        error stop 'generic PRINT subtraction expression serialization changed'
    end if
    call frontend_parse_program_unit_v2('print-generic-subtract-expression-list.f90', &
        generic_subtract_expression_list_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        unit%execution_part%print%output_items(1)%value /= 7 .or. &
        trim(unit%execution_part%print%output_items(2)%operator) /= '–' .or. &
        trim(unit%execution_part%print%output_items(2)%left) /= 'x' .or. &
        trim(unit%execution_part%print%output_items(2)%right) /= '2' .or. &
        trim(unit%execution_part%print%output_items(3)%kind) /= 'variable') then
        error stop 'generic PRINT list-position subtraction witness was rejected'
    end if
    call frontend_parse_program_unit_v2('print-generic-ascii-subtract-expression.f90', &
        generic_ascii_subtract_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 2 .or. &
        trim(unit%execution_part%print%output_items(1)%operator) /= '-' .or. &
        trim(unit%execution_part%print%output_items(1)%left) /= 'x' .or. &
        trim(unit%execution_part%print%output_items(1)%right) /= '2' .or. &
        unit%execution_part%print%output_items(2)%value /= 7) then
        error stop 'generic PRINT ASCII subtraction expression witness was rejected'
    end if
    call frontend_parse_program_unit_v2('print-generic-ascii-subtract-expression-list.f90', &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 5'//new_line('a')//'  print *, 7, x - 2, x'//new_line('a')// &
        'end program main'//new_line('a'), 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        unit%execution_part%print%output_items(1)%value /= 7 .or. &
        trim(unit%execution_part%print%output_items(2)%operator) /= '-' .or. &
        trim(unit%execution_part%print%output_items(3)%kind) /= 'variable') then
        error stop 'generic PRINT ASCII list-position subtraction witness was rejected'
    end if
    call assert_rejected('program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 5'//new_line('a')//'  print *, x - 101, 7'//new_line('a')// &
        'end program main'//new_line('a'))
    call assert_rejected(generic_variable_expression_wrong_operator)
    call assert_rejected(generic_variable_expression_wrong_name)
    call assert_rejected(generic_variable_expression_missing_second)
    call frontend_parse_program_unit_v2('print-variable.f90', variable_source, 'print-input', &
        unit, ok, message)
    if (.not. ok .or. unit%declaration_count /= 1 .or. unit%variable_count /= 1 .or. &
        unit%execution_part%sequence%assignment_count /= 1 .or. &
        trim(unit%execution_part%sequence%assignment(1)%variable) /= 'x' .or. &
        unit%execution_part%sequence%assignment(1)%expression%left_operand /= '17' .or. &
        unit%execution_part%print%output_value /= 17 .or. &
        trim(unit%execution_part%print%output_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 1)') == 0 .or. &
        index(trim(serialized), '(output-kind variable)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0 .or. &
        index(trim(serialized), '(output-rule R901)') == 0 .or. &
        index(trim(serialized), '(declaration-count 1)') == 0) then
        error stop 'PRINT *, x stored-variable serialization changed'
    end if
    unit%execution_part%print%output_name = 'y'
    if (print_stmt_validate(unit%execution_part%print, message)) then
        error stop 'mutated PRINT variable name passed validation'
    end if
    call assert_rejected(variable_missing_assignment)
    call assert_rejected(variable_wrong_name)
    call assert_rejected(variable_write)
    call frontend_parse_program_unit_v2('print-variable-23.f90', variable_23_source, &
        'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment(1)%expression%left_operand /= '23' .or. &
        unit%execution_part%print%output_value /= 23 .or. &
        trim(unit%execution_part%print%output_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name) then
        error stop 'PRINT *, x stored-value 23 witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-kind variable)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0) then
        error stop 'PRINT *, x stored-value 23 serialization changed'
    end if
    call assert_rejected(variable_24_source)
    call frontend_parse_program_unit_v2('print-variable-expression.f90', &
        variable_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%declaration_count /= 1 .or. unit%variable_count /= 1 .or. &
        unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%root%span%file) /= 'print-variable-expression.f90' .or. &
        trim(unit%root%span%source_hash) /= assignment_sequence_source_hash .or. &
        trim(unit%declaration%span%file) /= 'print-variable-expression.f90' .or. &
        trim(unit%declaration%span%source_hash) /= assignment_sequence_source_hash .or. &
        trim(unit%variable%span%file) /= 'print-variable-expression.f90' .or. &
        trim(unit%variable%span%source_hash) /= assignment_sequence_source_hash .or. &
        trim(unit%execution_part%sequence%assignment(1)%span%file) /= &
        'print-variable-expression.f90' .or. &
        trim(unit%execution_part%sequence%assignment(1)%span%source_hash) /= &
        assignment_sequence_source_hash .or. &
        trim(unit%execution_part%sequence%assignment(2)%span%file) /= &
        'print-variable-expression.f90' .or. &
        trim(unit%execution_part%sequence%assignment(2)%span%source_hash) /= &
        assignment_sequence_source_hash .or. &
        unit%execution_part%sequence%assignment(1)%expression%left_operand /= '23' .or. &
        trim(unit%execution_part%sequence%assignment(2)%variable) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '+' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '1' .or. &
        unit%execution_part%print%output_value /= 23 .or. &
        trim(unit%execution_part%print%output_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name) then
        error stop 'PRINT *, x after variable expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 2)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(assignment_sequence_source_hash)//')') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(print_policy_source_hash)//')') == 0 .or. &
        index(trim(serialized), '(output-kind variable)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0) then
        error stop 'PRINT *, x after variable expression serialization changed'
    end if
    call assert_rejected(variable_expression_missing_second)
    call assert_rejected(variable_expression_wrong_assignment)
    call assert_rejected(variable_expression_wrong_variable)
    call assert_rejected(variable_expression_write)
    call frontend_parse_program_unit_v2('print-variable-multiply-expression.f90', &
        variable_multiply_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= '23' .or. &
        trim(unit%execution_part%sequence%assignment(2)%variable) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '*' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '2' .or. &
        unit%execution_part%print%output_value /= 23 .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name) then
        error stop 'PRINT *, x after variable multiply expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 2)') == 0 .or. &
        index(trim(serialized), '(operator *)') == 0 .or. &
        index(trim(serialized), '(right-operand 2)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0) then
        error stop 'PRINT *, x after variable multiply expression serialization changed'
    end if
    call assert_rejected(variable_multiply_expression_missing_second)
    call assert_rejected(variable_multiply_expression_wrong_operator)
    call assert_rejected(variable_multiply_expression_wrong_name)
    call assert_rejected(variable_multiply_expression_write)
    call frontend_parse_program_unit_v2('print-variable-subtract-expression.f90', &
        variable_subtract_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '–' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '2' .or. &
        unit%execution_part%print%output_value /= 21) then
        error stop 'PRINT *, x after variable subtraction expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator –)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(assignment_sequence_source_hash)//')') == 0) then
        error stop 'PRINT *, x after variable subtraction expression serialization changed'
    end if
    call assert_rejected(variable_subtract_expression_wrong_operator)
    call frontend_parse_program_unit_v2('print-variable-divide-expression.f90', &
        variable_divide_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '/' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '2' .or. &
        unit%execution_part%print%output_value /= 12) then
        error stop 'PRINT *, x after variable division expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator /)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(assignment_sequence_source_hash)//')') == 0) then
        error stop 'PRINT *, x after variable division expression serialization changed'
    end if
    call assert_rejected(variable_divide_expression_wrong_operator)
    call frontend_parse_program_unit_v2('print-variable-power-expression.f90', &
        variable_power_expression_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= '2' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '**' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '3' .or. &
        unit%execution_part%print%output_value /= 8 .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name) then
        error stop 'PRINT *, x after variable power expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator **)') == 0 .or. &
        index(trim(serialized), '(right-operand 3)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(assignment_sequence_source_hash)//')') == 0) then
        error stop 'PRINT *, x after variable power expression serialization changed'
    end if
    call assert_rejected(variable_power_wrong_operator)
    call assert_rejected(variable_power_wrong_name)
    call assert_rejected(variable_power_write)
    call frontend_parse_program_unit_v2('print-variable-power-value.f90', &
        variable_power_value_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= '3' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '**' .or. &
        trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= '2' .or. &
        unit%execution_part%print%output_value /= 9) then
        error stop 'PRINT *, x after second variable power expression witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator **)') == 0 .or. &
        index(trim(serialized), '(right-operand 2)') == 0 .or. &
        index(trim(serialized), '(output-name x)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(assignment_sequence_source_hash)//')') == 0) then
        error stop 'PRINT *, x after second variable power expression serialization changed'
    end if
    call frontend_parse_program_unit_v2('print-variable-power-value-malformed.f90', &
        variable_power_value_malformed, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 1) then
        error stop 'generic one-item PRINT route was rejected'
    end if
    call assert_rejected(variable_power_value_wrong_operator)
    call assert_rejected(variable_power_value_wrong_name)
    call assert_rejected(variable_power_value_write)
    call frontend_parse_program_unit_v2('print-variable-power-value-two-item.f90', &
        variable_power_value_two_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
        unit%execution_part%print%output_count /= 2 .or. &
        trim(unit%execution_part%print%output_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_name) /= print_policy_variable_output_name .or. &
        unit%execution_part%print%output_value /= 9 .or. &
        trim(unit%execution_part%print%output_2_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_2_name) /= print_policy_variable_output_name .or. &
        unit%execution_part%print%output_2_value /= 9 .or. &
        trim(unit%execution_part%print%output_2_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 2)') == 0 .or. &
        index(trim(serialized), '(output-kind-2 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-2 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-2 R901)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(print_policy_source_hash)//')') == 0) then
        error stop 'PRINT *, x, x stored-variable serialization changed'
    end if
    call assert_rejected(variable_power_value_two_item_malformed)
    call assert_rejected(variable_power_value_two_item_wrong_second)
    call assert_rejected(variable_power_value_two_item_write)
    call frontend_parse_program_unit_v2('print-variable-power-value-three-item.f90', &
        variable_power_value_three_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 3 .or. &
        trim(unit%execution_part%print%output_2_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_3_name) /= print_policy_variable_output_name .or. &
        unit%execution_part%print%output_3_value /= 9 .or. &
        trim(unit%execution_part%print%output_3_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x, x, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 3)') == 0 .or. &
        index(trim(serialized), '(output-name-3 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-3 R901)') == 0) then
        error stop 'PRINT *, x, x, x stored-variable serialization changed'
    end if
    call assert_rejected(variable_power_value_three_item_wrong_second)
    call assert_rejected(variable_power_value_three_item_wrong_third)
    call assert_rejected(variable_power_value_three_item_write)
    call assert_rejected(variable_power_value_three_item_malformed)
    call frontend_parse_program_unit_v2('print-variable-power-value-four-item.f90', &
        variable_power_value_four_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 4 .or. &
        trim(unit%execution_part%print%output_4_kind) /= print_policy_variable_output_kind .or. &
        unit%execution_part%print%output_4_value /= 9 .or. &
        trim(unit%execution_part%print%output_4_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x, x, x, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 4)') == 0 .or. &
        index(trim(serialized), '(output-name-4 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-4 R901)') == 0) then
        error stop 'PRINT *, x, x, x, x stored-variable serialization changed'
    end if
    call assert_rejected(variable_power_value_four_item_wrong_fourth)
    call assert_rejected(variable_power_value_four_item_malformed)
    call assert_rejected(variable_power_value_four_item_write)
    call frontend_parse_program_unit_v2('print-variable-power-value-five-item.f90', &
        variable_power_value_five_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 5 .or. &
        trim(unit%execution_part%print%output_5_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_5_name) /= print_policy_variable_output_name .or. &
        unit%execution_part%print%output_5_value /= 9 .or. &
        trim(unit%execution_part%print%output_5_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x, x, x, x, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 5)') == 0 .or. &
        index(trim(serialized), '(output-kind-5 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-5 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-5 R901)') == 0) then
        error stop 'PRINT *, x, x, x, x, x stored-variable serialization changed'
    end if
    call frontend_parse_program_unit_v2('print-variable-power-value-six-item.f90', &
        variable_power_value_six_item_source, 'print-input', unit, ok, message)
    if (.not. ok .or. unit%execution_part%print%output_count /= 6 .or. &
        trim(unit%execution_part%print%output_2_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_2_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_2_rule) /= print_policy_variable_output_rule .or. &
        trim(unit%execution_part%print%output_3_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_3_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_3_rule) /= print_policy_variable_output_rule .or. &
        trim(unit%execution_part%print%output_4_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_4_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_4_rule) /= print_policy_variable_output_rule .or. &
        trim(unit%execution_part%print%output_5_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_5_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_5_rule) /= print_policy_variable_output_rule .or. &
        trim(unit%execution_part%print%output_6_kind) /= print_policy_variable_output_kind .or. &
        trim(unit%execution_part%print%output_6_name) /= print_policy_variable_output_name .or. &
        trim(unit%execution_part%print%output_6_rule) /= print_policy_variable_output_rule) then
        error stop 'PRINT *, x, x, x, x, x, x stored-variable witness was rejected'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(output-count 6)') == 0 .or. &
        index(trim(serialized), '(output-kind-2 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-2 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-2 R901)') == 0 .or. &
        index(trim(serialized), '(output-kind-3 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-3 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-3 R901)') == 0 .or. &
        index(trim(serialized), '(output-kind-4 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-4 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-4 R901)') == 0 .or. &
        index(trim(serialized), '(output-kind-5 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-5 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-5 R901)') == 0 .or. &
        index(trim(serialized), '(output-kind-6 variable)') == 0 .or. &
        index(trim(serialized), '(output-name-6 x)') == 0 .or. &
        index(trim(serialized), '(output-rule-6 R901)') == 0 .or. &
        index(trim(serialized), '(source-hash '//trim(print_policy_source_hash)//')') == 0) then
        error stop 'PRINT *, x, x, x, x, x, x stored-variable serialization changed'
    end if
    call assert_variable_repeat(variable_power_value_seven_item_source, 7)
    call assert_variable_repeat(variable_power_value_eight_item_source, 8)
    call assert_variable_repeat(variable_power_value_nine_item_source, 9)
    call assert_variable_repeat(variable_power_value_ten_item_source, 10)
    call assert_variable_repeat(variable_power_value_eleven_item_source, 11)
    call assert_variable_repeat(variable_power_value_twenty_item_source, 20)
    call assert_variable_repeat_count(21)
    call assert_variable_repeat_count(30)
    call assert_variable_repeat_count(40)
    call assert_variable_repeat_count(41)
    call assert_variable_repeat_count(50)
    call assert_variable_repeat_count(60)
    call assert_variable_repeat_count(61)
    call assert_variable_repeat_count(70)
    call assert_variable_repeat_count(80)
    call assert_variable_repeat_count(81)
    call assert_variable_repeat_count(90)
    call assert_variable_repeat_count(100)
    write (*, '(a)') 'frontend program-unit-v2 PRINT repeated-item checks: ok'

contains

    subroutine assert_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_program_unit_v2('negative-print.f90', value, 'print-input', &
            unit, ok, message)
        if (ok) error stop 'PRINT mutation was accepted'
    end subroutine assert_rejected

    subroutine assert_variable_repeat(value, expected_count)
        character(len=*), intent(in) :: value
        integer, intent(in) :: expected_count
        character(len=32) :: index_s
        character(len=64) :: expected_kind, expected_name, expected_rule
        integer :: item_index

        call frontend_parse_program_unit_v2('print-variable-repeat.f90', value, &
            'print-input', unit, ok, message)
        if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2 .or. &
            unit%execution_part%print%output_count /= expected_count .or. &
            unit%execution_part%print%output_value /= 9 .or. &
            trim(unit%execution_part%print%source_hash) /= trim(print_policy_source_hash)) then
            error stop 'PRINT repeated-variable frontend route was rejected'
        end if
        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok) error stop 'PRINT repeated-variable serialization failed'
        write (index_s, '(i0)') expected_count
        if (index(trim(serialized), '(output-count '//trim(index_s)//')') == 0 .or. &
            index(trim(serialized), '(source-hash '//trim(print_policy_source_hash)//')') == 0) then
            error stop 'PRINT repeated-variable count or provenance changed'
        end if
        do item_index = 2, expected_count
            write (index_s, '(i0)') item_index
            expected_kind = '(output-kind-'//trim(index_s)//' '// &
                trim(print_policy_variable_output_kind)//')'
            expected_name = '(output-name-'//trim(index_s)//' '// &
                trim(print_policy_variable_output_name)//')'
            expected_rule = '(output-rule-'//trim(index_s)//' '// &
                trim(print_policy_variable_output_rule)//')'
            if (index(trim(serialized), trim(expected_kind)) == 0 .or. &
                index(trim(serialized), trim(expected_name)) == 0 .or. &
                index(trim(serialized), trim(expected_rule)) == 0) then
                error stop 'PRINT repeated-variable AST-v2 item witness changed'
            end if
        end do
    end subroutine assert_variable_repeat

    subroutine assert_variable_repeat_count(expected_count)
        integer, intent(in) :: expected_count
        character(len=65536) :: generated_source

        generated_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
            '  x = x ** 2'//new_line('a')//'  print *, '// &
            repeat('x, ', expected_count - 1)//'x'//new_line('a')// &
            'end program main'//new_line('a')
        call assert_variable_repeat(trim(generated_source), expected_count)

        generated_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
            '  x = x ** 2'//new_line('a')//'  print *, '// &
            repeat('x, ', expected_count - 1)//'y'//new_line('a')// &
            'end program main'//new_line('a')
        call assert_rejected(trim(generated_source))

        generated_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
            '  x = x ** 2'//new_line('a')//'  write *, '// &
            repeat('x, ', expected_count - 1)//'x'//new_line('a')// &
            'end program main'//new_line('a')
        call assert_rejected(trim(generated_source))

        generated_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
            '  x = x ** 2'//new_line('a')//'  print *, '// &
            repeat('x, ', expected_count)//new_line('a')// &
            'end program main'//new_line('a')
        call assert_rejected(trim(generated_source))
        if (expected_count < 21 .or. expected_count > 100) then
            error stop 'invalid focused PRINT count'
        end if
    end subroutine assert_variable_repeat_count

end program test_frontend_program_unit_v2_print
