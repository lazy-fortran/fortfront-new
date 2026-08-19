module fortfront_program_unit_v2
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: program_root_t, program_declaration_t, &
        variable_declaration_t, program_root_to_sx, program_declaration_to_sx, &
        variable_declaration_to_sx, source_span_t, source_span_to_sx, &
        source_span_validate
    use fortfront_assignment_sequence, only: assignment_sequence_t, &
        frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, assignment_sequence_source_hash, &
        assignment_sequence_two_23_source, assignment_sequence_two_23_multiply_source, &
        assignment_sequence_two_23_subtract_source, assignment_sequence_two_24_divide_source, &
        assignment_sequence_two_2_power_source, assignment_sequence_two_3_power_source, &
        assignment_sequence_two_negative_source
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    use frontend_assignment_policy_generated, only: assignment_policy_integer_literal_min, &
        assignment_policy_integer_literal_max, assignment_policy_signed_integer_literal_min, &
        assignment_policy_signed_integer_literal_max
    use frontend_program_unit_v2_envelope_generated, only: &
        program_unit_v2_execution_part_policy_matches
    use frontend_stop_policy_generated, only: stop_policy_code, &
        stop_policy_code_rule, stop_policy_clause, &
        stop_policy_document, stop_policy_page, stop_policy_source_hash, &
        stop_policy_statement_rule
    use frontend_print_policy_generated, only: print_stmt_t, print_stmt_validate, &
        print_stmt_to_sx, print_policy_format_kind, print_policy_format_value, &
        print_policy_output_kind, print_policy_output_value, &
        print_policy_variable_output_kind, print_policy_variable_output_name, &
        print_policy_variable_output_rule, &
        print_policy_variable_value, print_policy_variable_value_2, &
        print_policy_variable_value_5, print_policy_variable_value_6, &
        print_policy_output_2_kind, print_policy_output_2_value, &
        print_policy_output_3_kind, print_policy_output_3_value, print_policy_output_3_rule, &
        print_policy_output_4_kind, print_policy_output_4_value, print_policy_output_4_rule, &
        print_policy_output_5_kind, print_policy_output_5_value, print_policy_output_5_rule, &
        print_policy_output_6_kind, print_policy_output_6_value, print_policy_output_6_rule, &
        print_policy_output_7_kind, print_policy_output_7_value, print_policy_output_7_rule, &
        print_policy_output_8_kind, print_policy_output_8_value, print_policy_output_8_rule, &
        print_policy_output_9_kind, print_policy_output_9_value, print_policy_output_9_rule, &
        print_policy_output_10_kind, print_policy_output_10_value, print_policy_output_10_rule, &
        print_policy_statement_rule, print_policy_format_rule, print_policy_output_rule, &
        print_policy_output_2_rule, &
        print_policy_document, print_policy_statement_clause, print_policy_format_clause, &
        print_policy_output_clause, print_policy_statement_page, &
        print_policy_format_page, print_policy_output_page, print_policy_source_hash
    use frontend_print_policy_generated, only: print_policy_generic_source_identity, &
        print_policy_expression_source_identity, print_policy_expression_operator, &
        print_policy_expression_left, print_policy_expression_right, &
        print_policy_expression_2_operator, print_policy_expression_2_left, &
        print_policy_expression_2_right, print_policy_expression_source, &
        print_policy_expression_2_source, print_policy_expression_3_operator, &
        print_policy_expression_3_left, print_policy_expression_3_right, &
        print_policy_expression_3_source, print_policy_expression_4_operator, &
        print_policy_expression_4_left, print_policy_expression_4_right, &
        print_policy_expression_4_source, print_policy_power_operator, &
        print_policy_power_left, print_policy_power_min, print_policy_power_max, &
        print_policy_expression_5_operator, print_policy_expression_5_left, &
        print_policy_expression_5_right, print_policy_expression_5_source, &
        print_policy_expression_6_operator, print_policy_expression_6_left, &
        print_policy_expression_6_right, print_policy_expression_6_source, &
        print_policy_expression_7_operator, print_policy_expression_7_left, &
        print_policy_expression_7_right, print_policy_expression_7_source, &
        print_policy_expression_8_operator, print_policy_expression_8_left, &
        print_policy_expression_8_right, print_policy_expression_8_source, &
        print_policy_expression_9_operator, print_policy_expression_9_left, &
        print_policy_expression_9_right, print_policy_expression_9_source, &
        print_policy_expression_10_operator, print_policy_expression_10_left, &
        print_policy_expression_10_right, print_policy_expression_10_source, &
        print_policy_expression_11_operator, print_policy_expression_11_left, &
        print_policy_expression_11_right, print_policy_expression_11_source, &
        print_policy_expression_12_operator, print_policy_expression_12_left, &
        print_policy_expression_12_right, print_policy_expression_12_source, &
        print_policy_integer_literal_min, &
        print_policy_signed_integer_literal_min, print_policy_signed_integer_literal_max, &
        print_policy_decimal_expression_valid, &
        print_policy_output_count_min, print_policy_output_count_max
    implicit none
    private

    integer(int64), parameter :: initialized_power_min = 2_int64
    integer(int64), parameter :: initialized_power_max = 4_int64

    type, public :: stop_stmt_t
        integer(int64) :: code = 0_int64
        type(source_span_t) :: span
        character(len=32) :: source_rule = ''
        character(len=32) :: code_rule = ''
        character(len=128) :: source_document = ''
        character(len=32) :: source_clause = ''
        integer(int64) :: source_page = 0_int64
        character(len=128) :: source_hash = ''
    end type stop_stmt_t

    type, public :: execution_part_t
        type(assignment_sequence_t) :: sequence
        integer(int64) :: stop_count = 0_int64
        type(stop_stmt_t) :: stop
        integer(int64) :: print_count = 0_int64
        type(print_stmt_t) :: print
    end type execution_part_t

    type, public :: program_unit_v2_t
        type(program_root_t) :: root
        integer(int64) :: declaration_count = 0_int64
        type(program_declaration_t) :: declaration
        integer(int64) :: variable_count = 0_int64
        type(variable_declaration_t) :: variable
        type(execution_part_t) :: execution_part
    end type program_unit_v2_t

    public :: frontend_parse_program_unit_v2
    public :: frontend_program_unit_v2_to_sx
    public :: stop_stmt_validate
    public :: print_stmt_validate

    character(len=*), parameter :: two_assignment_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: five_assignment_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_assignment_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: stop_seven_source = &
        'program p'//new_line('a')//'  stop 7'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_source = &
        'program p'//new_line('a')//'  print *, 7'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_source = &
        'program p'//new_line('a')//'  print *, 7, 8'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_nine_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_nine_ten_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_nine_ten_eleven_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_nine_ten_eleven_twelve_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11, 12'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_seven_eight_nine_ten_eleven_twelve_thirteen_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11, 12, 13'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_eight_item_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11, 12, 13, 14'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_nine_item_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_ten_item_source = &
        'program p'//new_line('a')//'  print *, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_generic_item_source = &
        'program p'//new_line('a')//'  print *, 17, 18, 19'//new_line('a')// &
        'end program p'//new_line('a')
    character(len=*), parameter :: print_variable_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 17'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_23_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_zero_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 0'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_2047_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 2047'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_negative_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = -5'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_negative_boundary_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = -100'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_multiply_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x * 2'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_subtract_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x – 2'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_divide_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')// &
        '  x = x / 2'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_expression_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')// &
        '  x = x ** 3'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_two_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_three_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_four_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_five_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_six_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_seven_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_eight_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_nine_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_ten_item_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_eleven_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_twelve_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_thirteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_fourteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_fifteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_sixteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_seventeen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_eighteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_nineteen_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_twenty_item_source = &
        'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')//'  x = x ** 2'//new_line('a')// &
        '  print *, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_two_item_malformed = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x,'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_two_item_wrong_second = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  print *, x, y'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: print_variable_power_value_two_item_write = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        '  write *, x, x'//new_line('a')// &
        'end program main'//new_line('a')

contains

    logical function is_variable_print_batch(source, count)
        character(len=*), intent(in) :: source
        integer, intent(out) :: count
        character(len=*), parameter :: marker = '  print *, x'
        character(len=*), parameter :: item = ', x'
        character(len=*), parameter :: suffix = new_line('a')//'end program main'//new_line('a')
        integer :: marker_start, prefix_length, suffix_start, line_length, position

        count = 0
        is_variable_print_batch = .false.
        marker_start = index(source, marker)
        if (marker_start <= 1) return
        prefix_length = marker_start - 1
        if (prefix_length /= index(print_variable_power_value_source, marker) - 1) return
        if (source(1:prefix_length) /= print_variable_power_value_source(1:prefix_length)) return
        suffix_start = len(source) - len(suffix) + 1
        if (suffix_start <= marker_start + len(marker)) return
        if (source(suffix_start:) /= suffix) return
        line_length = suffix_start - marker_start
        if (line_length < len(marker)) return
        if (mod(line_length - len(marker), len(item)) /= 0) return
        count = 1 + (line_length - len(marker)) / len(item)
        if (count < 1 .or. count > 100) then
            count = 0
            return
        end if
        do position = 1, count - 1
            if (source(marker_start + len(marker) + (position - 1) * len(item): &
                marker_start + len(marker) + position * len(item) - 1) /= item) then
                count = 0
                return
            end if
        end do
        is_variable_print_batch = .true.
    end function is_variable_print_batch

    subroutine frontend_parse_program_unit_v2(file_name, source, source_hash, &
            unit, ok, message)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(program_unit_v2_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(typed_program_unit_t) :: declaration_unit
        character(len=1024) :: declaration_source
        character(len=64) :: generic_initializer
        character(len=8) :: generic_operator
        character(len=16) :: generic_addend_text
        integer(int64) :: generic_addend
        character(len=128) :: execution_source_hash
        integer(int64) :: stored_value
        integer :: stored_value_status
        integer :: batch_count
        logical :: shape_matches, generic_assignment_shape

        unit = program_unit_v2_t()
        ok = .false.
        message = ''
        if (.not. program_unit_v2_execution_part_policy_matches('execution-part')) then
            message = 'execution-part-policy-mismatch'
            return
        end if
        call parse_stored_variable_initializer_source(source, declaration_source, stored_value, ok, shape_matches)
        if (shape_matches .and. .not. ok) then
            message = 'print-variable-value-rejected'
            return
        end if
        call parse_generic_initialized_add_source(source, declaration_source, generic_initializer, &
            generic_operator, generic_addend, stored_value, ok, generic_assignment_shape)
        if (generic_assignment_shape .and. .not. ok) then
            message = 'print-variable-value-rejected'
            return
        end if
        if (is_generic_print_list_source(source)) then
            call parse_generic_print_list(file_name, source, source_hash, unit, ok, message)
            return
        end if
        if (.not. is_variable_print_batch(source, batch_count)) batch_count = 0
        if (source == print_seven_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 1_int64
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 21_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 2_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 24_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_nine_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 3_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 27_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_nine_ten_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 4_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 31_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_nine_ten_eleven_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 5_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 35_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_nine_ten_eleven_twelve_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 6_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%output_6_kind = print_policy_output_6_kind
            unit%execution_part%print%output_6_value = print_policy_output_6_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 39_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%output_6_rule = print_policy_output_6_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%output_6_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%output_6_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_seven_eight_nine_ten_eleven_twelve_thirteen_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 7_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%output_6_kind = print_policy_output_6_kind
            unit%execution_part%print%output_6_value = print_policy_output_6_value
            unit%execution_part%print%output_7_kind = print_policy_output_7_kind
            unit%execution_part%print%output_7_value = print_policy_output_7_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 43_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%output_6_rule = print_policy_output_6_rule
            unit%execution_part%print%output_7_rule = print_policy_output_7_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%output_6_clause = print_policy_output_clause
            unit%execution_part%print%output_7_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%output_6_page = print_policy_output_page
            unit%execution_part%print%output_7_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_eight_item_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 8_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%output_6_kind = print_policy_output_6_kind
            unit%execution_part%print%output_6_value = print_policy_output_6_value
            unit%execution_part%print%output_7_kind = print_policy_output_7_kind
            unit%execution_part%print%output_7_value = print_policy_output_7_value
            unit%execution_part%print%output_8_kind = print_policy_output_8_kind
            unit%execution_part%print%output_8_value = print_policy_output_8_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 47_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%output_6_rule = print_policy_output_6_rule
            unit%execution_part%print%output_7_rule = print_policy_output_7_rule
            unit%execution_part%print%output_8_rule = print_policy_output_8_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%output_6_clause = print_policy_output_clause
            unit%execution_part%print%output_7_clause = print_policy_output_clause
            unit%execution_part%print%output_8_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%output_6_page = print_policy_output_page
            unit%execution_part%print%output_7_page = print_policy_output_page
            unit%execution_part%print%output_8_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_nine_item_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 9_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%output_6_kind = print_policy_output_6_kind
            unit%execution_part%print%output_6_value = print_policy_output_6_value
            unit%execution_part%print%output_7_kind = print_policy_output_7_kind
            unit%execution_part%print%output_7_value = print_policy_output_7_value
            unit%execution_part%print%output_8_kind = print_policy_output_8_kind
            unit%execution_part%print%output_8_value = print_policy_output_8_value
            unit%execution_part%print%output_9_kind = print_policy_output_9_kind
            unit%execution_part%print%output_9_value = print_policy_output_9_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 51_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%output_6_rule = print_policy_output_6_rule
            unit%execution_part%print%output_7_rule = print_policy_output_7_rule
            unit%execution_part%print%output_8_rule = print_policy_output_8_rule
            unit%execution_part%print%output_9_rule = print_policy_output_9_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%output_6_clause = print_policy_output_clause
            unit%execution_part%print%output_7_clause = print_policy_output_clause
            unit%execution_part%print%output_8_clause = print_policy_output_clause
            unit%execution_part%print%output_9_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%output_6_page = print_policy_output_page
            unit%execution_part%print%output_7_page = print_policy_output_page
            unit%execution_part%print%output_8_page = print_policy_output_page
            unit%execution_part%print%output_9_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_ten_item_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = print_policy_output_value
            unit%execution_part%print%output_count = 10_int64
            unit%execution_part%print%output_2_kind = print_policy_output_2_kind
            unit%execution_part%print%output_2_value = print_policy_output_2_value
            unit%execution_part%print%output_3_kind = print_policy_output_3_kind
            unit%execution_part%print%output_3_value = print_policy_output_3_value
            unit%execution_part%print%output_4_kind = print_policy_output_4_kind
            unit%execution_part%print%output_4_value = print_policy_output_4_value
            unit%execution_part%print%output_5_kind = print_policy_output_5_kind
            unit%execution_part%print%output_5_value = print_policy_output_5_value
            unit%execution_part%print%output_6_kind = print_policy_output_6_kind
            unit%execution_part%print%output_6_value = print_policy_output_6_value
            unit%execution_part%print%output_7_kind = print_policy_output_7_kind
            unit%execution_part%print%output_7_value = print_policy_output_7_value
            unit%execution_part%print%output_8_kind = print_policy_output_8_kind
            unit%execution_part%print%output_8_value = print_policy_output_8_value
            unit%execution_part%print%output_9_kind = print_policy_output_9_kind
            unit%execution_part%print%output_9_value = print_policy_output_9_value
            unit%execution_part%print%output_10_kind = print_policy_output_10_kind
            unit%execution_part%print%output_10_value = print_policy_output_10_value
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 55_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_2_rule
            unit%execution_part%print%output_3_rule = print_policy_output_3_rule
            unit%execution_part%print%output_4_rule = print_policy_output_4_rule
            unit%execution_part%print%output_5_rule = print_policy_output_5_rule
            unit%execution_part%print%output_6_rule = print_policy_output_6_rule
            unit%execution_part%print%output_7_rule = print_policy_output_7_rule
            unit%execution_part%print%output_8_rule = print_policy_output_8_rule
            unit%execution_part%print%output_9_rule = print_policy_output_9_rule
            unit%execution_part%print%output_10_rule = print_policy_output_10_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%output_4_clause = print_policy_output_clause
            unit%execution_part%print%output_5_clause = print_policy_output_clause
            unit%execution_part%print%output_6_clause = print_policy_output_clause
            unit%execution_part%print%output_7_clause = print_policy_output_clause
            unit%execution_part%print%output_8_clause = print_policy_output_clause
            unit%execution_part%print%output_9_clause = print_policy_output_clause
            unit%execution_part%print%output_10_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%output_4_page = print_policy_output_page
            unit%execution_part%print%output_5_page = print_policy_output_page
            unit%execution_part%print%output_6_page = print_policy_output_page
            unit%execution_part%print%output_7_page = print_policy_output_page
            unit%execution_part%print%output_8_page = print_policy_output_page
            unit%execution_part%print%output_9_page = print_policy_output_page
            unit%execution_part%print%output_10_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (source == print_generic_item_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_output_kind
            unit%execution_part%print%output_value = 17_int64
            unit%execution_part%print%output_count = 3_int64
            unit%execution_part%print%output_sequence_start = 17_int64
            unit%execution_part%print%output_sequence_length = 3_int64
            unit%execution_part%print%output_2_kind = print_policy_output_kind
            unit%execution_part%print%output_2_value = 18_int64
            unit%execution_part%print%output_3_kind = print_policy_output_kind
            unit%execution_part%print%output_3_value = 19_int64
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 10_int64
            unit%execution_part%print%span%end_byte = 27_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_output_rule
            unit%execution_part%print%output_2_rule = print_policy_output_rule
            unit%execution_part%print%output_3_rule = print_policy_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%output_2_clause = print_policy_output_clause
            unit%execution_part%print%output_3_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%output_2_page = print_policy_output_page
            unit%execution_part%print%output_3_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (.not. generic_assignment_shape .and. &
            (source == print_variable_expression_source .or. &
            source == print_variable_multiply_expression_source .or. &
            source == print_variable_subtract_expression_source .or. &
            source == print_variable_divide_expression_source .or. &
            source == print_variable_power_expression_source .or. &
            source == print_variable_power_value_source .or. &
            source == print_variable_power_value_two_item_source .or. &
            source == print_variable_power_value_three_item_source .or. &
            source == print_variable_power_value_four_item_source .or. &
            source == print_variable_power_value_five_item_source .or. &
            source == print_variable_power_value_six_item_source .or. &
            source == print_variable_power_value_seven_item_source .or. &
            source == print_variable_power_value_eight_item_source .or. &
            source == print_variable_power_value_nine_item_source .or. &
            source == print_variable_power_value_ten_item_source .or. &
            source == print_variable_power_value_eleven_item_source .or. &
            source == print_variable_power_value_twelve_item_source .or. &
            source == print_variable_power_value_thirteen_item_source .or. &
            source == print_variable_power_value_fourteen_item_source .or. &
            source == print_variable_power_value_fifteen_item_source .or. &
            source == print_variable_power_value_sixteen_item_source .or. &
            source == print_variable_power_value_seventeen_item_source .or. &
            source == print_variable_power_value_eighteen_item_source .or. &
            source == print_variable_power_value_nineteen_item_source .or. &
            source == print_variable_power_value_twenty_item_source .or. &
            batch_count > 0)) then
            declaration_source = 'program main'//new_line('a')// &
                '  integer :: x'//new_line('a')//'end program main'//new_line('a')
            call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
                assignment_sequence_source_hash, declaration_unit, ok, message)
            if (.not. ok) return
            if (source == print_variable_multiply_expression_source) then
                call frontend_parse_typed_assignment_sequence(file_name, &
                    assignment_sequence_two_23_multiply_source, assignment_sequence_source_hash, &
                    unit%execution_part%sequence, ok, message)
            else if (source == print_variable_subtract_expression_source) then
                call frontend_parse_typed_assignment_sequence(file_name, &
                    assignment_sequence_two_23_subtract_source, assignment_sequence_source_hash, &
                    unit%execution_part%sequence, ok, message)
            else if (source == print_variable_divide_expression_source) then
                call frontend_parse_typed_assignment_sequence(file_name, &
                    assignment_sequence_two_24_divide_source, assignment_sequence_source_hash, &
                    unit%execution_part%sequence, ok, message)
            else if (source == print_variable_power_expression_source) then
                call frontend_parse_typed_assignment_sequence(file_name, &
                    assignment_sequence_two_2_power_source, assignment_sequence_source_hash, &
                    unit%execution_part%sequence, ok, message)
            else if (source == print_variable_power_value_source .or. &
                    source == print_variable_power_value_two_item_source .or. &
                    source == print_variable_power_value_three_item_source .or. &
                    source == print_variable_power_value_four_item_source .or. &
                    source == print_variable_power_value_five_item_source .or. &
                    source == print_variable_power_value_six_item_source .or. &
                    source == print_variable_power_value_seven_item_source .or. &
                    source == print_variable_power_value_eight_item_source .or. &
                    source == print_variable_power_value_nine_item_source .or. &
                    source == print_variable_power_value_ten_item_source .or. &
                    source == print_variable_power_value_eleven_item_source .or. &
                    source == print_variable_power_value_twelve_item_source .or. &
                    source == print_variable_power_value_thirteen_item_source .or. &
                    source == print_variable_power_value_fourteen_item_source .or. &
                    source == print_variable_power_value_fifteen_item_source .or. &
                    source == print_variable_power_value_sixteen_item_source .or. &
                    source == print_variable_power_value_seventeen_item_source .or. &
                    source == print_variable_power_value_eighteen_item_source .or. &
                    source == print_variable_power_value_nineteen_item_source .or. &
                    source == print_variable_power_value_twenty_item_source .or. &
                    batch_count > 0) then
                call frontend_parse_typed_assignment_sequence(file_name, &
                    assignment_sequence_two_3_power_source, assignment_sequence_source_hash, &
                    unit%execution_part%sequence, ok, message)
            else
                call frontend_parse_typed_assignment_sequence(file_name, assignment_sequence_two_23_source, &
                    assignment_sequence_source_hash, unit%execution_part%sequence, ok, message)
            end if
            if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2_int64) then
                message = 'print-variable-expression-assignment-rejected'
                return
            end if
            unit%root = declaration_unit%root
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%declaration_count = declaration_unit%declaration_count
            unit%declaration = declaration_unit%declaration
            unit%variable_count = declaration_unit%variable_count
            unit%variable = declaration_unit%variable
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_variable_output_kind
            unit%execution_part%print%output_name = print_policy_variable_output_name
            if (source == print_variable_subtract_expression_source) then
                unit%execution_part%print%output_value = 21_int64
            else if (source == print_variable_divide_expression_source) then
                unit%execution_part%print%output_value = 12_int64
            else if (source == print_variable_power_expression_source) then
                unit%execution_part%print%output_value = print_policy_variable_value_5
            else if (source == print_variable_power_value_source .or. &
                    source == print_variable_power_value_two_item_source .or. &
                    source == print_variable_power_value_three_item_source .or. &
                    source == print_variable_power_value_four_item_source .or. &
                    source == print_variable_power_value_five_item_source .or. &
                    source == print_variable_power_value_six_item_source .or. &
                    source == print_variable_power_value_seven_item_source .or. &
                    source == print_variable_power_value_eight_item_source .or. &
                    source == print_variable_power_value_nine_item_source .or. &
                    source == print_variable_power_value_ten_item_source .or. &
                    source == print_variable_power_value_eleven_item_source .or. &
                    source == print_variable_power_value_twelve_item_source .or. &
                    source == print_variable_power_value_thirteen_item_source .or. &
                    source == print_variable_power_value_fourteen_item_source .or. &
                    source == print_variable_power_value_fifteen_item_source .or. &
                    source == print_variable_power_value_sixteen_item_source .or. &
                    source == print_variable_power_value_seventeen_item_source .or. &
                    source == print_variable_power_value_eighteen_item_source .or. &
                    source == print_variable_power_value_nineteen_item_source .or. &
                    source == print_variable_power_value_twenty_item_source .or. &
                    batch_count > 0) then
                unit%execution_part%print%output_value = print_policy_variable_value_6
            else
                unit%execution_part%print%output_value = print_policy_variable_value_2
            end if
            unit%execution_part%print%output_count = 1_int64
            if (source == print_variable_power_value_two_item_source) then
                unit%execution_part%print%output_count = 2_int64
                unit%execution_part%print%output_2_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_2_name = print_policy_variable_output_name
                unit%execution_part%print%output_2_value = print_policy_variable_value_6
                unit%execution_part%print%output_2_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_2_clause = print_policy_output_clause
                unit%execution_part%print%output_2_page = print_policy_output_page
            else if (source == print_variable_power_value_three_item_source) then
                unit%execution_part%print%output_count = 3_int64
                unit%execution_part%print%output_2_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_2_name = print_policy_variable_output_name
                unit%execution_part%print%output_2_value = print_policy_variable_value_6
                unit%execution_part%print%output_2_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_2_clause = print_policy_output_clause
                unit%execution_part%print%output_2_page = print_policy_output_page
                unit%execution_part%print%output_3_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_3_name = print_policy_variable_output_name
                unit%execution_part%print%output_3_value = print_policy_variable_value_6
                unit%execution_part%print%output_3_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_3_clause = print_policy_output_clause
                unit%execution_part%print%output_3_page = print_policy_output_page
            else if (source == print_variable_power_value_four_item_source) then
                unit%execution_part%print%output_count = 4_int64
                unit%execution_part%print%output_2_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_2_name = print_policy_variable_output_name
                unit%execution_part%print%output_2_value = print_policy_variable_value_6
                unit%execution_part%print%output_2_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_2_clause = print_policy_output_clause
                unit%execution_part%print%output_2_page = print_policy_output_page
                unit%execution_part%print%output_3_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_3_name = print_policy_variable_output_name
                unit%execution_part%print%output_3_value = print_policy_variable_value_6
                unit%execution_part%print%output_3_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_3_clause = print_policy_output_clause
                unit%execution_part%print%output_3_page = print_policy_output_page
                unit%execution_part%print%output_4_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_4_name = print_policy_variable_output_name
                unit%execution_part%print%output_4_value = print_policy_variable_value_6
                unit%execution_part%print%output_4_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_4_clause = print_policy_output_clause
                unit%execution_part%print%output_4_page = print_policy_output_page
            else if (source == print_variable_power_value_five_item_source) then
                unit%execution_part%print%output_count = 5_int64
                unit%execution_part%print%output_2_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_2_name = print_policy_variable_output_name
                unit%execution_part%print%output_2_value = print_policy_variable_value_6
                unit%execution_part%print%output_2_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_2_clause = print_policy_output_clause
                unit%execution_part%print%output_2_page = print_policy_output_page
                unit%execution_part%print%output_3_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_3_name = print_policy_variable_output_name
                unit%execution_part%print%output_3_value = print_policy_variable_value_6
                unit%execution_part%print%output_3_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_3_clause = print_policy_output_clause
                unit%execution_part%print%output_3_page = print_policy_output_page
                unit%execution_part%print%output_4_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_4_name = print_policy_variable_output_name
                unit%execution_part%print%output_4_value = print_policy_variable_value_6
                unit%execution_part%print%output_4_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_4_clause = print_policy_output_clause
                unit%execution_part%print%output_4_page = print_policy_output_page
                unit%execution_part%print%output_5_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_5_name = print_policy_variable_output_name
                unit%execution_part%print%output_5_value = print_policy_variable_value_6
                unit%execution_part%print%output_5_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_5_clause = print_policy_output_clause
                unit%execution_part%print%output_5_page = print_policy_output_page
            else if (source == print_variable_power_value_six_item_source) then
                unit%execution_part%print%output_count = 6_int64
                unit%execution_part%print%output_2_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_2_name = print_policy_variable_output_name
                unit%execution_part%print%output_2_value = print_policy_variable_value_6
                unit%execution_part%print%output_2_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_2_clause = print_policy_output_clause
                unit%execution_part%print%output_2_page = print_policy_output_page
                unit%execution_part%print%output_3_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_3_name = print_policy_variable_output_name
                unit%execution_part%print%output_3_value = print_policy_variable_value_6
                unit%execution_part%print%output_3_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_3_clause = print_policy_output_clause
                unit%execution_part%print%output_3_page = print_policy_output_page
                unit%execution_part%print%output_4_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_4_name = print_policy_variable_output_name
                unit%execution_part%print%output_4_value = print_policy_variable_value_6
                unit%execution_part%print%output_4_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_4_clause = print_policy_output_clause
                unit%execution_part%print%output_4_page = print_policy_output_page
                unit%execution_part%print%output_5_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_5_name = print_policy_variable_output_name
                unit%execution_part%print%output_5_value = print_policy_variable_value_6
                unit%execution_part%print%output_5_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_5_clause = print_policy_output_clause
                unit%execution_part%print%output_5_page = print_policy_output_page
                unit%execution_part%print%output_6_kind = print_policy_variable_output_kind
                unit%execution_part%print%output_6_name = print_policy_variable_output_name
                unit%execution_part%print%output_6_value = print_policy_variable_value_6
                unit%execution_part%print%output_6_rule = print_policy_variable_output_rule
                unit%execution_part%print%output_6_clause = print_policy_output_clause
                unit%execution_part%print%output_6_page = print_policy_output_page
            else if (source == print_variable_power_value_seven_item_source) then
                unit%execution_part%print%output_count = 7_int64
                call set_variable_print_items(unit%execution_part%print, 7)
            else if (source == print_variable_power_value_eight_item_source) then
                unit%execution_part%print%output_count = 8_int64
                call set_variable_print_items(unit%execution_part%print, 8)
            else if (source == print_variable_power_value_nine_item_source) then
                unit%execution_part%print%output_count = 9_int64
                call set_variable_print_items(unit%execution_part%print, 9)
            else if (source == print_variable_power_value_ten_item_source) then
                unit%execution_part%print%output_count = 10_int64
                call set_variable_print_items(unit%execution_part%print, 10)
            else if (source == print_variable_power_value_eleven_item_source) then
                unit%execution_part%print%output_count = 11_int64
                unit%execution_part%print%output_sequence_length = 11_int64
            else if (source == print_variable_power_value_twelve_item_source) then
                unit%execution_part%print%output_count = 12_int64
                unit%execution_part%print%output_sequence_length = 12_int64
            else if (source == print_variable_power_value_thirteen_item_source) then
                unit%execution_part%print%output_count = 13_int64
                unit%execution_part%print%output_sequence_length = 13_int64
            else if (source == print_variable_power_value_fourteen_item_source) then
                unit%execution_part%print%output_count = 14_int64
                unit%execution_part%print%output_sequence_length = 14_int64
            else if (source == print_variable_power_value_fifteen_item_source) then
                unit%execution_part%print%output_count = 15_int64
                unit%execution_part%print%output_sequence_length = 15_int64
            else if (source == print_variable_power_value_sixteen_item_source) then
                unit%execution_part%print%output_count = 16_int64
                unit%execution_part%print%output_sequence_length = 16_int64
            else if (source == print_variable_power_value_seventeen_item_source) then
                unit%execution_part%print%output_count = 17_int64
                unit%execution_part%print%output_sequence_length = 17_int64
            else if (source == print_variable_power_value_eighteen_item_source) then
                unit%execution_part%print%output_count = 18_int64
                unit%execution_part%print%output_sequence_length = 18_int64
            else if (source == print_variable_power_value_nineteen_item_source) then
                unit%execution_part%print%output_count = 19_int64
                unit%execution_part%print%output_sequence_length = 19_int64
            else if (source == print_variable_power_value_twenty_item_source) then
                unit%execution_part%print%output_count = 20_int64
                unit%execution_part%print%output_sequence_length = 20_int64
            else if (batch_count > 0) then
                unit%execution_part%print%output_count = int(batch_count, int64)
                unit%execution_part%print%output_sequence_length = int(batch_count, int64)
            end if
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = int(index(source, '  print *, x') - 1, int64)
            unit%execution_part%print%span%end_byte = &
                unit%execution_part%print%span%start_byte + merge(17_int64, 11_int64, &
                source == print_variable_power_value_three_item_source)
            if (source == print_variable_power_value_four_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 21_int64
            else if (source == print_variable_power_value_five_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 25_int64
            else if (source == print_variable_power_value_six_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 29_int64
            else if (source == print_variable_power_value_seven_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 33_int64
            else if (source == print_variable_power_value_eight_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 37_int64
            else if (source == print_variable_power_value_nine_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 41_int64
            else if (source == print_variable_power_value_ten_item_source) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 45_int64
            else if (unit%execution_part%print%output_sequence_length > 0_int64) then
                unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + &
                    3_int64 * unit%execution_part%print%output_count + 8_int64
            end if
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_variable_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            message = ''
            return
        end if
        if (source == print_variable_source .or. source == print_variable_23_source .or. &
            source == print_variable_zero_source .or. source == print_variable_2047_source .or. &
            source == print_variable_negative_source .or. &
            source == print_variable_negative_boundary_source) then
            if (source == print_variable_source) then
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = 17'//new_line('a')//'end program main'//new_line('a')
            else if (source == print_variable_23_source) then
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = 23'//new_line('a')//'end program main'//new_line('a')
            else if (source == print_variable_zero_source) then
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = 0'//new_line('a')//'end program main'//new_line('a')
            else if (source == print_variable_2047_source) then
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = 2047'//new_line('a')//'end program main'//new_line('a')
            else if (source == print_variable_negative_source) then
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = -5'//new_line('a')//'end program main'//new_line('a')
            else
                declaration_source = 'program main'//new_line('a')// &
                    '  integer :: x'//new_line('a')// &
                    '  x = -100'//new_line('a')//'end program main'//new_line('a')
            end if
            call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
                source_hash, declaration_unit, ok, message)
            if (.not. ok .or. declaration_unit%assignment_count /= 1_int64) then
                message = 'print-variable-assignment-rejected'
                return
            end if
            read (declaration_unit%assignment%expression%left_operand, *, iostat=stored_value_status) &
                stored_value
            if (stored_value_status /= 0) then
                message = 'print-variable-value-rejected'
                return
            end if
            if (source == print_variable_negative_source) then
                if (stored_value /= -5_int64) then
                    message = 'print-variable-value-rejected'
                    return
                end if
            else if (source == print_variable_negative_boundary_source) then
                if (stored_value /= -100_int64) then
                    message = 'print-variable-value-rejected'
                    return
                end if
            else if (source == print_variable_zero_source .or. &
                    source == print_variable_2047_source) then
                if (stored_value < int(assignment_policy_integer_literal_min, int64) .or. &
                    stored_value > int(assignment_policy_integer_literal_max, int64)) then
                    message = 'print-variable-value-rejected'
                    return
                end if
            else if (stored_value /= print_policy_variable_value .and. &
                    stored_value /= print_policy_variable_value_2) then
                message = 'print-variable-value-rejected'
                return
            end if
            unit%root = declaration_unit%root
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%declaration_count = declaration_unit%declaration_count
            unit%declaration = declaration_unit%declaration
            unit%variable_count = declaration_unit%variable_count
            unit%variable = declaration_unit%variable
            unit%execution_part%sequence%assignment_count = 1_int64
            unit%execution_part%sequence%assignment(1) = declaration_unit%assignment
            unit%execution_part%sequence%assignment(1)%span%end_byte = 31_int64
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_variable_output_kind
            unit%execution_part%print%output_name = print_policy_variable_output_name
            if (source == print_variable_zero_source .or. source == print_variable_2047_source .or. &
                source == print_variable_negative_source) then
                unit%execution_part%print%output_value = print_policy_variable_value
            else if (source == print_variable_negative_boundary_source) then
                unit%execution_part%print%output_value = print_policy_variable_value
            else
                unit%execution_part%print%output_value = stored_value
            end if
            unit%execution_part%print%output_count = 1_int64
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 34_int64
            unit%execution_part%print%span%end_byte = 45_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_variable_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            return
        end if
        if (.not. generic_assignment_shape) then
            call parse_stored_variable_initializer_source(source, declaration_source, stored_value, ok, shape_matches)
        end if
        if (ok .and. .not. generic_assignment_shape) then
            call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
                source_hash, declaration_unit, ok, message)
            if (.not. ok .or. declaration_unit%assignment_count /= 1_int64) then
                message = 'print-variable-assignment-rejected'
                return
            end if
            unit%root = declaration_unit%root
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%declaration_count = declaration_unit%declaration_count
            unit%declaration = declaration_unit%declaration
            unit%variable_count = declaration_unit%variable_count
            unit%variable = declaration_unit%variable
            unit%execution_part%sequence%assignment_count = 1_int64
            unit%execution_part%sequence%assignment(1) = declaration_unit%assignment
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_variable_output_kind
            unit%execution_part%print%output_name = print_policy_variable_output_name
            unit%execution_part%print%output_value = print_policy_variable_value
            unit%execution_part%print%output_count = 1_int64
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = 34_int64
            unit%execution_part%print%span%end_byte = 45_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_variable_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            message = ''
            return
        end if
        if (generic_assignment_shape) then
            call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
                source_hash, declaration_unit, ok, message)
            if (.not. ok) return
            call frontend_parse_typed_assignment_sequence(file_name, assignment_sequence_two_23_source, &
                source_hash, unit%execution_part%sequence, ok, message)
            if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2_int64) then
                message = 'print-variable-expression-assignment-rejected'
                return
            end if
            unit%execution_part%sequence%assignment(1)%expression%left_operand = trim(generic_initializer)
            unit%execution_part%sequence%assignment(2)%expression%operator = trim(generic_operator)
            write (generic_addend_text, '(i0)') generic_addend
            unit%execution_part%sequence%assignment(2)%expression%right_operand = &
                trim(generic_addend_text)
            unit%root = declaration_unit%root
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%declaration_count = declaration_unit%declaration_count
            unit%declaration = declaration_unit%declaration
            unit%variable_count = declaration_unit%variable_count
            unit%variable = declaration_unit%variable
            unit%execution_part%sequence%assignment(1)%span%start_byte = &
                int(index(source, '  x = ') - 1, int64)
            unit%execution_part%sequence%assignment(1)%span%end_byte = &
                unit%execution_part%sequence%assignment(1)%span%start_byte + &
                int(len_trim(generic_initializer) + 5, int64)
            unit%execution_part%sequence%assignment(2)%span%start_byte = &
                int(index(source, '  x = x '//trim(generic_operator)//' ') - 1, int64)
            unit%execution_part%sequence%assignment(2)%span%end_byte = &
                unit%execution_part%sequence%assignment(2)%span%start_byte + &
                int(index(source(unit%execution_part%sequence%assignment(2)%span%start_byte + 1:), &
                new_line('a')) - 2, int64)
            unit%execution_part%print_count = 1_int64
            unit%execution_part%print%format_kind = print_policy_format_kind
            unit%execution_part%print%format_value = print_policy_format_value
            unit%execution_part%print%output_kind = print_policy_variable_output_kind
            unit%execution_part%print%output_name = print_policy_variable_output_name
            unit%execution_part%print%output_value = print_policy_variable_value_2
            unit%execution_part%print%output_count = 1_int64
            unit%execution_part%print%span = unit%root%span
            unit%execution_part%print%span%start_byte = int(index(source, '  print *, x') - 1, int64)
            unit%execution_part%print%span%end_byte = unit%execution_part%print%span%start_byte + 11_int64
            unit%execution_part%print%statement_rule = print_policy_statement_rule
            unit%execution_part%print%format_rule = print_policy_format_rule
            unit%execution_part%print%output_rule = print_policy_variable_output_rule
            unit%execution_part%print%source_document = print_policy_document
            unit%execution_part%print%statement_clause = print_policy_statement_clause
            unit%execution_part%print%format_clause = print_policy_format_clause
            unit%execution_part%print%output_clause = print_policy_output_clause
            unit%execution_part%print%statement_page = print_policy_statement_page
            unit%execution_part%print%format_page = print_policy_format_page
            unit%execution_part%print%output_page = print_policy_output_page
            unit%execution_part%print%source_hash = print_policy_source_hash
            ok = .true.
            message = ''
            return
        end if
        if (source == stop_seven_source) then
            unit%root%name = 'p'
            unit%root%span%file = file_name
            unit%root%span%start_byte = 0_int64
            unit%root%span%end_byte = int(len(source), int64) - 1_int64
            unit%root%span%source_hash = source_hash
            unit%execution_part%stop_count = 1_int64
            unit%execution_part%stop%code = stop_policy_code
            unit%execution_part%stop%span = unit%root%span
            unit%execution_part%stop%span%start_byte = 10_int64
            unit%execution_part%stop%span%end_byte = 17_int64
            unit%execution_part%stop%source_rule = stop_policy_statement_rule
            unit%execution_part%stop%code_rule = stop_policy_code_rule
            unit%execution_part%stop%source_document = stop_policy_document
            unit%execution_part%stop%source_clause = stop_policy_clause
            unit%execution_part%stop%source_page = stop_policy_page
            unit%execution_part%stop%source_hash = stop_policy_source_hash
            ok = .true.
            return
        end if
        declaration_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'end program main'//new_line('a')
        if (source == two_assignment_source) then
            execution_source_hash = assignment_sequence_source_hash
        else if (source == five_assignment_source) then
            execution_source_hash = 'l3-raw-program-five-assignment-v1'
        else if (source == six_assignment_source) then
            execution_source_hash = 'l3-raw-program-six-assignment-v1'
        else
            message = 'unsupported-program-unit-v2'
            return
        end if
        call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
            source_hash, declaration_unit, ok, message)
        if (.not. ok) return
        call frontend_parse_typed_assignment_sequence(file_name, source, &
            trim(execution_source_hash), unit%execution_part%sequence, ok, message)
        if (.not. ok) return

        unit%root = declaration_unit%root
        unit%root%span%end_byte = int(len(source), int64) - 1_int64
        unit%declaration_count = declaration_unit%declaration_count
        unit%declaration = declaration_unit%declaration
        unit%variable_count = declaration_unit%variable_count
        unit%variable = declaration_unit%variable
        ok = .true.
        message = ''
    end subroutine frontend_parse_program_unit_v2

    subroutine parse_generic_initialized_add_source(source, declaration_source, &
            initializer, operator, addend, stored_value, ok, matches_shape)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: declaration_source, initializer, operator
        integer(int64), intent(out) :: addend
        integer(int64), intent(out) :: stored_value
        logical, intent(out) :: ok, matches_shape
        character(len=*), parameter :: prefix = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = '
        character(len=*), parameter :: assignment_prefix = '  x = x '
        character(len=*), parameter :: print_suffix = new_line('a')// &
            '  print *, x'//new_line('a')//'end program main'//new_line('a')
        character(len=1024) :: initializer_source, parsed_declaration_source, tail
        character(len=64) :: assignment_line
        integer :: initializer_end, assignment_end, token_length
        logical :: initializer_shape_matches

        declaration_source = ''
        initializer = ''
        operator = ''
        addend = 0_int64
        stored_value = 0_int64
        ok = .false.
        matches_shape = .false.
        if (len(source) <= len(prefix) + len(print_suffix) + len(assignment_prefix)) return
        if (source(:len(prefix)) /= prefix) return
        tail = source(len(prefix) + 1:)
        initializer_end = index(tail, new_line('a'))
        if (initializer_end <= 1) return
        token_length = initializer_end - 1
        if (token_length > len(initializer)) return
        initializer = tail(:token_length)
        tail = tail(initializer_end + 1:)
        assignment_end = index(tail, new_line('a'))
        if (assignment_end <= len(assignment_prefix) + 1 .or. assignment_end > len(assignment_line)) return
        assignment_line = ''
        assignment_line(:assignment_end - 1) = tail(:assignment_end - 1)
        if (tail(assignment_end:) /= print_suffix) return
        matches_shape = .true.
        if (.not. parse_generic_update_line(assignment_line(:assignment_end - 1), operator, addend)) return
        initializer_source = prefix//trim(initializer)//new_line('a')// &
            '  print *, x'//new_line('a')//'end program main'//new_line('a')
        call parse_stored_variable_initializer_source(trim(initializer_source), parsed_declaration_source, &
            stored_value, ok, initializer_shape_matches)
        if (.not. initializer_shape_matches) then
            ok = .false.
            return
        end if
        if (.not. ok) return
        declaration_source = parsed_declaration_source
    end subroutine parse_generic_initialized_add_source

    logical function parse_generic_update_line(line, operator, addend)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: operator
        integer(int64), intent(out) :: addend
        character(len=32) :: token
        integer :: status, position

        parse_generic_update_line = .false.
        operator = ''
        addend = 0_int64
        if (len_trim(line) <= len('  x = x ')) return
        if (len_trim(line) >= len('  x = x + ')) then
            if (line(:len('  x = x + ')) == '  x = x + ') then
                operator = '+'
                token = adjustl(line(len('  x = x + ') + 1:))
            end if
        end if
        if (len_trim(operator) == 0 .and. len_trim(line) >= len('  x = x - ')) then
            if (line(:len('  x = x - ')) == '  x = x - ') then
                operator = '-'
                token = adjustl(line(len('  x = x - ') + 1:))
            end if
        end if
        if (len_trim(operator) == 0 .and. len_trim(line) >= len('  x = x * ')) then
            if (line(:len('  x = x * ')) == '  x = x * ') then
                operator = '*'
                token = adjustl(line(len('  x = x * ') + 1:))
            end if
        end if
        if (len_trim(operator) == 0 .and. len_trim(line) >= len('  x = x / ')) then
            if (line(:len('  x = x / ')) == '  x = x / ') then
                operator = '/'
                token = adjustl(line(len('  x = x / ') + 1:))
            end if
        end if
        if (len_trim(operator) == 0 .and. len_trim(line) >= len('  x = x ** ')) then
            if (line(:len('  x = x ** ')) == '  x = x ** ') then
                operator = '**'
                token = adjustl(line(len('  x = x ** ') + 1:))
            end if
        end if
        if (len_trim(operator) == 0) return
        if (len_trim(token) == 0) return
        do position = 1, len_trim(token)
            if (token(position:position) < '0' .or. token(position:position) > '9') return
        end do
        read (token(:len_trim(token)), *, iostat=status) addend
        if (status /= 0) return
        if (operator == '**') then
            if (addend < initialized_power_min .or. &
                addend > initialized_power_max) return
        else if (addend < print_policy_output_count_min .or. &
                addend > print_policy_output_count_max) then
            return
        end if
        parse_generic_update_line = .true.
    end function parse_generic_update_line

    subroutine parse_stored_variable_initializer_source(source, declaration_source, stored_value, ok, matches_shape)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: declaration_source
        integer(int64), intent(out) :: stored_value
        logical, intent(out) :: ok
        logical, intent(out) :: matches_shape
        character(len=*), parameter :: prefix = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = '
        character(len=*), parameter :: suffix = new_line('a')//'  print *, x'//new_line('a')// &
            'end program main'//new_line('a')
        character(len=64) :: token
        integer :: token_start, token_end, token_length, position, status

        declaration_source = ''
        stored_value = 0_int64
        ok = .false.
        matches_shape = .false.
        if (len(source) <= len(prefix) + len(suffix)) return
        if (source(:len(prefix)) /= prefix) return
        token_start = len(prefix) + 1
        token_end = len(source) - len(suffix)
        if (token_end < token_start) return
        if (source(token_end + 1:) /= suffix) return
        token_length = token_end - token_start + 1
        if (token_length > len(token)) return
        if (index(source(token_start:token_end), new_line('a')) /= 0) return
        matches_shape = .true.
        token = ''
        token(:token_length) = source(token_start:token_end)
        if (token_length == 0) return
        position = 1
        if (token(position:position) == '-') then
            if (token_length == 1) return
            position = 2
        end if
        do while (position <= token_length)
            if (token(position:position) < '0' .or. token(position:position) > '9') return
            position = position + 1
        end do
        read (token(:token_length), *, iostat=status) stored_value
        if (status /= 0) return
        if (token(1:1) == '-') then
            if (stored_value < int(assignment_policy_signed_integer_literal_min, int64) .or. &
                stored_value > int(assignment_policy_signed_integer_literal_max, int64)) return
        else if (stored_value < int(assignment_policy_integer_literal_min, int64) .or. &
                stored_value > int(assignment_policy_integer_literal_max, int64)) then
            return
        end if
        declaration_source = prefix//trim(token)//new_line('a')//'end program main'//new_line('a')
        ok = .true.
    end subroutine parse_stored_variable_initializer_source

    logical function is_generic_print_list_source(source)
        character(len=*), intent(in) :: source
        integer :: print_start
        character(len=*), parameter :: prefix_3 = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')
        character(len=*), parameter :: prefix_4 = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 4'//new_line('a')
        character(len=*), parameter :: prefix_5 = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')
        is_generic_print_list_source = .false.
        print_start = index(source, '  print *,')
        if (print_start == 0) return
        is_generic_print_list_source = (print_start == len(prefix_3) + 1 .and. &
            index(source, prefix_3) == 1 .or. print_start == len(prefix_4) + 1 .and. &
            index(source, prefix_4) == 1 .or. print_start == len(prefix_5) + 1 .and. &
            index(source, prefix_5) == 1) .and. &
            index(source, new_line('a')//'end program main'//new_line('a')) > print_start
    end function is_generic_print_list_source

    logical function is_print_power_literal(token)
        character(len=*), intent(in) :: token
        integer(int64) :: exponent
        integer :: index, status, token_length

        is_print_power_literal = .false.
        token_length = len_trim(token)
        if (token_length < 6 .or. token(:5) /= 'x ** ') return
        do index = 6, token_length
            if (token(index:index) < '0' .or. token(index:index) > '9') return
        end do
        read (token(6:token_length), *, iostat=status) exponent
        if (status /= 0) return
        if (exponent < print_policy_power_min .or. exponent > print_policy_power_max) return
        is_print_power_literal = .true.
    end function is_print_power_literal

    logical function is_print_integer_literal(token)
        character(len=*), intent(in) :: token
        integer(int64) :: value
        integer :: index, status, token_length

        is_print_integer_literal = .false.
        token_length = len_trim(token)
        if (token_length == 0) return
        index = 1
        if (token(index:index) == '-') then
            if (token_length == 1) return
            index = 2
        end if
        do while (index <= token_length)
            if (token(index:index) < '0' .or. token(index:index) > '9') return
            index = index + 1
        end do
        read (token(:token_length), *, iostat=status) value
        if (status /= 0) return
        if (token(1:1) == '-') then
            if (value < print_policy_signed_integer_literal_min .or. &
                value > print_policy_signed_integer_literal_max) return
        else
            if (value < print_policy_integer_literal_min) return
        end if
        is_print_integer_literal = .true.
    end function is_print_integer_literal

    logical function is_print_decimal_expression(token)
        character(len=*), intent(in) :: token

        is_print_decimal_expression = print_policy_decimal_expression_valid(token)
    end function is_print_decimal_expression

    subroutine parse_generic_print_list(file_name, source, source_hash, unit, ok, message)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(program_unit_v2_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(typed_program_unit_t) :: declaration_unit
        character(len=256) :: declaration_source
        character(len=256) :: line, rest, token
        character(len=32) :: parsed_items(16)
        integer :: print_start, line_end, token_end, item_count, item_index
        logical :: has_expression
        integer(int64) :: source_start, source_end

        unit = program_unit_v2_t()
        ok = .false.
        message = 'generic-print-list-rejected'
        print_start = index(source, '  print *,')
        if (print_start == 0) return
        line_end = index(source(print_start:), new_line('a')) + print_start - 1
        if (line_end < print_start) return
        line = source(print_start:line_end - 1)
        rest = adjustl(line(len('  print *,') + 1:))
        if (len_trim(rest) == 0) return

        item_count = 0
        has_expression = .false.
        parsed_items = ''
        do while (len_trim(rest) > 0)
            item_count = item_count + 1
            if (item_count > 16) return
            token_end = index(rest, ',')
            if (token_end == 0) then
                token = trim(rest)
                rest = ''
            else
                token = trim(rest(:token_end - 1))
                rest = adjustl(rest(token_end + 1:))
                if (len_trim(rest) == 0) return
            end if
            parsed_items(item_count) = token
            if (token == 'x' .or. is_print_power_literal(token) .or. &
                is_print_decimal_expression(token) .or. &
                token == print_policy_expression_source .or. &
                token == print_policy_expression_2_source .or. &
                token == print_policy_expression_3_source .or. &
                token == print_policy_expression_4_source .or. &
                token == print_policy_expression_5_source .or. &
                token == print_policy_expression_6_source .or. &
                token == print_policy_expression_7_source .or. &
                token == print_policy_expression_8_source .or. &
                token == print_policy_expression_9_source .or. &
                token == print_policy_expression_10_source .or. &
                token == print_policy_expression_11_source .or. &
                token == print_policy_expression_12_source) then
                if (is_print_power_literal(token) .or. is_print_decimal_expression(token)) &
                    has_expression = .true.
                cycle
            else if (is_print_integer_literal(token)) then
                cycle
            else
                return
            end if
        end do
        if (item_count < print_policy_output_count_min .or. &
            item_count > print_policy_output_count_max) return
        if (index(source, '  x = ') == 0) return
        if (index(source, '  x = x ') > 0) return

        if (index(source, '  x = 5'//new_line('a')) > 0) then
            declaration_source = 'program main'//new_line('a')// &
                '  integer :: x'//new_line('a')//'  x = 5'//new_line('a')// &
                'end program main'//new_line('a')
        else if (index(source, '  x = 4'//new_line('a')) > 0) then
            declaration_source = 'program main'//new_line('a')// &
                '  integer :: x'//new_line('a')//'  x = 4'//new_line('a')// &
                'end program main'//new_line('a')
        else
            declaration_source = 'program main'//new_line('a')// &
                '  integer :: x'//new_line('a')//'  x = 3'//new_line('a')// &
                'end program main'//new_line('a')
        end if
        call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
            assignment_sequence_source_hash, declaration_unit, ok, message)
        if (.not. ok) return
        unit%root = declaration_unit%root
        unit%root%span%file = file_name
        unit%root%span%end_byte = int(len(source), int64) - 1_int64
        if (has_expression .or. any(parsed_items(:item_count) == print_policy_expression_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_2_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_3_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_4_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_5_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_6_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_7_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_8_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_9_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_10_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_11_source) .or. &
            any(parsed_items(:item_count) == print_policy_expression_12_source)) then
            unit%root%span%source_hash = print_policy_expression_source_identity
        else
            unit%root%span%source_hash = print_policy_generic_source_identity
        end if
        unit%declaration_count = declaration_unit%declaration_count
        unit%declaration = declaration_unit%declaration
        unit%declaration%span%source_hash = unit%root%span%source_hash
        unit%variable_count = declaration_unit%variable_count
        unit%variable = declaration_unit%variable
        unit%variable%span%source_hash = unit%root%span%source_hash
        unit%execution_part%sequence%assignment_count = declaration_unit%assignment_count
        unit%execution_part%sequence%assignment(1) = declaration_unit%assignment
        unit%execution_part%sequence%assignment(1)%span%source_hash = unit%root%span%source_hash
        unit%execution_part%print_count = 1_int64
        unit%execution_part%print%format_kind = print_policy_format_kind
        unit%execution_part%print%format_value = print_policy_format_value
        unit%execution_part%print%output_count = int(item_count, int64)
        allocate (unit%execution_part%print%output_items(item_count))
        do item_index = 1, item_count
            token = parsed_items(item_index)
            if (token == 'x') then
                unit%execution_part%print%output_items(item_index)%kind = 'variable'
                unit%execution_part%print%output_items(item_index)%name = 'x'
                unit%execution_part%print%output_items(item_index)%rule = 'R901'
            else if (is_print_power_literal(token) .or. is_print_decimal_expression(token) .or. &
                    token == print_policy_expression_source .or. &
                    token == print_policy_expression_2_source .or. &
                    token == print_policy_expression_3_source .or. &
                    token == print_policy_expression_4_source .or. &
                    token == print_policy_expression_5_source .or. &
                    token == print_policy_expression_6_source .or. &
                    token == print_policy_expression_7_source .or. &
                    token == print_policy_expression_8_source .or. &
                    token == print_policy_expression_9_source .or. &
                    token == print_policy_expression_10_source .or. &
                    token == print_policy_expression_11_source .or. &
                    token == print_policy_expression_12_source) then
                unit%execution_part%print%output_items(item_index)%kind = 'integer-expression'
                if (is_print_decimal_expression(token)) then
                    unit%execution_part%print%output_items(item_index)%operator = token(3:3)
                    unit%execution_part%print%output_items(item_index)%left = 'x'
                    unit%execution_part%print%output_items(item_index)%right = token(5:)
                else if (is_print_power_literal(token)) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_power_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_power_left
                    unit%execution_part%print%output_items(item_index)%right = token(6:)
                else if (token == print_policy_expression_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_right
                else if (token == print_policy_expression_2_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_2_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_2_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_2_right
                else if (token == print_policy_expression_3_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_3_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_3_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_3_right
                else if (token == print_policy_expression_4_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_4_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_4_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_4_right
                else if (token == print_policy_expression_5_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_5_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_5_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_5_right
                else if (token == print_policy_expression_7_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_7_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_7_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_7_right
                else if (token == print_policy_expression_8_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_8_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_8_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_8_right
                else if (token == print_policy_expression_9_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_9_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_9_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_9_right
                else if (token == print_policy_expression_10_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_10_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_10_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_10_right
                else if (token == print_policy_expression_11_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_11_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_11_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_11_right
                else if (token == print_policy_expression_12_source) then
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_12_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_12_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_12_right
                else
                    unit%execution_part%print%output_items(item_index)%operator = &
                        print_policy_expression_6_operator
                    unit%execution_part%print%output_items(item_index)%left = &
                        print_policy_expression_6_left
                    unit%execution_part%print%output_items(item_index)%right = &
                        print_policy_expression_6_right
                end if
                unit%execution_part%print%output_items(item_index)%rule = 'R1217'
            else
                if (.not. is_print_integer_literal(token)) return
                read (token, *) unit%execution_part%print%output_items(item_index)%value
                unit%execution_part%print%output_items(item_index)%kind = 'integer-literal'
                unit%execution_part%print%output_items(item_index)%rule = 'R1217'
            end if
            unit%execution_part%print%output_items(item_index)%clause = &
                print_policy_output_clause
            unit%execution_part%print%output_items(item_index)%page = &
                print_policy_output_page
        end do
        unit%execution_part%print%span = unit%root%span
        source_start = int(print_start - 1, int64)
        source_end = int(line_end - 1, int64)
        unit%execution_part%print%span%start_byte = source_start
        unit%execution_part%print%span%end_byte = source_end
        unit%execution_part%print%statement_rule = print_policy_statement_rule
        unit%execution_part%print%format_rule = print_policy_format_rule
        unit%execution_part%print%output_rule = print_policy_output_rule
        unit%execution_part%print%source_document = print_policy_document
        unit%execution_part%print%statement_clause = print_policy_statement_clause
        unit%execution_part%print%format_clause = print_policy_format_clause
        unit%execution_part%print%output_clause = print_policy_output_clause
        unit%execution_part%print%statement_page = print_policy_statement_page
        unit%execution_part%print%format_page = print_policy_format_page
        unit%execution_part%print%output_page = print_policy_output_page
        unit%execution_part%print%source_hash = print_policy_source_hash
        unit%execution_part%print%source_identity = unit%root%span%source_hash
        ok = print_stmt_validate(unit%execution_part%print, message)
    end subroutine parse_generic_print_list

    subroutine set_variable_print_items(print_stmt, count)
        type(print_stmt_t), intent(inout) :: print_stmt
        integer, intent(in) :: count

        if (count >= 2) then
            print_stmt%output_2_kind = print_policy_variable_output_kind
            print_stmt%output_2_name = print_policy_variable_output_name
            print_stmt%output_2_value = print_policy_variable_value_6
            print_stmt%output_2_rule = print_policy_variable_output_rule
            print_stmt%output_2_clause = print_policy_output_clause
            print_stmt%output_2_page = print_policy_output_page
        end if
        if (count >= 3) then
            print_stmt%output_3_kind = print_policy_variable_output_kind
            print_stmt%output_3_name = print_policy_variable_output_name
            print_stmt%output_3_value = print_policy_variable_value_6
            print_stmt%output_3_rule = print_policy_variable_output_rule
            print_stmt%output_3_clause = print_policy_output_clause
            print_stmt%output_3_page = print_policy_output_page
        end if
        if (count >= 4) then
            print_stmt%output_4_kind = print_policy_variable_output_kind
            print_stmt%output_4_name = print_policy_variable_output_name
            print_stmt%output_4_value = print_policy_variable_value_6
            print_stmt%output_4_rule = print_policy_variable_output_rule
            print_stmt%output_4_clause = print_policy_output_clause
            print_stmt%output_4_page = print_policy_output_page
        end if
        if (count >= 5) then
            print_stmt%output_5_kind = print_policy_variable_output_kind
            print_stmt%output_5_name = print_policy_variable_output_name
            print_stmt%output_5_value = print_policy_variable_value_6
            print_stmt%output_5_rule = print_policy_variable_output_rule
            print_stmt%output_5_clause = print_policy_output_clause
            print_stmt%output_5_page = print_policy_output_page
        end if
        if (count >= 6) then
            print_stmt%output_6_kind = print_policy_variable_output_kind
            print_stmt%output_6_name = print_policy_variable_output_name
            print_stmt%output_6_value = print_policy_variable_value_6
            print_stmt%output_6_rule = print_policy_variable_output_rule
            print_stmt%output_6_clause = print_policy_output_clause
            print_stmt%output_6_page = print_policy_output_page
        end if
        if (count >= 7) then
            print_stmt%output_7_kind = print_policy_variable_output_kind
            print_stmt%output_7_name = print_policy_variable_output_name
            print_stmt%output_7_value = print_policy_variable_value_6
            print_stmt%output_7_rule = print_policy_variable_output_rule
            print_stmt%output_7_clause = print_policy_output_clause
            print_stmt%output_7_page = print_policy_output_page
        end if
        if (count >= 8) then
            print_stmt%output_8_kind = print_policy_variable_output_kind
            print_stmt%output_8_name = print_policy_variable_output_name
            print_stmt%output_8_value = print_policy_variable_value_6
            print_stmt%output_8_rule = print_policy_variable_output_rule
            print_stmt%output_8_clause = print_policy_output_clause
            print_stmt%output_8_page = print_policy_output_page
        end if
        if (count >= 9) then
            print_stmt%output_9_kind = print_policy_variable_output_kind
            print_stmt%output_9_name = print_policy_variable_output_name
            print_stmt%output_9_value = print_policy_variable_value_6
            print_stmt%output_9_rule = print_policy_variable_output_rule
            print_stmt%output_9_clause = print_policy_output_clause
            print_stmt%output_9_page = print_policy_output_page
        end if
        if (count >= 10) then
            print_stmt%output_10_kind = print_policy_variable_output_kind
            print_stmt%output_10_name = print_policy_variable_output_name
            print_stmt%output_10_value = print_policy_variable_value_6
            print_stmt%output_10_rule = print_policy_variable_output_rule
            print_stmt%output_10_clause = print_policy_output_clause
            print_stmt%output_10_page = print_policy_output_page
        end if
    end subroutine set_variable_print_items

    subroutine frontend_program_unit_v2_to_sx(unit, output, ok, message)
        type(program_unit_v2_t), intent(in) :: unit
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=65536) :: root_sx, declaration_sx, variable_sx, sequence_sx, print_sx

        output = ''
        ok = .false.
        message = ''
        if (unit%execution_part%stop_count == 1_int64) then
            if (unit%declaration_count /= 0_int64 .or. unit%variable_count /= 0_int64 .or. &
                unit%execution_part%stop%code /= stop_policy_code) then
                message = 'invalid-program-unit-v2-stop-cardinality'
                return
            end if
            if (.not. stop_stmt_validate(unit%execution_part%stop, message)) return
            call program_root_to_sx(unit%root, root_sx, ok, message)
            if (.not. ok) return
            call source_span_to_sx(unit%execution_part%stop%span, variable_sx, ok, message)
            if (.not. ok) return
            write (sequence_sx, '(i0)') unit%execution_part%stop%source_page
            output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
                '(declaration-count 0) (declaration) (variable-count 0) (variable) '// &
                '(execution-part (stop-stmt (code 7) (span '//trim(variable_sx)//') '// &
                '(source-rule '//trim(unit%execution_part%stop%source_rule)//') '// &
                '(code-rule '//trim(unit%execution_part%stop%code_rule)//') '// &
                '(source-document '//trim(unit%execution_part%stop%source_document)//') '// &
                '(source-clause '//trim(unit%execution_part%stop%source_clause)//') '// &
                '(source-page '//trim(sequence_sx)//') '// &
                '(source-hash '//trim(unit%execution_part%stop%source_hash)//'))))'
            ok = .true.
            return
        end if
        if (unit%execution_part%print_count == 1_int64) then
            if ((unit%declaration_count /= 0_int64 .or. unit%variable_count /= 0_int64) .and. &
                (unit%declaration_count /= 1_int64 .or. unit%variable_count /= 1_int64 .or. &
                (unit%execution_part%sequence%assignment_count /= 1_int64 .and. &
                unit%execution_part%sequence%assignment_count /= 2_int64))) then
                message = 'invalid-program-unit-v2-print-cardinality'
                return
            end if
            call print_stmt_to_sx(unit%execution_part%print, print_sx, ok, message)
            if (.not. ok) return
            call program_root_to_sx(unit%root, root_sx, ok, message)
            if (.not. ok) return
            if (unit%declaration_count == 1_int64) then
                call program_declaration_to_sx(unit%declaration, declaration_sx, ok, message)
                if (.not. ok) return
                call variable_declaration_to_sx(unit%variable, variable_sx, ok, message)
                if (.not. ok) return
                call frontend_typed_assignment_sequence_to_sx(unit%execution_part%sequence, &
                    sequence_sx, ok, message)
                if (.not. ok) return
                output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
                    '(declaration-count 1) (declaration '//trim(declaration_sx)//') '// &
                    '(variable-count 1) (variable '//trim(variable_sx)//') '// &
                    '(execution-part '//trim(sequence_sx)//' '//trim(print_sx)//'))'
            else
                output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
                    '(declaration-count 0) (declaration) (variable-count 0) (variable) '// &
                    '(execution-part '//trim(print_sx)//'))'
            end if
            ok = .true.
            return
        end if
        if (unit%declaration_count /= 1_int64 .or. unit%variable_count /= 1_int64) then
            message = 'invalid-program-unit-v2-cardinality'
            return
        end if
        call program_root_to_sx(unit%root, root_sx, ok, message)
        if (.not. ok) return
        call program_declaration_to_sx(unit%declaration, declaration_sx, ok, message)
        if (.not. ok) return
        call variable_declaration_to_sx(unit%variable, variable_sx, ok, message)
        if (.not. ok) return
        call frontend_typed_assignment_sequence_to_sx(unit%execution_part%sequence, &
            sequence_sx, ok, message)
        if (.not. ok) return
        output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
            '(declaration-count 1) (declaration '//trim(declaration_sx)//') '// &
            '(variable-count 1) (variable '//trim(variable_sx)//') '// &
            '(execution-part '//trim(sequence_sx)//'))'
        ok = .true.
    end subroutine frontend_program_unit_v2_to_sx

    logical function stop_stmt_validate(value, message)
        type(stop_stmt_t), intent(in) :: value
        character(len=*), intent(out) :: message

        stop_stmt_validate = .false.
        message = ''
        if (value%code /= stop_policy_code) then
            message = 'invalid-stop-code'
            return
        end if
        if (trim(value%source_rule) /= trim(stop_policy_statement_rule) .or. &
            trim(value%code_rule) /= trim(stop_policy_code_rule)) then
            message = 'invalid-stop-source-rule'
            return
        end if
        if (trim(value%source_document) /= trim(stop_policy_document) .or. &
            trim(value%source_clause) /= trim(stop_policy_clause) .or. &
            value%source_page /= stop_policy_page) then
            message = 'invalid-stop-source-location'
            return
        end if
        if (trim(value%source_hash) /= trim(stop_policy_source_hash)) then
            message = 'invalid-stop-source-hash'
            return
        end if
        if (.not. source_span_validate(value%span, message)) return
        stop_stmt_validate = .true.
    end function stop_stmt_validate

end module fortfront_program_unit_v2
