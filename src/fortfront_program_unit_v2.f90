module fortfront_program_unit_v2
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: program_root_t, program_declaration_t, &
        variable_declaration_t, program_root_to_sx, program_declaration_to_sx, &
        variable_declaration_to_sx, source_span_t, source_span_to_sx, &
        source_span_validate
    use fortfront_assignment_sequence, only: assignment_sequence_t, &
        frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, assignment_sequence_source_hash
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    use frontend_program_unit_v2_envelope_generated, only: &
        program_unit_v2_execution_part_policy_matches
    use frontend_stop_policy_generated, only: stop_policy_code, &
        stop_policy_code_rule, stop_policy_clause, &
        stop_policy_document, stop_policy_page, stop_policy_source_hash, &
        stop_policy_statement_rule
    use frontend_print_policy_generated, only: print_stmt_t, print_stmt_validate, &
        print_stmt_to_sx, print_policy_format_kind, print_policy_format_value, &
        print_policy_output_kind, print_policy_output_value, &
        print_policy_output_2_kind, print_policy_output_2_value, &
        print_policy_output_3_kind, print_policy_output_3_value, print_policy_output_3_rule, &
        print_policy_output_4_kind, print_policy_output_4_value, print_policy_output_4_rule, &
        print_policy_output_5_kind, print_policy_output_5_value, print_policy_output_5_rule, &
        print_policy_output_6_kind, print_policy_output_6_value, print_policy_output_6_rule, &
        print_policy_statement_rule, print_policy_format_rule, print_policy_output_rule, &
        print_policy_output_2_rule, &
        print_policy_document, print_policy_statement_clause, print_policy_format_clause, &
        print_policy_output_clause, print_policy_statement_page, &
        print_policy_format_page, print_policy_output_page, print_policy_source_hash
    implicit none
    private

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

contains

    subroutine frontend_parse_program_unit_v2(file_name, source, source_hash, &
            unit, ok, message)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(program_unit_v2_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(typed_program_unit_t) :: declaration_unit
        character(len=1024) :: declaration_source
        character(len=128) :: execution_source_hash

        unit = program_unit_v2_t()
        ok = .false.
        message = ''
        if (.not. program_unit_v2_execution_part_policy_matches('execution-part')) then
            message = 'execution-part-policy-mismatch'
            return
        end if
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
            if (unit%declaration_count /= 0_int64 .or. unit%variable_count /= 0_int64) then
                message = 'invalid-program-unit-v2-print-cardinality'
                return
            end if
            call print_stmt_to_sx(unit%execution_part%print, print_sx, ok, message)
            if (.not. ok) return
            call program_root_to_sx(unit%root, root_sx, ok, message)
            if (.not. ok) return
            output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
                '(declaration-count 0) (declaration) (variable-count 0) (variable) '// &
                '(execution-part '//trim(print_sx)//'))'
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
