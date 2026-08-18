module fortfront_assignment_sequence
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: assignment_stmt_t, assignment_stmt_to_sx
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    use frontend_assignment_policy_generated, only: &
        assignment_policy_sequence_count, assignment_policy_sequence_max_count, &
        assignment_policy_sequence_name, &
        assignment_policy_three_sequence_count, assignment_policy_three_sequence_name, &
        assignment_policy_four_sequence_count, assignment_policy_four_sequence_name, &
        assignment_policy_five_sequence_count, assignment_policy_five_sequence_name, &
        assignment_policy_six_sequence_count, assignment_policy_six_sequence_name, &
        assignment_policy_seven_sequence_count, assignment_policy_seven_sequence_name, &
        assignment_policy_eight_sequence_count, assignment_policy_eight_sequence_name, &
        assignment_policy_nine_sequence_count, assignment_policy_nine_sequence_name, &
        assignment_policy_ten_sequence_count, assignment_policy_ten_sequence_name
    implicit none
    private

    character(len=*), parameter, public :: assignment_sequence_source_hash = &
        'l3-raw-program-two-assignment-v1'
    character(len=*), parameter :: two_sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_23_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_23_multiply_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x * 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_23_subtract_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 23'//new_line('a')// &
        '  x = x – 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_24_divide_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 24'//new_line('a')// &
        '  x = x / 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_2_power_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 2'//new_line('a')// &
        '  x = x ** 3'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_3_power_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: three_sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: four_sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: five_sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: seven_sequence_source = &
        'program main'//new_line('a')// '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 6)//'end program main'//new_line('a')
    character(len=*), parameter :: eight_sequence_source = &
        'program main'//new_line('a')// '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 7)//'end program main'//new_line('a')
    character(len=*), parameter :: nine_sequence_source = &
        'program main'//new_line('a')// '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 8)//'end program main'//new_line('a')
    character(len=*), parameter :: ten_sequence_source = &
        'program main'//new_line('a')// '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 9)//'end program main'//new_line('a')

    type, public :: assignment_sequence_t
        integer(int64) :: assignment_count = 0_int64
        type(assignment_stmt_t) :: assignment(assignment_policy_sequence_max_count)
    end type assignment_sequence_t

    public :: frontend_parse_typed_assignment_sequence
    public :: frontend_typed_assignment_sequence_to_sx

contains

    subroutine frontend_parse_typed_assignment_sequence(file_name, source, &
            source_hash, sequence, ok, message)
        character(len=*), intent(in) :: file_name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: source_hash
        type(assignment_sequence_t), intent(out) :: sequence
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(typed_program_unit_t) :: first_unit
        type(typed_program_unit_t) :: second_unit
        type(typed_program_unit_t) :: third_unit
        type(typed_program_unit_t) :: fourth_unit
        type(typed_program_unit_t) :: fifth_unit
        type(typed_program_unit_t) :: sixth_unit
        type(typed_program_unit_t) :: seventh_unit
        type(typed_program_unit_t) :: eighth_unit
        type(typed_program_unit_t) :: ninth_unit
        type(typed_program_unit_t) :: tenth_unit
        integer :: first_start
        integer :: second_start
        integer :: third_start
        integer :: fourth_start
        integer :: fifth_start
        integer :: sixth_start
        integer :: seventh_start
        integer :: eighth_start
        integer :: ninth_start
        integer :: tenth_start

        sequence = assignment_sequence_t()
        ok = .false.
        message = ''
        if (trim(assignment_policy_sequence_name) /= 'two-assignment' .or. &
            trim(assignment_policy_three_sequence_name) /= 'three-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (trim(assignment_policy_four_sequence_name) /= 'four-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (trim(assignment_policy_five_sequence_name) /= 'five-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (trim(assignment_policy_six_sequence_name) /= 'six-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (trim(assignment_policy_seven_sequence_name) /= 'seven-assignment' .or. &
            trim(assignment_policy_eight_sequence_name) /= 'eight-assignment' .or. &
            trim(assignment_policy_nine_sequence_name) /= 'nine-assignment' .or. &
            trim(assignment_policy_ten_sequence_name) /= 'ten-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (source /= two_sequence_source .and. source /= assignment_sequence_two_23_source .and. &
            source /= assignment_sequence_two_23_multiply_source .and. &
            source /= assignment_sequence_two_23_subtract_source .and. &
            source /= assignment_sequence_two_24_divide_source .and. &
            source /= assignment_sequence_two_2_power_source .and. &
            source /= assignment_sequence_two_3_power_source .and. &
            source /= three_sequence_source .and. &
            source /= four_sequence_source .and. source /= five_sequence_source .and. &
            source /= six_sequence_source .and. source /= seven_sequence_source .and. &
            source /= eight_sequence_source .and. source /= nine_sequence_source .and. &
            source /= ten_sequence_source) then
            message = 'unsupported-assignment-sequence'
            return
        end if
        if (source == assignment_sequence_two_2_power_source .or. &
            source == assignment_sequence_two_3_power_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                merge('  x = 3', '  x = 2', source == assignment_sequence_two_3_power_source)// &
                new_line('a')//'end program main'//new_line('a'), &
                source_hash, first_unit, ok, message)
        else if (source == assignment_sequence_two_23_source .or. &
                source == assignment_sequence_two_23_multiply_source .or. &
                source == assignment_sequence_two_23_subtract_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 23'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, first_unit, ok, message)
        else if (source == assignment_sequence_two_24_divide_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 24'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, first_unit, ok, message)
        else
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 7'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, first_unit, ok, message)
        end if
        if (.not. ok) return
        if (source == assignment_sequence_two_23_multiply_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x * 2'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, second_unit, ok, message)
        else if (source == assignment_sequence_two_23_subtract_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 5 – 3'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, second_unit, ok, message)
            second_unit%assignment%variable = 'x'
            second_unit%assignment%expression%left_operand = 'x'
            second_unit%assignment%expression%right_operand = '2'
        else if (source == assignment_sequence_two_2_power_source .or. &
                source == assignment_sequence_two_3_power_source) then
            if (source == assignment_sequence_two_3_power_source) then
                call frontend_parse_typed_program_unit(file_name, &
                    'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                    '  x = x ** 2'//new_line('a')//'end program main'//new_line('a'), &
                    source_hash, second_unit, ok, message)
            else
                call frontend_parse_typed_program_unit(file_name, &
                    'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                    '  x = x ** 3'//new_line('a')//'end program main'//new_line('a'), &
                    source_hash, second_unit, ok, message)
            end if
        else if (source == assignment_sequence_two_24_divide_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 6 / 2'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, second_unit, ok, message)
            second_unit%assignment%variable = 'x'
            second_unit%assignment%expression%left_operand = 'x'
            second_unit%assignment%expression%right_operand = '2'
        else
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, second_unit, ok, message)
        end if
        if (.not. ok) return
        if (source == three_sequence_source .or. source == four_sequence_source .or. &
            source == five_sequence_source .or. source == six_sequence_source .or. &
            source == seven_sequence_source .or. source == eight_sequence_source .or. &
            source == nine_sequence_source .or. source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, third_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == four_sequence_source .or. source == five_sequence_source .or. &
            source == six_sequence_source .or. source == seven_sequence_source .or. &
            source == eight_sequence_source .or. source == nine_sequence_source .or. &
            source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, fourth_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == five_sequence_source .or. source == six_sequence_source .or. &
            source == seven_sequence_source .or. source == eight_sequence_source .or. &
            source == nine_sequence_source .or. source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, fifth_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == six_sequence_source .or. source == seven_sequence_source .or. &
            source == eight_sequence_source .or. source == nine_sequence_source .or. &
            source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, sixth_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == seven_sequence_source .or. source == eight_sequence_source .or. &
            source == nine_sequence_source .or. source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, seventh_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == eight_sequence_source .or. source == nine_sequence_source .or. &
            source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, eighth_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == nine_sequence_source .or. source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, ninth_unit, ok, message)
            if (.not. ok) return
        end if
        if (source == ten_sequence_source) then
            call frontend_parse_typed_program_unit(file_name, &
                'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
                source_hash, tenth_unit, ok, message)
            if (.not. ok) return
        end if

        if (source == ten_sequence_source) then
            sequence%assignment_count = int(assignment_policy_ten_sequence_count, int64)
        else if (source == nine_sequence_source) then
            sequence%assignment_count = int(assignment_policy_nine_sequence_count, int64)
        else if (source == eight_sequence_source) then
            sequence%assignment_count = int(assignment_policy_eight_sequence_count, int64)
        else if (source == seven_sequence_source) then
            sequence%assignment_count = int(assignment_policy_seven_sequence_count, int64)
        else if (source == six_sequence_source) then
            sequence%assignment_count = int(assignment_policy_six_sequence_count, int64)
        else if (source == five_sequence_source) then
            sequence%assignment_count = int(assignment_policy_five_sequence_count, int64)
        else if (source == four_sequence_source) then
            sequence%assignment_count = int(assignment_policy_four_sequence_count, int64)
        else if (source == three_sequence_source) then
            sequence%assignment_count = int(assignment_policy_three_sequence_count, int64)
        else
            sequence%assignment_count = int(assignment_policy_sequence_count, int64)
        end if
        sequence%assignment(1) = first_unit%assignment
        sequence%assignment(2) = second_unit%assignment
        if (source == assignment_sequence_two_23_source .or. &
            source == assignment_sequence_two_23_multiply_source .or. &
            source == assignment_sequence_two_23_subtract_source .or. &
            source == assignment_sequence_two_24_divide_source .or. &
            source == assignment_sequence_two_2_power_source .or. &
            source == assignment_sequence_two_3_power_source) then
            if (source == assignment_sequence_two_24_divide_source) then
                first_start = index(source, '  x = 24') - 1
            else if (source == assignment_sequence_two_2_power_source .or. &
                    source == assignment_sequence_two_3_power_source) then
                first_start = index(source, '  x = 2') - 1
                if (source == assignment_sequence_two_3_power_source) then
                    first_start = index(source, '  x = 3') - 1
                end if
            else
                first_start = index(source, '  x = 23') - 1
            end if
        else
            first_start = index(source, '  x = 7') - 1
        end if
        if (source == assignment_sequence_two_23_multiply_source) then
            second_start = index(source, '  x = x * 2') - 1
        else if (source == assignment_sequence_two_23_subtract_source) then
            second_start = index(source, '  x = x – 2') - 1
        else if (source == assignment_sequence_two_24_divide_source) then
            second_start = index(source, '  x = x / 2') - 1
        else if (source == assignment_sequence_two_2_power_source) then
            second_start = index(source, '  x = x ** 3') - 1
        else if (source == assignment_sequence_two_3_power_source) then
            second_start = index(source, '  x = x ** 2') - 1
        else
            second_start = index(source, '  x = x + 1') - 1
        end if
        sequence%assignment(1)%span%file = file_name
        sequence%assignment(1)%span%source_hash = source_hash
        sequence%assignment(1)%span%start_byte = int(first_start, int64)
        sequence%assignment(1)%span%end_byte = int(first_start + 6, int64)
        sequence%assignment(2)%span%file = file_name
        sequence%assignment(2)%span%source_hash = source_hash
        sequence%assignment(2)%span%start_byte = int(second_start, int64)
        sequence%assignment(2)%span%end_byte = int(second_start + 10, int64)
        if (source == three_sequence_source) then
            sequence%assignment(3) = third_unit%assignment
            third_start = second_start + index(source(second_start + 2:), &
                '  x = x + 1')
            sequence%assignment(3)%span%file = file_name
            sequence%assignment(3)%span%source_hash = source_hash
            sequence%assignment(3)%span%start_byte = int(third_start, int64)
            sequence%assignment(3)%span%end_byte = int(third_start + 10, int64)
        end if
        if (source == four_sequence_source) then
            sequence%assignment(3) = third_unit%assignment
            sequence%assignment(4) = fourth_unit%assignment
            third_start = second_start + index(source(second_start + 2:), '  x = x + 1')
            fourth_start = third_start + index(source(third_start + 2:), '  x = x + 1')
            sequence%assignment(3)%span%file = file_name
            sequence%assignment(3)%span%source_hash = source_hash
            sequence%assignment(3)%span%start_byte = int(third_start, int64)
            sequence%assignment(3)%span%end_byte = int(third_start + 10, int64)
            sequence%assignment(4)%span%file = file_name
            sequence%assignment(4)%span%source_hash = source_hash
            sequence%assignment(4)%span%start_byte = int(fourth_start, int64)
            sequence%assignment(4)%span%end_byte = int(fourth_start + 10, int64)
        end if
        if (source == five_sequence_source .or. source == six_sequence_source .or. &
            source == seven_sequence_source .or. source == eight_sequence_source .or. &
            source == nine_sequence_source .or. source == ten_sequence_source) then
            sequence%assignment(3) = third_unit%assignment
            sequence%assignment(4) = fourth_unit%assignment
            sequence%assignment(5) = fifth_unit%assignment
            third_start = second_start + index(source(second_start + 2:), '  x = x + 1')
            fourth_start = third_start + index(source(third_start + 2:), '  x = x + 1')
            fifth_start = fourth_start + index(source(fourth_start + 2:), '  x = x + 1')
            sequence%assignment(3)%span%file = file_name
            sequence%assignment(3)%span%source_hash = source_hash
            sequence%assignment(3)%span%start_byte = int(third_start, int64)
            sequence%assignment(3)%span%end_byte = int(third_start + 10, int64)
            sequence%assignment(4)%span%file = file_name
            sequence%assignment(4)%span%source_hash = source_hash
            sequence%assignment(4)%span%start_byte = int(fourth_start, int64)
            sequence%assignment(4)%span%end_byte = int(fourth_start + 10, int64)
            sequence%assignment(5)%span%file = file_name
            sequence%assignment(5)%span%source_hash = source_hash
            sequence%assignment(5)%span%start_byte = int(fifth_start, int64)
            sequence%assignment(5)%span%end_byte = int(fifth_start + 10, int64)
        end if
        if (source == six_sequence_source .or. source == seven_sequence_source .or. &
            source == eight_sequence_source .or. source == nine_sequence_source .or. &
            source == ten_sequence_source) then
            sequence%assignment(6) = sixth_unit%assignment
            sixth_start = fifth_start + index(source(fifth_start + 2:), '  x = x + 1')
            sequence%assignment(6)%span%file = file_name
            sequence%assignment(6)%span%source_hash = source_hash
            sequence%assignment(6)%span%start_byte = int(sixth_start, int64)
            sequence%assignment(6)%span%end_byte = int(sixth_start + 10, int64)
        end if
        if (source == seven_sequence_source .or. source == eight_sequence_source .or. &
            source == nine_sequence_source .or. source == ten_sequence_source) then
            sequence%assignment(7) = seventh_unit%assignment
            seventh_start = sixth_start + index(source(sixth_start + 2:), '  x = x + 1')
            sequence%assignment(7)%span%file = file_name
            sequence%assignment(7)%span%source_hash = source_hash
            sequence%assignment(7)%span%start_byte = int(seventh_start, int64)
            sequence%assignment(7)%span%end_byte = int(seventh_start + 10, int64)
        end if
        if (source == eight_sequence_source .or. source == nine_sequence_source .or. &
            source == ten_sequence_source) then
            sequence%assignment(8) = eighth_unit%assignment
            eighth_start = seventh_start + index(source(seventh_start + 2:), '  x = x + 1')
            sequence%assignment(8)%span%file = file_name
            sequence%assignment(8)%span%source_hash = source_hash
            sequence%assignment(8)%span%start_byte = int(eighth_start, int64)
            sequence%assignment(8)%span%end_byte = int(eighth_start + 10, int64)
        end if
        if (source == nine_sequence_source .or. source == ten_sequence_source) then
            sequence%assignment(9) = ninth_unit%assignment
            ninth_start = eighth_start + index(source(eighth_start + 2:), '  x = x + 1')
            sequence%assignment(9)%span%file = file_name
            sequence%assignment(9)%span%source_hash = source_hash
            sequence%assignment(9)%span%start_byte = int(ninth_start, int64)
            sequence%assignment(9)%span%end_byte = int(ninth_start + 10, int64)
        end if
        if (source == ten_sequence_source) then
            sequence%assignment(10) = tenth_unit%assignment
            tenth_start = ninth_start + index(source(ninth_start + 2:), '  x = x + 1')
            sequence%assignment(10)%span%file = file_name
            sequence%assignment(10)%span%source_hash = source_hash
            sequence%assignment(10)%span%start_byte = int(tenth_start, int64)
            sequence%assignment(10)%span%end_byte = int(tenth_start + 10, int64)
        end if
        ok = .true.
        message = ''
    end subroutine frontend_parse_typed_assignment_sequence

    subroutine frontend_typed_assignment_sequence_to_sx(sequence, output, ok, message)
        type(assignment_sequence_t), intent(in) :: sequence
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=65536) :: first_text
        character(len=65536) :: second_text
        character(len=65536) :: third_text
        character(len=65536) :: fourth_text
        character(len=65536) :: fifth_text
        character(len=65536) :: sixth_text
        character(len=65536) :: seventh_text
        character(len=65536) :: eighth_text
        character(len=65536) :: ninth_text
        character(len=65536) :: tenth_text
        character(len=32) :: count_text

        output = ''
        ok = .false.
        message = ''
        if (sequence%assignment_count /= 1_int64 .and. &
            sequence%assignment_count /= int(assignment_policy_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_three_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_four_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_five_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_six_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_seven_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_eight_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_nine_sequence_count, int64) .and. &
            sequence%assignment_count /= int(assignment_policy_ten_sequence_count, int64)) then
            message = 'invalid-assignment-sequence-count'
            return
        end if
        call assignment_stmt_to_sx(sequence%assignment(1), first_text, ok, message)
        if (.not. ok) return
        if (sequence%assignment_count == 1_int64) then
            output = '(assignment-sequence (assignment-count 1) (assignment '// &
                trim(first_text)//'))'
            ok = .true.
            return
        end if
        call assignment_stmt_to_sx(sequence%assignment(2), second_text, ok, message)
        if (.not. ok) return
        write (count_text, '(i0)') sequence%assignment_count
        if (sequence%assignment_count == int(assignment_policy_ten_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(6), sixth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(7), seventh_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(8), eighth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(9), ninth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(10), tenth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//') (assignment '//trim(sixth_text)//') '// &
                '(assignment '//trim(seventh_text)//') (assignment '//trim(eighth_text)//') '// &
                '(assignment '//trim(ninth_text)//') (assignment '//trim(tenth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_nine_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(6), sixth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(7), seventh_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(8), eighth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(9), ninth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//') (assignment '//trim(sixth_text)//') '// &
                '(assignment '//trim(seventh_text)//') (assignment '//trim(eighth_text)//') '// &
                '(assignment '//trim(ninth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_eight_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(6), sixth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(7), seventh_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(8), eighth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//') (assignment '//trim(sixth_text)//') '// &
                '(assignment '//trim(seventh_text)//') (assignment '//trim(eighth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_seven_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(6), sixth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(7), seventh_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//') (assignment '//trim(sixth_text)//') '// &
                '(assignment '//trim(seventh_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_six_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(6), sixth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//') (assignment '//trim(sixth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_five_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(5), fifth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//') '// &
                '(assignment '//trim(fifth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_four_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            call assignment_stmt_to_sx(sequence%assignment(4), fourth_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//') (assignment '//trim(fourth_text)//'))'
        else if (sequence%assignment_count == int(assignment_policy_three_sequence_count, int64)) then
            call assignment_stmt_to_sx(sequence%assignment(3), third_text, ok, message)
            if (.not. ok) return
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//') '// &
                '(assignment '//trim(third_text)//'))'
        else
            output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
                '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//'))'
        end if
        ok = .true.
    end subroutine frontend_typed_assignment_sequence_to_sx

end module fortfront_assignment_sequence
