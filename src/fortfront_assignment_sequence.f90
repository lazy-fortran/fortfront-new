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
    character(len=*), parameter, public :: assignment_sequence_two_3_power_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 3'//new_line('a')// &
        '  x = x ** 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter, public :: assignment_sequence_two_negative_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = -5'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
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

        character(len=*), parameter :: envelope_prefix = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')
        character(len=*), parameter :: envelope_suffix = 'end program main'//new_line('a')
        type(typed_program_unit_t) :: unit
        character(len=:), allocatable :: assignment_line
        character(len=:), allocatable :: assignment_source
        integer :: sequence_count
        integer :: current_position
        integer :: line_end_position
        integer :: newline_position
        integer :: assignment_end_position
        logical :: power_sequence

        sequence = assignment_sequence_t()
        ok = .false.
        message = ''
        if (len(source) < len(envelope_prefix) + len(envelope_suffix)) then
            message = 'unsupported-assignment-sequence'
            return
        end if
        if (source(1:len(envelope_prefix)) /= envelope_prefix .or. &
            source(len(source) - len(envelope_suffix) + 1:) /= envelope_suffix) then
            message = 'unsupported-assignment-sequence'
            return
        end if

        sequence_count = 0
        current_position = len(envelope_prefix) + 1
        assignment_end_position = len(source) - len(envelope_suffix)
        power_sequence = .false.
        do while (current_position <= assignment_end_position)
            newline_position = index(source(current_position:assignment_end_position), new_line('a'))
            if (newline_position == 0) then
                message = 'unsupported-assignment-sequence'
                return
            end if
            line_end_position = current_position + newline_position - 2
            if (line_end_position < current_position) then
                message = 'unsupported-assignment-sequence'
                return
            end if
            sequence_count = sequence_count + 1
            if (sequence_count > assignment_policy_sequence_max_count) then
                message = 'unsupported-assignment-sequence'
                return
            end if
            assignment_line = source(current_position:line_end_position)
            assignment_source = envelope_prefix//assignment_line//new_line('a')//envelope_suffix
            call frontend_parse_typed_program_unit(file_name, assignment_source, source_hash, &
                unit, ok, message)
            if (.not. ok) return
            if (sequence_count == 1) then
                if (trim(unit%assignment%expression%kind) /= 'integer-literal') then
                    ok = .false.
                    message = 'unsupported-assignment-sequence'
                    return
                end if
            else
                if (trim(unit%assignment%expression%operator) == '**') then
                    if (sequence_count /= 2 .or. &
                        trim(unit%assignment%expression%left_operand) /= 'x' .or. &
                        trim(unit%assignment%expression%right_operand) /= '2' .or. &
                        trim(sequence%assignment(1)%expression%left_operand) /= '3') then
                        ok = .false.
                        message = 'unsupported-assignment-sequence'
                        return
                    end if
                    power_sequence = .true.
                else
                    if (trim(unit%assignment%expression%kind) /= 'binary-expression' .or. &
                        trim(unit%assignment%expression%operator) /= '+' .or. &
                        trim(unit%assignment%expression%left_operand) /= 'x' .or. &
                        trim(unit%assignment%expression%right_operand) /= '1') then
                        ok = .false.
                        message = 'unsupported-assignment-sequence'
                        return
                    end if
                end if
            end if
            sequence%assignment(sequence_count) = unit%assignment
            sequence%assignment(sequence_count)%span%start_byte = &
                int(current_position - 1, int64) + unit%assignment%span%start_byte - len(envelope_prefix)
            sequence%assignment(sequence_count)%span%end_byte = &
                int(current_position - 1, int64) + unit%assignment%span%end_byte - len(envelope_prefix)
            sequence%assignment(sequence_count)%span%file = file_name
            sequence%assignment(sequence_count)%span%source_hash = source_hash
            current_position = current_position + newline_position
        end do
        if (sequence_count < 2) then
            ok = .false.
            message = 'unsupported-assignment-sequence'
            return
        end if
        if (power_sequence) then
            if (sequence_count /= 2) then
                ok = .false.
                message = 'unsupported-assignment-sequence'
                return
            end if
        end if
        sequence%assignment_count = int(sequence_count, int64)
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
