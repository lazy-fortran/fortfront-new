module fortfront_assignment_sequence
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: assignment_stmt_t, assignment_stmt_to_sx
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    use frontend_assignment_policy_generated, only: &
        assignment_policy_sequence_count, assignment_policy_sequence_name
    implicit none
    private

    character(len=*), parameter, public :: assignment_sequence_source_hash = &
        'l3-raw-program-two-assignment-v1'
    character(len=*), parameter :: sequence_source = &
        'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')

    type, public :: assignment_sequence_t
        integer(int64) :: assignment_count = 0_int64
        type(assignment_stmt_t) :: assignment(assignment_policy_sequence_count)
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
        integer :: first_start
        integer :: second_start

        sequence = assignment_sequence_t()
        ok = .false.
        message = ''
        if (trim(assignment_policy_sequence_name) /= 'two-assignment') then
            message = 'assignment-sequence-policy-mismatch'
            return
        end if
        if (source /= sequence_source) then
            message = 'unsupported-two-assignment-sequence'
            return
        end if
        call frontend_parse_typed_program_unit(file_name, &
            'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = 7'//new_line('a')//'end program main'//new_line('a'), &
            source_hash, first_unit, ok, message)
        if (.not. ok) return
        call frontend_parse_typed_program_unit(file_name, &
            'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = x + 1'//new_line('a')//'end program main'//new_line('a'), &
            source_hash, second_unit, ok, message)
        if (.not. ok) return

        sequence%assignment_count = int(assignment_policy_sequence_count, int64)
        sequence%assignment(1) = first_unit%assignment
        sequence%assignment(2) = second_unit%assignment
        first_start = index(source, '  x = 7') - 1
        second_start = index(source, '  x = x + 1') - 1
        sequence%assignment(1)%span%file = file_name
        sequence%assignment(1)%span%source_hash = source_hash
        sequence%assignment(1)%span%start_byte = int(first_start, int64)
        sequence%assignment(1)%span%end_byte = int(first_start + 6, int64)
        sequence%assignment(2)%span%file = file_name
        sequence%assignment(2)%span%source_hash = source_hash
        sequence%assignment(2)%span%start_byte = int(second_start, int64)
        sequence%assignment(2)%span%end_byte = int(second_start + 10, int64)
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
        character(len=32) :: count_text

        output = ''
        ok = .false.
        message = ''
        if (sequence%assignment_count /= int(assignment_policy_sequence_count, int64)) then
            message = 'invalid-two-assignment-count'
            return
        end if
        call assignment_stmt_to_sx(sequence%assignment(1), first_text, ok, message)
        if (.not. ok) return
        call assignment_stmt_to_sx(sequence%assignment(2), second_text, ok, message)
        if (.not. ok) return
        write (count_text, '(i0)') sequence%assignment_count
        output = '(assignment-sequence (assignment-count '//trim(count_text)//') '// &
            '(assignment '//trim(first_text)//') (assignment '//trim(second_text)//'))'
        ok = .true.
    end subroutine frontend_typed_assignment_sequence_to_sx

end module fortfront_assignment_sequence
