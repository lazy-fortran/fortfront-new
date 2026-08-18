program test_frontend_typed_assignment_sequence_v1
    use fortfront_assignment_sequence, only: &
        assignment_sequence_t, frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, &
        assignment_sequence_source_hash
    implicit none

    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: three_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x - 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: swapped = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = 7'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: missing_second = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  y = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(assignment_sequence_t) :: sequence

    call frontend_parse_typed_assignment_sequence('sequence.f90', source, &
        assignment_sequence_source_hash, sequence, ok, message)
    if (.not. ok) error stop 'two-assignment sequence was rejected'
    if (sequence%assignment_count /= 2) error stop 'sequence count changed'
    if (trim(sequence%assignment(1)%variable) /= 'x' .or. &
        trim(sequence%assignment(1)%expression%kind) /= 'integer-literal' .or. &
        trim(sequence%assignment(1)%expression%left_operand) /= '7') &
        error stop 'first assignment record changed'
    if (trim(sequence%assignment(2)%variable) /= 'x' .or. &
        trim(sequence%assignment(2)%expression%kind) /= 'binary-expression' .or. &
        trim(sequence%assignment(2)%expression%operator) /= '+' .or. &
        trim(sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
        trim(sequence%assignment(2)%expression%right_operand) /= '1') &
        error stop 'second assignment record changed'
    if (sequence%assignment(1)%span%start_byte >= sequence%assignment(2)%span%start_byte) &
        error stop 'assignment order was not preserved'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 2)') == 0 .or. &
        index(trim(serialized), '(left-operand 7)') == 0 .or. &
        index(trim(serialized), '(left-operand x)') == 0) &
        error stop 'sequence serialization changed'

    call frontend_parse_typed_assignment_sequence('three-sequence.f90', three_source, &
        'l3-raw-program-three-assignment-v1', sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 3) &
        error stop 'three-assignment sequence was rejected'
    if (trim(sequence%assignment(3)%expression%operator) /= '+' .or. &
        trim(sequence%assignment(3)%expression%left_operand) /= 'x' .or. &
        trim(sequence%assignment(3)%expression%right_operand) /= '1' .or. &
        sequence%assignment(2)%span%start_byte >= sequence%assignment(3)%span%start_byte) &
        error stop 'third assignment record changed'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 3)') == 0 .or. &
        index(trim(serialized), '(assignment (assignment-stmt') == 0) &
        error stop 'three-assignment serialization changed'

    call check_rejected(swapped)
    call check_rejected(missing_second)
    call check_two(source)
    call check_rejected(wrong_variable)
    call check_rejected(wrong_operator)
    write (*, '(a)') 'frontend typed assignment sequence v1 checks: ok'

contains

    subroutine check_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_typed_assignment_sequence('negative.f90', value, &
            assignment_sequence_source_hash, sequence, ok, message)
        if (ok) error stop 'invalid assignment sequence was accepted'
    end subroutine check_rejected

    subroutine check_two(value)
        character(len=*), intent(in) :: value

        call frontend_parse_typed_assignment_sequence('two-sequence.f90', value, &
            assignment_sequence_source_hash, sequence, ok, message)
        if (.not. ok .or. sequence%assignment_count /= 2) &
            error stop 'missing-third neighbour was not bounded to count two'
    end subroutine check_two

end program test_frontend_typed_assignment_sequence_v1
