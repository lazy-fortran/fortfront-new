program test_frontend_typed_assignment_sequence_v1
    use fortfront_assignment_sequence, only: &
        assignment_sequence_t, frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, &
        assignment_sequence_source_hash, assignment_sequence_two_negative_source, &
        assignment_sequence_two_3_power_source
    implicit none

    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: three_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: four_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: five_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
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
    character(len=*), parameter :: four_wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x - 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: four_wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  y = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: four_swapped = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = 7'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: six_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: seven_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 6)//'end program main'//new_line('a')
    character(len=*), parameter :: eight_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 7)//'end program main'//new_line('a')
    character(len=*), parameter :: nine_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 8)//'end program main'//new_line('a')
    character(len=*), parameter :: ten_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 9)//'end program main'//new_line('a')
    character(len=*), parameter :: seven_wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 5)//'  x = x - 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: eight_wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 6)//'  y = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: nine_swapped = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 6)//'  x = 7'//new_line('a')// &
        repeat('  x = x + 1'//new_line('a'), 1)//'end program main'//new_line('a')
    character(len=*), parameter :: five_wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x - 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: five_wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  y = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: five_swapped = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = 7'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x - 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: six_wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  y = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: six_swapped = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
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

    call frontend_parse_typed_assignment_sequence('negative-initializer.f90', &
        assignment_sequence_two_negative_source, 'l3-raw-program-v2', sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 2 .or. &
        trim(sequence%assignment(1)%expression%left_operand) /= '-5' .or. &
        sequence%assignment(1)%span%start_byte /= 28 .or. &
        sequence%assignment(1)%span%end_byte /= 35 .or. &
        sequence%assignment(2)%span%start_byte /= 37 .or. &
        sequence%assignment(2)%span%end_byte /= 47 .or. &
        sequence%assignment(1)%span%end_byte - sequence%assignment(1)%span%start_byte /= 7 .or. &
        trim(sequence%assignment(1)%span%source_hash) /= 'l3-raw-program-v2' .or. &
        trim(sequence%assignment(2)%expression%operator) /= '+') &
        error stop 'signed initializer sequence changed'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(left-operand -5)') == 0 .or. &
        index(trim(serialized), 'l3-raw-program-v2') == 0) &
        error stop 'signed initializer sequence serialization changed'

    call frontend_parse_typed_assignment_sequence('power-sequence.f90', &
        assignment_sequence_two_3_power_source, assignment_sequence_source_hash, &
        sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 2 .or. &
        trim(sequence%assignment(1)%expression%left_operand) /= '3' .or. &
        trim(sequence%assignment(2)%expression%operator) /= '**' .or. &
        sequence%assignment(1)%span%start_byte /= 28 .or. &
        sequence%assignment(1)%span%end_byte /= 34 .or. &
        sequence%assignment(2)%span%start_byte /= 36 .or. &
        sequence%assignment(2)%span%end_byte /= 47) &
        error stop 'power assignment sequence spans changed'

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

    call frontend_parse_typed_assignment_sequence('four-sequence.f90', four_source, &
        'l3-raw-program-four-assignment-v1', sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 4) &
        error stop 'four-assignment sequence was rejected'
    if (trim(sequence%assignment(4)%variable) /= 'x' .or. &
        trim(sequence%assignment(4)%expression%operator) /= '+' .or. &
        sequence%assignment(3)%span%start_byte >= sequence%assignment(4)%span%start_byte) &
        error stop 'fourth assignment record changed'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 4)') == 0 .or. &
        index(trim(serialized), '(assignment (assignment-stmt') == 0) &
        error stop 'four-assignment serialization changed'

    call frontend_parse_typed_assignment_sequence('five-sequence.f90', five_source, &
        'l3-raw-program-five-assignment-v1', sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 5) &
        error stop 'five-assignment sequence was rejected'
    if (trim(sequence%assignment(5)%variable) /= 'x' .or. &
        trim(sequence%assignment(5)%expression%operator) /= '+' .or. &
        trim(sequence%assignment(5)%expression%left_operand) /= 'x' .or. &
        trim(sequence%assignment(5)%expression%right_operand) /= '1' .or. &
        sequence%assignment(4)%span%start_byte >= sequence%assignment(5)%span%start_byte .or. &
        trim(sequence%assignment(5)%span%source_hash) /= 'l3-raw-program-five-assignment-v1') &
        error stop 'fifth assignment record changed'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 5)') == 0) &
        error stop 'five-assignment serialization changed'
    call frontend_parse_typed_assignment_sequence('six-sequence.f90', six_source, &
        'l3-raw-program-six-assignment-v1', sequence, ok, message)
    if (.not. ok .or. sequence%assignment_count /= 6) &
        error stop 'six-assignment sequence was rejected'
    if (trim(sequence%assignment(6)%variable) /= 'x' .or. &
        trim(sequence%assignment(6)%expression%operator) /= '+' .or. &
        trim(sequence%assignment(6)%expression%left_operand) /= 'x' .or. &
        trim(sequence%assignment(6)%expression%right_operand) /= '1' .or. &
        sequence%assignment(5)%span%start_byte >= sequence%assignment(6)%span%start_byte .or. &
        trim(sequence%assignment(6)%span%source_hash) /= 'l3-raw-program-six-assignment-v1') &
        error stop 'sixth assignment record changed'
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-count 6)') == 0 .or. &
        index(trim(serialized), '(assignment (assignment-stmt') == 0) &
        error stop 'six-assignment serialization changed'
    call check_positive(seven_source, 7, 'seven')
    call check_positive(eight_source, 8, 'eight')
    call check_positive(nine_source, 9, 'nine')
    call check_positive(ten_source, 10, 'ten')
    call frontend_parse_typed_assignment_sequence('missing-fifth.f90', four_source, &
        'l3-raw-program-four-assignment-v1', sequence, ok, message)
    sequence%assignment_count = 5
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (ok) error stop 'missing fifth assignment was serialized'
    call frontend_parse_typed_assignment_sequence('missing-sixth.f90', five_source, &
        'l3-raw-program-five-assignment-v1', sequence, ok, message)
    sequence%assignment_count = 6
    call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    if (ok) error stop 'missing sixth assignment was serialized'

    call check_rejected(swapped)
    call check_rejected(missing_second)
    call check_two(source)
    call check_rejected(wrong_variable)
    call check_rejected(wrong_operator)
    call check_rejected(four_wrong_variable)
    call check_rejected(four_wrong_operator)
    call check_rejected(four_swapped)
    call check_rejected(five_wrong_operator)
    call check_rejected(five_wrong_variable)
    call check_rejected(five_swapped)
    call check_rejected(six_wrong_operator)
    call check_rejected(six_wrong_variable)
    call check_rejected(six_swapped)
    call check_rejected(seven_wrong_operator)
    call check_rejected(eight_wrong_variable)
    call check_rejected(nine_swapped)
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

    subroutine check_positive(value, expected_count, label)
        character(len=*), intent(in) :: value, label
        integer, intent(in) :: expected_count

        call frontend_parse_typed_assignment_sequence(label//'-sequence.f90', value, &
            assignment_sequence_source_hash, sequence, ok, message)
        if (.not. ok .or. sequence%assignment_count /= expected_count) &
            error stop 'generated assignment sequence was rejected'
        if (trim(sequence%assignment(expected_count)%variable) /= 'x' .or. &
            trim(sequence%assignment(expected_count)%expression%operator) /= '+' .or. &
            sequence%assignment(expected_count - 1)%span%start_byte >= &
            sequence%assignment(expected_count)%span%start_byte) &
            error stop 'generated assignment sequence record changed'
        call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
        if (.not. ok .or. index(trim(serialized), &
            '(assignment-count '//trim(adjustl(to_string(expected_count)))//')') == 0) &
            error stop 'generated assignment sequence serialization changed'
    end subroutine check_positive

    function to_string(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write (text, '(i0)') value
    end function to_string

end program test_frontend_typed_assignment_sequence_v1
