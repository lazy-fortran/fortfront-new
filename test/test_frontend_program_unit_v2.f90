program test_frontend_program_unit_v2
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: wrong_name = 'program other'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program other'//new_line('a')
    character(len=*), parameter :: wrong_type = 'program main'//new_line('a')// &
        '  real :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x * 1'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: wrong_order = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = 7'//new_line('a')//'end program main'//new_line('a')
    character(len=*), parameter :: five_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_wrong_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x * 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: six_wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  y = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a')
    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    character(len=65536) :: expected
    integer :: i
    logical :: ok

    call frontend_parse_program_unit_v2('program.f90', source, 'v2-test', unit, ok, message)
    if (.not. ok) error stop 'v2 envelope rejected source: '//trim(message)
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok) error stop 'v2 envelope serialization failed'
    expected = '(program-unit-v2 (root (program-root (name main) (span (source-span '// &
        '(file program.f90) (start-byte 0) (end-byte 64) (source-hash v2-test))))) '// &
        '(declaration-count 1) (declaration (program-declaration '// &
        '(declaration-kind program) (name main) (span (source-span '// &
        '(file program.f90) (start-byte 0) (end-byte 12) (source-hash v2-test))))) '// &
        '(variable-count 1) (variable (variable-declaration (type-spec integer) '// &
        '(name x) (span (source-span (file program.f90) (start-byte 13) '// &
        '(end-byte 27) (source-hash v2-test))))) (execution-part '// &
        '(assignment-sequence (assignment-count 2) (assignment '// &
        '(assignment-stmt (variable x) (expression (assignment-expression '// &
        '(kind integer-literal) (operator ) (left-operand 7) (right-operand ))) '// &
        '(span (source-span (file program.f90) (start-byte 28) (end-byte 34) '// &
        '(source-hash l3-raw-program-two-assignment-v1))))) (assignment '// &
        '(assignment-stmt (variable x) (expression (assignment-expression '// &
        '(kind binary-expression) (operator +) (left-operand x) (right-operand 1))) '// &
        '(span (source-span (file program.f90) (start-byte 36) (end-byte 46) '// &
        '(source-hash l3-raw-program-two-assignment-v1))))))))'
    if (trim(serialized) /= trim(expected)) then
        error stop 'v2 envelope serialization changed'
    end if
    call frontend_parse_program_unit_v2('program-five.f90', five_source, 'v2-five-test', &
        unit, ok, message)
    if (.not. ok) error stop 'v2 envelope rejected five-assignment source: '//trim(message)
    if (unit%execution_part%sequence%assignment_count /= 5 .or. &
        trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= '7') then
        error stop 'v2 five-assignment count or initializer changed'
    end if
    do i = 2, 5
        if (trim(unit%execution_part%sequence%assignment(i)%variable) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%operator) /= '+' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%left_operand) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%right_operand) /= '1' .or. &
            trim(unit%execution_part%sequence%assignment(i)%span%source_hash) /= &
            'l3-raw-program-five-assignment-v1') then
            error stop 'v2 five-assignment record changed'
        end if
    end do
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok) error stop 'v2 five-assignment serialization failed'
    if (index(trim(serialized), '(declaration-count 1)') == 0 .or. &
        index(trim(serialized), '(variable-count 1)') == 0 .or. &
        index(trim(serialized), '(assignment-count 5)') == 0 .or. &
        index(trim(serialized), 'l3-raw-program-five-assignment-v1') == 0) then
        error stop 'v2 five-assignment envelope changed'
    end if
    call frontend_parse_program_unit_v2('program-six.f90', six_source, 'v2-six-test', &
        unit, ok, message)
    if (.not. ok) error stop 'v2 envelope rejected six-assignment source: '//trim(message)
    if (unit%execution_part%sequence%assignment_count /= 6 .or. &
        trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= '7') then
        error stop 'v2 six-assignment count or initializer changed'
    end if
    do i = 2, 6
        if (trim(unit%execution_part%sequence%assignment(i)%variable) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%operator) /= '+' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%left_operand) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(i)%expression%right_operand) /= '1' .or. &
            trim(unit%execution_part%sequence%assignment(i)%span%source_hash) /= &
            'l3-raw-program-six-assignment-v1') then
            error stop 'v2 six-assignment record changed'
        end if
    end do
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok) error stop 'v2 six-assignment serialization failed'
    if (index(trim(serialized), '(execution-part (assignment-sequence (assignment-count 6)') == 0 .or. &
        index(trim(serialized), 'l3-raw-program-six-assignment-v1') == 0) then
        error stop 'v2 six-assignment envelope changed'
    end if
    call check_rejected(wrong_name)
    call check_rejected(wrong_type)
    call check_rejected(wrong_operator)
    call check_rejected(wrong_order)
    call check_rejected(six_wrong_operator)
    call check_rejected(six_wrong_variable)
    write (*, '(a)') 'frontend program-unit-v2 envelope checks: ok'

contains

    subroutine check_rejected(value)
        character(len=*), intent(in) :: value
        call frontend_parse_program_unit_v2('negative.f90', value, 'v2-test', unit, ok, message)
        if (ok) error stop 'v2 envelope accepted mutation'
    end subroutine check_rejected

end program test_frontend_program_unit_v2
