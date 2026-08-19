program test_frontend_typed_assignment_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, typed_program_unit_t, &
        assignment_policy_source_rule
    use frontend_assignment_policy_generated, only: &
        assignment_policy_integer_literal_min, assignment_policy_integer_literal_max, &
        assignment_policy_signed_integer_literal_min, assignment_policy_signed_integer_literal_max
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-integer-assignment-v1'
    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_7 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_0 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 0'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_minus_1 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = -1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_2047 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 2047'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_2048 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 2048'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_literal_minus_101 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = -101'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_real_literal = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7.0'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_add = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 1 + 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_variable_add = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_variable_multiply = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x * 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_variable_add_2 = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = x + 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_malformed_variable = 'program main'//new_line('a')// &
        '  integer :: xx'//new_line('a')//'  xx = xx + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_wrong_variable_rhs = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = y + 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_subtract = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5 – 3'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_multiply = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 2 * 3'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: source_divide = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 6 / 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_rhs = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x ='//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  y = 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: changed_operator = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 1 - 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_subtract_left = 'program main'// &
        new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = – 3'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_subtract_right = 'program main'// &
        new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 5 –'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: changed_multiply_operator = 'program main'// &
        new_line('a')//'  integer :: x'//new_line('a')//'  x = 2 ** 4'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_divide_left = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = / 2'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_divide_right = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 6 /'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    if (trim(assignment_policy_source_rule) /= 'R1033') error stop 'source rule changed'
    if (assignment_policy_integer_literal_min /= 0 .or. &
        assignment_policy_integer_literal_max /= 2047 .or. &
        assignment_policy_signed_integer_literal_min /= -100 .or. &
        assignment_policy_signed_integer_literal_max /= -1) error stop 'literal range changed'

    call frontend_parse_typed_program_unit('assignment.f90', source, source_hash, &
        unit, ok, message)
    if (.not. ok) error stop 'integer assignment witness was rejected'
    if (unit%assignment_count /= 1_int64) error stop 'assignment count changed'
    if (trim(unit%assignment%variable) /= 'x') error stop 'assignment variable changed'
    if (trim(unit%assignment%expression%kind) /= 'integer-literal' .or. &
        trim(unit%assignment%expression%left_operand) /= '1') &
        error stop 'assignment literal expression changed'
    if (unit%assignment%span%start_byte /= 28_int64 .or. &
        unit%assignment%span%end_byte /= 34_int64) error stop 'assignment span changed'
    if (trim(unit%assignment%span%source_hash) /= source_hash) &
        error stop 'assignment source hash changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-stmt') == 0) &
        error stop 'assignment AST output missing'

    call frontend_parse_typed_program_unit('assignment-literal-0.f90', source_literal_0, &
        source_hash, unit, ok, message)
    if (.not. ok .or. trim(unit%assignment%expression%left_operand) /= '0') &
        error stop 'minimum decimal integer literal was rejected'

    call frontend_parse_typed_program_unit('assignment-literal-7.f90', source_literal_7, &
        source_hash, unit, ok, message)
    if (.not. ok) error stop 'decimal integer literal assignment was rejected'
    if (trim(unit%assignment%expression%kind) /= 'integer-literal' .or. &
        trim(unit%assignment%expression%left_operand) /= '7' .or. &
        trim(unit%assignment%expression%operator) /= '' .or. &
        trim(unit%assignment%expression%right_operand) /= '') &
        error stop 'decimal integer literal AST changed'

    call frontend_parse_typed_program_unit('assignment-literal-2047.f90', &
        source_literal_2047, source_hash, unit, ok, message)
    if (.not. ok .or. trim(unit%assignment%expression%left_operand) /= '2047') &
        error stop 'maximum decimal integer literal was rejected'

    call frontend_parse_typed_program_unit('assignment-literal-minus-1.f90', &
        source_literal_minus_1, source_hash, unit, ok, message)
    if (.not. ok .or. trim(unit%assignment%expression%left_operand) /= '-1' .or. &
        unit%assignment%span%start_byte /= 28_int64 .or. &
        unit%assignment%span%end_byte /= 35_int64 .or. &
        trim(unit%assignment%span%source_hash) /= source_hash) &
        error stop 'signed decimal integer literal assignment changed'

    call frontend_parse_typed_program_unit('assignment-literal-minus-101.f90', &
        source_literal_minus_101, source_hash, unit, ok, message)
    if (ok) error stop 'out-of-range signed literal was accepted'

    call frontend_parse_typed_program_unit('assignment-variable-add.f90', &
        source_variable_add, source_hash, unit, ok, message)
    if (.not. ok) error stop 'variable add assignment witness was rejected'
    if (trim(unit%assignment%expression%kind) /= 'binary-expression' .or. &
        trim(unit%assignment%expression%operator) /= '+' .or. &
        trim(unit%assignment%expression%left_operand) /= 'x' .or. &
        trim(unit%assignment%expression%right_operand) /= '1') &
        error stop 'variable add expression changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator +)') == 0 .or. &
        index(trim(serialized), '(left-operand x)') == 0 .or. &
        index(trim(serialized), '(right-operand 1)') == 0) &
        error stop 'variable add AST missing'

    call frontend_parse_typed_program_unit('assignment-add.f90', source_add, source_hash, &
        unit, ok, message)
    if (.not. ok) error stop 'integer add assignment witness was rejected'
    if (trim(unit%assignment%expression%kind) /= 'binary-expression') &
        error stop 'binary expression kind missing'
    if (trim(unit%assignment%expression%operator) /= '+') &
        error stop 'binary expression operator missing'
    if (trim(unit%assignment%expression%left_operand) /= '1' .or. &
        trim(unit%assignment%expression%right_operand) /= '2') &
        error stop 'binary expression operands changed'
    if (unit%assignment%span%start_byte /= 28_int64 .or. &
        unit%assignment%span%end_byte /= 38_int64) error stop 'add assignment span changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator +)') == 0 .or. &
        index(trim(serialized), '(left-operand 1)') == 0 .or. &
        index(trim(serialized), '(right-operand 2)') == 0) &
        error stop 'structured binary expression AST missing'

    call frontend_parse_typed_program_unit('assignment-subtract.f90', source_subtract, &
        source_hash, unit, ok, message)
    if (.not. ok) error stop 'integer subtract assignment witness was rejected'
    if (trim(unit%assignment%expression%kind) /= 'binary-expression' .or. &
        trim(unit%assignment%expression%operator) /= '–' .or. &
        trim(unit%assignment%expression%left_operand) /= '5' .or. &
        trim(unit%assignment%expression%right_operand) /= '3') &
        error stop 'subtract binary expression changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator –)') == 0 .or. &
        index(trim(serialized), '(left-operand 5)') == 0 .or. &
        index(trim(serialized), '(right-operand 3)') == 0) &
        error stop 'subtract binary expression AST missing'

    call frontend_parse_typed_program_unit('assignment-multiply.f90', source_multiply, &
        source_hash, unit, ok, message)
    if (.not. ok) error stop 'integer multiply assignment witness was rejected'
    if (trim(unit%assignment%expression%kind) /= 'binary-expression' .or. &
        trim(unit%assignment%expression%operator) /= '*' .or. &
        trim(unit%assignment%expression%left_operand) /= '2' .or. &
        trim(unit%assignment%expression%right_operand) /= '3') &
        error stop 'multiply binary expression changed'
    if (unit%assignment%span%start_byte /= 28_int64 .or. &
        unit%assignment%span%end_byte /= 38_int64) error stop 'multiply assignment span changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(expression (assignment-expression') == 0 .or. &
        index(trim(serialized), '(operator *)') == 0 .or. &
        index(trim(serialized), '(left-operand 2)') == 0 .or. &
        index(trim(serialized), '(right-operand 3)') == 0) &
        error stop 'multiply binary expression AST missing'

    call frontend_parse_typed_program_unit('assignment-regression.f90', source_add, &
        source_hash, unit, ok, message)
    if (.not. ok .or. trim(unit%assignment%expression%operator) /= '+' .or. &
        trim(unit%assignment%expression%left_operand) /= '1') &
        error stop 'plus assignment regressed after multiply'

    call frontend_parse_typed_program_unit('assignment-divide.f90', source_divide, &
        source_hash, unit, ok, message)
    if (.not. ok) error stop 'integer divide assignment witness was rejected'
    if (trim(unit%assignment%expression%kind) /= 'binary-expression' .or. &
        trim(unit%assignment%expression%operator) /= '/' .or. &
        trim(unit%assignment%expression%left_operand) /= '6' .or. &
        trim(unit%assignment%expression%right_operand) /= '2') &
        error stop 'divide binary expression changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(operator /)') == 0 .or. &
        index(trim(serialized), '(left-operand 6)') == 0 .or. &
        index(trim(serialized), '(right-operand 2)') == 0) &
        error stop 'divide binary expression AST missing'

    call check_rejected(missing_rhs)
    call check_rejected(wrong_variable)
    call check_rejected(changed_operator)
    call check_rejected(changed_multiply_operator)
    call check_rejected(missing_divide_left)
    call check_rejected(missing_divide_right)
    call check_rejected(missing_subtract_left)
    call check_rejected(missing_subtract_right)
    call check_rejected(source_literal_2048)
    call check_rejected(source_real_literal)
    call check_rejected(source_variable_multiply)
    call check_rejected(source_variable_add_2)
    call check_rejected(source_malformed_variable)
    call check_rejected(source_wrong_variable_rhs)
    write (*, '(a)') 'frontend typed assignment v1 checks: ok'

contains

    subroutine check_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_typed_program_unit('assignment-negative.f90', value, &
            source_hash, unit, ok, message)
        if (ok) error stop 'malformed assignment was accepted'
    end subroutine check_rejected

end program test_frontend_typed_assignment_v1
