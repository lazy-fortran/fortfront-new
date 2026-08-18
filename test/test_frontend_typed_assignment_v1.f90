program test_frontend_typed_assignment_v1
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, typed_program_unit_t, &
        assignment_policy_source_rule
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-integer-assignment-v1'
    character(len=*), parameter :: source = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: missing_rhs = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x ='//new_line('a')// &
        'end program main'//new_line('a')
    character(len=*), parameter :: wrong_variable = 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  y = 1'//new_line('a')// &
        'end program main'//new_line('a')
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok
    type(typed_program_unit_t) :: unit

    if (trim(assignment_policy_source_rule) /= 'R1033') error stop 'source rule changed'

    call frontend_parse_typed_program_unit('assignment.f90', source, source_hash, &
        unit, ok, message)
    if (.not. ok) error stop 'integer assignment witness was rejected'
    if (unit%assignment_count /= 1_int64) error stop 'assignment count changed'
    if (trim(unit%assignment%variable) /= 'x') error stop 'assignment variable changed'
    if (trim(unit%assignment%expression) /= '1') error stop 'assignment expression changed'
    if (unit%assignment%span%start_byte /= 28_int64 .or. &
        unit%assignment%span%end_byte /= 34_int64) error stop 'assignment span changed'
    if (trim(unit%assignment%span%source_hash) /= source_hash) &
        error stop 'assignment source hash changed'
    call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(assignment-stmt') == 0) &
        error stop 'assignment AST output missing'

    call check_rejected(missing_rhs)
    call check_rejected(wrong_variable)
    write (*, '(a)') 'frontend typed assignment v1 checks: ok'

contains

    subroutine check_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_typed_program_unit('assignment-negative.f90', value, &
            source_hash, unit, ok, message)
        if (ok) error stop 'malformed assignment was accepted'
    end subroutine check_rejected

end program test_frontend_typed_assignment_v1
