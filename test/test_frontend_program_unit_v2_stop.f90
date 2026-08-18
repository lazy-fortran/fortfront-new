program test_frontend_program_unit_v2_stop
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t, stop_stmt_validate
    use frontend_stop_policy_generated, only: stop_policy_code, &
        stop_policy_code_rule, stop_policy_page, stop_policy_source_hash, &
        stop_policy_statement_rule
    implicit none

    character(len=*), parameter :: source = 'program p'//new_line('a')// &
        '  stop 7'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: stop_eight = 'program p'//new_line('a')// &
        '  stop 8'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: missing_code = 'program p'//new_line('a')// &
        '  stop'//new_line('a')//'end program p'//new_line('a')
    character(len=*), parameter :: error_stop = 'program p'//new_line('a')// &
        '  error stop 7'//new_line('a')//'end program p'//new_line('a')
    character(len=256) :: message
    character(len=65536) :: serialized
    logical :: ok
    type(program_unit_v2_t) :: unit

    call frontend_parse_program_unit_v2('stop.f90', source, 'stop-input', unit, ok, message)
    if (.not. ok) error stop 'bounded STOP 7 source was rejected'
    if (unit%execution_part%stop_count /= 1 .or. &
        unit%execution_part%stop%code /= stop_policy_code .or. &
        trim(unit%execution_part%stop%source_rule) /= stop_policy_statement_rule .or. &
        trim(unit%execution_part%stop%code_rule) /= stop_policy_code_rule .or. &
        unit%execution_part%stop%source_page /= stop_policy_page .or. &
        trim(unit%execution_part%stop%source_hash) /= stop_policy_source_hash) then
        error stop 'STOP 7 typed provenance changed'
    end if
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok .or. index(trim(serialized), '(code 7)') == 0 .or. &
        index(trim(serialized), stop_policy_source_hash) == 0 .or. &
        index(trim(serialized), '(source-page 214)') == 0) then
        error stop 'STOP 7 serialization changed'
    end if
    unit%execution_part%stop%code = 8
    if (stop_stmt_validate(unit%execution_part%stop, message)) then
        error stop 'mutated STOP code passed validation'
    end if

    call assert_rejected(stop_eight)
    call assert_rejected(missing_code)
    call assert_rejected(error_stop)
    write (*, '(a)') 'frontend program-unit-v2 STOP 7 checks: ok'

contains

    subroutine assert_rejected(value)
        character(len=*), intent(in) :: value

        call frontend_parse_program_unit_v2('negative-stop.f90', value, 'stop-input', &
            unit, ok, message)
        if (ok) error stop 'STOP mutation was accepted'
    end subroutine assert_rejected

end program test_frontend_program_unit_v2_stop
