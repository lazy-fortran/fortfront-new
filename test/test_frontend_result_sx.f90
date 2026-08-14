program test_frontend_result_sx
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: diagnostic_t, frontend_accepted, &
        frontend_rejected, frontend_result_from_sx, frontend_result_t, &
        frontend_result_to_sx, root_kind_none, root_kind_program, &
        severity_error, severity_warning
    implicit none

    type(frontend_result_t) :: rejected, parsed, accepted
    character(len=4096) :: serialized, reread
    character(len=256) :: message
    logical :: ok

    rejected%status = frontend_rejected
    rejected%root_kind = root_kind_none
    rejected%diagnostic_count = 2_int64
    allocate (rejected%diagnostics(2))
    call set_diagnostic(rejected%diagnostics(1), severity_warning, &
        'unresolved-syntax', 'unit.f90', 4_int64, 10_int64, 'source-hash')
    call set_diagnostic(rejected%diagnostics(2), severity_error, &
        'invalid-program', 'other.f90', 11_int64, 17_int64, 'source-hash-2')

    call frontend_result_to_sx(rejected, serialized, ok, message)
    call assert_true(ok, 'rejected result failed canonical SX writing')
    call assert_equal(trim(serialized), &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 2) (diagnostics (diagnostic (status rejected) '// &
        '(severity warning) (message unresolved-syntax) (span (file unit.f90) '// &
        '(start-byte 4) (end-byte 10) (source-hash source-hash))) '// &
        '(diagnostic (status rejected) (severity error) '// &
        '(message invalid-program) (span (file other.f90) (start-byte 11) '// &
        '(end-byte 17) (source-hash source-hash-2)))))', &
        'rejected result canonical SX changed')

    call frontend_result_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'canonical rejected result SX was rejected')
    call assert_equal(parsed%diagnostics(1)%message, 'unresolved-syntax', &
        'first diagnostic message was not preserved')
    call assert_equal(parsed%diagnostics(2)%message, 'invalid-program', &
        'second diagnostic message was not preserved')
    call assert_equal(parsed%diagnostics(1)%span%file, 'unit.f90', &
        'first diagnostic file was not preserved')
    call assert_equal(parsed%diagnostics(2)%span%source_hash, 'source-hash-2', &
        'second diagnostic provenance was not preserved')
    call assert_equal_integer(parsed%diagnostics(1)%span%start_byte, 4_int64, &
        'first diagnostic span was not preserved')
    call assert_equal_integer(parsed%diagnostics(2)%span%end_byte, 17_int64, &
        'second diagnostic span was not preserved')
    call frontend_result_to_sx(parsed, reread, ok, message)
    call assert_true(ok, 'reread rejected result failed SX writing')
    call assert_equal(trim(reread), trim(serialized), &
        'rejected result SX did not round-trip')

    call assert_invalid_sx('(frontend-result (status rejected) '// &
        '(root-kind none) (diagnostic-count 2))', 'malformed-sx-diagnostics')
    call assert_invalid_sx('(frontend-result (status rejected) '// &
        '(root-kind none) (diagnostic-count 1) (diagnostics '// &
        '(diagnostic (status rejected) (severity error) '// &
        '(message invalid-program) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 10) (source-hash))))', &
        'malformed-diagnostic-source-hash')
    call assert_invalid_sx('(frontend-result (status rejected) '// &
        '(root-kind none) (diagnostic-count 1) (diagnostics '// &
        '(diagnostic (status rejected) (severity error) '// &
        '(message invalid-program) (span (file unit.f90) (start-byte 10) '// &
        '(end-byte 4) (source-hash source-hash))))', &
        'invalid-diagnostic-span')

    accepted%status = frontend_accepted
    accepted%root_kind = root_kind_program
    call frontend_result_to_sx(accepted, serialized, ok, message)
    call assert_true(ok, 'accepted result failed compatibility SX writing')
    call assert_equal(trim(serialized), &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))', 'accepted SX compatibility changed')
    call frontend_result_from_sx(serialized, parsed, ok, message)
    call assert_true(ok, 'accepted compatibility SX was rejected')
    call assert_equal(parsed%status, frontend_accepted, &
        'accepted compatibility status changed')
    call assert_equal(parsed%root_kind, root_kind_program, &
        'accepted compatibility root kind changed')

    write (*, '(a)') 'frontend result SX behavioral checks: ok'

contains

    subroutine set_diagnostic(value, severity, message, file, start_byte, &
            end_byte, source_hash)
        type(diagnostic_t), intent(out) :: value
        character(len=*), intent(in) :: severity, message, file, source_hash
        integer(int64), intent(in) :: start_byte, end_byte

        value%status = frontend_rejected
        value%severity = severity
        value%message = message
        value%span%file = file
        value%span%start_byte = start_byte
        value%span%end_byte = end_byte
        value%span%source_hash = source_hash
    end subroutine set_diagnostic

    subroutine assert_invalid_sx(value, expected_message)
        character(len=*), intent(in) :: value, expected_message

        type(frontend_result_t) :: invalid_result

        call frontend_result_from_sx(value, invalid_result, ok, message)
        call assert_true(.not. ok, 'invalid frontend result SX was accepted')
        call assert_equal(message, expected_message, &
            'invalid frontend result SX reported the wrong failure')
    end subroutine assert_invalid_sx

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, failure)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: failure

        if (actual /= expected) error stop failure
    end subroutine assert_equal_integer

    subroutine assert_true(value, failure)
        logical, intent(in) :: value
        character(len=*), intent(in) :: failure

        if (.not. value) error stop failure
    end subroutine assert_true

end program test_frontend_result_sx
