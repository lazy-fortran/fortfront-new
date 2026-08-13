program test_diagnostic
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: diagnostic_from_sx, diagnostic_table_capacity, &
        diagnostic_t, diagnostic_to_sx, diagnostic_validate, &
        frontend_rejected, frontend_validate_diagnostic_table, &
        severity_warning
    implicit none

    type(diagnostic_t) :: diagnostic, parsed
    type(diagnostic_t) :: diagnostics(diagnostic_table_capacity)
    character(len=2048) :: serialized, reread
    character(len=128) :: message
    logical :: ok

    call set_diagnostic(diagnostic)
    call assert_true(diagnostic_validate(diagnostic, message), &
        'valid diagnostic was rejected')
    call diagnostic_to_sx(diagnostic, serialized, ok, message)
    call assert_true(ok, 'valid diagnostic failed SX writing')
    call assert_equal(trim(serialized), &
        '(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 10) (source-hash source-hash)))', &
        'diagnostic canonical SX oracle changed')

    call diagnostic_from_sx(serialized, parsed, ok, message, 'source-hash')
    call assert_true(ok, 'canonical diagnostic SX was rejected')
    call assert_equal(parsed%status, frontend_rejected, &
        'reader lost diagnostic status')
    call assert_equal(parsed%severity, severity_warning, &
        'reader lost diagnostic severity')
    call assert_equal(parsed%message, 'unresolved-syntax', &
        'reader lost diagnostic message')
    call assert_equal(parsed%span%file, 'unit.f90', &
        'reader lost diagnostic file')
    call assert_equal_integer(parsed%span%start_byte, 4_int64, &
        'reader lost diagnostic start byte')
    call assert_equal_integer(parsed%span%end_byte, 10_int64, &
        'reader lost diagnostic end byte')
    call assert_equal(parsed%span%source_hash, 'source-hash', &
        'reader lost diagnostic source hash')
    call diagnostic_to_sx(parsed, reread, ok, message)
    call assert_true(ok, 'reread diagnostic failed SX writing')
    call assert_equal(trim(reread), trim(serialized), &
        'diagnostic SX did not round-trip')

    diagnostic%status = 'unknown'
    call assert_invalid(diagnostic, 'invalid-diagnostic-status')
    call set_diagnostic(diagnostic)
    diagnostic%severity = 'fatal'
    call assert_invalid(diagnostic, 'invalid-diagnostic-severity')
    call set_diagnostic(diagnostic)
    diagnostic%message = ''
    call assert_invalid(diagnostic, 'missing-diagnostic-message')
    call set_diagnostic(diagnostic)
    diagnostic%span%file = ''
    call assert_invalid(diagnostic, 'missing-diagnostic-file')
    call set_diagnostic(diagnostic)
    diagnostic%span%source_hash = ''
    call assert_invalid(diagnostic, 'missing-diagnostic-source-hash')
    call set_diagnostic(diagnostic)
    diagnostic%message = 'message with-space'
    call assert_invalid(diagnostic, 'invalid-diagnostic-message')
    call set_diagnostic(diagnostic)
    diagnostic%span%start_byte = -1_int64
    call assert_invalid(diagnostic, 'negative-diagnostic-start-byte')
    call set_diagnostic(diagnostic)
    diagnostic%span%end_byte = 3_int64
    call assert_invalid(diagnostic, 'invalid-diagnostic-span')

    call set_diagnostic(diagnostics(1))
    call assert_true(frontend_validate_diagnostic_table(diagnostics, 0_int64, &
        message), 'empty diagnostic table was rejected')
    call assert_true(frontend_validate_diagnostic_table(diagnostics, 1_int64, &
        message, 'source-hash'), 'valid diagnostic table was rejected')
    diagnostics(1)%severity = 'fatal'
    call assert_invalid_table(diagnostics, 1_int64, 'invalid-diagnostic-severity')
    call assert_invalid_table(diagnostics, -1_int64, 'negative-diagnostic-count')
    call assert_invalid_table(diagnostics, &
        int(diagnostic_table_capacity, int64) + 1_int64, &
        'diagnostic-table-capacity-exceeded')
    call assert_invalid_table(diagnostics(1:1), 2_int64, &
        'diagnostic-count-exceeds-array')

    call assert_invalid_sx('(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 10) (source-hash source-hash)) extra)', &
        'malformed-diagnostic')
    call assert_invalid_sx(trim(serialized)//' trailing', 'malformed-diagnostic')
    call assert_invalid_sx('(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 10) (source-hash source-hash)', 'malformed-diagnostic-span')
    call assert_invalid_sx('(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) '// &
        '(start-byte 9223372036854775808) (end-byte 10) '// &
        '(source-hash source-hash)))', 'diagnostic-start-byte-too-large')
    call assert_invalid_sx('(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 9223372036854775808) (source-hash source-hash)))', &
        'diagnostic-end-byte-too-large')
    call assert_invalid_sx('(diagnostic (status rejected) (severity warning) '// &
        '(message unresolved-syntax) (span (file unit.f90) (start-byte 4) '// &
        '(end-byte 10) (source-hash))', 'malformed-diagnostic-source-hash')

    call diagnostic_from_sx(serialized, parsed, ok, message, 'tampered-hash')
    call assert_true(.not. ok, 'tampered diagnostic provenance was accepted')
    call assert_equal(message, 'diagnostic-source-hash-mismatch', &
        'tampered provenance reported the wrong failure')

    write (*, '(a)') 'diagnostic behavioral checks: ok'

contains

    subroutine set_diagnostic(value)
        type(diagnostic_t), intent(out) :: value

        value%status = frontend_rejected
        value%severity = severity_warning
        value%message = 'unresolved-syntax'
        value%span%file = 'unit.f90'
        value%span%start_byte = 4_int64
        value%span%end_byte = 10_int64
        value%span%source_hash = 'source-hash'
    end subroutine set_diagnostic

    subroutine assert_invalid(value, expected_message)
        type(diagnostic_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message

        character(len=2048) :: output
        character(len=128) :: validation_message
        logical :: valid

        valid = diagnostic_validate(value, validation_message)
        call assert_true(.not. valid, 'invalid diagnostic was accepted')
        call assert_equal(validation_message, expected_message, &
            'invalid diagnostic reported the wrong failure')
        call diagnostic_to_sx(value, output, valid, validation_message)
        call assert_true(.not. valid, 'invalid diagnostic was serialized')
    end subroutine assert_invalid

    subroutine assert_invalid_sx(value, expected_message)
        character(len=*), intent(in) :: value, expected_message

        call diagnostic_from_sx(value, parsed, ok, message)
        call assert_true(.not. ok, 'invalid diagnostic SX was accepted')
        call assert_equal(message, expected_message, &
            'invalid diagnostic SX reported the wrong failure')
    end subroutine assert_invalid_sx

    subroutine assert_invalid_table(items, count, expected_message)
        type(diagnostic_t), intent(in) :: items(:)
        integer(int64), intent(in) :: count
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_validate_diagnostic_table(items, count, message)
        call assert_true(.not. valid, 'invalid diagnostic table was accepted')
        call assert_equal(message, expected_message, &
            'invalid diagnostic table reported the wrong failure')
    end subroutine assert_invalid_table

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

end program test_diagnostic
