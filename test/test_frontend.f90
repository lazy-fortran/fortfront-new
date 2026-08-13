program test_frontend
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_parse, &
        frontend_read, frontend_rejected, frontend_result_from_sx, &
        root_kind_none, root_kind_program, &
        severity_error, frontend_result_t, standardir_syntax_item_t, &
        frontend_result_to_sx, frontend_validate
    implicit none

    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: syntax_item
    character(len=256) :: sx
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    call assert_equal(result%status, frontend_accepted, &
        'non-empty source was rejected')
    call assert_equal(result%root_kind, root_kind_program, &
        'accepted root kind was not program')
    call assert_equal(result%root%name, 'unit', &
        'program name was not parsed')
    call assert_equal_integer(result%diagnostic_count, 0_int64, &
        'accepted source produced a diagnostic')
    call frontend_result_to_sx(result, sx, ok, message)
    call assert_true(ok, 'accepted result failed SX validation')
    call assert_equal(trim(sx), &
        '(frontend-result (status accepted) (root-kind program) '// &
        '(diagnostic-count 0))', 'accepted SX oracle changed')
    call assert_sx_round_trip(sx, 'accepted SX did not round-trip')
    result%root_kind = root_kind_none
    call assert_invalid_result(result, 'invalid-accepted-result')
    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    call assert_equal(result%root%span%file, 'unit.f90', &
        'root file was not retained')
    call assert_equal(result%root%span%source_hash, 'hash-positive', &
        'root source hash was not retained')
    call assert_equal_integer(result%root%span%start_byte, 0_int64, &
        'root start byte was not zero')
    call assert_equal_integer(result%root%span%end_byte, 16_int64, &
        'root end byte was not source length')

    call frontend_read('without-witness.f90', &
        'program unit'//new_line('a')//'end', 'hash-no-witness', result)
    call assert_equal(result%status, frontend_rejected, &
        'compatibility wrapper accepted source without a witness')
    call assert_equal(result%diagnostics(1)%message, 'missing-syntax-witness', &
        'missing witness diagnostic changed')

    call frontend_read('empty.f90', '', 'hash-negative', result)
    call assert_equal(result%status, frontend_rejected, &
        'empty source was accepted')
    call assert_equal(result%root_kind, root_kind_none, &
        'rejected root kind was not none')
    call assert_equal_integer(result%diagnostic_count, 1_int64, &
        'empty source did not produce one diagnostic')
    call assert_equal(result%diagnostics(1)%message, 'empty-source', &
        'empty-source diagnostic message changed')
    call assert_equal(result%diagnostics(1)%severity, severity_error, &
        'empty-source diagnostic severity changed')
    call assert_equal(result%diagnostics(1)%span%file, 'empty.f90', &
        'diagnostic file was not retained')
    call assert_equal(result%diagnostics(1)%span%source_hash, 'hash-negative', &
        'diagnostic source hash was not retained')
    call frontend_result_to_sx(result, sx, ok, message)
    call assert_true(ok, 'rejected result failed SX validation')
    call assert_equal(trim(sx), &
        '(frontend-result (status rejected) (root-kind none) '// &
        '(diagnostic-count 1))', 'rejected SX oracle changed')
    call assert_sx_round_trip(sx, 'rejected SX did not round-trip')

    call assert_invalid_sx('(frontend-result (status unknown) '// &
        '(root-kind none) (diagnostic-count 1))', 'invalid-result-status')
    call assert_invalid_sx('(frontend-result (status accepted) '// &
        '(root-kind program) (diagnostic-count 1))', 'invalid-accepted-result')
    call assert_invalid_sx('(frontend-result (status rejected) '// &
        '(root-kind none) (diagnostic-count 0))', 'invalid-rejected-result')
    call assert_invalid_sx('(frontend-result (status accepted) '// &
        '(root-kind unknown) (diagnostic-count 0))', 'invalid-result-root-kind')
    call assert_invalid_sx('(frontend-result (status rejected) '// &
        '(root-kind none) (diagnostic-count -1))', &
        'negative-diagnostic-count')
    call assert_invalid_sx('(frontend-result (status accepted) '// &
        '(root-kind program) (diagnostic-count 0)', 'malformed-sx-record')
    call assert_invalid_sx('(frontend-result (status accepted) '// &
        '(root-kind program) (diagnostic-count 0) (extra x)))', &
        'malformed-sx-record')

    syntax_item%lhs = 'module'
    call frontend_parse('module.f90', 'module unit'//new_line('a')//'end', &
        'hash-unsupported', syntax_item, result)
    call assert_equal(result%status, frontend_rejected, &
        'unsupported syntax was accepted')
    call assert_equal(result%diagnostics(1)%message, 'unsupported-syntax-item', &
        'unsupported syntax diagnostic changed')
    call assert_equal(result%diagnostics(1)%span%file, 'module.f90', &
        'unsupported syntax span lost its file')

    call set_program_witness(syntax_item)
    syntax_item%resolution = 'unresolved'
    call frontend_parse('unresolved.f90', 'program unit'//new_line('a')//'end', &
        'hash-unresolved', syntax_item, result)
    call assert_equal(result%status, frontend_rejected, &
        'unresolved syntax was accepted')
    call assert_equal(result%diagnostics(1)%message, 'unresolved-syntax', &
        'unresolved syntax diagnostic changed')
    call assert_equal(result%diagnostics(1)%span%source_hash, 'hash-unresolved', &
        'unresolved syntax span lost its hash')

    call set_program_witness(syntax_item)
    call frontend_parse('broken.f90', 'program'//new_line('a')//'end', &
        'hash-invalid', syntax_item, result)
    call assert_equal(result%status, frontend_rejected, &
        'malformed program was accepted')
    call assert_equal(result%diagnostics(1)%message, 'invalid-program', &
        'malformed program diagnostic changed')

    result%status = 'unknown'
    call assert_invalid_result(result, 'invalid-result-status')
    result%status = frontend_rejected
    result%root_kind = root_kind_program
    call assert_invalid_result(result, 'invalid-rejected-result')
    result%root_kind = root_kind_none
    result%diagnostic_count = 0_int64
    call assert_invalid_result(result, 'diagnostic-count-mismatch')
    result%diagnostic_count = 1_int64
    result%diagnostics(1)%status = 'unknown'
    call assert_invalid_result(result, 'invalid-diagnostic-status')
    result%diagnostics(1)%status = frontend_rejected
    result%diagnostics(1)%severity = 'fatal'
    call assert_invalid_result(result, 'invalid-diagnostic-severity')

    write (*, '(a)') 'frontend behavioral checks: ok'

contains

    subroutine set_program_witness(syntax_item)
        type(standardir_syntax_item_t), intent(out) :: syntax_item

        syntax_item%id = 'R501'
        syntax_item%lhs = 'program'
        syntax_item%origin = 'mechanical'
        syntax_item%resolution = 'resolved'
        syntax_item%source%document = 'J3-24-007'
        syntax_item%source%clause = '1'
        syntax_item%source%rule = 'R501'
        syntax_item%source%page = 45_int64
        syntax_item%source%source_hash = 'fixture'
    end subroutine set_program_witness

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (trim(actual) /= trim(expected)) error stop message
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_integer

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

    subroutine assert_invalid_result(value, expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_message

        character(len=256) :: serialized
        character(len=128) :: validation_message
        logical :: valid

        valid = frontend_validate(value, validation_message)
        call assert_true(.not. valid, 'invalid result was accepted')
        call assert_equal(validation_message, expected_message, &
            'invalid result reported the wrong failure')
        call frontend_result_to_sx(value, serialized, valid, validation_message)
        call assert_true(.not. valid, 'invalid result was serialized')
    end subroutine assert_invalid_result

    subroutine assert_sx_round_trip(serialized, failure_message)
        character(len=*), intent(in) :: serialized, failure_message

        character(len=256) :: reread, message
        type(frontend_result_t) :: parsed
        logical :: ok

        call frontend_result_from_sx(serialized, parsed, ok, message)
        call assert_true(ok, failure_message)
        call frontend_result_to_sx(parsed, reread, ok, message)
        call assert_true(ok, failure_message)
        call assert_equal(trim(reread), trim(serialized), failure_message)
    end subroutine assert_sx_round_trip

    subroutine assert_invalid_sx(serialized, expected_message)
        character(len=*), intent(in) :: serialized, expected_message

        character(len=128) :: message
        character(len=256) :: reread
        type(frontend_result_t) :: parsed
        logical :: ok

        call frontend_result_from_sx(serialized, parsed, ok, message)
        call assert_true(.not. ok, 'malformed SX was accepted')
        call assert_equal(message, expected_message, &
            'malformed SX reported the wrong failure')
        call frontend_result_to_sx(parsed, reread, ok, message)
        call assert_true(.not. ok, 'invalid SX produced a valid result')
    end subroutine assert_invalid_sx

end program test_frontend
