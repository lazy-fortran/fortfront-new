program test_frontend_result_span_query
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_parse, &
        frontend_query_result_span, frontend_read, frontend_result_t, &
        root_kind_program, source_span_t, standardir_syntax_item_t
    implicit none

    type(frontend_result_t) :: result
    type(source_span_t) :: span
    type(standardir_syntax_item_t) :: syntax_item
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', syntax_item, result)
    call assert_equal(result%status, frontend_accepted, &
        'fixture source was not accepted')

    ok = frontend_query_result_span(result, root_kind_program, 'unit.f90', &
        'hash-positive', 0_int64, 16_int64, span, message)
    call assert_true(ok, 'valid result span query was rejected')
    call assert_equal(span%file, 'unit.f90', 'result span query changed source file')
    call assert_equal(span%source_hash, 'hash-positive', &
        'result span query changed source hash')
    call assert_equal_integer(span%start_byte, 0_int64, &
        'result span query changed start byte')
    call assert_equal_integer(span%end_byte, 16_int64, &
        'result span query changed end byte')

    call assert_rejected(result, 'source', 'unit.f90', 'hash-positive', &
        0_int64, 16_int64, 'frontend-result-root-kind-mismatch')
    call assert_rejected(result, root_kind_program, 'other.f90', 'hash-positive', &
        0_int64, 16_int64, 'frontend-result-source-file-mismatch')
    call assert_rejected(result, root_kind_program, 'unit.f90', 'other-hash', &
        0_int64, 16_int64, 'frontend-result-source-hash-mismatch')
    call assert_rejected(result, root_kind_program, 'unit.f90', 'hash-positive', &
        1_int64, 16_int64, 'frontend-result-source-span-mismatch')
    call assert_rejected(result, root_kind_program, 'unit.f90', 'hash-positive', &
        0_int64, 15_int64, 'frontend-result-source-span-mismatch')
    call assert_rejected(result, '', 'unit.f90', 'hash-positive', 0_int64, 16_int64, &
        'invalid-expected-root-kind')
    call assert_rejected(result, root_kind_program, '', 'hash-positive', &
        0_int64, 16_int64, 'missing-expected-source-file')
    call assert_rejected(result, root_kind_program, 'unit.f90', '', &
        0_int64, 16_int64, 'missing-expected-source-hash')
    call assert_rejected(result, root_kind_program, 'unit.f90', 'hash-positive', &
        -1_int64, 16_int64, 'invalid-expected-source-span')

    result%root%span%end_byte = -1_int64
    call assert_rejected(result, root_kind_program, 'unit.f90', 'hash-positive', &
        0_int64, 16_int64, 'invalid-program-root-span')

    call frontend_read('rejected.f90', '', 'hash-rejected', result)
    call assert_rejected(result, root_kind_program, 'rejected.f90', 'hash-rejected', &
        0_int64, 0_int64, 'rejected-frontend-result')

    write (*, '(a)') 'frontend result span query behavioral checks: ok'

contains

    subroutine set_program_witness(value)
        type(standardir_syntax_item_t), intent(out) :: value

        value%id = 'R501'
        value%lhs = 'program'
        value%origin = 'mechanical'
        value%resolution = 'resolved'
        value%source%document = 'J3-24-007'
        value%source%clause = '1'
        value%source%rule = 'R501'
        value%source%page = 45_int64
        value%source%source_hash = 'fixture'
    end subroutine set_program_witness

    subroutine assert_rejected(value, expected_kind, expected_file, expected_hash, &
            expected_start, expected_end, expected_message)
        type(frontend_result_t), intent(in) :: value
        character(len=*), intent(in) :: expected_kind, expected_file, expected_hash
        integer(int64), intent(in) :: expected_start, expected_end
        character(len=*), intent(in) :: expected_message

        logical :: valid

        valid = frontend_query_result_span(value, expected_kind, expected_file, &
            expected_hash, expected_start, expected_end, span, message)
        call assert_true(.not. valid, 'invalid result span query was accepted')
        call assert_equal(message, expected_message, &
            'result span query reported the wrong failure')
    end subroutine assert_rejected

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

end program test_frontend_result_span_query
