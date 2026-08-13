program test_frontend
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_accepted, frontend_read, &
        frontend_rejected, root_kind_none, root_kind_source, &
        severity_error, frontend_result_t
    implicit none

    type(frontend_result_t) :: result

    call frontend_read('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-positive', result)
    call assert_equal(result%status, frontend_accepted, &
        'non-empty source was rejected')
    call assert_equal(result%root_kind, root_kind_source, &
        'accepted root kind was not source')
    call assert_equal_integer(result%diagnostic_count, 0_int64, &
        'accepted source produced a diagnostic')
    call assert_equal(result%root%span%file, 'unit.f90', &
        'root file was not retained')
    call assert_equal(result%root%span%source_hash, 'hash-positive', &
        'root source hash was not retained')
    call assert_equal_integer(result%root%span%start_byte, 0_int64, &
        'root start byte was not zero')
    call assert_equal_integer(result%root%span%end_byte, 16_int64, &
        'root end byte was not source length')

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

    write (*, '(a)') 'frontend behavioral checks: ok'

contains

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (trim(actual) /= trim(expected)) error stop message
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_integer

end program test_frontend
