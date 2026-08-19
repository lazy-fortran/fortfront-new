program test_frontend_program_unit_v2_initialized_divisor
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok

    call assert_accepted('42', '2')
    call assert_accepted('-42', '10')

    call assert_rejected('42', 'x / 0')
    call assert_rejected('42', 'x / 11')
    call assert_rejected('42.0', 'x / 2')
    call assert_rejected('42', 'x / 2.0')
    call assert_rejected('42', 'x / x')
    call assert_rejected('42', 'x + 2')
    call assert_rejected('42', 'y / 2')
    call assert_rejected('42', 'x /')

    write (*, '(a)') 'frontend program-unit-v2 initialized divisor checks: ok'

contains

    subroutine assert_accepted(initializer, divisor)
        character(len=*), intent(in) :: initializer, divisor
        character(len=1024) :: source
        character(len=*), parameter :: source_hash = 'initialized-divisor-input'
        integer(int64) :: first_start, first_end
        integer(int64) :: second_start, second_end
        integer(int64) :: print_start, print_end

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(initializer)//new_line('a')//'  x = x / '//trim(divisor)// &
            new_line('a')//'  print *, x'//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-divisor.f90', trim(source), &
            source_hash, unit, ok, message)
        if (.not. ok .or. unit%execution_part%sequence%assignment_count /= 2_int64 .or. &
            unit%execution_part%print_count /= 1_int64 .or. &
            trim(unit%root%name) /= 'main' .or. trim(unit%variable%name) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(1)%variable) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(1)%expression%kind) /= &
            'integer-literal' .or. &
            trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= &
            trim(initializer) .or. &
            trim(unit%execution_part%sequence%assignment(2)%variable) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%kind) /= &
            'binary-expression' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '/' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= &
            trim(divisor) .or. &
            trim(unit%execution_part%print%output_name) /= 'x') then
            error stop 'valid initialized divisor changed'
        end if

        first_start = int(index(trim(source), '  x = '//trim(initializer)) - 1, int64)
        first_end = first_start + int(len_trim(initializer) + 5, int64)
        second_start = int(index(trim(source), '  x = x / '//trim(divisor)) - 1, int64)
        second_end = second_start + int(len('  x = x / ') + len_trim(divisor) - 1, int64)
        print_start = int(index(trim(source), '  print *, x') - 1, int64)
        print_end = print_start + int(len('  print *, x') - 1, int64)
        call assert_span(unit%root%span%file, unit%root%span%source_hash, &
            unit%root%span%start_byte, unit%root%span%end_byte, &
            'root', 'initialized-divisor.f90', source_hash, 0_int64, &
            int(len_trim(source) - 1, int64))
        call assert_span(unit%declaration%span%file, unit%declaration%span%source_hash, &
            unit%declaration%span%start_byte, unit%declaration%span%end_byte, &
            'declaration', 'initialized-divisor.f90', source_hash, 0_int64, &
            int(index(trim(source), new_line('a')) - 1, int64))
        call assert_provenance(unit%variable%span%file, unit%variable%span%source_hash, &
            unit%variable%span%start_byte, unit%variable%span%end_byte, 'variable', &
            'initialized-divisor.f90', source_hash)
        call assert_span(unit%execution_part%sequence%assignment(1)%span%file, &
            unit%execution_part%sequence%assignment(1)%span%source_hash, &
            unit%execution_part%sequence%assignment(1)%span%start_byte, &
            unit%execution_part%sequence%assignment(1)%span%end_byte, &
            'initializer', 'initialized-divisor.f90', source_hash, first_start, first_end)
        call assert_span(unit%execution_part%sequence%assignment(2)%span%file, &
            unit%execution_part%sequence%assignment(2)%span%source_hash, &
            unit%execution_part%sequence%assignment(2)%span%start_byte, &
            unit%execution_part%sequence%assignment(2)%span%end_byte, &
            'divisor', 'initialized-divisor.f90', source_hash, second_start, second_end)
        call assert_span(unit%execution_part%print%span%file, &
            unit%execution_part%print%span%source_hash, &
            unit%execution_part%print%span%start_byte, unit%execution_part%print%span%end_byte, &
            'print', 'initialized-divisor.f90', source_hash, print_start, print_end)

        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(trim(serialized), '(assignment-count 2)') == 0 .or. &
            index(trim(serialized), '(operator /)') == 0 .or. &
            index(trim(serialized), '(left-operand x)') == 0 .or. &
            index(trim(serialized), '(right-operand '//trim(divisor)//')') == 0 .or. &
            index(trim(serialized), '(source-hash '//source_hash//')') == 0) then
            error stop 'initialized divisor serialization changed'
        end if
    end subroutine assert_accepted

    subroutine assert_rejected(initializer, assignment)
        character(len=*), intent(in) :: initializer, assignment
        character(len=1024) :: source

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(initializer)//new_line('a')//'  '//trim(assignment)// &
            new_line('a')//'  print *, x'//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-divisor-negative.f90', trim(source), &
            'initialized-divisor-negative-input', unit, ok, message)
        if (ok) error stop 'invalid initialized divisor was accepted'
    end subroutine assert_rejected

    subroutine assert_provenance(file_name, source_hash, start_byte, end_byte, label, &
            expected_file, expected_hash)
        character(len=*), intent(in) :: file_name, source_hash, label
        character(len=*), intent(in) :: expected_file, expected_hash
        integer(int64), intent(in) :: start_byte, end_byte

        if (trim(file_name) /= trim(expected_file) .or. trim(source_hash) /= trim(expected_hash) .or. &
            start_byte < 0_int64 .or. end_byte <= start_byte) then
            error stop 'initialized divisor provenance changed: '//trim(label)
        end if
    end subroutine assert_provenance

    subroutine assert_span(file_name, source_hash, start_byte, end_byte, label, &
            expected_file, expected_hash, expected_start, expected_end)
        character(len=*), intent(in) :: file_name, source_hash, label
        character(len=*), intent(in) :: expected_file, expected_hash
        integer(int64), intent(in) :: start_byte, end_byte, expected_start, expected_end

        if (trim(file_name) /= trim(expected_file) .or. trim(source_hash) /= trim(expected_hash) .or. &
            start_byte /= expected_start .or. end_byte /= expected_end) then
            error stop 'initialized divisor provenance changed: '//trim(label)
        end if
    end subroutine assert_span

end program test_frontend_program_unit_v2_initialized_divisor
