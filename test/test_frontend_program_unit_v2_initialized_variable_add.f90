program test_frontend_program_unit_v2_initialized_variable_add
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok

    call assert_accepted('3')
    call assert_accepted('42')

    call assert_rejected('x = x - x', 'x')
    call assert_rejected('x = x + y', 'x')
    call assert_rejected('y = x + x', 'x')
    call assert_rejected('x = x + x', 'y')
    call assert_rejected('x = 3.0', 'x')

    write (*, '(a)') 'frontend program-unit-v2 initialized variable-add checks: ok'

contains

    subroutine assert_accepted(initializer)
        character(len=*), intent(in) :: initializer
        character(len=1024) :: source
        character(len=*), parameter :: source_hash = 'initialized-variable-add-input'

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(initializer)//new_line('a')//'  x = x + x'// &
            new_line('a')//'  print *, x'//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-variable-add.f90', trim(source), &
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
            trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '+' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= 'x' .or. &
            trim(unit%execution_part%print%output_name) /= 'x') then
            error stop 'valid initialized variable add changed'
        end if

        call assert_provenance(unit%root%span%file, unit%root%span%source_hash, &
            unit%root%span%start_byte, unit%root%span%end_byte, 'root', &
            'initialized-variable-add.f90', source_hash)
        call assert_provenance(unit%declaration%span%file, unit%declaration%span%source_hash, &
            unit%declaration%span%start_byte, unit%declaration%span%end_byte, 'declaration', &
            'initialized-variable-add.f90', source_hash)
        call assert_provenance(unit%variable%span%file, unit%variable%span%source_hash, &
            unit%variable%span%start_byte, unit%variable%span%end_byte, 'variable', &
            'initialized-variable-add.f90', source_hash)
        call assert_provenance(unit%execution_part%sequence%assignment(1)%span%file, &
            unit%execution_part%sequence%assignment(1)%span%source_hash, &
            unit%execution_part%sequence%assignment(1)%span%start_byte, &
            unit%execution_part%sequence%assignment(1)%span%end_byte, 'initializer', &
            'initialized-variable-add.f90', source_hash)
        call assert_provenance(unit%execution_part%sequence%assignment(2)%span%file, &
            unit%execution_part%sequence%assignment(2)%span%source_hash, &
            unit%execution_part%sequence%assignment(2)%span%start_byte, &
            unit%execution_part%sequence%assignment(2)%span%end_byte, 'variable-add', &
            'initialized-variable-add.f90', source_hash)
        call assert_provenance(unit%execution_part%print%span%file, &
            unit%execution_part%print%span%source_hash, &
            unit%execution_part%print%span%start_byte, unit%execution_part%print%span%end_byte, &
            'print', 'initialized-variable-add.f90', source_hash)

        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(trim(serialized), '(assignment-count 2)') == 0 .or. &
            index(trim(serialized), '(operator +)') == 0 .or. &
            index(trim(serialized), '(left-operand x)') == 0 .or. &
            index(trim(serialized), '(right-operand x)') == 0 .or. &
            index(trim(serialized), '(source-hash '//source_hash//')') == 0) then
            error stop 'initialized variable add serialization changed'
        end if
    end subroutine assert_accepted

    subroutine assert_rejected(assignment, printed_name)
        character(len=*), intent(in) :: assignment, printed_name
        character(len=1024) :: source

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = 3'//new_line('a')//'  '//trim(assignment)//new_line('a')// &
            '  print *, '//trim(printed_name)//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-variable-add-negative.f90', &
            trim(source), 'initialized-variable-add-negative-input', unit, ok, message)
        if (ok) error stop 'invalid initialized variable add was accepted'
    end subroutine assert_rejected

    subroutine assert_provenance(file_name, source_hash, start_byte, end_byte, label, &
            expected_file, expected_hash)
        character(len=*), intent(in) :: file_name, source_hash, label
        character(len=*), intent(in) :: expected_file, expected_hash
        integer(int64), intent(in) :: start_byte, end_byte

        if (trim(file_name) /= trim(expected_file) .or. trim(source_hash) /= trim(expected_hash) .or. &
            start_byte < 0_int64 .or. end_byte <= start_byte) then
            error stop 'initialized variable add provenance changed: '//trim(label)
        end if
    end subroutine assert_provenance

end program test_frontend_program_unit_v2_initialized_variable_add
