program test_frontend_program_unit_v2_initialized_variable_multiply
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok

    call assert_accepted('42')
    call assert_accepted('-42')

    call assert_rejected('42', 'y = x * x', 'x')
    call assert_rejected('42', 'x = x * y', 'x')
    call assert_rejected('42', 'x = x * x', 'y')
    call assert_rejected('42.0', 'x = x * x', 'x')
    call assert_rejected('2048', 'x = x * x', 'x')

    write (*, '(a)') 'frontend program-unit-v2 initialized variable-multiply checks: ok'

contains

    subroutine assert_accepted(initializer)
        character(len=*), intent(in) :: initializer
        character(len=1024) :: source
        character(len=*), parameter :: source_hash = 'initialized-variable-multiply-input'

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(initializer)//new_line('a')//'  x = x * x'//new_line('a')// &
            '  print *, x'//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-variable-multiply.f90', trim(source), &
            source_hash, unit, ok, message)
        if (.not. ok) error stop 'valid initialized variable multiply was rejected'
        if (unit%execution_part%sequence%assignment_count /= 2_int64) then
            error stop 'initialized variable multiply assignment count changed'
        end if
        if (unit%execution_part%print_count /= 1_int64) then
            error stop 'initialized variable multiply print count changed'
        end if
        if (trim(unit%root%name) /= 'main' .or. trim(unit%variable%name) /= 'x') then
            error stop 'initialized variable multiply names changed'
        end if
        if (trim(unit%execution_part%sequence%assignment(1)%expression%kind) /= &
            'integer-literal' .or. trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= &
            trim(initializer)) then
            error stop 'initialized variable multiply initializer AST changed'
        end if
        if (trim(unit%execution_part%sequence%assignment(2)%expression%kind) /= &
            'binary-expression' .or. trim(unit%execution_part%sequence%assignment(2)%expression%operator) /= '*' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%left_operand) /= 'x' .or. &
            trim(unit%execution_part%sequence%assignment(2)%expression%right_operand) /= 'x') then
            error stop 'initialized variable multiply expression AST changed'
        end if
        if (trim(unit%execution_part%print%output_name) /= 'x') then
            error stop 'initialized variable multiply printed name changed'
        end if

        call assert_provenance(unit%root%span%source_hash, 'root')
        call assert_provenance(unit%declaration%span%source_hash, 'declaration')
        call assert_provenance(unit%variable%span%source_hash, 'variable')
        call assert_provenance(unit%execution_part%sequence%assignment(1)%span%source_hash, &
            'initializer assignment')
        call assert_provenance(unit%execution_part%sequence%assignment(2)%span%source_hash, &
            'multiply assignment')
        call assert_provenance(unit%execution_part%print%span%source_hash, 'print')
        if (unit%execution_part%sequence%assignment(2)%span%end_byte <= &
            unit%execution_part%sequence%assignment(2)%span%start_byte) then
            error stop 'initialized variable multiply assignment span changed'
        end if

        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok) error stop 'initialized variable multiply SX serialization failed'
        if (index(trim(serialized), '(assignment-count 2)') == 0 .or. &
            index(trim(serialized), '(operator *)') == 0 .or. &
            index(trim(serialized), '(left-operand x)') == 0 .or. &
            index(trim(serialized), '(right-operand x)') == 0 .or. &
            index(trim(serialized), '(output-name x)') == 0 .or. &
            index(trim(serialized), '(source-hash '//source_hash//')') == 0) then
            error stop 'initialized variable multiply SX serialization changed'
        end if
    end subroutine assert_accepted

    subroutine assert_rejected(initializer, assignment, printed_name)
        character(len=*), intent(in) :: initializer, assignment, printed_name
        character(len=1024) :: source

        source = 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
            '  x = '//trim(initializer)//new_line('a')//'  '//trim(assignment)//new_line('a')// &
            '  print *, '//trim(printed_name)//new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-variable-multiply-negative.f90', trim(source), &
            'initialized-variable-multiply-negative-input', unit, ok, message)
        if (ok) error stop 'invalid initialized variable multiply was accepted'
    end subroutine assert_rejected

    subroutine assert_provenance(source_hash, label)
        character(len=*), intent(in) :: source_hash, label

        if (trim(source_hash) /= 'initialized-variable-multiply-input') then
            error stop 'initialized variable multiply provenance changed: '//trim(label)
        end if
    end subroutine assert_provenance

end program test_frontend_program_unit_v2_initialized_variable_multiply
