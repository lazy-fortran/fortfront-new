program test_frontend_program_unit_v2_y_initializer
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    type(program_unit_v2_t) :: unit
    character(len=65536) :: serialized
    character(len=256) :: message
    logical :: ok

    call assert_accepted('3')
    call assert_accepted('-4')

    call assert_rejected('  integer :: x', 'y = 3', 'y')
    call assert_rejected('  integer :: y', 'x = 3', 'x')
    call assert_rejected('  real :: y', 'y = 3', 'y')
    call assert_rejected('  integer :: y', 'y = 2', 'y')
    call assert_rejected('  integer :: y', 'y = -5', 'y')

    write (*, '(a)') 'frontend program-unit-v2 initialized variable-y initializer checks: ok'

contains

    subroutine assert_accepted(initializer)
        character(len=*), intent(in) :: initializer
        character(len=1024) :: source
        character(len=*), parameter :: source_file = &
            'initialized-variable-y-initializer.f90'
        character(len=*), parameter :: source_hash = &
            'initialized-variable-y-initializer-input'
        integer(int64) :: assignment_start, assignment_end, print_start, print_end
        integer(int64) :: expected_value

        source = 'program main'//new_line('a')//'  integer :: y'//new_line('a')// &
            '  y = '//trim(initializer)//new_line('a')//'  print *, y'// &
            new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2(source_file, trim(source), source_hash, &
            unit, ok, message)
        read (initializer, *) expected_value
        if (.not. ok .or. unit%declaration_count /= 1_int64 .or. &
            unit%variable_count /= 1_int64 .or. unit%execution_part%sequence%assignment_count /= &
            1_int64 .or. unit%execution_part%print_count /= 1_int64 .or. &
            trim(unit%root%name) /= 'main' .or. trim(unit%declaration%name) /= 'main' .or. &
            trim(unit%variable%name) /= 'y' .or. trim(unit%variable%type_spec) /= 'integer' .or. &
            trim(unit%execution_part%sequence%assignment(1)%variable) /= 'y' .or. &
            trim(unit%execution_part%sequence%assignment(1)%expression%kind) /= &
            'integer-literal' .or. trim(unit%execution_part%sequence%assignment(1)%expression%left_operand) /= &
            trim(initializer) .or. unit%execution_part%print%output_name /= 'y' .or. &
            unit%execution_part%print%output_value /= expected_value) then
            error stop 'valid initialized variable y initializer changed'
        end if

        assignment_start = int(index(trim(source), '  y = '//trim(initializer)) - 1, int64)
        assignment_end = assignment_start + int(len('  y = ') + len_trim(initializer) - 1, int64)
        print_start = int(index(trim(source), '  print *, y') - 1, int64)
        print_end = print_start + int(len('  print *, y') - 1, int64)
        call assert_span(unit%root%span%file, unit%root%span%source_hash, &
            unit%root%span%start_byte, unit%root%span%end_byte, 'root', source_file, &
            source_hash, 0_int64, int(len_trim(source) - 1, int64))
        call assert_span(unit%declaration%span%file, unit%declaration%span%source_hash, &
            unit%declaration%span%start_byte, unit%declaration%span%end_byte, 'declaration', &
            source_file, source_hash, 0_int64, int(index(trim(source), new_line('a')) - 1, int64))
        call assert_span(unit%variable%span%file, unit%variable%span%source_hash, &
            unit%variable%span%start_byte, unit%variable%span%end_byte, 'variable', source_file, &
            source_hash, int(index(trim(source), '  integer :: y') - 1, int64), &
            int(index(trim(source), '  integer :: y') + len('  integer :: y') - 2, int64))
        call assert_span(unit%execution_part%sequence%assignment(1)%span%file, &
            unit%execution_part%sequence%assignment(1)%span%source_hash, &
            unit%execution_part%sequence%assignment(1)%span%start_byte, &
            unit%execution_part%sequence%assignment(1)%span%end_byte, 'assignment', source_file, &
            source_hash, assignment_start, assignment_end)
        call assert_span(unit%execution_part%print%span%file, &
            unit%execution_part%print%span%source_hash, unit%execution_part%print%span%start_byte, &
            unit%execution_part%print%span%end_byte, 'print', source_file, source_hash, &
            print_start, print_end)

        call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
        if (.not. ok .or. index(trim(serialized), '(name y)') == 0 .or. &
            index(trim(serialized), '(left-operand '//trim(initializer)//')') == 0 .or. &
            index(trim(serialized), '(output-name y)') == 0 .or. &
            index(trim(serialized), '(source-hash '//source_hash//')') == 0) then
            error stop 'initialized variable y initializer serialization changed'
        end if
    end subroutine assert_accepted

    subroutine assert_rejected(declaration, assignment, printed_name)
        character(len=*), intent(in) :: declaration, assignment, printed_name
        character(len=1024) :: source

        source = 'program main'//new_line('a')//trim(declaration)//new_line('a')// &
            '  '//trim(assignment)//new_line('a')//'  print *, '//trim(printed_name)// &
            new_line('a')//'end program main'//new_line('a')
        call frontend_parse_program_unit_v2('initialized-variable-y-initializer-negative.f90', &
            trim(source), 'initialized-variable-y-initializer-negative-input', unit, ok, message)
        if (ok) error stop 'invalid initialized variable y initializer was accepted'
    end subroutine assert_rejected

    subroutine assert_span(file_name, source_hash, start_byte, end_byte, label, expected_file, &
            expected_hash, expected_start, expected_end)
        character(len=*), intent(in) :: file_name, source_hash, label
        character(len=*), intent(in) :: expected_file, expected_hash
        integer(int64), intent(in) :: start_byte, end_byte, expected_start, expected_end

        if (trim(file_name) /= trim(expected_file) .or. trim(source_hash) /= trim(expected_hash) .or. &
            start_byte /= expected_start .or. end_byte /= expected_end) then
            error stop 'initialized variable y initializer span changed: '//trim(label)
        end if
    end subroutine assert_span

end program test_frontend_program_unit_v2_y_initializer
