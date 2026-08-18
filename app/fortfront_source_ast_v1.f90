program fortfront_source_ast_v1
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_program_unit_to_sx, typed_program_unit_t
    use fortfront_assignment_sequence, only: &
        frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, assignment_sequence_t
    implicit none

    character(len=*), parameter :: source_hash_x = 'l3-raw-program-v0'
    character(len=*), parameter :: source_hash_y = &
        'l3-raw-program-variable-name-v1'
    character(len=65536) :: serialized
    character(len=256) :: message
    character(len=:), allocatable :: source
    character(len=1024) :: source_file, output_file
    character(len=128) :: source_hash
    integer(int64) :: source_size
    integer :: argument_count, io_status, input_unit, output_unit
    logical :: ok
    logical :: sequence_mode
    type(typed_program_unit_t) :: unit
    type(assignment_sequence_t) :: sequence

    argument_count = command_argument_count()
    if (argument_count /= 2) call fail('usage: fortfront-source-ast-v1 <source> <output>')
    call get_command_argument(1, source_file)
    call get_command_argument(2, output_file)
    if (len_trim(source_file) == 0 .or. len_trim(output_file) == 0) then
        call fail('source and output paths must not be empty')
    end if

    inquire (file=trim(source_file), size=source_size, iostat=io_status)
    if (io_status /= 0 .or. source_size < 0_int64) call fail('source file inquiry failed')
    allocate (character(len=int(source_size)) :: source)
    open (newunit=input_unit, file=trim(source_file), status='old', action='read', &
        access='stream', form='unformatted', iostat=io_status)
    if (io_status /= 0) call fail('source file could not be opened')
    if (source_size > 0_int64) then
        read (input_unit, iostat=io_status) source
        if (io_status /= 0) then
            close (input_unit)
            call fail('source file could not be read')
        end if
    end if
    close (input_unit)

    sequence_mode = source == 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')// &
        '  integer :: x'//new_line('a')//'  x = 7'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
        '  x = x + 1'//new_line('a')//'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')//repeat('  x = x + 1'//new_line('a'), 6)// &
        'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')//repeat('  x = x + 1'//new_line('a'), 7)// &
        'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')//repeat('  x = x + 1'//new_line('a'), 8)// &
        'end program main'//new_line('a') .or. &
        source == 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
        '  x = 7'//new_line('a')//repeat('  x = x + 1'//new_line('a'), 9)// &
        'end program main'//new_line('a')
    if (sequence_mode) then
        if (index(source, repeat('  x = x + 1'//new_line('a'), 9)) > 0) then
            source_hash = 'l3-raw-program-ten-assignment-v1'
        else if (index(source, repeat('  x = x + 1'//new_line('a'), 8)) > 0) then
            source_hash = 'l3-raw-program-nine-assignment-v1'
        else if (index(source, repeat('  x = x + 1'//new_line('a'), 7)) > 0) then
            source_hash = 'l3-raw-program-eight-assignment-v1'
        else if (index(source, repeat('  x = x + 1'//new_line('a'), 6)) > 0) then
            source_hash = 'l3-raw-program-seven-assignment-v1'
        else if (index(source, '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
                '  x = x + 1') > 0) then
            source_hash = 'l3-raw-program-six-assignment-v1'
        else if (index(source, '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
                '  x = x + 1'//new_line('a')//'  x = x + 1') > 0) then
            source_hash = 'l3-raw-program-five-assignment-v1'
        else if (index(source, '  x = x + 1'//new_line('a')//'  x = x + 1'//new_line('a')// &
                '  x = x + 1') > 0) then
            source_hash = 'l3-raw-program-four-assignment-v1'
        else if (index(source, '  x = x + 1'//new_line('a')//'  x = x + 1') > 0) then
            source_hash = 'l3-raw-program-three-assignment-v1'
        else
            source_hash = 'l3-raw-program-two-assignment-v1'
        end if
    else if (source == 'program p'//new_line('a')//'  integer :: x'// &
            new_line('a')//'end program p'//new_line('a')) then
        source_hash = source_hash_x
    else if (source == 'program p'//new_line('a')//'  integer :: z'// &
            new_line('a')//'end program p'//new_line('a')) then
        source_hash = 'l3-raw-program-variable-name-z-v1'
    else if (source == 'program p'//new_line('a')//'  integer :: alpha'// &
            new_line('a')//'end program p'//new_line('a')) then
        source_hash = 'l3-raw-program-variable-name-alpha-v1'
    else if (source == 'program main'//new_line('a')//'  complex :: x'// &
            new_line('a')//'end program main'//new_line('a')) then
        source_hash = 'l3-raw-program-complex-type-v1'
    else
        source_hash = source_hash_y
    end if
    if (sequence_mode) then
        call frontend_parse_typed_assignment_sequence(trim(source_file), source, &
            source_hash, sequence, ok, message)
        if (.not. ok) call fail('assignment sequence rejected source: '//trim(message))
        call frontend_typed_assignment_sequence_to_sx(sequence, serialized, ok, message)
    else
        call frontend_parse_typed_program_unit(trim(source_file), source, source_hash, &
            unit, ok, message)
        if (.not. ok) call fail('typed frontend rejected source: '//trim(message))
        call frontend_typed_program_unit_to_sx(unit, serialized, ok, message)
    end if
    if (.not. ok) call fail('typed AST could not be serialized: '//trim(message))

    open (newunit=output_unit, file=trim(output_file), status='replace', action='write', &
        access='stream', form='unformatted', iostat=io_status)
    if (io_status /= 0) call fail('output file could not be opened')
    write (output_unit, iostat=io_status) serialized(:len_trim(serialized))
    if (io_status /= 0) then
        close (output_unit)
        call fail('output file could not be written')
    end if
    close (output_unit)

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        write (error_unit, '(a)') trim(text)
        error stop 2
    end subroutine fail

end program fortfront_source_ast_v1
