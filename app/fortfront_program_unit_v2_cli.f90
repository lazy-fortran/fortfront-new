program fortfront_program_unit_v2_cli
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fortfront_program_unit_v2, only: frontend_parse_program_unit_v2, &
        frontend_program_unit_v2_to_sx, program_unit_v2_t
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-v2'
    character(len=65536) :: serialized
    character(len=256) :: message
    character(len=:), allocatable :: source
    character(len=1024) :: source_file, output_file
    integer(int64) :: source_size
    integer :: argument_count, io_status, input_unit, output_unit
    logical :: ok
    type(program_unit_v2_t) :: unit

    argument_count = command_argument_count()
    if (argument_count /= 2) then
        call fail('usage: fortfront-program-unit-v2 <source> <output>')
    end if
    call get_command_argument(1, source_file)
    call get_command_argument(2, output_file)
    if (len_trim(source_file) == 0 .or. len_trim(output_file) == 0) then
        call fail('source and output paths must not be empty')
    end if

    inquire (file=trim(source_file), size=source_size, iostat=io_status)
    if (io_status /= 0 .or. source_size < 0_int64) then
        call fail('source file inquiry failed')
    end if
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

    call frontend_parse_program_unit_v2(trim(source_file), source, source_hash, unit, ok, &
        message)
    if (.not. ok) call fail('program-unit-v2 rejected source: '//trim(message))
    call frontend_program_unit_v2_to_sx(unit, serialized, ok, message)
    if (.not. ok) call fail('program-unit-v2 could not be serialized: '//trim(message))

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

end program fortfront_program_unit_v2_cli
