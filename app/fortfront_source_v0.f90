program fortfront_source_v0
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fortfront_frontend, only: frontend_parse, frontend_result_t, &
        frontend_result_to_sx, standardir_syntax_item_t
    implicit none

    character(len=*), parameter :: source_hash = 'l3-raw-program-v0'
    character(len=32768) :: serialized
    character(len=256) :: message
    character(len=:), allocatable :: source
    character(len=1024) :: source_file, output_file
    integer(int64) :: source_size
    integer :: argument_count, io_status, input_unit, output_unit
    logical :: ok
    type(frontend_result_t) :: result
    type(standardir_syntax_item_t) :: witness

    argument_count = command_argument_count()
    if (argument_count /= 2) call fail('usage: fortfront-source-v0 <source> <output>')
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

    call set_r501_program_witness(witness)
    call frontend_parse(trim(source_file), source, source_hash, witness, result)
    call frontend_result_to_sx(result, serialized, ok, message)
    if (.not. ok) call fail('frontend result could not be serialized: '//trim(message))

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

    subroutine set_r501_program_witness(value)
        type(standardir_syntax_item_t), intent(out) :: value

        value%id = 'R501'
        value%lhs = 'program'
        value%origin = 'mechanical'
        value%resolution = 'resolved'
        value%source%document = 'J3-24-007'
        value%source%clause = '5'
        value%source%rule = 'R501'
        value%source%page = 53_int64
        value%source%source_hash = &
            '1cf538329c57e4f617adb36f2c7cd91a5a5561c78bcce16ec96f7ff1a9979f9'
    end subroutine set_r501_program_witness

    subroutine fail(text)
        character(len=*), intent(in) :: text

        write (error_unit, '(a)') trim(text)
        error stop 2
    end subroutine fail

end program fortfront_source_v0
