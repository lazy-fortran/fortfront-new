program fortfront_grammar_runtime_cli
    use, intrinsic :: iso_fortran_env, only: error_unit, iostat_end
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_grammar_runtime, only: fortfront_grammar_runtime_finalize, &
        fortfront_grammar_runtime_initialized, fortfront_grammar_runtime_load_file, &
        fortfront_grammar_runtime_malformed, fortfront_grammar_runtime_push, &
        fortfront_grammar_runtime_status_name, fortfront_grammar_runtime_t
    implicit none

    integer, parameter :: case_line_capacity = 262144
    type(fortfront_grammar_runtime_t) :: runtime
    type(fortfront_grammar_frontier_result_t), allocatable :: output(:)
    character(len=1024) :: path, start_lhs, token, case_file
    character(len=256) :: message
    integer :: argument_count, i, line_count, rule_count, output_count, status
    logical :: batch_mode

    argument_count = command_argument_count()
    if (argument_count < 2) then
        call print_usage()
        stop 2
    end if
    batch_mode = .false.
    if (argument_count == 4) then
        call get_command_argument(3, token)
        if (trim(token) == '--case-file') then
            batch_mode = .true.
            call get_command_argument(4, case_file)
        end if
    end if
    call get_command_argument(1, path)
    call get_command_argument(2, start_lhs)
    call fortfront_grammar_runtime_load_file(runtime, trim(path), trim(start_lhs), rule_count, &
        line_count, status, message)
    if (.not. batch_mode) then
        write (*, '("rules=",i0)') rule_count
        write (*, '("lines=",i0)') line_count
    end if
    if (status /= fortfront_grammar_runtime_initialized) then
        if (batch_mode) then
            write (error_unit, '("outcome=",a)') &
                trim(fortfront_grammar_runtime_status_name(status))
            write (error_unit, '("message=",a)') trim(message)
        else
            write (*, '("outcome=",a)') trim(fortfront_grammar_runtime_status_name(status))
            write (*, '("message=",a)') trim(message)
        end if
        stop 1
    end if

    if (batch_mode) then
        call run_case_file(runtime, trim(case_file), max(1, rule_count), status, message)
        if (status /= fortfront_grammar_runtime_initialized) then
            write (error_unit, '("outcome=",a)') &
                trim(fortfront_grammar_runtime_status_name(status))
            write (error_unit, '("message=",a)') trim(message)
            stop 1
        end if
        stop
    end if

    allocate(output(max(1, rule_count)))
    do i = 3, argument_count
        call get_command_argument(i, token)
        call fortfront_grammar_runtime_push(runtime, trim(token), output, output_count, status, &
            message)
        if (status == fortfront_grammar_runtime_malformed) then
            write (*, '("outcome=",a)') trim(fortfront_grammar_runtime_status_name(status))
            write (*, '("message=",a)') trim(message)
            stop 1
        end if
    end do
    call fortfront_grammar_runtime_finalize(runtime, output, output_count, status, message)
    write (*, '("outcome=",a)') trim(fortfront_grammar_runtime_status_name(status))
    write (*, '("message=",a)') trim(message)

contains

    subroutine print_usage()
        write (*, '(a)') 'usage: fortfront-grammar-runtime <grammar-file> <start-lhs> [token ...]'
        write (*, '(a)') '       fortfront-grammar-runtime <grammar-file> <start-lhs> '// &
            '--case-file <tsv>'
    end subroutine print_usage

    subroutine run_case_file(template, path, output_capacity, status, message)
        type(fortfront_grammar_runtime_t), intent(in) :: template
        character(len=*), intent(in) :: path
        integer, intent(in) :: output_capacity
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        type(fortfront_grammar_runtime_t) :: case_runtime
        type(fortfront_grammar_frontier_result_t), allocatable :: case_output(:)
        character(len=case_line_capacity) :: line
        character(len=256) :: io_message
        integer :: unit, io_status, line_number

        status = fortfront_grammar_runtime_malformed
        message = ''
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=io_status, &
            iomsg=io_message)
        if (io_status /= 0) then
            message = 'grammar-runtime-case-file-open-failed: '//trim(io_message)
            return
        end if
        allocate(case_output(output_capacity))
        line_number = 0
        do
            read (unit, '(A)', iostat=io_status, iomsg=io_message) line
            if (io_status == iostat_end) exit
            if (io_status /= 0) then
                close (unit)
                message = 'grammar-runtime-case-file-read-failed: '//trim(io_message)
                return
            end if
            line_number = line_number + 1
            case_runtime = template
            call run_case_line(case_runtime, case_output, line, line_number)
        end do
        close (unit)
        status = fortfront_grammar_runtime_initialized
        message = 'grammar-runtime-case-file-is-consumed'
    end subroutine run_case_file

    subroutine run_case_line(runtime, output, line, line_number)
        type(fortfront_grammar_runtime_t), intent(inout) :: runtime
        type(fortfront_grammar_frontier_result_t), intent(out) :: output(:)
        character(len=*), intent(in) :: line
        integer, intent(in) :: line_number

        character(len=256) :: runtime_message
        integer :: first_tab, next_tab, field_end, line_end, position, status, output_count
        logical :: malformed

        line_end = len_trim(line)
        first_tab = index(line, achar(9))
        if (first_tab == 0) then
            if (line_end == 0) then
                call emit_case('', fortfront_grammar_runtime_malformed, &
                    'grammar-runtime-case-line-'//trim(integer_text(line_number))// &
                    '-has-empty-case-id')
            else
                call fortfront_grammar_runtime_finalize(runtime, output, output_count, status, &
                    runtime_message)
                call emit_case(line(1:line_end), status, runtime_message)
            end if
            return
        end if
        if (first_tab == 1) then
            call emit_case('', fortfront_grammar_runtime_malformed, &
                'grammar-runtime-case-line-'//trim(integer_text(line_number))// &
                '-has-empty-case-id')
            return
        end if

        malformed = .false.
        position = first_tab + 1
        do
            if (position > line_end) then
                malformed = .true.
                runtime_message = 'grammar-runtime-case-line-'//trim(integer_text(line_number))// &
                    '-has-empty-token'
                exit
            end if
            next_tab = index(line(position:line_end), achar(9))
            if (next_tab == 0) then
                field_end = line_end
            else
                field_end = position + next_tab - 2
            end if
            if (field_end < position .or. len_trim(line(position:field_end)) == 0) then
                malformed = .true.
                runtime_message = 'grammar-runtime-case-line-'//trim(integer_text(line_number))// &
                    '-has-empty-token'
                exit
            end if
            call fortfront_grammar_runtime_push(runtime, line(position:field_end), output, &
                output_count, status, runtime_message)
            if (status == fortfront_grammar_runtime_malformed) then
                malformed = .true.
                exit
            end if
            if (next_tab == 0) exit
            position = field_end + 2
        end do
        if (malformed) then
            call emit_case(line(1:first_tab - 1), fortfront_grammar_runtime_malformed, &
                runtime_message)
            return
        end if
        call fortfront_grammar_runtime_finalize(runtime, output, output_count, status, &
            runtime_message)
        call emit_case(line(1:first_tab - 1), status, runtime_message)
    end subroutine run_case_line

    subroutine emit_case(case_id, status, message)
        character(len=*), intent(in) :: case_id, message
        integer, intent(in) :: status

        write (*, '(a,a,a,a,a)') trim(case_id), achar(9), &
            trim(fortfront_grammar_runtime_status_name(status)), achar(9), trim(message)
    end subroutine emit_case

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function integer_text

end program fortfront_grammar_runtime_cli
