program fortfront_grammar_runtime_cli
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_grammar_runtime, only: fortfront_grammar_runtime_finalize, &
        fortfront_grammar_runtime_initialized, fortfront_grammar_runtime_load_file, &
        fortfront_grammar_runtime_malformed, fortfront_grammar_runtime_push, &
        fortfront_grammar_runtime_status_name, fortfront_grammar_runtime_t
    implicit none

    type(fortfront_grammar_runtime_t) :: runtime
    type(fortfront_grammar_frontier_result_t), allocatable :: output(:)
    character(len=1024) :: path, start_lhs, token
    character(len=256) :: message
    integer :: argument_count, i, line_count, rule_count, output_count, status

    argument_count = command_argument_count()
    if (argument_count < 2) then
        write (*, '(a)') 'usage: fortfront-grammar-runtime <grammar-file> <start-lhs> [token ...]'
        stop 2
    end if
    call get_command_argument(1, path)
    call get_command_argument(2, start_lhs)
    call fortfront_grammar_runtime_load_file(runtime, trim(path), trim(start_lhs), rule_count, &
        line_count, status, message)
    write (*, '("rules=",i0)') rule_count
    write (*, '("lines=",i0)') line_count
    if (status /= fortfront_grammar_runtime_initialized) then
        write (*, '("outcome=",a)') trim(fortfront_grammar_runtime_status_name(status))
        write (*, '("message=",a)') trim(message)
        stop 1
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
end program fortfront_grammar_runtime_cli
