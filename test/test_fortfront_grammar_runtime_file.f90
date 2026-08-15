program test_fortfront_grammar_runtime_file
    use fortfront_grammar_frontier, only: fortfront_grammar_frontier_result_t
    use fortfront_grammar_runtime, only: fortfront_grammar_runtime_accepted, &
        fortfront_grammar_runtime_ambiguous, &
        fortfront_grammar_runtime_initialized, &
        fortfront_grammar_runtime_load_file, fortfront_grammar_runtime_malformed, &
        fortfront_grammar_runtime_push, fortfront_grammar_runtime_t
    implicit none

    character(len=*), parameter :: path = ".test-standardir-grammar-v0.sx"
    character(len=*), parameter :: rule_a = &
        "(syntax-rule (id A) (alternative 1) (lhs root) (root 1) "// &
        "(nodes (grammar-nodes (grammar-node token x 1 false 0 0))) "// &
        "(source (source-ref (document test) (clause c) (rule A) (page 1) "// &
        "(source-hash hash-a))) (origin mechanical) (resolution resolved))"
    character(len=*), parameter :: rule_b = &
        "(syntax-rule (id B) (alternative 1) (lhs root) (root 1) "// &
        "(nodes (grammar-nodes (grammar-node token x 1 false 0 0))) "// &
        "(source (source-ref (document test) (clause c) (rule B) (page 1) "// &
        "(source-hash hash-b))) (origin mechanical) (resolution resolved))"
    type(fortfront_grammar_runtime_t) :: runtime
    type(fortfront_grammar_frontier_result_t) :: output(4)
    character(len=256) :: message
    integer :: line_count, output_count, rule_count, status

    call write_file([rule_a, rule_b])
    call fortfront_grammar_runtime_load_file(runtime, path, "root", rule_count, line_count, &
        status, message)
    call require(status == fortfront_grammar_runtime_initialized .and. rule_count == 2 .and. &
        line_count == 2, "multi-rule file was not loaded with both counts")
    call fortfront_grammar_runtime_push(runtime, "x", output, output_count, status, message)
    call require(status == fortfront_grammar_runtime_ambiguous .and. output_count == 2, &
        "multi-rule file did not preserve ambiguity")

    call write_file([rule_a, rule_a])
    call fortfront_grammar_runtime_load_file(runtime, path, "root", rule_count, line_count, &
        status, message)
    call require(status == fortfront_grammar_runtime_initialized .and. rule_count == 2 .and. &
        line_count == 2, "duplicate source occurrences were not loaded")
    call fortfront_grammar_runtime_push(runtime, "x", output, output_count, status, message)
    call require(status == fortfront_grammar_runtime_accepted .and. output_count == 1, &
        "identical source occurrences were not normalized")

    call write_file([rule_a, "(syntax-rule"])
    call fortfront_grammar_runtime_load_file(runtime, path, "root", rule_count, line_count, &
        status, message)
    call require(status == fortfront_grammar_runtime_malformed .and. rule_count == 1 .and. &
        line_count == 2, "malformed line did not report its line and rule counts")
    call fortfront_grammar_runtime_push(runtime, "x", output, output_count, status, message)
    call require(status == fortfront_grammar_runtime_malformed .and. output_count == 0, &
        "failed file load did not transactionally clear the runtime")

    call delete_file()
    print "(a)", "fortfront grammar runtime file behavioral checks: ok"

contains

    subroutine write_file(lines)
        character(len=*), intent(in) :: lines(:)
        integer :: i, unit

        open (newunit=unit, file=path, status="replace", action="write")
        do i = 1, size(lines)
            write (unit, "(a)") trim(lines(i))
        end do
        close (unit)
    end subroutine write_file

    subroutine delete_file()
        integer :: unit

        open (newunit=unit, file=path, status="old")
        close (unit, status="delete")
    end subroutine delete_file

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop failure
    end subroutine require

end program test_fortfront_grammar_runtime_file
