program test_standardir_syntax_item
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: frontend_parse, frontend_rejected, &
        frontend_result_t, standardir_syntax_item_from_sx, standardir_syntax_item_t, &
        standardir_syntax_item_to_sx, standardir_syntax_item_validate
    implicit none

    character(len=*), parameter :: resolved_sx = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: unresolved_sx = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture))) (origin mechanical) (resolution unresolved))'
    character(len=*), parameter :: disputed_sx = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture))) (origin mechanical) (resolution disputed))'
    type(standardir_syntax_item_t) :: item, parsed
    character(len=2048) :: serialized
    character(len=128) :: message
    logical :: ok

    call assert_round_trip(resolved_sx, 'resolved')
    call assert_round_trip(unresolved_sx, 'unresolved')
    call assert_round_trip(disputed_sx, 'disputed')

    call standardir_syntax_item_from_sx(resolved_sx, parsed, ok, message, 'fixture')
    call assert_true(ok, 'matching source provenance was rejected')
    call standardir_syntax_item_from_sx(resolved_sx, parsed, ok, message, 'tampered')
    call assert_true(.not. ok, 'tampered source provenance was accepted')
    call assert_equal(message, 'syntax-item-source-hash-mismatch', &
        'tampered source provenance reported the wrong failure')
    call standardir_syntax_item_from_sx(resolved_sx, parsed, ok, message, '')
    call assert_true(.not. ok, 'empty expected provenance was accepted')
    call assert_equal(message, 'missing-expected-source-hash', &
        'empty expected provenance reported the wrong failure')

    call assert_invalid_sx('(syntax-item (id R501) (lhs program) (source '// &
        '(source-ref (document J3-24-007) (clause 1) (rule R501) (page 0) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved))', &
        'invalid-syntax-item-provenance')
    call assert_invalid_sx(resolved_sx//' trailing', 'malformed-syntax-item')
    call assert_invalid_sx('(syntax-item (id R501) (lhs program) (source '// &
        '(source-ref (document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved) '// &
        '(extra x))', 'malformed-syntax-item')
    call assert_invalid_sx('(syntax-item (id R501) (lhs program) (source '// &
        '(source-ref (document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture)) (origin mechanical) (resolution resolved))', &
        'malformed-syntax-item-source')

    call standardir_syntax_item_from_sx(resolved_sx, item, ok, message)
    call assert_true(ok, 'resolved syntax item could not be read')
    item%source%page = 0_int64
    call standardir_syntax_item_to_sx(item, serialized, ok, message)
    call assert_true(.not. ok, 'invalid syntax provenance was serialized')
    call assert_equal(message, 'invalid-syntax-item-provenance', &
        'invalid syntax provenance reported the wrong failure')

    call assert_parser_rejects(unresolved_sx, 'unresolved')
    call assert_parser_rejects(disputed_sx, 'disputed')

    write (*, '(a)') 'standardir syntax-item SX behavioral checks: ok'

contains

    subroutine assert_round_trip(expected, expected_resolution)
        character(len=*), intent(in) :: expected, expected_resolution

        character(len=2048) :: reread
        character(len=128) :: local_message
        type(standardir_syntax_item_t) :: value
        logical :: local_ok

        call standardir_syntax_item_from_sx(expected, value, local_ok, local_message)
        call assert_true(local_ok, 'canonical syntax item SX was rejected')
        call assert_equal(value%id, 'R501', 'reader lost syntax item id')
        call assert_equal(value%lhs, 'program', 'reader lost syntax item lhs')
        call assert_equal(value%source%document, 'J3-24-007', &
            'reader lost source document')
        call assert_equal(value%source%clause, '1', 'reader lost source clause')
        call assert_equal(value%source%rule, 'R501', 'reader lost source rule')
        call assert_equal_integer(value%source%page, 45_int64, &
            'reader lost source page')
        call assert_equal(value%source%source_hash, 'fixture', &
            'reader lost source hash')
        call assert_equal(value%origin, 'mechanical', 'reader lost origin')
        call assert_equal(value%resolution, expected_resolution, &
            'reader lost resolution')
        call assert_true(standardir_syntax_item_validate(value, local_message), &
            'reader produced an invalid syntax item')
        call standardir_syntax_item_to_sx(value, reread, local_ok, local_message)
        call assert_true(local_ok, 'reread syntax item failed SX writing')
        call assert_equal(trim(reread), trim(expected), &
            'syntax item SX did not round-trip through canonical oracle')
    end subroutine assert_round_trip

    subroutine assert_invalid_sx(value, expected_message)
        character(len=*), intent(in) :: value, expected_message

        character(len=128) :: local_message
        type(standardir_syntax_item_t) :: local_item
        logical :: local_ok

        call standardir_syntax_item_from_sx(value, local_item, local_ok, local_message)
        call assert_true(.not. local_ok, 'invalid syntax item SX was accepted')
        call assert_equal(local_message, expected_message, &
            'invalid syntax item SX reported the wrong failure')
    end subroutine assert_invalid_sx

    subroutine assert_parser_rejects(value, expected_resolution)
        character(len=*), intent(in) :: value, expected_resolution

        character(len=128) :: local_message
        type(frontend_result_t) :: result
        type(standardir_syntax_item_t) :: local_item
        logical :: local_ok

        call standardir_syntax_item_from_sx(value, local_item, local_ok, local_message)
        call assert_true(local_ok, 'non-resolved syntax item could not be read')
        call frontend_parse('unit.f90', 'program unit'//new_line('a')//'end', &
            'fixture', local_item, result)
        call assert_equal(result%status, frontend_rejected, &
            'non-resolved syntax item was accepted by the parser')
        call assert_equal(result%diagnostics(1)%message, 'unresolved-syntax', &
            'non-'//trim(expected_resolution)//' syntax item changed parser rejection')
    end subroutine assert_parser_rejects

    subroutine assert_equal(actual, expected, failure)
        character(len=*), intent(in) :: actual, expected, failure

        if (trim(actual) /= trim(expected)) error stop failure
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, failure)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: failure

        if (actual /= expected) error stop failure
    end subroutine assert_equal_integer

    subroutine assert_true(value, failure)
        logical, intent(in) :: value
        character(len=*), intent(in) :: failure

        if (.not. value) error stop failure
    end subroutine assert_true

end program test_standardir_syntax_item
