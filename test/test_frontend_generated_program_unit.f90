program test_frontend_generated_program_unit
    use, intrinsic :: iso_fortran_env, only: int64
    use fortfront_frontend, only: declaration_kind_module, declaration_kind_program, &
        frontend_generated_program_unit_to_sx, &
        frontend_parse_generated_program_unit, &
        frontend_validate_generated_program_unit, generated_program_unit_t, &
        standardir_syntax_item_t
    implicit none

    type(generated_program_unit_t) :: unit
    type(standardir_syntax_item_t) :: syntax_item
    character(len=16384) :: serialized
    character(len=128) :: message
    logical :: ok

    call set_program_witness(syntax_item)
    call frontend_parse_generated_program_unit('unit.f90', &
        'program unit'//new_line('a')//'end', 'hash-positive', syntax_item, &
        unit, ok, message)
    call assert_true(ok, 'minimal program witness was rejected')
    call assert_equal(message, '', 'accepted witness returned a diagnostic')
    call assert_true(frontend_validate_generated_program_unit(unit, message), &
        'generated program unit was not valid')
    call assert_equal(unit%root%name, 'unit', 'generated root name changed')
    call assert_equal(unit%root%span%file, 'unit.f90', &
        'generated root source file changed')
    call assert_equal(unit%root%span%source_hash, 'hash-positive', &
        'generated root source hash changed')
    call assert_equal_integer(unit%root%span%start_byte, 0_int64, &
        'generated root start span changed')
    call assert_equal_integer(unit%root%span%end_byte, 16_int64, &
        'generated root end span changed')
    call assert_equal_integer(unit%declaration_count, 1_int64, &
        'generated declaration count changed')
    call assert_equal(unit%declaration%declaration_kind, declaration_kind_program, &
        'generated declaration kind changed')
    call assert_equal(unit%declaration%name, 'unit', &
        'generated declaration name changed')
    call assert_equal(unit%declaration%span%file, 'unit.f90', &
        'generated declaration source file changed')
    call assert_equal(unit%declaration%span%source_hash, 'hash-positive', &
        'generated declaration source hash changed')
    call assert_equal_integer(unit%declaration%span%start_byte, 0_int64, &
        'generated declaration start span changed')
    call assert_equal_integer(unit%declaration%span%end_byte, 12_int64, &
        'generated declaration end span changed')
    call frontend_generated_program_unit_to_sx(unit, serialized, ok, message)
    call assert_true(ok, 'generated program unit failed SX serialization')
    call assert_equal(trim(serialized), &
        '(program-unit (root (program-root (name unit) (span '// &
        '(source-span (file unit.f90) (start-byte 0) (end-byte 16) '// &
        '(source-hash hash-positive))))) (declaration-count 1) '// &
        '(declaration (program-declaration (declaration-kind program) '// &
        '(name unit) (span (source-span (file unit.f90) (start-byte 0) '// &
        '(end-byte 12) (source-hash hash-positive))))))', &
        'generated program-unit SX oracle changed')

    call assert_accepted_source('program unit'//new_line('a')//'end program', &
        'unit')
    call assert_accepted_source('program unit'//new_line('a')//'end program unit', &
        'unit')
    call assert_accepted_source('program Unit'//new_line('a')//'END PROGRAM UNIT', &
        'Unit')
    call assert_accepted_spans('  program unit_2 '//new_line('a')// &
        ' end program unit_2 ', 'unit_2', 2_int64, 37_int64, 2_int64, 16_int64)

    syntax_item%lhs = 'module'
    call assert_accepted_module_source('module unit'//new_line('a')//'end', 'unit')
    call assert_accepted_module_source('module unit'//new_line('a')//'end module', &
        'unit')
    call assert_accepted_module_source('module Unit'//new_line('a')// &
        'END MODULE UNIT', 'Unit')
    call assert_accepted_module_spans('  module unit_2 '//new_line('a')// &
        ' end module unit_2 ', 'unit_2', 2_int64, 35_int64, 2_int64, 15_int64)

    syntax_item%lhs = 'program'
    call assert_rejected('empty.f90', '', 'hash-empty', syntax_item, &
        'empty-source')
    call assert_rejected('broken.f90', 'program'//new_line('a')//'end', &
        'hash-broken', syntax_item, 'invalid-program')
    call assert_rejected('mismatch.f90', 'program unit'//new_line('a')// &
        'end program other', 'hash-mismatch', syntax_item, 'invalid-program')
    call assert_rejected('empty-name.f90', 'program '//new_line('a')//'end', &
        'hash-empty-name', syntax_item, 'invalid-program')
    call assert_rejected('punctuation-name.f90', &
        'program unit-name'//new_line('a')//'end', 'hash-punctuation-name', &
        syntax_item, 'invalid-program')
    call assert_rejected('malformed-name.f90', &
        'program 2unit'//new_line('a')//'end', 'hash-malformed-name', &
        syntax_item, 'invalid-program')
    call assert_rejected('extra-header-token.f90', &
        'program unit extra'//new_line('a')//'end', 'hash-extra-header', &
        syntax_item, 'invalid-program')
    call assert_rejected('extra-terminator-token.f90', &
        'program unit'//new_line('a')//'end program unit extra', &
        'hash-extra-terminator', syntax_item, 'invalid-program')
    call assert_rejected('invalid-terminator.f90', &
        'program unit'//new_line('a')//'end module', 'hash-invalid-terminator', &
        syntax_item, 'invalid-program')

    syntax_item%lhs = 'module'
    call assert_rejected('module-mismatch.f90', 'module unit'//new_line('a')// &
        'end module other', 'hash-module-mismatch', syntax_item, 'invalid-program')
    call assert_rejected('module-empty-name.f90', 'module '//new_line('a')//'end', &
        'hash-module-empty-name', syntax_item, 'invalid-program')
    call assert_rejected('module-invalid-name.f90', &
        'module 2unit'//new_line('a')//'end', 'hash-module-invalid-name', &
        syntax_item, 'invalid-program')
    call assert_rejected('module-extra-header-token.f90', &
        'module unit extra'//new_line('a')//'end', 'hash-module-extra-header', &
        syntax_item, 'invalid-program')
    call assert_rejected('module-extra-terminator-token.f90', &
        'module unit'//new_line('a')//'end module unit extra', &
        'hash-module-extra-terminator', syntax_item, 'invalid-program')
    call assert_rejected('module-invalid-terminator.f90', &
        'module unit'//new_line('a')//'end program', 'hash-module-invalid-terminator', &
        syntax_item, 'invalid-program')

    syntax_item%lhs = 'program'
    call assert_rejected('', 'program unit'//new_line('a')//'end', &
        'hash-invalid-file', syntax_item, 'invalid-source-span-file')
    call assert_rejected('unit.f90', 'program unit'//new_line('a')//'end', &
        '', syntax_item, 'invalid-source-span-source-hash')

    syntax_item%source%page = 0_int64
    call assert_rejected('unit.f90', 'program unit'//new_line('a')//'end', &
        'hash-invalid-witness', syntax_item, 'invalid-syntax-provenance')

    write (*, '(a)') 'frontend generated program-unit behavioral checks: ok'

contains

    subroutine set_program_witness(value)
        type(standardir_syntax_item_t), intent(out) :: value

        value%id = 'R501'
        value%lhs = 'program'
        value%origin = 'mechanical'
        value%resolution = 'resolved'
        value%source%document = 'J3-24-007'
        value%source%clause = '1'
        value%source%rule = 'R501'
        value%source%page = 45_int64
        value%source%source_hash = 'fixture'
    end subroutine set_program_witness

    subroutine assert_accepted_source(source, expected_name)
        character(len=*), intent(in) :: source, expected_name

        call frontend_parse_generated_program_unit('variant.f90', source, &
            'hash-variant', syntax_item, unit, ok, message)
        call assert_true(ok, 'valid terminator variant was rejected')
        call assert_equal(message, '', 'valid terminator variant returned a diagnostic')
        call assert_equal(unit%root%name, expected_name, &
            'valid terminator variant changed the program name')
        call assert_equal(unit%root%span%source_hash, 'hash-variant', &
            'valid terminator variant lost source hash provenance')
    end subroutine assert_accepted_source

    subroutine assert_accepted_spans(source, expected_name, expected_root_start, &
            expected_root_end, expected_declaration_start, expected_declaration_end)
        character(len=*), intent(in) :: source, expected_name
        integer(int64), intent(in) :: expected_root_start, expected_root_end
        integer(int64), intent(in) :: expected_declaration_start
        integer(int64), intent(in) :: expected_declaration_end

        call frontend_parse_generated_program_unit('spans.f90', source, &
            'hash-spans', syntax_item, unit, ok, message)
        call assert_true(ok, 'whitespace-padded witness was rejected')
        call assert_equal(unit%root%name, expected_name, &
            'whitespace-padded witness changed the program name')
        call assert_equal(unit%root%span%file, 'spans.f90', &
            'exact root span lost source file')
        call assert_equal(unit%root%span%source_hash, 'hash-spans', &
            'exact root span lost source hash')
        call assert_equal_integer(unit%root%span%start_byte, expected_root_start, &
            'root span did not start at the first program token')
        call assert_equal_integer(unit%root%span%end_byte, expected_root_end, &
            'root span did not end at the terminator name')
        call assert_equal_integer(unit%declaration%span%start_byte, &
            expected_declaration_start, &
            'declaration span did not start at the program token')
        call assert_equal_integer(unit%declaration%span%end_byte, &
            expected_declaration_end, &
            'declaration span did not end at the program name')
    end subroutine assert_accepted_spans

    subroutine assert_accepted_module_source(source, expected_name)
        character(len=*), intent(in) :: source, expected_name

        call frontend_parse_generated_program_unit('module.f90', source, &
            'hash-module', syntax_item, unit, ok, message)
        call assert_true(ok, 'valid module terminator variant was rejected')
        call assert_equal(message, '', &
            'valid module terminator variant returned a diagnostic')
        call assert_equal(unit%root%name, expected_name, &
            'valid module terminator variant changed the module name')
        call assert_equal(unit%declaration%declaration_kind, declaration_kind_module, &
            'valid module changed its declaration kind')
        call assert_equal(unit%declaration%name, expected_name, &
            'valid module changed its declaration name')
    end subroutine assert_accepted_module_source

    subroutine assert_accepted_module_spans(source, expected_name, expected_root_start, &
            expected_root_end, expected_declaration_start, expected_declaration_end)
        character(len=*), intent(in) :: source, expected_name
        integer(int64), intent(in) :: expected_root_start, expected_root_end
        integer(int64), intent(in) :: expected_declaration_start
        integer(int64), intent(in) :: expected_declaration_end

        call frontend_parse_generated_program_unit('module-spans.f90', source, &
            'hash-module-spans', syntax_item, unit, ok, message)
        call assert_true(ok, 'whitespace-padded module witness was rejected')
        call assert_equal(unit%root%name, expected_name, &
            'whitespace-padded module changed the name')
        call assert_equal_integer(unit%root%span%start_byte, expected_root_start, &
            'module root span did not start at the first token')
        call assert_equal_integer(unit%root%span%end_byte, expected_root_end, &
            'module root span did not end at the terminator name')
        call assert_equal_integer(unit%declaration%span%start_byte, &
            expected_declaration_start, &
            'module declaration span did not start at the module token')
        call assert_equal_integer(unit%declaration%span%end_byte, &
            expected_declaration_end, &
            'module declaration span did not end at the module name')
    end subroutine assert_accepted_module_spans

    subroutine assert_rejected(file_name, source, source_hash, witness, expected)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(standardir_syntax_item_t), intent(in) :: witness
        character(len=*), intent(in) :: expected

        call frontend_parse_generated_program_unit(file_name, source, source_hash, &
            witness, unit, ok, message)
        call assert_true(.not. ok, 'invalid witness source was accepted')
        call assert_equal(message, expected, 'wrong generated parser diagnostic')
    end subroutine assert_rejected

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

end program test_frontend_generated_program_unit
