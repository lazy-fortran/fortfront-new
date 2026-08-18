module fortfront_program_unit_v2
    use, intrinsic :: iso_fortran_env, only: int64
    use frontend_ast_v1_generated, only: program_root_t, program_declaration_t, &
        variable_declaration_t, program_root_to_sx, program_declaration_to_sx, &
        variable_declaration_to_sx
    use fortfront_assignment_sequence, only: assignment_sequence_t, &
        frontend_parse_typed_assignment_sequence, &
        frontend_typed_assignment_sequence_to_sx, assignment_sequence_source_hash
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        typed_program_unit_t
    use frontend_program_unit_v2_envelope_generated, only: &
        program_unit_v2_execution_part_policy_matches
    implicit none
    private

    type, public :: execution_part_t
        type(assignment_sequence_t) :: sequence
    end type execution_part_t

    type, public :: program_unit_v2_t
        type(program_root_t) :: root
        integer(int64) :: declaration_count = 0_int64
        type(program_declaration_t) :: declaration
        integer(int64) :: variable_count = 0_int64
        type(variable_declaration_t) :: variable
        type(execution_part_t) :: execution_part
    end type program_unit_v2_t

    public :: frontend_parse_program_unit_v2
    public :: frontend_program_unit_v2_to_sx

contains

    subroutine frontend_parse_program_unit_v2(file_name, source, source_hash, &
            unit, ok, message)
        character(len=*), intent(in) :: file_name, source, source_hash
        type(program_unit_v2_t), intent(out) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(typed_program_unit_t) :: declaration_unit
        character(len=1024) :: declaration_source

        unit = program_unit_v2_t()
        ok = .false.
        message = ''
        if (.not. program_unit_v2_execution_part_policy_matches('execution-part')) then
            message = 'execution-part-policy-mismatch'
            return
        end if
        declaration_source = 'program main'//new_line('a')// &
            '  integer :: x'//new_line('a')//'end program main'//new_line('a')
        if (source /= 'program main'//new_line('a')//'  integer :: x'//new_line('a')// &
                '  x = 7'//new_line('a')//'  x = x + 1'//new_line('a')// &
                'end program main'//new_line('a')) then
            message = 'unsupported-program-unit-v2'
            return
        end if
        call frontend_parse_typed_program_unit(file_name, trim(declaration_source), &
            source_hash, declaration_unit, ok, message)
        if (.not. ok) return
        call frontend_parse_typed_assignment_sequence(file_name, source, &
            assignment_sequence_source_hash, unit%execution_part%sequence, ok, message)
        if (.not. ok) return

        unit%root = declaration_unit%root
        unit%root%span%end_byte = int(len(source), int64) - 1_int64
        unit%declaration_count = declaration_unit%declaration_count
        unit%declaration = declaration_unit%declaration
        unit%variable_count = declaration_unit%variable_count
        unit%variable = declaration_unit%variable
        ok = .true.
        message = ''
    end subroutine frontend_parse_program_unit_v2

    subroutine frontend_program_unit_v2_to_sx(unit, output, ok, message)
        type(program_unit_v2_t), intent(in) :: unit
        character(len=*), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=65536) :: root_sx, declaration_sx, variable_sx, sequence_sx

        output = ''
        ok = .false.
        message = ''
        if (unit%declaration_count /= 1_int64 .or. unit%variable_count /= 1_int64) then
            message = 'invalid-program-unit-v2-cardinality'
            return
        end if
        call program_root_to_sx(unit%root, root_sx, ok, message)
        if (.not. ok) return
        call program_declaration_to_sx(unit%declaration, declaration_sx, ok, message)
        if (.not. ok) return
        call variable_declaration_to_sx(unit%variable, variable_sx, ok, message)
        if (.not. ok) return
        call frontend_typed_assignment_sequence_to_sx(unit%execution_part%sequence, &
            sequence_sx, ok, message)
        if (.not. ok) return
        output = '(program-unit-v2 (root '//trim(root_sx)//') '// &
            '(declaration-count 1) (declaration '//trim(declaration_sx)//') '// &
            '(variable-count 1) (variable '//trim(variable_sx)//') '// &
            '(execution-part '//trim(sequence_sx)//'))'
        ok = .true.
    end subroutine frontend_program_unit_v2_to_sx

end module fortfront_program_unit_v2
