program test_frontend_typed_intrinsic_type_specs_v1
    use fortfront_frontend, only: frontend_parse_typed_program_unit, &
        frontend_typed_variable_declaration_to_sx, typed_program_unit_t
    use frontend_type_specs_generated, only: intrinsic_type_spec_table, &
        intrinsic_type_spec_lookup
    implicit none

    type(typed_program_unit_t) :: unit
    character(len=1024) :: serialized
    character(len=256) :: message
    logical :: ok
    integer :: spec_index, variable_start

    if (.not. intrinsic_type_spec_lookup('  integer :: y', spec_index, variable_start)) &
        error stop 'generated integer lookup failed'
    if (trim(intrinsic_type_spec_table(spec_index)%source_rule) /= 'R705' .or. &
        trim(intrinsic_type_spec_table(spec_index)%source_document) /= 'J3-24-007' .or. &
        intrinsic_type_spec_table(spec_index)%source_page /= 67) &
        error stop 'generated integer source rule changed'
    if (.not. intrinsic_type_spec_lookup('  real :: x', spec_index, variable_start)) &
        error stop 'generated real lookup failed'
    if (trim(intrinsic_type_spec_table(spec_index)%source_rule) /= 'R706') &
        error stop 'generated real source rule changed'
    if (.not. intrinsic_type_spec_lookup('  double precision :: x', spec_index, variable_start)) &
        error stop 'generated double precision lookup failed'
    if (trim(intrinsic_type_spec_table(spec_index)%source_rule) /= 'R707') &
        error stop 'generated double precision source rule changed'
    if (.not. intrinsic_type_spec_lookup('  complex :: x', spec_index, variable_start)) &
        error stop 'generated complex lookup failed'
    if (len_trim(intrinsic_type_spec_table(spec_index)%source_rule) /= 0) &
        error stop 'complex source rule was invented'

    call check_type('program p'//new_line('a')//'  integer :: y'//new_line('a')// &
        'end program p'//new_line('a'), 'integer', 'y')
    call check_type('program p'//new_line('a')//'  real :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'real', 'x')
    call check_type('program p'//new_line('a')//'  double precision :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'double-precision', 'x')
    call check_type('program p'//new_line('a')//'  complex :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'complex', 'x')

    call frontend_parse_typed_program_unit('bad.f90', &
        'program p'//new_line('a')//'  real :: y'//new_line('a')// &
        'end program p'//new_line('a'), 'type-spec-test', unit, ok, message)
    if (ok .or. trim(message) /= 'unsupported-typed-program-unit') &
        error stop 'non-permitted intrinsic variable name was accepted'

    call frontend_parse_typed_program_unit('bad.f90', &
        'program p'//new_line('a')//'  logical :: x'//new_line('a')// &
        'end program p'//new_line('a'), 'type-spec-test', unit, ok, message)
    if (ok .or. trim(message) /= 'unsupported-typed-program-unit') &
        error stop 'unsupported intrinsic type was accepted'
    write (*, '(a)') 'frontend typed intrinsic type-spec checks: ok'

contains

    subroutine check_type(source, expected, name)
        character(len=*), intent(in) :: source, expected, name
        character(len=1024) :: expected_serialized
        character(len=32) :: start_byte, end_byte

        call frontend_parse_typed_program_unit('type.f90', source, 'type-spec-test', &
            unit, ok, message)
        if (.not. ok) error stop 'generated type-spec lookup rejected '//trim(expected)//'/'//trim(name)//': '//trim(message)
        if (trim(unit%variable%type_spec) /= trim(expected)) &
            error stop 'generated type-spec canonical value changed'
        if (trim(unit%variable%name) /= trim(name)) &
            error stop 'generated type-spec variable extraction changed'
        call frontend_typed_variable_declaration_to_sx(unit%variable, serialized, &
            ok, message)
        if (.not. ok) error stop 'generated variable declaration serialization failed'
        write (start_byte, '(i0)') unit%variable%span%start_byte
        write (end_byte, '(i0)') unit%variable%span%end_byte
        expected_serialized = '(variable-declaration (type-spec '//trim(expected)// &
            ') (name '//trim(name)//') (span (source-span (file type.f90) '// &
            '(start-byte '//trim(start_byte)//') (end-byte '//trim(end_byte)//') '// &
            '(source-hash type-spec-test))))'
        if (trim(serialized) /= trim(expected_serialized)) &
            error stop 'generated variable declaration payload changed'
    end subroutine check_type

end program test_frontend_typed_intrinsic_type_specs_v1
