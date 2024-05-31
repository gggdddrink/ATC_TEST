class ZLS_CLAS_ATCCHECK3 definition
  public
  final
  create public .

public section.

  interfaces IF_CI_ATC_CHECK .

  constants:
    BEGIN OF finding_codes,
      move TYPE if_ci_atc_check=>ty_finding_code VALUE 'MOVE',
      compute TYPE if_ci_atc_check=>ty_finding_code VALUE 'COMPUTE',
    END OF finding_codes .
  constants:
    begin of quickfix_codes,
      move type cl_ci_atc_quickfixes=>ty_quickfix_code value 'MOVE',
      compute type cl_ci_atc_quickfixes=>ty_quickfix_code value 'COMPUTE',
    end of quickfix_codes .

  methods CONSTRUCTOR .
PROTECTED SECTION.
PRIVATE SECTION.
  TYPES ty_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_move_info,
      source TYPE if_ci_atc_source_code_provider=>ty_tokens,
      target TYPE if_ci_atc_source_code_provider=>ty_tokens,
      is_exact TYPE abap_bool,
      is_corresponding TYPE abap_bool,
      is_cast TYPE abap_bool,
    END OF ty_move_info.

  METHODS analyze_procedure
    IMPORTING procedure TYPE if_ci_atc_source_code_provider=>ty_procedure
    RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
  METHODS construct_assignment
    IMPORTING move_info TYPE ty_move_info
    RETURNING VALUE(assignment) TYPE ty_code.
  METHODS parse_move
    IMPORTING statement TYPE if_ci_atc_source_code_provider=>ty_statement
    RETURNING VALUE(move_info) TYPE ty_move_info.
  METHODS flatten_tokens
    IMPORTING tokens TYPE if_ci_atc_source_code_provider=>ty_tokens
    RETURNING VALUE(code) TYPE string.
  METHODS break_into_lines
    IMPORTING code TYPE string
    RETURNING VALUE(code_lines) TYPE ty_code.

  DATA code_provider TYPE REF TO if_ci_atc_source_code_provider.
  DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.

  DATA check_moves TYPE abap_bool.
  DATA check_computes TYPE abap_bool.
ENDCLASS.



CLASS ZLS_CLAS_ATCCHECK3 IMPLEMENTATION.


  METHOD ANALYZE_PROCEDURE.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>)
        WHERE keyword = 'MOVE' OR keyword = 'MOVE-CORRESPONDING' OR keyword = 'COMPUTE'.
      DATA(statement_idx) = sy-tabix.
      CASE <statement>-keyword.
        WHEN 'MOVE' OR 'MOVE-CORRESPONDING'.
          IF check_moves = abap_true.
            DATA(move_quickfixes) = assistant_factory->create_quickfixes( ).
            move_quickfixes->create_quickfix( quickfix_codes-move )->replace(
              context = assistant_factory->create_quickfix_context( VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_idx to = statement_idx ) ) )
              code = construct_assignment( parse_move( <statement> ) )
            ).
            INSERT VALUE #(
               code = finding_codes-move
               location = code_provider->get_statement_location( <statement> )
               checksum = code_provider->get_statement_checksum( <statement> )
               details = assistant_factory->create_finding_details( )->attach_quickfixes( move_quickfixes )
            ) INTO TABLE findings.
          ENDIF.

        WHEN 'COMPUTE'.
          IF check_computes = abap_true.
            ASSIGN <statement>-tokens[ 1 ] TO FIELD-SYMBOL(<first_token>).
            IF <first_token>-references IS INITIAL AND <first_token>-lexeme = 'COMPUTE'.
              DATA(compute_quickfixes) = assistant_factory->create_quickfixes( ).
              compute_quickfixes->create_quickfix( quickfix_codes-compute )->replace(
                context = assistant_factory->create_quickfix_context( VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_idx to = statement_idx ) tokens = VALUE #( from = 1 to = 1 ) ) )
                code = VALUE #( )
              ).
              INSERT VALUE #(
                code = finding_codes-compute
                location = code_provider->get_statement_location( <statement> )
                checksum = code_provider->get_statement_checksum( <statement> )
                details = assistant_factory->create_finding_details( )->attach_quickfixes( compute_quickfixes )
              ) INTO TABLE findings.
            ENDIF.
          ENDIF.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD BREAK_INTO_LINES.
    CONSTANTS allowed_line_length TYPE i VALUE 255.
    DATA(remaining_chunk) = strlen( code ).
    WHILE remaining_chunk > 0.
      DATA(already_chopped_chars) = lines( code_lines ) * allowed_line_length.
      DATA(chars_to_chop) = COND #( WHEN remaining_chunk > allowed_line_length THEN allowed_line_length ELSE remaining_chunk ).
      INSERT code+already_chopped_chars(chars_to_chop) INTO TABLE code_lines.
      remaining_chunk -= chars_to_chop.
    ENDWHILE.
  ENDMETHOD.


  METHOD CONSTRUCTOR.
    check_moves = abap_true.
    check_computes = abap_true.
  ENDMETHOD.


  METHOD CONSTRUCT_ASSIGNMENT.
    DATA(lhs) = flatten_tokens( move_info-target ).
    DATA(rhs) = flatten_tokens( move_info-source ).
    DATA(operator) = COND #(
      WHEN move_info-is_corresponding = abap_true THEN `CORRESPONDING`
      WHEN move_info-is_exact = abap_true THEN `EXACT`
      WHEN move_info-is_cast = abap_true THEN `CAST`
    ).
    DATA(needs_inner_exact) = xsdbool( move_info-is_corresponding = abap_true AND move_info-is_exact = abap_true ).
    DATA(flat_new_statement) = COND #(
      WHEN operator IS INITIAL
        THEN |{ lhs } = { rhs }|
        ELSE |{ lhs } = { operator } #( { COND #( WHEN move_info-is_corresponding = abap_true THEN |BASE ( { lhs } ) | ) }| &&
                                       |{ COND #( WHEN needs_inner_exact = abap_true THEN |EXACT #( { rhs } )| ELSE rhs ) } )|
    ).
    flat_new_statement &&= '.'.
    assignment = break_into_lines( flat_new_statement ).
  ENDMETHOD.


  METHOD FLATTEN_TOKENS.
    code = REDUCE #( INIT str = `` FOR tok IN tokens NEXT str = |{ str }{ tok-lexeme } | ).
  ENDMETHOD.


  METHOD IF_CI_ATC_CHECK~GET_META_DATA.
    meta_data = NEW meta_data(
      check_moves = check_moves
      check_computes = check_computes
    ).
  ENDMETHOD.


  METHOD IF_CI_ATC_CHECK~RUN.
    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD IF_CI_ATC_CHECK~SET_ASSISTANT_FACTORY.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD IF_CI_ATC_CHECK~SET_ATTRIBUTES ##NEEDED.
    DATA(check_move_value) = attributes[ name = `CheckMoves` ]-value.
    check_moves = check_move_value->*.
    DATA(check_compute_value) = attributes[ name = `CheckComputes` ]-value.
    check_moves = check_compute_value->*.
  ENDMETHOD.


  METHOD IF_CI_ATC_CHECK~VERIFY_PREREQUISITES ##NEEDED.

  ENDMETHOD.


  METHOD PARSE_MOVE.
    move_info-is_corresponding = xsdbool( statement-keyword = 'MOVE-CORRESPONDING' ).
    move_info-is_exact         = xsdbool( statement-tokens[ 2 ]-lexeme = 'EXACT' AND statement-tokens[ 2 ]-references IS INITIAL ).
    DATA(start_of_source) = COND #( WHEN move_info-is_exact = abap_true THEN 3 ELSE 2 ).
    DATA(found_to) = abap_false.
    LOOP AT statement-tokens FROM start_of_source ASSIGNING FIELD-SYMBOL(<token>).
      IF <token>-references IS INITIAL AND <token>-lexeme = 'TO'.
        found_to = abap_true.
      ELSEIF <token>-references IS INITIAL AND <token>-lexeme = '?TO'.
        move_info-is_cast = abap_true.
        found_to = abap_true.
      ELSEIF found_to = abap_false.
        INSERT <token> INTO TABLE move_info-source.
      ELSE.
        INSERT <token> INTO TABLE move_info-target.
      ENDIF.
  ENDLOOP.
  ENDMETHOD.
ENDCLASS.
