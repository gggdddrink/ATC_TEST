CLASS meta_data DEFINITION FINAL.
PUBLIC SECTION.
  INTERFACES if_ci_atc_check_meta_data.

  METHODS constructor
    IMPORTING
      check_moves TYPE abap_bool
      check_computes TYPE abap_bool.

  DATA check_moves TYPE abap_bool.
  DATA check_computes TYPE abap_bool.
ENDCLASS.

CLASS meta_data IMPLEMENTATION.

  METHOD if_ci_atc_check_meta_data~get_attributes ##NEEDED.
    attributes = VALUE #(
      ( name = `CheckMoves` kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean value = ref #( check_moves ) )
      ( name = `CheckComputes` kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean value = ref #( check_computes ) )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_checked_object_types.
    types = VALUE #( ( 'PROG' ) ( 'FUGR' ) ( 'CLAS' ) ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_description.
    description = 'Detect obsolete MOVE and COMPUTE statements.'(des).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_finding_code_infos.
    finding_code_infos = VALUE #(
      ( code = cl_ci_atc_check_example=>finding_codes-move severity = if_ci_atc_check=>finding_severities-warning text = 'Don''t use MOVE!'(mov) )
      ( code = cl_ci_atc_check_example=>finding_codes-compute severity = if_ci_atc_check=>finding_severities-warning text = 'Superfluous COMPUTE.'(cmp) )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~uses_checksums.
    uses_checksums = abap_true.
  ENDMETHOD.

  METHOD constructor.
    me->check_moves = check_moves.
    me->check_computes = check_computes.
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_quickfix_code_infos.
    quickfix_code_infos = value #(
      ( code = cl_ci_atc_check_example=>quickfix_codes-move short_text = 'Replace MOVE with assignment'(qmv) )
      ( code = cl_ci_atc_check_example=>quickfix_codes-compute short_text = 'Remove COMPUTE'(qcm) )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~is_remote_enabled.
    is_remote_enabled = abap_true.
  ENDMETHOD.

ENDCLASS.
