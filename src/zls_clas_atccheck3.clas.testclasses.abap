CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE 'CL_CI_ATC_EXAMPLE_TEST_1'.
    CONSTANTS:
      BEGIN OF test_class_methods,
        moves TYPE c LENGTH 30 VALUE 'MOVES',
        computes TYPE c LENGTH 30 VALUE 'COMPUTES',
      END OF test_class_methods.

    METHODS simple_class FOR TESTING RAISING cx_static_check.

    METHODS setup.
    METHODS teardown.
ENDCLASS.


CLASS test IMPLEMENTATION.

  METHOD setup.
    cl_feature_toggle_4_test=>define_test_toggle( cl_ci_test_atc_check_wrapper=>atc_check_feature_toggle ).
  ENDMETHOD.

  METHOD teardown.
    cl_feature_toggle_4_test=>cleanup_test_toggle( ).
  ENDMETHOD.

  METHOD simple_class.
    DATA(move_location) = VALUE if_ci_atc_check=>ty_location(
      object = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-moves ) )
      position = VALUE #( line = 4 column = 4 )
    ).
    DATA(compute_location) = VALUE if_ci_atc_check=>ty_location(
      object = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-computes ) )
      position = VALUE #( line = 4 column = 4 )
    ).
    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check = NEW cl_ci_atc_check_example( )
      object = VALUE #( type = 'CLAS' name = test_class )
      expected_findings = VALUE #( (
        code = cl_ci_atc_check_example=>finding_codes-move
        location = move_location
        quickfixes = VALUE #( (
          quickfix_code = cl_ci_atc_check_example=>quickfix_codes-move
          location = move_location
          code = VALUE #(
            ( `var_1 = var_2.` )
          )
        ) )
      ) (
        code = cl_ci_atc_check_example=>finding_codes-compute
        location = VALUE #(
          object = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-computes ) )
          position = VALUE #( line = 4 column = 4 )
        )
        quickfixes = VALUE #( (
          quickfix_code = cl_ci_atc_check_example=>quickfix_codes-compute
          location = compute_location
          code = VALUE #(
            ( `var_2 = var_1.` )
          )
        ) )
      ) )
      asserter_config = VALUE #( quickfixes = abap_true )
    ).
  ENDMETHOD.

ENDCLASS.
