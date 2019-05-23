CLASS ltc_test_object_class DEFINITION
      FOR TESTING
      RISK LEVEL HARMLESS
      DURATION SHORT
      FINAL.
  PRIVATE SECTION.
    METHODS:
      test_to_string_works FOR TESTING,
      test_hash_code_works FOR TESTING,
      test_equals_workds FOR TESTING.
ENDCLASS.

CLASS ltc_test_object_class IMPLEMENTATION.
  METHOD test_to_string_works.
    DATA: class_descr   TYPE REF TO cl_abap_classdescr,
          simple_object TYPE REF TO lcl_object.

    simple_object = NEW lcl_object( ).
    class_descr ?= cl_abap_classdescr=>describe_by_object_ref( simple_object ).
    cl_abap_unit_assert=>assert_true( boolc( simple_object->to_string( ) CS '\PROGRAM=ZTDDBE_PART1\CLASS=LCL_OBJECT@' ) ).
  ENDMETHOD.

  METHOD test_hash_code_works.
    DATA: code          TYPE i,
          simple_object TYPE REF TO lcl_object.

    simple_object = NEW lcl_object( ).
    code = simple_object->hash_code( ).
    cl_abap_unit_assert=>assert_not_initial( code ).
    simple_object = NEW lcl_object( ).
    cl_abap_unit_assert=>assert_differs( exp = code act = simple_object->hash_code( ) ).
  ENDMETHOD.

  METHOD test_equals_workds.
    DATA: simple_object TYPE REF TO lcl_object.

    simple_object = NEW lcl_object( ).
    cl_abap_unit_assert=>assert_true( boolc( simple_object->equals( simple_object ) ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_test_multycurrency_money DEFINITION
      FOR TESTING
      RISK LEVEL HARMLESS
      DURATION SHORT
      FINAL.
  PRIVATE SECTION.
    METHODS:
      assert_equals IMPORTING i_exp           TYPE REF TO lcl_object
                              i_act           TYPE REF TO lcl_object
                    RETURNING VALUE(r_result) TYPE abap_bool,
      ""
      test_multiplication FOR TESTING,
      test_equality FOR TESTING,
      test_franc_multiplication FOR TESTING.
ENDCLASS.

CLASS ltc_test_multycurrency_money IMPLEMENTATION.
  METHOD assert_equals.
    cl_abap_unit_assert=>assert_true( i_exp->equals( i_act ) ).
  ENDMETHOD.

  METHOD test_multiplication.
    DATA: five    TYPE REF TO lcl_dollar.

    five = NEW lcl_dollar( 5 ).
    assert_equals( i_exp = NEW lcl_dollar( 10 ) i_act = five->times( 2 ) ).
    assert_equals( i_exp = NEW lcl_dollar( 15 ) i_act = five->times( 3 ) ).
  ENDMETHOD.

  METHOD test_equality.
    cl_abap_unit_assert=>assert_true( NEW lcl_dollar( 5 )->equals( NEW lcl_dollar( 5 ) ) ).
    cl_abap_unit_assert=>assert_false( NEW lcl_dollar( 5 )->equals( NEW lcl_dollar( 6 ) ) ).
    cl_abap_unit_assert=>assert_true( NEW lcl_franc( 5 )->equals( NEW lcl_franc( 5 ) ) ).
    cl_abap_unit_assert=>assert_false( NEW lcl_franc( 5 )->equals( NEW lcl_franc( 6 ) ) ).
  ENDMETHOD.

  METHOD test_franc_multiplication.
    DATA: five TYPE REF TO lcl_franc.

    five = NEW lcl_franc( 5 ).
    assert_equals( i_exp = NEW lcl_franc( 10 ) i_act = five->times( 2 ) ).
    assert_equals( i_exp = NEW lcl_franc( 15 ) i_act = five->times( 3 ) ).
  ENDMETHOD.
ENDCLASS.
