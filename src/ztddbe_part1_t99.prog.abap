CLASS ltc_test_object_class DEFINITION
      FOR TESTING
      RISK LEVEL HARMLESS
      DURATION SHORT
      FINAL.
  PRIVATE SECTION.
    METHODS:
      test_to_string_works FOR TESTING,
      test_hash_code_works FOR TESTING,
      test_equals_works FOR TESTING,
      test_get_class_works for testing.
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

  METHOD test_equals_works.
    DATA: simple_object TYPE REF TO lcl_object.

    simple_object = NEW lcl_object( ).
    cl_abap_unit_assert=>assert_true( boolc( simple_object->equals( simple_object ) ) ).
  ENDMETHOD.

  METHOD test_get_class_works.
    DATA: simple_object TYPE REF TO lcl_object.

    simple_object = NEW lcl_object( ).
    cl_abap_unit_assert=>assert_true( boolc( simple_object->get_class( )->absolute_name CS 'CLASS=LCL_OBJECT' ) ).
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
      test_franc_multiplication FOR TESTING,
      test_currency for testing.
ENDCLASS.

CLASS ltc_test_multycurrency_money IMPLEMENTATION.
  METHOD assert_equals.
    cl_abap_unit_assert=>assert_true( i_exp->equals( i_act ) ).
  ENDMETHOD.

  METHOD test_multiplication.
    DATA: five TYPE REF TO lcl_money.

    five = lcl_money=>dollar( 5 ).
    assert_equals( i_exp = lcl_money=>dollar( 10 ) i_act = five->times( 2 ) ).
    assert_equals( i_exp = lcl_money=>dollar( 15 ) i_act = five->times( 3 ) ).
  ENDMETHOD.

  METHOD test_equality.
    cl_abap_unit_assert=>assert_true( lcl_money=>dollar( 5 )->equals( lcl_money=>dollar( 5 ) ) ).
    cl_abap_unit_assert=>assert_false( lcl_money=>dollar( 5 )->equals( lcl_money=>dollar( 6 ) ) ).
    cl_abap_unit_assert=>assert_true( lcl_money=>franc( 5 )->equals( lcl_money=>franc( 5 ) ) ).
    cl_abap_unit_assert=>assert_false( lcl_money=>franc( 5 )->equals( lcl_money=>franc( 6 ) ) ).
    cl_abap_unit_assert=>assert_false( lcl_money=>franc( 5 )->equals( lcl_money=>dollar( 5 ) ) ).
  ENDMETHOD.

  METHOD test_franc_multiplication.
    DATA: five TYPE REF TO lcl_money.

    five = lcl_money=>franc( 5 ).
    assert_equals( i_exp = lcl_money=>franc( 10 ) i_act = five->times( 2 ) ).
    assert_equals( i_exp = lcl_money=>franc( 15 ) i_act = five->times( 3 ) ).
  ENDMETHOD.

  METHOD test_currency.
    cl_abap_unit_assert=>assert_equals( exp = 'USD' act = lcl_money=>dollar( 1 )->get_currency( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'CHF' act = lcl_money=>franc( 1 )->get_currency( ) ).
  ENDMETHOD.
ENDCLASS.
