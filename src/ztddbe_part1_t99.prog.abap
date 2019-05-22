CLASS ltc_test_multycurrency_money DEFINITION
      FOR TESTING
      RISK LEVEL HARMLESS
      DURATION SHORT
      FINAL.
  PRIVATE SECTION.
    METHODS:
      test_multiplication FOR TESTING,
      test_equality for testing.
ENDCLASS.

CLASS ltc_test_multycurrency_money IMPLEMENTATION.
  METHOD test_multiplication.
    DATA: five    TYPE REF TO lcl_dollar,
          product TYPE REF TO lcl_dollar.

    five = NEW lcl_dollar( 5 ).
    product = five->times( 2 ).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = product->amount ).
    product = five->times( 3 ).
    cl_abap_unit_assert=>assert_equals( exp = 15 act = product->amount ).
  ENDMETHOD.

  METHOD test_equality.
    cl_abap_unit_assert=>assert_true( new lcl_dollar( 5 )->equals( new lcl_dollar( 5 ) ) ).
    cl_abap_unit_assert=>assert_false( new lcl_dollar( 5 )->equals( new lcl_dollar( 6 ) ) ).
  ENDMETHOD.
ENDCLASS.
