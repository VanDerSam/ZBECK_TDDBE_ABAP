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
      test_get_class_works FOR TESTING.
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
      assert_equals IMPORTING i_exp           TYPE REF TO object
                              i_act           TYPE REF TO object
                    RETURNING VALUE(r_result) TYPE abap_bool,
      ""
      test_multiplication FOR TESTING,
      test_equality FOR TESTING,
      test_franc_multiplication FOR TESTING,
      test_currency FOR TESTING,
      test_simple_addition FOR TESTING,
      test_plus_returs_sum FOR TESTING,
      test_reduce_sum FOR TESTING,
      test_reduce_money FOR TESTING,
      test_reduce_money_diff_curr FOR TESTING,
      test_identity_rate FOR TESTING,
      test_mixed_addition FOR TESTING,
      test_sum_plus_money FOR TESTING,
      test_sum_times FOR TESTING.
ENDCLASS.

CLASS ltc_test_multycurrency_money IMPLEMENTATION.
  METHOD assert_equals.
    DATA: obj1 TYPE REF TO lcl_object,
          obj2 TYPE REF TO lcl_object.
    obj1 ?= i_exp.
    obj2 ?= i_act.
    cl_abap_unit_assert=>assert_true( obj1->equals( obj2 ) ).
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

  METHOD test_simple_addition.
    DATA: five    TYPE REF TO lcl_money,
          sum     TYPE REF TO lif_expression,
          bank    TYPE REF TO lcl_bank,
          reduced TYPE REF TO lcl_money.

    five = lcl_money=>dollar( 5 ).
    sum = five->plus( five ).
    bank = NEW lcl_bank( ).
    reduced = bank->reduce( i_source = sum i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 10 ) i_act = reduced ).
  ENDMETHOD.

  METHOD test_plus_returs_sum.
    DATA: five   TYPE REF TO lcl_money,
          result TYPE REF TO lif_expression,
          sum    TYPE REF TO lcl_sum.

    five = lcl_money=>dollar( 5 ).
    result = five->plus( five ).
    sum ?= result.
    assert_equals( i_exp = five i_act = sum->augend ).
    assert_equals( i_exp = five i_act = sum->addend ).
  ENDMETHOD.

  METHOD test_reduce_sum.
    DATA: sum    TYPE REF TO lif_expression,
          bank   TYPE REF TO lcl_bank,
          result TYPE REF TO lcl_money.

    sum = NEW lcl_sum( i_augend = lcl_money=>dollar( 3 ) i_addend = lcl_money=>dollar( 4 ) ).
    bank = NEW lcl_bank( ).
    result = bank->reduce( i_source = sum i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 7 ) i_act = result ).
  ENDMETHOD.

  METHOD test_reduce_money.
    DATA: bank   TYPE REF TO lcl_bank,
          result TYPE REF TO lcl_money.

    bank = NEW lcl_bank( ).
    result = bank->reduce( i_source = lcl_money=>dollar( 1 ) i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 1 ) i_act = result ).
  ENDMETHOD.

  METHOD test_reduce_money_diff_curr.
    DATA: bank   TYPE REF TO lcl_bank,
          result TYPE REF TO lcl_money.

    bank = NEW lcl_bank( ).
    bank->add_rate( i_from = `CHF` i_to = `USD` i_rate = 2 ).
    result = bank->reduce( i_source = lcl_money=>franc( 2 ) i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 1 ) i_act = result ).
  ENDMETHOD.

  METHOD test_identity_rate.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = NEW lcl_bank( )->rate( i_from = `USD` i_to = `USD` ) ).
  ENDMETHOD.

  METHOD test_mixed_addition.
    DATA: five_bucks TYPE REF TO lif_expression,
          ten_francs TYPE REF TO lif_expression,
          bank       TYPE REF TO lcl_bank,
          result     TYPE REF TO lcl_money.

    five_bucks = lcl_money=>dollar( 5 ).
    ten_francs = lcl_money=>franc( 10 ).
    bank = NEW lcl_bank( ).
    bank->add_rate( i_from = `CHF` i_to = `USD` i_rate = 2 ).
    result = bank->reduce( i_source = five_bucks->plus( i_addend = ten_francs ) i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 10 ) i_act = result ).
  ENDMETHOD.

  METHOD test_sum_plus_money.
    DATA: five_bucks TYPE REF TO lif_expression,
          ten_francs TYPE REF TO lif_expression,
          bank       TYPE REF TO lcl_bank,
          result     TYPE REF TO lcl_money,
          sum        TYPE REF TO lif_expression.

    five_bucks = lcl_money=>dollar( 5 ).
    ten_francs = lcl_money=>franc( 10 ).
    bank = NEW lcl_bank( ).
    bank->add_rate( i_from = `CHF` i_to = `USD` i_rate = 2 ).
    sum = NEW lcl_sum( i_augend = five_bucks i_addend = ten_francs )->plus( five_bucks ).
    result = bank->reduce( i_source = sum i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 15 ) i_act = result ).
  ENDMETHOD.

  METHOD test_sum_times.
    DATA: five_bucks TYPE REF TO lif_expression,
          ten_francs TYPE REF TO lif_expression,
          bank       TYPE REF TO lcl_bank,
          result     TYPE REF TO lcl_money,
          sum        TYPE REF TO lif_expression.

    five_bucks = lcl_money=>dollar( 5 ).
    ten_francs = lcl_money=>franc( 10 ).
    bank = NEW lcl_bank( ).
    bank->add_rate( i_from = `CHF` i_to = `USD` i_rate = 2 ).
    sum = NEW lcl_sum( i_augend = five_bucks i_addend = ten_francs )->times( 2 ).
    result = bank->reduce( i_source = sum i_to = `USD` ).
    assert_equals( i_exp = lcl_money=>dollar( 20 ) i_act = result ).
  ENDMETHOD.
ENDCLASS.
