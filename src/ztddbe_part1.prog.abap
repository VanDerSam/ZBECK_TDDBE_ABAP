REPORT ztddbe_part1.

CLASS lcl_object DEFINITION.
  PUBLIC SECTION.
    METHODS:
      hash_code
        RETURNING VALUE(r_code) TYPE i,

      to_string
        RETURNING VALUE(r_str) TYPE string,

      equals
        IMPORTING i_obj           TYPE REF TO lcl_object
        RETURNING VALUE(r_result) TYPE abap_bool,

      get_class
        RETURNING VALUE(r_class_descr) TYPE REF TO cl_abap_classdescr.
ENDCLASS.

CLASS lcl_object IMPLEMENTATION.
  METHOD hash_code.
    " See additional info there - https://stackoverflow.com/questions/20383307/getting-an-abap-objects-identity-number
    CALL 'OBJMGR_GET_INFO' ID 'OPNAME' FIELD 'GET_OBJID'
                           ID 'OBJID'  FIELD r_code
                           ID 'OBJ'    FIELD me.
  ENDMETHOD.

  METHOD to_string.
    DATA: class_descr TYPE REF TO cl_abap_classdescr.
    class_descr ?= cl_abap_classdescr=>describe_by_object_ref( me ).
    r_str = |{ class_descr->absolute_name }@{ me->hash_code( ) }|.
  ENDMETHOD.

  METHOD equals.
    r_result = boolc( me = i_obj ).
  ENDMETHOD.

  METHOD get_class.
    r_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( me ).
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_expression.
ENDINTERFACE.

CLASS lcl_money DEFINITION DEFERRED.

CLASS lcl_bank DEFINITION.
  PUBLIC SECTION.
    METHODS:
      reduce IMPORTING i_source       TYPE REF TO lif_expression
                       i_to           TYPE string
             RETURNING VALUE(r_value) TYPE REF TO lcl_money.
ENDCLASS.

CLASS lcl_money DEFINITION
      INHERITING FROM lcl_object.
  PUBLIC SECTION.
    INTERFACES: lif_expression.

    CLASS-METHODS:
      dollar IMPORTING i_amount       TYPE i
             RETURNING VALUE(r_value) TYPE REF TO lcl_money,

      franc IMPORTING i_amount       TYPE i
            RETURNING VALUE(r_value) TYPE REF TO lcl_money.
    METHODS:
      constructor
        IMPORTING i_amount   TYPE i
                  i_currency TYPE string,

      equals REDEFINITION,

      times
        IMPORTING i_multiplier   TYPE i
        RETURNING VALUE(r_value) TYPE REF TO lcl_money,

      get_currency
        RETURNING VALUE(r_currency) TYPE string,

      plus IMPORTING i_addend       TYPE REF TO lcl_money
           RETURNING VALUE(r_value) TYPE REF TO lif_expression.

  PROTECTED SECTION.
    DATA: amount   TYPE i,
          currency TYPE string.
ENDCLASS.

CLASS lcl_money IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    amount = i_amount.
    currency = i_currency.
  ENDMETHOD.

  METHOD equals.
    DATA: money TYPE REF TO lcl_money.
    money ?= i_obj.
    IF ( me->amount = money->amount AND me->get_currency( ) = money->get_currency( ) ).
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD dollar.
    r_value = NEW lcl_money( i_amount = i_amount i_currency = `USD` ).
  ENDMETHOD.

  METHOD franc.
    r_value = NEW lcl_money( i_amount = i_amount i_currency = `CHF` ).
  ENDMETHOD.

  METHOD get_currency.
    r_currency = currency.
  ENDMETHOD.

  METHOD times.
    r_value = NEW lcl_money( i_amount = amount * i_multiplier i_currency = currency ).
  ENDMETHOD.

  METHOD plus.
    r_value = NEW lcl_money( i_amount = amount + i_addend->amount i_currency = currency ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_bank IMPLEMENTATION.
  METHOD reduce.
    r_value = lcl_money=>dollar( 10 ).
  ENDMETHOD.
ENDCLASS.

INCLUDE ztddbe_part1_t99.
