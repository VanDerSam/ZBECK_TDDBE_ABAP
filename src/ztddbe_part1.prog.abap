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

CLASS lcl_money DEFINITION
      INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS:
      equals REDEFINITION.
  PROTECTED SECTION.
    DATA: amount TYPE i.
ENDCLASS.

CLASS lcl_money IMPLEMENTATION.
  METHOD equals.
    DATA: money TYPE REF TO lcl_money.
    money ?= i_obj.
    IF ( me->amount = money->amount AND me->get_class( )->absolute_name = money->get_class( )->absolute_name ).
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dollar DEFINITION
      INHERITING FROM lcl_money.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_amount TYPE i,

      times IMPORTING i_multiplier   TYPE i
            RETURNING VALUE(r_value) TYPE REF TO lcl_dollar.
ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    amount = i_amount.
  ENDMETHOD.

  METHOD times.
    r_value = NEW lcl_dollar( amount * i_multiplier ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_franc DEFINITION
      INHERITING FROM lcl_money.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_amount TYPE i,

      times IMPORTING i_multiplier   TYPE i
            RETURNING VALUE(r_value) TYPE REF TO lcl_franc.
ENDCLASS.

CLASS lcl_franc IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    amount = i_amount.
  ENDMETHOD.

  METHOD times.
    r_value = NEW lcl_franc( amount * i_multiplier ).
  ENDMETHOD.
ENDCLASS.

INCLUDE ztddbe_part1_t99.
