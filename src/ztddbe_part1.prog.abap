REPORT ztddbe_part1.

CLASS lcl_dollar DEFINITION.
  PUBLIC SECTION.
    DATA: amount TYPE i.

    METHODS:
      constructor
        IMPORTING
          i_amount TYPE i,
      times
        IMPORTING
          i_multiplier   TYPE i
        RETURNING
          VALUE(r_value) TYPE REF TO lcl_dollar,

      equals
        IMPORTING
                  i_object            TYPE REF TO object
        RETURNING VALUE(r_cmp_result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.
  METHOD constructor.
    amount = i_amount.
  ENDMETHOD.

  METHOD times.
    r_value = NEW lcl_dollar( amount * i_multiplier ).
  ENDMETHOD.

  METHOD equals.
    DATA: dollar TYPE REF TO lcl_dollar.
    dollar ?= i_object.
    IF ( me->amount = dollar->amount ).
      r_cmp_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

INCLUDE ztddbe_part1_t99.
