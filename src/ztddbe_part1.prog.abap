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
          VALUE(r_value) TYPE REF TO lcl_dollar.
ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.
  METHOD constructor.
    amount = i_amount.
  ENDMETHOD.

  METHOD times.
    r_value = NEW lcl_dollar( amount * i_multiplier ).
  ENDMETHOD.
ENDCLASS.

INCLUDE ztddbe_part1_t99.
