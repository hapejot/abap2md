CLASS zcl_abap2md_md_cell DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING i_content TYPE any.
    METHODS get_width
      RETURNING
        VALUE(r_result) TYPE i.
    METHODS get_height           RETURNING
                                   VALUE(r_height) TYPE i.
    METHODS get_line
      IMPORTING
        VALUE(i_idx)    TYPE i
      RETURNING
        VALUE(r_result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA text TYPE stringtab.
    METHODS init_from_clike      IMPORTING
                                   i_content TYPE clike.
    METHODS init_from_table      IMPORTING
                                   i_content TYPE ANY TABLE.
    METHODS init_from_string     IMPORTING
                                   i_content TYPE string.

ENDCLASS.



CLASS zcl_abap2md_md_cell IMPLEMENTATION.


  METHOD constructor.
    DESCRIBE FIELD i_content TYPE DATA(field_type).
    CASE field_type.
      WHEN 'C'. " Char like
        init_from_clike( i_content ).
      WHEN 'g'. " string
        init_from_string( i_content ).

      WHEN 'N'. " Numeric
      WHEN 'D'. " Date
      WHEN 'T'. " Time

      WHEN 'I'. " Integer
      WHEN 'p'. " Packed
      WHEN 'f'. " Float

      WHEN 'x'. " X
      WHEN 'y'. " X-String

      WHEN 'l'. " data reference
      WHEN 'r'. " object reference

      WHEN 'u'. " flat structure
      WHEN 'v'. " deep structure
      WHEN 'h'. " table
        init_from_table( i_content ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_height.
    r_height = lines( text ).
  ENDMETHOD.


  METHOD get_line.
    r_result = VALUE #( text[ i_idx ] OPTIONAL ).
  ENDMETHOD.


  METHOD get_width.
    r_result = 0.
    LOOP AT text INTO DATA(line).
      DATA(len) = strlen( line ).
      IF len > r_result.
        r_result = len.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD init_from_clike.
    text = VALUE #( ( |{ i_content }| ) ).
  ENDMETHOD.


  METHOD init_from_string.
    text = VALUE #( ( i_content ) ).
  ENDMETHOD.


  METHOD init_from_table.
    FIELD-SYMBOLS: <row> TYPE any.
    LOOP AT i_content ASSIGNING <row>.
      APPEND |{ <row> }| TO text.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
