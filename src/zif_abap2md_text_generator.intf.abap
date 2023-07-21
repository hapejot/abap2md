INTERFACE zif_abap2md_text_generator
  PUBLIC .
  TYPES:
    BEGIN OF t_field_spec,
      name  TYPE string,
      title TYPE string,
      style TYPE string,
    END OF t_field_spec,
    t_field_specs TYPE STANDARD TABLE OF t_field_spec WITH EMPTY KEY.
  METHODS:
    generate        CHANGING ct_text TYPE rswsourcet
                    RAISING  zcx_abap2md_error,
    heading         IMPORTING iv_level      TYPE i
                              iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    text            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    new_paragraph   RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    definition      IMPORTING iv_def        TYPE any
                              iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    code            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    table           IMPORTING it_table      TYPE table
                              it_fields     TYPE t_field_specs
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    result          RETURNING
                      VALUE(r_result) TYPE rswsourcet.

ENDINTERFACE.
