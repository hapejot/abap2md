INTERFACE zif_abap2md_text_generator
  PUBLIC .
  METHODS:
    generate        CHANGING ct_text TYPE rswsourcet
                    RAISING  zcx_abap2md_error,
    heading         IMPORTING iv_level      TYPE i
                              iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    text            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    new_paragraph   RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    definition      IMPORTING iv_text       TYPE any
                              iv_def        TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    code            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO zif_abap2md_text_generator,
    result          RETURNING
                      VALUE(r_result) TYPE rswsourcet.

ENDINTERFACE.
