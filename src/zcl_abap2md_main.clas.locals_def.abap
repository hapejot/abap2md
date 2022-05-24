*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section



INTERFACE lif_parser.
  METHODS next_chunk RETURNING VALUE(ls_chunk) TYPE rswsourcet.
ENDINTERFACE.

CLASS lcl_comment_parser DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_parser.
    METHODS constructor
      IMPORTING
        i_text TYPE rswsourcet.
  PRIVATE SECTION.
    DATA mt_text TYPE rswsourcet.

ENDCLASS.


CLASS lcl_tag_def_parser DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_parser.
    METHODS constructor
      IMPORTING
        i_src TYPE REF TO lif_parser.
  PRIVATE SECTION.
    TYPES: BEGIN OF pair,
             keyword TYPE string,
             text    TYPE string,
           END OF pair.
    DATA src TYPE REF TO lif_parser.
    DATA mt_chunk TYPE rswsourcet.
    DATA mode TYPE c.
    DATA: pairs TYPE STANDARD TABLE OF pair,
          p     TYPE lcl_tag_def_parser=>pair.
ENDCLASS.

INTERFACE lif_text_generator.
  METHODS:
    generate        CHANGING ct_text TYPE stringtab
                    RAISING  zcx_abap2md_error,
    heading         IMPORTING iv_level      TYPE i
                              iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO lif_text_generator,
    text            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO lif_text_generator,
    new_paragraph   RETURNING VALUE(ro_gen) TYPE REF TO lif_text_generator,
    definition      IMPORTING iv_text       TYPE any
                              iv_def        TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO lif_text_generator,
    code            IMPORTING iv_text       TYPE any
                    RETURNING VALUE(ro_gen) TYPE REF TO lif_text_generator,
    result          RETURNING
                      VALUE(r_result) TYPE stringtab.
ENDINTERFACE.

CLASS lcl_markdown DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_text_generator.
  PRIVATE SECTION.
    DATA: mt_text TYPE stringtab.
ENDCLASS.

INTERFACE lif_info.
  METHODS read_main
    RAISING
      zcx_abap2md_error.
  METHODS build_doc_structure
    RAISING
      zcx_abap2md_error.
  METHODS generate_markdown
    CHANGING
      ct_text TYPE stringtab
    RAISING
      zcx_abap2md_error.

ENDINTERFACE.
