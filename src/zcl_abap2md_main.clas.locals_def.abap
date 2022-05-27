*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section



INTERFACE lif_parser.
  METHODS next_chunk RETURNING VALUE(r_chunk) TYPE rswsourcet.
ENDINTERFACE.

CLASS lcl_comment_parser DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_parser.
    METHODS constructor
      IMPORTING
        i_text TYPE rswsourcet.
  PRIVATE SECTION.
    DATA mt_text TYPE rswsourcet.
    METHODS has_more_lines
      RETURNING
        VALUE(r_result) TYPE abap_bool.
    METHODS read_next_line
      RETURNING
        VALUE(rv_text) TYPE string.

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
    generate        CHANGING ct_text TYPE rswsourcet
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
                      VALUE(r_result) TYPE rswsourcet.
ENDINTERFACE.

CLASS lcl_markdown DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_text_generator.
  PRIVATE SECTION.
    DATA: mt_text TYPE rswsourcet.
    METHODS generate_table
      IMPORTING
        io_type          TYPE REF TO cl_abap_structdescr
        it_tab           TYPE STANDARD TABLE
      RETURNING
        VALUE(r_out_tab) TYPE rswsourcet.
ENDCLASS.

CLASS lcl_doc_generator DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_current_text TYPE REF TO stringtab.
    METHODS add_text
      IMPORTING
        i_code TYPE stringtab.
    METHODS main_text
      IMPORTING
        i_text TYPE REF TO stringtab.
    METHODS generate_markdown
      CHANGING
        ct_text TYPE stringtab.
    TYPES: BEGIN OF subsection,
             name  TYPE string,
             title TYPE string,
             text  TYPE rswsourcet,
           END OF subsection.
    TYPES: BEGIN OF section,
             name        TYPE string,
             title       TYPE string,
             text        TYPE rswsourcet,
             subsections TYPE STANDARD TABLE OF subsection WITH KEY name,
           END OF section.
    TYPES: BEGIN OF page,
             name     TYPE string,
             title    TYPE string,
             text     TYPE rswsourcet,
             sections TYPE STANDARD TABLE OF section WITH KEY name,
           END OF page.

    DATA:
            m_pages TYPE STANDARD TABLE OF page WITH KEY name READ-ONLY.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mr_current_page    TYPE REF TO page,
      mr_current_section TYPE REF TO section,
      mr_current_text    TYPE REF TO stringtab,
      mr_main_text       TYPE REF TO stringtab.
    METHODS first_word
      CHANGING
        c_chunk         TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.


ENDCLASS.
INTERFACE lif_info.
  METHODS read_main
    RAISING
      zcx_abap2md_error.
  METHODS build_doc_structure
    IMPORTING i_gen TYPE REF TO lcl_doc_generator OPTIONAL
    RAISING
              zcx_abap2md_error.
  METHODS generate_markdown
    CHANGING
      ct_text TYPE rswsourcet
    RAISING
      zcx_abap2md_error.

ENDINTERFACE.
