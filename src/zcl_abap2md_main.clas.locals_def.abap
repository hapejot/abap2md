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
          p TYPE lcl_tag_def_parser=>pair.
ENDCLASS.
