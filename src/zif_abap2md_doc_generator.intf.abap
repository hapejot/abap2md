INTERFACE zif_abap2md_doc_generator
  PUBLIC .
  METHODS add_text
    IMPORTING
      i_code TYPE stringtab.
  METHODS main_text
    IMPORTING
      i_text TYPE REF TO zabap2md_text.
  METHODS doc
    RETURNING
      VALUE(r_result) TYPE REF TO zabap2md_doc_structure.

ENDINTERFACE.
