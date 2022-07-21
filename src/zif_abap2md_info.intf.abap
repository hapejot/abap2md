INTERFACE zif_abap2md_info
  PUBLIC .
  METHODS read_main
    RAISING
      zcx_abap2md_error.
  METHODS build_doc_structure
    IMPORTING i_gen TYPE REF TO zif_abap2md_doc_generator OPTIONAL
    RAISING
              zcx_abap2md_error.
  METHODS generate_markdown
    CHANGING
      ct_text TYPE rswsourcet
    RAISING
      zcx_abap2md_error.

ENDINTERFACE.
