interface ZIF_ABAP2MD_DOC_GENERATOR
  public .
  METHODS add_text
    IMPORTING
      i_code TYPE stringtab.
  METHODS main_text
    IMPORTING
      i_text TYPE REF TO stringtab.

endinterface.
