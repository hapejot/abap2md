INTERFACE zif_abap2md_parser
  PUBLIC .
  METHODS next_chunk RETURNING VALUE(r_chunk) TYPE rswsourcet.
  METHODS next_token
    RETURNING
      value(r_result) TYPE zabap2md_token.

ENDINTERFACE.
