INTERFACE zif_abap2md_parser
  PUBLIC .
  METHODS next_token
    RETURNING
      value(r_result) TYPE zabap2md_token.

ENDINTERFACE.
