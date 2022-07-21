CLASS zcl_abap2md_comment_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_parser.
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



CLASS zcl_abap2md_comment_parser IMPLEMENTATION.
  METHOD constructor.

    me->mt_text = i_text.

  ENDMETHOD.

  METHOD zif_abap2md_parser~next_chunk.
    DATA: in_comment TYPE abap_bool,
          lv_text    TYPE string.

    WHILE has_more_lines( ).
      lv_text = read_next_line( ).
      CASE in_comment.
        WHEN abap_true.
          IF lv_text = '*/'.
            in_comment = abap_false.
            EXIT.
          ENDIF.
          IF strlen( lv_text ) > 0 AND lv_text(1) = '*'.
            IF strlen( lv_text ) > 2.
              APPEND |{ substring( val = lv_text off = 2 ) }| TO r_chunk.
            ELSE.
              APPEND || TO r_chunk.
            ENDIF.
          ELSE.
            in_comment = abap_false.
            EXIT.
          ENDIF.

        WHEN abap_false.
          IF 0 <= find( val   = lv_text
                        regex = '^\*\*/').
            IF strlen( lv_text ) > 3.
              APPEND lv_text+3 TO r_chunk.
            ENDIF.
            in_comment = abap_true.
          ENDIF.
      ENDCASE.
    ENDWHILE.
  ENDMETHOD.

  METHOD read_next_line.

    rv_text = mt_text[ 1 ].
    DELETE mt_text INDEX 1.


  ENDMETHOD.



  METHOD has_more_lines.
    r_result = boolc( mt_text IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.
