CLASS zcl_abap2md_comment_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abap2md_parser .

    METHODS constructor
      IMPORTING
        !i_text TYPE rswsourcet .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_text TYPE rswsourcet.
    DATA: m_regex   TYPE REF TO cl_abap_regex,
          m_matcher TYPE REF TO cl_abap_matcher.
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


  METHOD has_more_lines.
    r_result = boolc( mt_text IS NOT INITIAL ).
  ENDMETHOD.


  METHOD read_next_line.

    rv_text = mt_text[ 1 ].
    DELETE mt_text INDEX 1.


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


  METHOD zif_abap2md_parser~next_token.
    IF m_regex IS INITIAL.
      m_regex = NEW cl_abap_regex(
          pattern       = |^(\\*\\*/) *\|^\\*(.*$)\|^(\\*/) *$|
      ).
    ENDIF.
    IF m_matcher IS INITIAL.
      m_matcher = m_regex->create_matcher(
          table         = mt_text    " Table to be Searched in
      ).
    ENDIF.

    IF m_matcher->find_next( ).
      DATA(s1) = m_matcher->get_submatch( 1 ).
      IF s1 IS NOT INITIAL.
        r_result = VALUE #( type = 'START' ).
      ENDIF.
      DATA(s2) = m_matcher->get_submatch( 2 ).
      IF s2 IS NOT INITIAL.
        r_result = VALUE #( type = 'LINE'  ).
      ENDIF.
      DATA(s3) = m_matcher->get_submatch( 3 ).
      IF s3 IS NOT INITIAL.
        r_result = VALUE #( type = 'END'  ).
      ENDIF.
    ENDIF.
*      CATCH cx_sy_matcher.    "
*  CATCH cx_sy_regex.  "
  ENDMETHOD.
ENDCLASS.
