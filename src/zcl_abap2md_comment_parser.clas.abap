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
          m_matcher TYPE REF TO cl_abap_matcher,
          m_pushed  TYPE zabap2md_token.
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



  METHOD zif_abap2md_parser~next_token.
    DATA rules TYPE stringtab.
    rules = VALUE #(    ( `^(\*\*/) *$` )       " #1
                        ( `^(\*\*/) (.*)$` )    " #2 #3
                        ( `^(\*/)$` )           " #4
                        ( `^(\*) ?(.*)$` )      " #5 #6
                        ( `^ *" (.*)$` )        " #7
                         ).
    IF m_regex IS INITIAL.
      CONCATENATE LINES OF rules INTO DATA(pattern) SEPARATED BY '|'.
      m_regex = NEW cl_abap_regex( pattern ).
    ENDIF.
    IF m_matcher IS INITIAL.
      m_matcher = m_regex->create_matcher(
          table         = mt_text    " Table to be Searched in
      ).
    ENDIF.
    IF m_pushed IS INITIAL.
      IF m_matcher->find_next( ).
        DATA(s1) = m_matcher->get_submatch( 1 ).
        DATA(s2) = m_matcher->get_submatch( 2 ).
        DATA(s3) = m_matcher->get_submatch( 3 ).
        DATA(s4) = m_matcher->get_submatch( 4 ).
        DATA(s5) = m_matcher->get_submatch( 5 ).
        DATA(s6) = m_matcher->get_submatch( 6 ).
        DATA(s7) = m_matcher->get_submatch( 7 ).
        DATA(line) = m_matcher->get_line( ).
        CLEAR r_result. " not sure if this is needed
        IF s1 IS NOT INITIAL.
          r_result = VALUE #( type = 'START' ).
        ELSEIF s2 IS NOT INITIAL.
          r_result = VALUE #( type = 'START'  ).
          IF s3 IS NOT INITIAL.
            m_pushed = VALUE #(  type = 'LINE' value = s3 line = line ).
          ENDIF.
        ELSEIF s5 IS NOT INITIAL.
          r_result = VALUE #( type = 'LINE'  value = s6  ).
        ELSEIF s4 IS NOT INITIAL.
          r_result = VALUE #( type = 'END'  ).
        ELSEIF s7 IS NOT INITIAL.
          r_result = VALUE #( type = 'LINE'  value = s7  ).
        ENDIF.
        IF r_result IS NOT INITIAL.
          r_result-line = line.
        ENDIF.
      ENDIF.
    ELSE.
      r_result = m_pushed.
      CLEAR m_pushed.
    ENDIF.
*      CATCH cx_sy_matcher.    "
*  CATCH cx_sy_regex.  "
  ENDMETHOD.
ENDCLASS.
