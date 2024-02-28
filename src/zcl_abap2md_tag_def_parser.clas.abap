CLASS zcl_abap2md_tag_def_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abap2md_parser .

    METHODS constructor
      IMPORTING
        !i_src  TYPE REF TO zif_abap2md_parser OPTIONAL
        !i_text TYPE REF TO zabap2md_text OPTIONAL
          PREFERRED PARAMETER i_src .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF pair,
             keyword TYPE string,
             text    TYPE string,
           END OF pair.
    DATA src TYPE REF TO zif_abap2md_parser.
    DATA mt_chunk TYPE rswsourcet.
    DATA mode TYPE c.
    DATA: pairs     TYPE STANDARD TABLE OF pair,
          p         TYPE pair,
          m_regex   TYPE REF TO cl_abap_regex,
          m_pushed  TYPE zabap2md_token,
          m_matcher TYPE REF TO cl_abap_matcher.
ENDCLASS.



CLASS zcl_abap2md_tag_def_parser IMPLEMENTATION.


  METHOD constructor.

    me->src = i_src.

    IF i_text IS NOT INITIAL.
      me->mt_chunk = i_text->*.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abap2md_parser~next_chunk.
    ASSERT 1 = 0. " ensure error if used. This stuff is outdated.
    DATA out TYPE REF TO string.

    " we have pairs to work on, so we return those otherwise
    " we extract the pairs form *mt_chunk*
    IF pairs IS INITIAL.
      " read raw data if nothing is there to work on.
      IF mt_chunk IS INITIAL.
        IF src IS BOUND.
          mt_chunk = src->next_chunk( ).
        ENDIF.
        CLEAR mode.
      ENDIF.
      LOOP AT mt_chunk INTO DATA(line).
        SPLIT line AT '@' INTO TABLE DATA(lt_parts).
        LOOP AT lt_parts INTO DATA(part).
          IF sy-tabix > 1.
            IF strlen( part ) = 0.
              DATA(part_idx) = sy-tabix.
              APPEND VALUE #( text = |@{ lt_parts[ part_idx + 1 ] }| )
                    TO pairs.
              DELETE lt_parts INDEX part_idx + 1.
            ELSE.
              " finds the first word in *part* by searching for a space.
              DATA(x) = xsdbool( part CA space ).
              DATA(idx) = sy-fdpos.
              IF idx >= strlen( part ).
                APPEND VALUE #( keyword = part )
                      TO pairs.
              ELSE.
                APPEND VALUE #( keyword = part(idx) text = condense( val = substring( val = part off = idx + 1 ) from = `` ) ) " from -  empty should remove only leading and trailing blanks.
                      TO pairs.
              ENDIF.
            ENDIF.
          ELSE.
            " if there are @ signs, lines are > 1 and we do not add empty lines
            CONDENSE part.
            IF lines( lt_parts ) = 1 OR part IS NOT INITIAL.
              APPEND VALUE #( keyword = '@n' text = part ) TO pairs.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF sy-subrc = 4. " no parts means empty line... they should be preserved.
          APPEND VALUE #( keyword = '@n' ) TO pairs.
        ENDIF.
      ENDLOOP.
      IF mt_chunk IS NOT INITIAL.
        APPEND VALUE #( keyword = '@c' ) TO pairs.
      ENDIF.
      CLEAR mt_chunk[].
    ENDIF.

    IF mode = 'K'.
      mode = 'T'.
      APPEND INITIAL LINE TO r_chunk REFERENCE INTO out.
      out->* = p-text.
      DELETE pairs INDEX 1.
    ENDIF.

    WHILE pairs IS NOT INITIAL.
      p = pairs[ 1 ].
      CASE p-keyword.
        WHEN '@n'.
          mode = 'T'.
          APPEND INITIAL LINE TO r_chunk REFERENCE INTO out.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN ``.
          mode = 'T'.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN OTHERS.
          IF mode IS INITIAL.
            r_chunk = VALUE #( ( |@{ p-keyword }| ) ).
            mode = 'K'.
          ELSE.
            CLEAR mode.
          ENDIF.
          RETURN.
      ENDCASE.
    ENDWHILE.



  ENDMETHOD.


  METHOD zif_abap2md_parser~next_token.
    DATA: rules TYPE stringtab,
          token TYPE zabap2md_token,
          lines TYPE stringtab.
    rules = VALUE #(    ( |^(```[\{][.][a-zA-Z]+)|                        ) " F
                        ( `^ *([0-9]{1,4}[.-][0-9]{1,2}[.-][0-9]{1,4})`   ) " A
                        ( `@([a-zA-Z0-9]+) *`                             ) " B
                        ( `(@@)`                                          ) " G
                        ( `( *[.,;:/] *)`                                 ) " C
                        ( ` *([^ .,;:/@]+)`                               ) " D regular words
                        ( `^( *)$`                                        ) " E
                        ).
    IF m_regex IS INITIAL.
      CONCATENATE LINES OF rules INTO DATA(pattern) SEPARATED BY '|'.
      m_regex = NEW cl_abap_regex( pattern ).
    ENDIF.
    IF m_matcher IS INITIAL.
      DO.
        token = src->next_token( ).
        IF token-type = 'START' OR token IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.
      IF token-type = 'START'.
        DO.
          token = src->next_token( ).
          IF token-type = 'LINE'.
            APPEND token-value TO lines.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        m_matcher = m_regex->create_matcher(
            table         = lines    " Table to be Searched in
        ).
        m_pushed = VALUE #( type = 'START' line = 1 ).
      ENDIF.
    ENDIF.
    IF m_pushed IS INITIAL AND m_matcher IS BOUND.
      IF m_matcher->find_next( ).
        DATA(s_a) = m_matcher->get_submatch( 2 ).
        DATA(s_b) = m_matcher->get_submatch( 3 ).
        DATA(s_c) = m_matcher->get_submatch( 5 ).
        DATA(s_d) = m_matcher->get_submatch( 6 ).
        DATA(s_e) = m_matcher->get_submatch( 7 ).
        DATA(s_f) = m_matcher->get_submatch( 1 ).
        DATA(s_g) = m_matcher->get_submatch( 4 ).
        r_result = VALUE #( line = m_matcher->get_line( ) ).
        IF s_b IS NOT INITIAL.
          r_result-type = 'CMD'.
          r_result-value = s_b.
        ELSEIF s_g IS NOT INITIAL.
          r_result-type = 'CMD'.
          r_result-value = '@'.
        ELSEIF s_c IS NOT INITIAL.
          r_result-type = 'SEP'.
          r_result-value = s_c.
        ELSEIF s_d IS NOT INITIAL.
          r_result-type = 'WORD'.
          r_result-value = s_d.
        ELSEIF s_a IS NOT INITIAL.
          DATA year(4) TYPE c.
          DATA month(2) TYPE c.
          DATA day(2) TYPE c.
          SPLIT s_a AT '.' INTO day month year.
          IF year IS INITIAL.
            SPLIT s_a AT '-' INTO year month day.
          ENDIF.
          r_result-type = 'DATE'.

          r_result-value = |{ year }{
                                                      month ALIGN = RIGHT PAD = '0' WIDTH = 2 }{
                                                      day ALIGN = RIGHT PAD = '0' WIDTH = 2 }|.
        ELSEIF s_f IS NOT INITIAL.
          r_result-type = 'WORD'.
          r_result-value = s_f.
        ELSE. " the only alternative with an empty result so we cannot check for it.
          r_result-type = 'PARSEP'.
        ENDIF.
      ELSE.
        CLEAR m_matcher.
        r_result-type = 'END'.
      ENDIF.
    ELSE.
      r_result = m_pushed.
      CLEAR m_pushed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
