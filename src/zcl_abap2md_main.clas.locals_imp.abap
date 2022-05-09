*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_comment_parser IMPLEMENTATION.

  METHOD constructor.

    me->mt_text = i_text.

  ENDMETHOD.

  METHOD lif_parser~next_chunk.
    DATA in_comment TYPE abap_bool.
    DATA: lv_text LIKE LINE OF mt_text.

    LOOP AT mt_text INTO lv_text.
      IF lv_text = '*/'.
        in_comment = abap_false.
        EXIT.
      ENDIF.

      IF abap_true = in_comment.
        IF strlen( lv_text ) > 0 AND lv_text(1) = '*'.
          IF strlen( lv_text ) > 2.
            APPEND |{ substring( val = lv_text off = 2 ) }| TO ls_chunk.
          ELSE.
            APPEND || TO ls_chunk.
          ENDIF.
        ELSE.
          in_comment = abap_false.
          EXIT.
        ENDIF.
      ENDIF.

      IF lv_text = '**/'.
        in_comment = abap_true.
      ENDIF.
    ENDLOOP.
    CLEAR mt_text[].
  ENDMETHOD.

ENDCLASS.

CLASS lcl_tag_def_parser IMPLEMENTATION.

  METHOD constructor.

    me->src = i_src.
    mode = 'T'.
  ENDMETHOD.

  METHOD lif_parser~next_chunk.
    DATA out TYPE REF TO string.


    IF mt_chunk IS INITIAL.
      mt_chunk = src->next_chunk( ).
    ENDIF.

    IF pairs IS INITIAL.
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
              DATA(x) = xsdbool( part CA space ).
              DATA(idx) = sy-fdpos.
              IF idx >= strlen( part ).
                APPEND VALUE #( keyword = part )
                      TO pairs.
              ELSE.
                APPEND VALUE #( keyword = part(idx) text = substring( val = part off = idx + 1 ) )
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
      CLEAR mt_chunk[].
    ENDIF.

    IF mode = 'K'.
      mode = 'T'.
      APPEND INITIAL LINE TO ls_chunk REFERENCE INTO out.
      out->* = p-text.
      DELETE pairs INDEX 1.
    ENDIF.

    WHILE pairs IS NOT INITIAL.
      p = pairs[ 1 ].
      CASE p-keyword.
        WHEN '@n'.
          mode = 'T'.
          APPEND INITIAL LINE TO ls_chunk REFERENCE INTO out.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN ``.
          mode = 'T'.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN OTHERS.
          IF mode IS INITIAL.
            ls_chunk = VALUE #( ( |@{ p-keyword }| ) ).
            mode = 'K'.
          ELSE.
            CLEAR mode.
          ENDIF.
          RETURN.
      ENDCASE.
    ENDWHILE.



  ENDMETHOD.

ENDCLASS.
