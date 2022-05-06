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
  ENDMETHOD.

ENDCLASS.

CLASS lcl_tag_def_parser IMPLEMENTATION.

  METHOD constructor.

    me->src = i_src.
    mode = 'T'.
  ENDMETHOD.

  METHOD lif_parser~next_chunk.

    IF mt_chunk IS INITIAL.
      mt_chunk = src->next_chunk( ).
    ENDIF.
    WHILE lines( mt_chunk ) > 0.
      DATA(line) = mt_chunk[ 1 ].
      DELETE mt_chunk INDEX 1.
      SPLIT line AT '@' INTO TABLE DATA(lt_parts).

      WHILE lines( lt_parts ) > 0.
        DATA(plain) = |{ lt_parts[ 1 ] }|.
        CASE mode.
          WHEN 'T'.
            IF plain IS NOT INITIAL.
              APPEND plain TO ls_chunk.
            ENDIF.
            DELETE lt_parts INDEX 1.
            IF lines( lt_parts ) > 0.
              mode = 'K'.
            ENDIF.
          WHEN 'K'.
            DATA(x) = xsdbool( plain CA space ).
            DATA(idx) = sy-fdpos.
            ls_chunk = VALUE #( ( |@{ plain(idx) }| ) ).
            DELETE lt_parts INDEX 1.
        ENDCASE.
      ENDWHILE.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
