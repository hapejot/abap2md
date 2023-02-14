CLASS zcl_abap2md_tag_def_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_parser.
    METHODS constructor
      IMPORTING
        i_src  TYPE REF TO zif_abap2md_parser OPTIONAL
        i_text TYPE REF TO zabap2md_text OPTIONAL
          PREFERRED PARAMETER i_src .
  PRIVATE SECTION.
    TYPES: BEGIN OF pair,
             keyword TYPE string,
             text    TYPE string,
           END OF pair.
    DATA src TYPE REF TO zif_abap2md_parser.
    DATA mt_chunk TYPE rswsourcet.
    DATA mode TYPE c.
    DATA: pairs TYPE STANDARD TABLE OF pair,
          p     TYPE pair.
ENDCLASS.



CLASS zcl_abap2md_tag_def_parser IMPLEMENTATION.

  METHOD constructor.

    me->src = i_src.

    IF i_text IS NOT INITIAL.
      me->mt_chunk = i_text->*.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abap2md_parser~next_chunk.
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

  ENDMETHOD.

ENDCLASS.
