CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      strange_of_comments FOR TESTING RAISING cx_static_check,
      strange_of_comments2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD strange_of_comments.
    DATA s TYPE stringtab.
    DATA toks TYPE STANDARD TABLE OF zabap2md_token.
    s = VALUE #(  ( `  METHOD update_service_notification.`)
                  ( `    " 2020-12-08 / PJL / X0006715 / moved reading equis from remote out of loop.`)
                  ( `    " So this heavy duty function is only performed once.`)
                  ( `    " looking up the main equipment.`)
                  ( `*--------------------------------------------------------------------*`)
                  ( `* CRQ000000273264  Mia Sophie Behrendt                    30.04.2021 *`)
                  ( `* Es existiert ein Problem bei den Kombi-Aufträgen bei BAT gestoßen. *`)
                  ( `* In der Konstellation wurde die Option ‚Abbau mit Techniker‘        *`)
                  ( `* gewählt. Hier legt das Servicematerial 9960DX00057 einen Service-  *`)
                  ( `* Auftrag für den Abbau an.                                          *`)
                  ( `*--------------------------------------------------------------------*`)
                  ( `* CRQ000000383646  Ralf Guschmann, WSC GmbH               13.02.2023 *`)
                  ( `* DCA Phase 2: ZE Partner BD                                         *`)
                  ( `*--------------------------------------------------------------------*`)
                  ( ``)
                  ( `    DATA lo_notification     TYPE REF TO zcl_cs_notification.`)
                  ( `    DATA lv_notification     TYPE qmel-qmnum.`)
                  ( `    DATA lv_materialclas     TYPE c.`)
                  ( `    DATA lt_status           TYPE STANDARD TABLE OF jstat.`)    ).
    DATA toks_exp LIKE toks.
    DATA(l0) = CAST zif_abap2md_parser( NEW zcl_abap2md_comment_parser( s ) ).
    DO.
      DATA(token) = l0->next_token( ).
      IF token IS INITIAL.
        EXIT.
      ENDIF.
      APPEND token TO toks.
    ENDDO.
    toks_exp = VALUE #(
        ( type = 'LINE' line =  2 value = `2020-12-08 / PJL / X0006715 / moved reading equis from remote out of loop.`  )
        ( type = 'LINE' line =  3 value = `So this heavy duty function is only performed once.` )
        ( type = 'LINE' line =  4 value = `looking up the main equipment.` )
        ( type = 'LINE' line =  5 value = `--------------------------------------------------------------------*` )
        ( type = 'LINE' line =  6 value = `CRQ000000273264  Mia Sophie Behrendt                    30.04.2021 *` )
        ( type = 'LINE' line =  7 value = `Es existiert ein Problem bei den Kombi-Aufträgen bei BAT gestoßen. *` )
        ( type = 'LINE' line =  8 value = `In der Konstellation wurde die Option ‚Abbau mit Techniker‘        *` )
        ( type = 'LINE' line =  9 value = `gewählt. Hier legt das Servicematerial 9960DX00057 einen Service-  *` )
        ( type = 'LINE' line = 10 value = `Auftrag für den Abbau an.                                          *` )
        ( type = 'LINE' line = 11 value = `--------------------------------------------------------------------*` )
        ( type = 'LINE' line = 12 value = `CRQ000000383646  Ralf Guschmann, WSC GmbH               13.02.2023 *` )
        ( type = 'LINE' line = 13 value = `DCA Phase 2: ZE Partner BD                                         *` )
        ( type = 'LINE' line = 14 value = `--------------------------------------------------------------------*` )
     ).
    cl_abap_unit_assert=>assert_equals(  exp = toks_exp act = toks ).
  ENDMETHOD.

  METHOD strange_of_comments2.
    DATA s TYPE stringtab.
    DATA toks TYPE STANDARD TABLE OF zabap2md_token.
    s = VALUE #(    ( `  METHOD update_service_notification.` )
                    ( `    " 2020-12-08 / PJL / X0006715 / explanation.` )
                    ( `    " text.` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `* CRQ000000273264  Mia Sophie Behrendt                    30.04.2021 *` )
                    ( `* explanation.                                                       *` )
                    ( `* text.                                                              *` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `* CRQ000000383646  Ralf Guschmann, WSC GmbH               13.02.2023 *` )
                    ( `* text                                                               *` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `` )
                    ( `    DATA lo_notification     TYPE REF TO zcl_cs_notification.` )
                    ( `    DATA lv_notification     TYPE qmel-qmnum.` )
                    ( `    DATA lv_materialclas     TYPE c.` )
                    ( `    DATA lt_status           TYPE STANDARD TABLE OF jstat.` )      ).
    DATA toks_exp LIKE toks.
    DATA(l0) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( s ) ) ).
    DO.
      DATA(token) = l0->next_token( ).
      IF token IS INITIAL.
        EXIT.
      ENDIF.
      APPEND token TO toks.
    ENDDO.
    toks_exp = VALUE #(
            ( type = 'WORD' line = 10 value = `2020-12-08` )
            ( type = 'WORD' line = 10 value = `/` )
            ( type = 'WORD' line = 10 value = `PJL` )
            ( type = 'WORD' line = 10 value = `/` )
            ( type = 'WORD' line = 10 value = `X0006715` )
            ( type = 'WORD' line = 10 value = `/` )
            ( type = 'WORD' line = 10 value = `explanation` )
            ( type = 'WORD' line = 10 value = `.` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `text` )
            ( type = 'WORD' line = 10 value = `.` )
            ( type = 'WORD' line = 10 value = `--------------------------------------------------------------------*` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `CRQ000000273264` )
            ( type = 'WORD' line = 10 value = `Mia` )
            ( type = 'WORD' line = 10 value = `Sophie` )
            ( type = 'WORD' line = 10 value = `Behrendt` )
            ( type = 'WORD' line = 10 value = `30.04.2021` )
            ( type = 'WORD' line = 10 value = `*` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `explanation` )
            ( type = 'WORD' line = 10 value = `.` )
            ( type = 'WORD' line = 10 value = `*` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `text` )
            ( type = 'WORD' line = 10 value = `.` )
            ( type = 'WORD' line = 10 value = `*` )
            ( type = 'WORD' line = 10 value = `--------------------------------------------------------------------*` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `CRQ000000383646` )
            ( type = 'WORD' line = 10 value = `Ralf` )
            ( type = 'WORD' line = 10 value = `Guschmann,` )
            ( type = 'WORD' line = 10 value = `WSC` )
            ( type = 'WORD' line = 10 value = `GmbH` )
            ( type = 'WORD' line = 10 value = `13.02.2023` )
            ( type = 'WORD' line = 10 value = `*` )
            ( type = 'WORD' line = 10 value = `` )
            ( type = 'WORD' line = 10 value = `text` )
            ( type = 'WORD' line = 10 value = `*` )
            ( type = 'WORD' line = 10 value = `--------------------------------------------------------------------*` )
     ).
    cl_abap_unit_assert=>assert_equals(  exp = toks_exp act = toks ).
  ENDMETHOD.

ENDCLASS."* use this source file for your ABAP unit test classes
