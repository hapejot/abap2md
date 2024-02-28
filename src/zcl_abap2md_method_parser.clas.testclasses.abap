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
                  ( `    " 2020-12-08 / ABC / U1234567 / moved reading equis from remote out of loop.`)
                  ( `    " So this heavy duty function is only performed once.`)
                  ( `    " looking up the main equipment.`)
                  ( `*--------------------------------------------------------------------*`)
                  ( `* CRQ000000273264  Max Mustermann-Baumann                 30.04.2021 *`)
                  ( `* Es existiert ein Problem bei den Kombi-Aufträgen bei BAT gestoßen. *`)
                  ( `* In der Konstellation wurde die Option ‚Abbau mit Techniker‘        *`)
                  ( `* gewählt. Hier legt das Servicematerial 9960DX00057 einen Service-  *`)
                  ( `* Auftrag für den Abbau an.                                          *`)
                  ( `*--------------------------------------------------------------------*`)
                  ( `* CRQ000000383646  Moriz Baumann-Mustermann               13.02.2023 *`)
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
        ( type = 'LINE' line =  2 value = `2020-12-08 / ABC / U1234567 / moved reading equis from remote out of loop.`  )
        ( type = 'LINE' line =  3 value = `So this heavy duty function is only performed once.` )
        ( type = 'LINE' line =  4 value = `looking up the main equipment.` )
        ( type = 'LINE' line =  5 value = `--------------------------------------------------------------------*` )
        ( type = 'LINE' line =  6 value = `CRQ000000273264  Max Mustermann-Baumann                 30.04.2021 *` )
        ( type = 'LINE' line =  7 value = `Es existiert ein Problem bei den Kombi-Aufträgen bei BAT gestoßen. *` )
        ( type = 'LINE' line =  8 value = `In der Konstellation wurde die Option ‚Abbau mit Techniker‘        *` )
        ( type = 'LINE' line =  9 value = `gewählt. Hier legt das Servicematerial 9960DX00057 einen Service-  *` )
        ( type = 'LINE' line = 10 value = `Auftrag für den Abbau an.                                          *` )
        ( type = 'LINE' line = 11 value = `--------------------------------------------------------------------*` )
        ( type = 'LINE' line = 12 value = `CRQ000000383646  Moriz Baumann-Mustermann               13.02.2023 *` )
        ( type = 'LINE' line = 13 value = `DCA Phase 2: ZE Partner BD                                         *` )
        ( type = 'LINE' line = 14 value = `--------------------------------------------------------------------*` )
     ).
    cl_abap_unit_assert=>assert_equals(  exp = toks_exp act = toks ).
  ENDMETHOD.

  METHOD strange_of_comments2.
    DATA s TYPE stringtab.
    DATA toks TYPE STANDARD TABLE OF zabap2md_token.
    s = VALUE #(    ( `  METHOD update_service_notification.` )
                    ( `**/` )
                    ( `    " 2020-12-08 / ABC / U1234567 / explanation.` )
                    ( `    " text.` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `* CRQ000000273264  Max Mustermann-Baumann                 30.04.2021 *` )
                    ( `* explanation.                                                       *` )
                    ( `* text.                                                              *` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `* CRQ000000383646  Moriz Baumann-Mustermann               13.02.2023 *` )
                    ( `* text                                                               *` )
                    ( `*--------------------------------------------------------------------*` )
                    ( `*/` )
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
            ( type = 'START' line = 1 )
            ( type = 'DATE' line = 1 value = `20201208` )
            ( type = 'SEP'  line = 1 value = ` / ` )
            ( type = 'WORD' line = 1 value = `ABC` )
            ( type = 'SEP'  line = 1 value = ` / ` )
            ( type = 'WORD' line = 1 value = `U1234567` )
            ( type = 'SEP'  line = 1 value = ` / ` )
            ( type = 'WORD' line = 1 value = `explanation` )
            ( type = 'SEP'  line = 1 value = `.` )
            ( type = 'WORD' line = 2 value = `text` )
            ( type = 'SEP'  line = 2 value = `.` )
            ( type = 'WORD' line = 3 value = `--------------------------------------------------------------------*` )
            ( type = 'WORD' line = 4 value = `CRQ000000273264` )
            ( type = 'WORD' line = 4 value = `Max` )
            ( type = 'WORD' line = 4 value = `Mustermann-Baumann` )
            ( type = 'WORD' line = 4 value = `30` )
            ( type = 'SEP'  line = 4 value = `.` )
            ( type = 'WORD' line = 4 value = `04` )
            ( type = 'SEP'  line = 4 value = `.` )
            ( type = 'WORD' line = 4 value = `2021` )
            ( type = 'WORD' line = 4 value = `*` )
            ( type = 'WORD' line = 5 value = `explanation` )
            ( type = 'SEP'  line = 5 value = `.                                                       ` )
            ( type = 'WORD' line = 5 value = `*` )
            ( type = 'WORD' line = 6 value = `text` )
            ( type = 'SEP'  line = 6 value = `.                                                              ` )
            ( type = 'WORD' line = 6 value = `*` )
            ( type = 'WORD' line = 7 value = `--------------------------------------------------------------------*` )
            ( type = 'WORD' line = 8 value = `CRQ000000383646` )
            ( type = 'WORD' line = 8 value = `Moriz` )
            ( type = 'WORD' line = 8 value = `Baumann-Mustermann` )
            ( type = 'WORD' line = 8 value = `13` )
            ( type = 'SEP'  line = 8 value = `.` )
            ( type = 'WORD' line = 8 value = `02` )
            ( type = 'SEP'  line = 8 value = `.` )
            ( type = 'WORD' line = 8 value = `2023` )
            ( type = 'WORD' line = 8 value = `*` )
            ( type = 'WORD' line = 9 value = `text` )
            ( type = 'WORD' line = 9 value = `*` )
            ( type = 'WORD' line = 10 value = `--------------------------------------------------------------------*` )
            ( type = 'END' )
     ).
    cl_abap_unit_assert=>assert_equals(  exp = toks_exp act = toks ).
  ENDMETHOD.

ENDCLASS."* use this source file for your ABAP unit test classes
