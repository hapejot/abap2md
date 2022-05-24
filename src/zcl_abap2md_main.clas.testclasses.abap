CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: comment_parse_stg1 FOR TESTING RAISING cx_static_check,
      comment_parse_stg2 FOR TESTING RAISING cx_static_check.
    METHODS get_code_fragment
      RETURNING
        VALUE(rs_meth) TYPE rswsourcet.
ENDCLASS.
CLASS zcl_abap2md_main DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main IMPLEMENTATION.


  METHOD comment_parse_stg1.
    DATA chunk TYPE rswsourcet.

    DATA(lv_code) = get_code_fragment( ).
    DATA(cut) = CAST lif_parser( NEW lcl_comment_parser( lv_code ) ).
    chunk = VALUE #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*@@EXCEPTION*, *@@PARAM*, *@@RETURN*, *@@THROWS*.| )
                ( || )
                ( || )
                ( |@param EV_NEXT_TAG        Next/Last found tag| )
                ( |@param EV_TAG_VALUE       Tag value| )
                ( |@param et_description     Description found until found tag| )
                ( |@param   CV_NEXT_INDENT     Next indent  to  be used| )
                ( |@param CT_SOURCE          Source description| )
                ( || )
                ( |@return abap_bool *abap_true* if succesfull,| )
                ( |                  *abap_false* otherwise| )
                ( |@raising cx_nothing          Exception for nothing.| )
                ( |@throws cx_something         Exception for something.| )
                ( |@exception cx_anything       Exception for anything.| )
    ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = chunk act = cut->next_chunk( ) ).

  ENDMETHOD.

  METHOD comment_parse_stg2.
    DATA chunks TYPE STANDARD TABLE OF rswsourcet.

    DATA(lv_code) = get_code_fragment( ).
    DATA(cut) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( lv_code ) ) ).
    APPEND VALUE #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*@EXCEPTION*, *@PARAM*, *@RETURN*, *@THROWS*.| )
                ( || )
                ( || ) )                                                        TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( (  |EV_NEXT_TAG        Next/Last found tag| ) )             TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |EV_TAG_VALUE       Tag value| ) )                        TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |et_description     Description found until found tag| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |CV_NEXT_INDENT     Next indent  to  be used| ) )           TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |CT_SOURCE          Source description| )
                    ( || )  )                                                   TO chunks.
    APPEND VALUE #( ( |@return| ) )                                             TO chunks.
    APPEND VALUE #( ( |abap_bool *abap_true* if succesfull,| )
                    ( |*abap_false* otherwise| ) )                              TO chunks.
    APPEND VALUE #( ( |@raising| ) )                                            TO chunks.
    APPEND VALUE #( ( |cx_nothing          Exception for nothing.| )  )         TO chunks.
    APPEND VALUE #( ( |@throws| ) )                                             TO chunks.
    APPEND VALUE #( ( |cx_something         Exception for something.| )  )      TO chunks.
    APPEND VALUE #( ( |@exception| ) )                                          TO chunks.
    APPEND VALUE #( ( |cx_anything       Exception for anything.| )  )          TO chunks.
    APPEND VALUE #( )                                                           TO chunks.

    LOOP AT chunks INTO DATA(chunk).
      cl_abap_unit_assert=>assert_equals( msg = |{ sy-tabix }| exp = chunk act = cut->next_chunk( ) ).
    ENDLOOP.

  ENDMETHOD.



  METHOD get_code_fragment.

    rs_meth  = VALUE #(
                ( |  METHOD find_next_jd_tag.                                            | )
                ( |**/| )
                ( |* This method collects the description until the next JavaDoc-like tag| )
                ( |* Known tag values:| )
                ( |* *@@EXCEPTION*, *@@PARAM*, *@@RETURN*, *@@THROWS*.| )
                ( |*| )
                ( |*| )
                ( |* @param EV_NEXT_TAG        Next/Last found tag| )
                ( |* @param EV_TAG_VALUE       Tag value| )
                ( |* @param et_description     Description found until found tag| )
                ( |* @param   CV_NEXT_INDENT     Next indent  to  be used| )  " test irregular spaces
                ( |* @param CT_SOURCE          Source description| )
                ( |*| )
                ( |* @return abap_bool *abap_true* if succesfull,| )
                ( |*                   *abap_false* otherwise| )
                ( |* @raising cx_nothing          Exception for nothing.| )
                ( |* @throws cx_something         Exception for something.| )
                ( |* @exception cx_anything       Exception for anything.| )
                ( |*/| )
                ( || )
                ( || )
                ( |    DATA lt_source            TYPE rswsourcet.| )
                ( |    DATA lv_source            TYPE string.| )
                ( |    DATA lv_source_tmp        TYPE string.| )
                ( |    DATA lv_source_before     TYPE string.| )
            ).


  ENDMETHOD.



ENDCLASS.

CLASS ltcl_markdown DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      generate FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_markdown IMPLEMENTATION.


  METHOD generate.
    DATA(cut) = CAST lif_text_generator( NEW lcl_markdown( ) ).
    cl_abap_unit_assert=>assert_bound( cut ).
    DATA(code) = VALUE stringtab(
            ( `PUBLIC METHOD CONSTRUCTOR` )
            ( `IMPORTING` )
            ( `    PREVIOUS                            TYPE PREVIOUS` )
            ( `    TEXTID                              TYPE IF_T100_MESSAGE=>T100KEY` ) ).
    cut->heading(
            iv_level = 1
            iv_text  = `NAME`
    )->text( `ZCX_ABAP2MD_ERROR - Converter Error`
    )->heading(
            iv_level = 2
            iv_text = `CONSTRUCTOR`
    )->text( `CONSTRUCTOR`
    )->new_paragraph(
    )->text( value stringtab( ( `some additional text here for showing this is a` )
                              ( `multiline paragraph.` ) )
    )->code( code
    )->definition(
            iv_text = `reference to the previous exception if any. Initial if unknown.`
            iv_def = `PREVIOUS`
    )->definition(
            iv_text = VALUE stringtab( ( `Text ID referring to the text` )
                                       ( `defining the message that will be used.`) )
            iv_def = `TEXTID`
    ).
    DATA(text) = cut->result( ).

    DATA(expected_text) = VALUE stringtab(
            ( `# NAME` )
            (  )
            ( `ZCX_ABAP2MD_ERROR - Converter Error` )
            (  )
            ( `## CONSTRUCTOR` )
            (  )
            ( `CONSTRUCTOR` )
            (  )
            ( `some additional text here for showing this is a` )
            ( `multiline paragraph.` )
            ( )
            ( `    PUBLIC METHOD CONSTRUCTOR` )
            ( `    IMPORTING` )
            ( `        PREVIOUS                            TYPE PREVIOUS` )
            ( `        TEXTID                              TYPE IF_T100_MESSAGE=>T100KEY` )
            ( )
            ( `**PREVIOUS**` )
            ( `:  reference to the previous exception if any. Initial if unknown.` )
            ( )
            ( `**TEXTID**` )
            ( `:  Text ID referring to the text` )
            ( `   defining the message that will be used.` )
    ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text act = text ).
  ENDMETHOD.

ENDCLASS.
