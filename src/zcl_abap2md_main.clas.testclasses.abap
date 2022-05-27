CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mv_code TYPE stringtab.
    METHODS: comment_parse_stg1 FOR TESTING RAISING cx_static_check,
      comment_parse_stg2 FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.
CLASS zcl_abap2md_main DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.
    mv_code  = VALUE stringtab(
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

  METHOD comment_parse_stg1.
    DATA chunk TYPE rswsourcet.

    DATA(cut) = CAST lif_parser( NEW lcl_comment_parser( mv_code ) ).
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

    DATA(cut) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( mv_code ) ) ).
    APPEND VALUE #( ( |This method collects the description until the next JavaDoc-like tag| )
                    ( |Known tag values:| )
                    ( |*@EXCEPTION*, *@PARAM*, *@RETURN*, *@THROWS*.| )
                    ( || )
                    ( || ) )                                                    TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |EV_NEXT_TAG        Next/Last found tag| ) )              TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |EV_TAG_VALUE       Tag value| ) )                        TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |et_description     Description found until found tag| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) )                                              TO chunks.
    APPEND VALUE #( ( |CV_NEXT_INDENT     Next indent  to  be used| ) )         TO chunks.
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
    APPEND VALUE #( ( |@@c| ) )                                                 TO chunks.
    APPEND VALUE #( ( ) )                                                       TO chunks.
    APPEND VALUE #( )                                                           TO chunks.

    LOOP AT chunks INTO DATA(chunk).
      cl_abap_unit_assert=>assert_equals( msg = |{ sy-tabix }| exp = chunk act = cut->next_chunk( ) ).
    ENDLOOP.

  ENDMETHOD.







ENDCLASS.

CLASS ltcl_parse_comment_multi DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mv_code TYPE stringtab.
    METHODS setup.
    METHODS parse_page_cmd FOR TESTING RAISING cx_static_check.
    METHODS parse_comments FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_parse_comment_multi IMPLEMENTATION.
  METHOD setup.
    mv_code = VALUE stringtab(
                    ( `**/` )
                    ( `* @page page1 A documentation page` )
                    ( `* @tableofcontents` )
                    ( `* Leading text.` )
                    ( `* @section sec An example section` )
                    ( `* This page contains the subsections @ref subsection1 and @ref subsection2.` )
                    ( `* For more info see page @ref page2.` )
                    ( `* @subsection subsection1 The first subsection` )
                    ( `* Text.` )
                    ( `* @subsection subsection2 The second subsection` )
                    ( `* More text.` )
                    ( `*/` )
                    ( ` ` )
                    ( `**/ @page page2 Another page` )
                    ( `*  Even more info.` )
                    ( `*/` ) ).

  ENDMETHOD.

  METHOD parse_comments.
    DATA(cut) = CAST lif_parser( NEW lcl_comment_parser( mv_code ) ).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = lines( cut->next_chunk( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2  act = lines( cut->next_chunk( ) ) ) .
    cl_abap_unit_assert=>assert_initial( act = cut->next_chunk( ) ).
  ENDMETHOD.


  METHOD parse_page_cmd.
    DATA chunks TYPE STANDARD TABLE OF rswsourcet.

    DATA(cut) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( mv_code ) ) ).
    APPEND VALUE #( ( `@page` ) )                                               TO chunks.
    APPEND VALUE #( ( `page1 A documentation page` ) )                          TO chunks.
    APPEND VALUE #( ( `@tableofcontents` ) )                                    TO chunks.
    APPEND VALUE #( ( ) ( `Leading text.` ) )                                   TO chunks.
    APPEND VALUE #( ( `@section` ) )                                            TO chunks.

    APPEND VALUE #( ( `sec An example section` )
                    ( `This page contains the subsections` ) )                  TO chunks.
    APPEND VALUE #( ( `@ref` ) )                                                TO chunks.
    APPEND VALUE #( ( `subsection1 and` ) )                                     TO chunks.
    APPEND VALUE #( ( `@ref` ) )                                                TO chunks.
    APPEND VALUE #( ( `subsection2.` ) ( `For more info see page` ) )           TO chunks.

    APPEND VALUE #( ( `@ref` ) )                                                TO chunks.
    APPEND VALUE #( ( `page2.` ) )                                              TO chunks.
    APPEND VALUE #( ( `@subsection` ) )                                         TO chunks.
    APPEND VALUE #( ( `subsection1 The first subsection` ) ( `Text.` ) )        TO chunks.

    APPEND VALUE #( ( `@subsection` ) )                                         TO chunks.
    APPEND VALUE #( ( `subsection2 The second subsection` ) ( `More text.` ) )  TO chunks.
    APPEND VALUE #( ( `@@c` ) )                                                 TO chunks.
    APPEND VALUE #( ( ) )                                                       TO chunks.

    APPEND VALUE #( ( `@page` ) )                                               TO chunks.
    APPEND VALUE #( ( `page2 Another page` ) ( `Even more info.` ) )            TO chunks.
    APPEND VALUE #( ( `@@c` ) )                                                 TO chunks.
    APPEND VALUE #( ( ) )                                                       TO chunks.
    APPEND VALUE #(  )                                                       TO chunks.

    LOOP AT chunks INTO DATA(chunk).
      cl_abap_unit_assert=>assert_equals( msg = |{ sy-tabix }| exp = chunk act = cut->next_chunk( ) ).
    ENDLOOP.

  ENDMETHOD.



ENDCLASS.



CLASS ltcl_markdown DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      generate FOR TESTING RAISING cx_static_check,
      second_heading FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_markdown IMPLEMENTATION.

  METHOD second_heading.
    DATA(cut) = CAST lif_text_generator( NEW lcl_markdown( ) ).
    cl_abap_unit_assert=>assert_bound( cut ).
    cut->heading(
            iv_level = 1
            iv_text  = `First`
      )->text( 'Text.'
      )->heading(
            iv_level = 1
            iv_text = 'Second'
      )->definition(
        iv_text = 'Text.'
        iv_def  = 'Def'
      )->heading(
        iv_level = 1
        iv_text  = 'Third'
      )->text( 'End.' ).
    DATA(text) = cut->result( ).

    cl_abap_unit_assert=>assert_equals(
            msg = 'result'
            exp = VALUE stringtab(
                    ( |# First| )
                    ( )
                    ( |Text.| )
                    (  )
                    ( |# Second| )
                    ( )
                    ( )
                    ( |**Def**| )
                    ( |:  Text.| )
                    ( )
                    ( |# Third| )
                    ( )
                    ( |End.| )
                     )
            act = text ).
  ENDMETHOD.


  METHOD generate.
    TYPES: BEGIN OF row,
             no    TYPE i,
             name  TYPE string,
             value TYPE string,
           END OF row.
    DATA sample_table TYPE STANDARD TABLE OF row.

    sample_table = VALUE #( ( no = 1 name = 'Peter' value = 'Was auch immer' )
                            ( no = 2 name = 'Karin' value = 'Krank' ) ).

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
    )->text( sample_table
    )->text( VALUE stringtab( ( `some additional text here for showing this is a` )
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
            ( ` NO | NAME  | VALUE         ` )
            ( `----|-------|----------------` )
            ( ` 1  | Peter | Was auch immer` )
            ( ` 2  | Karin | Krank         ` )
            ( )
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


CLASS ltcl_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mv_code TYPE stringtab.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check,
      setup.
ENDCLASS.


CLASS ltcl_generator IMPLEMENTATION.

  METHOD setup.
    mv_code = VALUE stringtab(
                    ( `**/` )
                    ( `* Outside Text before any page.` )
                    ( `* @page page1 A documentation page` )
                    ( `* @tableofcontents` )
                    ( `* Leading text.` )
                    ( `* @section sec An example section` )
                    ( `* This page contains the subsections @ref subsection1 and @ref subsection2.` )
                    ( `* For more info see page @ref page2.` )
                    ( `* @subsection subsection1 The first subsection` )
                    ( `* Text.` )
                    ( `* @subsection subsection2 The second subsection` )
                    ( `* More text.` )
                    ( `*/` )
                    ( ` ` )
                    ( `**/ @page page2 Another page` )
                    ( `*  Even more info.` )
                    ( `*/` )
                    ( `**/ Just some more text outside of a page` )
                    ( `*  this belongs then to the current element documentation.` )
                    ( `*/` )
     ).

  ENDMETHOD.

  METHOD first_test.
    DATA main_text TYPE stringtab.
    DATA(cut) = NEW lcl_doc_generator( REF #( main_text ) ).
    cl_abap_unit_assert=>assert_bound( cut ).
    cut->add_text( mv_code ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of pages'
                                        exp = 2
                                        act = lines( cut->m_pages ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'name of first page'
                                        exp = 'page1'
                                        act = cut->m_pages[ 1 ]-name ).
    cl_abap_unit_assert=>assert_equals( msg = 'content of first page'
                                        exp = VALUE stringtab(
                                                      ( `@tableofcontents` )
                                                      ( )
                                                      ( `Leading text.` ) )
                                        act = cut->m_pages[ 1 ]-text ).
    cl_abap_unit_assert=>assert_equals( msg = 'sections of first page'
                                        exp = 1
                                        act = lines( cut->m_pages[ 1 ]-sections ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'subsections of first page'
                                        exp = 2
                                        act = lines( cut->m_pages[ 1 ]-sections[ 1 ]-subsections ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'name of 2nd subsection'
                                        exp = 'subsection2'
                                        act = cut->m_pages[ 1 ]-sections[ 1 ]-subsections[ 2 ]-name ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of lines of 2nd subsection'
                                        exp = 1
                                        act = lines( cut->m_pages[ 1 ]-sections[ 1 ]-subsections[ 2 ]-text ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'name of 2nd page'
                                        exp = 'page2'
                                        act = cut->m_pages[ 2 ]-name ).
    cl_abap_unit_assert=>assert_equals( msg = 'outside text'
                                        exp = VALUE stringtab(
                                                    ( |Outside Text before any page.| )
                                                    (  )
                                                    (  )
                                                    ( |Just some more text outside of a page| )
                                                    ( |this belongs then to the current element documentation.| )
                                                    (  ) )
                                        act = main_text ).

*    cl_abap_unit_assert=>assert_equals( msg = 'length outside text' exp = 1 act = lines( main_text ) ).
*    cl_abap_unit_assert=>assert_equals( msg = 'outside text' exp = 'Outside Text before any page.' act = main_text[ 1 ] ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_gen_multi_comps DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      generate FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_gen_multi_comps IMPLEMENTATION.

  METHOD generate.
    DATA(cut) = NEW zcl_abap2md_main( ).
    cut->add( 'ZCL_ABAP2MD_MAIN' ). " class
    cut->add( 'zr_abap2md_main' ). " report
    cut->add( 'z_abap2md_genrate_single' ). " function module
    DATA(text) = cut->generate_multiple( ).

    cl_abap_unit_assert=>assert_table_contains(
        line             = `# ZCL_ABAP2MD_MAIN`
        table            =  text     ).
    cl_abap_unit_assert=>assert_table_contains(
        line             = `# ZR_ABAP2MD_MAIN`
        table            =  text     ).
    cl_abap_unit_assert=>assert_table_contains(
        line             = `in a more automated fashion on the client.`
        table            =  text     ).
    cl_abap_unit_assert=>assert_table_contains(
        line             = `# ABAP to Markdown`
        table            =  text     ).
  ENDMETHOD.

ENDCLASS.
