CLASS ltcl_parse_comment_multi DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mv_code TYPE stringtab.
    METHODS setup.

    METHODS parse_comments FOR TESTING RAISING cx_static_check.
    "! test
    "! @raising cx_static_check |
    METHODS parse_comments_v2 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_parse_comment_multi IMPLEMENTATION.
  METHOD setup.
    mv_code = VALUE stringtab(
                    ( `**/` )
                    ( `* @page page1 A documentation page` )
                    ( `*@tableofcontents` )
                    ( |*| )
                    ( `* Leading text.` )
                    ( `*@section sec An example section` )
                    ( `* This page contains the subsections @ref subsection1 and @ref subsection2.` )
                    ( `* For more info see page @ref page2.` )
                    ( `*@subsection subsection1 The first subsection` )
                    ( `* Text.with dots` )
                    ( `* @subsection subsection2 The second subsection` )
                    ( `* More text.` )
                    ( `*/` )
                    ( ` ` )
                    ( `    " test ` )
                    ( `*------* ` )
                    ( `*------- ` )
                    ( `******** ` )
                    ( `**/ @page page2 Another page` )
                    ( `* Even more info.` )
                    ( `*/` ) ).

  ENDMETHOD.

  METHOD parse_comments.
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_comment_parser( mv_code ) ).
    cl_abap_unit_assert=>assert_equals( exp = 11 act = lines( cut->next_chunk( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2  act = lines( cut->next_chunk( ) ) ) .
    cl_abap_unit_assert=>assert_initial( act = cut->next_chunk( ) ).
  ENDMETHOD.

  METHOD parse_comments_v2.
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_comment_parser( mv_code ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' line =  1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  2 value = '@page page1 A documentation page'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  3 value = '@tableofcontents'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  4 value = ''  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  5 value = 'Leading text.'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  6 value = '@section sec An example section'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  7 value = 'This page contains the subsections @ref subsection1 and @ref subsection2.'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  8 value = 'For more info see page @ref page2.'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line =  9 value = '@subsection subsection1 The first subsection'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 10 value = 'Text.with dots'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 11 value = '@subsection subsection2 The second subsection'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 12 value = 'More text.'  ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'END'   line = 13 ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 15 value = `test ` ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 16 value = `------* ` ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 17 value = `------- ` ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 18 value = `******* ` ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' line = 19 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 19 value = '@page page2 Another page' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE'  line = 20 value = 'Even more info.' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'END'   line = 21 ) act = cut->next_token( ) ).
  ENDMETHOD.

ENDCLASS.
