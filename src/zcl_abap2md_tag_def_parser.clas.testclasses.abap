CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA code TYPE stringtab.
    METHODS:
      parse FOR TESTING RAISING cx_static_check,
      parse_alternative FOR TESTING RAISING cx_static_check,
      escapes FOR TESTING RAISING cx_static_check,
      setup.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.
  METHOD setup.
    code = VALUE stringtab(
                    ( `**/` )
                    ( `* @page page1 A documentation page` )
                    ( `*@@tableofcontents` )
                    ( |*| )
                    ( `* Leading text.` )
                    ( `*@section sec An example section` )
                    ( `* This page contains the subsections @ref subsection1 and @ref subsection2.` )
                    ( `*      For more info see page @ref page2.` )
                    ( `*@subsection subsection1 The first subsection` )
                    ( `* Text.` )
                    ( `* @subsection subsection2 The second subsection` )
                    ( `* More text.` )
                    ( |* ```\{.plantuml caption="General Flow" width=50%\}| )
                    ( |* Client -> Main: generate_multi| )
                    ( |* ```| )
                    ( `*/` )
                    ( ` ` )
                    ( `**/ @page page2 Another page` )
                    ( `*  Even more info.` )
                    ( `*/` )
                    ( |**/| )
                    ( |* 24.3.2021 / IG / D6000803| )
                    ( |* comment| )
                    ( |*| )
                    ( |* 2021-04-20 / PJL / X0006715| )
                    ( |* comment| )
                     ).

  ENDMETHOD.

  METHOD parse.
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( code ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'page' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page1' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'A' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'documentation' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = '@' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'tableofcontents' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'PARSEP' line = 3 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'Leading' line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'text' line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'section' line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'sec' line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'An' line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'example' line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'section' line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'This' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'contains' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'the' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsections' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'ref' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection1' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'and' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'ref' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection2' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 6 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'For' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'more' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'info' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'see' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'ref' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page2' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 7 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'subsection' line = 8 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection1' line = 8 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'The' line = 8 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'first' line = 8 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection' line = 8 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'Text' line = 9 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 9 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'subsection' line = 10 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection2' line = 10 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'The' line = 10 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'second' line = 10 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'subsection' line = 10 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'More' line = 11 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'text' line = 11 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 11 ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |```\{.plantuml| line = 12 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |caption="General| line = 12 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |Flow"| line = 12 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |width=50%\}| line = 12 ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |Client| line = 13 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |->| line = 13 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |Main| line = 13 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = |: | line = 13 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |generate_multi| line = 13 ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = |```| line = 14 ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'END' ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD'  value = 'page' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page2' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'Another' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'page' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'Even' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'more' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'info' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'  value = '.' line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'END' ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START'                      line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'DATE'   value = '20210324'  line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'    value = ` / `       line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'IG'        line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'    value = ` / `       line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'D6000803'  line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'comment'   line = 2 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'PARSEP'                     line = 3 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'DATE'   value = '20210420'  line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'    value = ` / `       line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'PJL'       line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'SEP'    value = ` / `       line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'X0006715'  line = 4 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD'   value = 'comment'   line = 5 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'END' ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).

  ENDMETHOD.

  METHOD parse_alternative.
    DATA ss TYPE stringtab.
    ss = VALUE #(     ( `"""""""` )
                      ( `"      ` )
                      ( `"""""""` )
                      ( `*******` )
                      ( `*     *` )
                      ( `*******` )    ).
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( ss ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( ) act = cut->next_token( ) ).
  ENDMETHOD.

  METHOD escapes.
    DATA ss TYPE stringtab.
    ss = VALUE #(     ( `**/` )
                      ( `* @@cmd  test` )
                       ).
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( ss ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' line = 1 )           act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'CMD' value = '@' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'cmd' line = 1 ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'WORD' value = 'test' line = 1 ) act = cut->next_token( ) ).

  ENDMETHOD.



ENDCLASS.
