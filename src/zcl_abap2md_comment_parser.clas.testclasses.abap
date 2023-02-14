CLASS ltcl_parse_comment_multi DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mv_code TYPE stringtab.
    METHODS setup.
    METHODS parse_page_cmd FOR TESTING RAISING cx_static_check.
    METHODS parse_comments FOR TESTING RAISING cx_static_check.
    METHODS parse_comments_v2 FOR TESTING RAISING cx_static_check.
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
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_comment_parser( mv_code ) ).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = lines( cut->next_chunk( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2  act = lines( cut->next_chunk( ) ) ) .
    cl_abap_unit_assert=>assert_initial( act = cut->next_chunk( ) ).
  ENDMETHOD.

  METHOD parse_comments_v2.
    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_comment_parser( mv_code ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'START' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE zabap2md_token( type = 'LINE' ) act = cut->next_token( ) ).
  ENDMETHOD.



  METHOD parse_page_cmd.
    DATA chunks TYPE STANDARD TABLE OF rswsourcet.

    DATA(cut) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( mv_code ) ) ).
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
