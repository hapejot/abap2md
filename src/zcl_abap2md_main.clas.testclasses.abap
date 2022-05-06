CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      comment_parsing FOR TESTING RAISING cx_static_check,
      comment_parse_stg1 FOR TESTING RAISING cx_static_check,
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
                ( |*EXCEPTION*, *PARAM*, *RETURN*, *THROWS* started by a leading @| )
                ( || )
                ( || )
                ( |@param EV_NEXT_TAG        Next/Last found tag| )
                ( |@param EV_TAG_VALUE       Tag value| )
                ( |@param ET_DESCRIPTION     Description found until found tag| )
                ( |@param CV_NEXT_INDENT     Next indent to be used| )
                ( |@param CT_SOURCE          Source description| )
                ( || )
    ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = chunk act = cut->next_chunk( ) ).

  ENDMETHOD.

  METHOD comment_parse_stg2.
    DATA chunk01 TYPE rswsourcet.
    DATA chunk02 TYPE rswsourcet.
    DATA chunk03 TYPE rswsourcet.
    DATA chunk04 TYPE rswsourcet.
    DATA chunk05 TYPE rswsourcet.
    DATA chunk06 TYPE rswsourcet.
    DATA chunk07 TYPE rswsourcet.
    DATA chunk08 TYPE rswsourcet.
    DATA chunk09 TYPE rswsourcet.
    DATA chunk10 TYPE rswsourcet.
    DATA chunk11 TYPE rswsourcet.
    DATA chunk12 TYPE rswsourcet.

    DATA(lv_code) = get_code_fragment( ).
    DATA(cut) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( lv_code ) ) ).
    chunk01 = VALUE #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*@EXCEPTION*, *@PARAM*, *@RETURN*, *@THROWS*.| )
                ( || )
                ( || ) ).
    chunk02 = VALUE #(
                ( |@param| ) ).
    chunk03 = VALUE #( (  |EV_NEXT_TAG        Next/Last found tag| ) ).
    chunk04 = VALUE #( ( |@param| ) ).
    chunk05 = VALUE #( ( |EV_TAG_VALUE       Tag value| ) ).
    chunk06 = VALUE #( ( |@param| ) ).
    chunk07 = VALUE #( ( |ET_DESCRIPTION     Description found until found tag| ) ).
    chunk08 = VALUE #( ( |@param| ) ).
    chunk09 = VALUE #( ( |CV_NEXT_INDENT     Next indent to be used| ) ).
    chunk10 = VALUE #( ( |@param| ) ).
    chunk11 = VALUE #( ( |CT_SOURCE          Source description| )
                        ( || )  ).


    cl_abap_unit_assert=>assert_equals( msg = '1' exp = chunk01 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '2' exp = chunk02 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '3' exp = chunk03 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '4' exp = chunk04 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '5' exp = chunk05 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '6' exp = chunk06 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '7' exp = chunk07 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '8' exp = chunk08 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '9' exp = chunk09 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '10' exp = chunk10 act = cut->next_chunk( ) ).
    cl_abap_unit_assert=>assert_equals( msg = '11' exp = chunk11 act = cut->next_chunk( ) ).

  ENDMETHOD.

  METHOD comment_parsing.
    DATA: ls_meth TYPE zcl_abap2md_main=>crms_method_info.
    DATA(cut) = NEW zcl_abap2md_main( ).

    ls_meth-description = get_code_fragment( ).
    cut->parse_docu_jd(
      CHANGING
        cs_method_info = ls_meth
    ).
    DATA lt_desc LIKE ls_meth-description.
    lt_desc = VALUE #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*EXCEPTION*, *PARAM*, *RETURN*, *THROWS* started by a leading @| )

     ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = lt_desc act = ls_meth-description ).
  ENDMETHOD.

  METHOD get_code_fragment.

    rs_meth  = VALUE #(
                ( |  METHOD find_next_jd_tag.                                            | )
                ( |**/| )
                ( |* This method collects the description until the next JavaDoc-like tag| )
                ( |* Known tag values:| )
                ( |* *EXCEPTION*, *PARAM*, *RETURN*, *THROWS* started by a leading @| )
                ( |*| )
                ( |*| )
                ( |* @param EV_NEXT_TAG        Next/Last found tag| )
                ( |* @param EV_TAG_VALUE       Tag value| )
                ( |* @param ET_DESCRIPTION     Description found until found tag| )
                ( |* @param CV_NEXT_INDENT     Next indent to be used| )
                ( |* @param CT_SOURCE          Source description| )
                ( |*| )
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
