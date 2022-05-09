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
                ( |*@@EXCEPTION*, *@@PARAM*, *@@RETURN*, *@@THROWS*.| )
                ( || )
                ( || )
                ( |@param EV_NEXT_TAG        Next/Last found tag| )
                ( |@param EV_TAG_VALUE       Tag value| )
                ( |@param et_description     Description found until found tag| )
                ( |@param CV_NEXT_INDENT     Next indent to be used| )
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
                ( || ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) ) TO chunks.
    APPEND VALUE #( (  |EV_NEXT_TAG        Next/Last found tag| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) ) TO chunks.
    APPEND VALUE #( ( |EV_TAG_VALUE       Tag value| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) ) TO chunks.
    APPEND VALUE #( ( |et_description     Description found until found tag| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) ) TO chunks.
    APPEND VALUE #( ( |CV_NEXT_INDENT     Next indent to be used| ) ) TO chunks.
    APPEND VALUE #( ( |@param| ) ) TO chunks.
    APPEND VALUE #( ( |CT_SOURCE          Source description| ) ( || )  ) TO chunks.
    APPEND VALUE #( ( |@return| ) ) TO chunks.
    APPEND VALUE #( ( |abap_bool *abap_true* if succesfull,| )  ( |*abap_false* otherwise| ) ) TO chunks.
    APPEND VALUE #( ( |@raising| ) ) TO chunks.
    APPEND VALUE #( ( |cx_nothing          Exception for nothing.| )  ) TO chunks.
    APPEND VALUE #( ( |@throws| ) ) TO chunks.
    APPEND VALUE #( ( |cx_something         Exception for something.| )  ) TO chunks.
    APPEND VALUE #( ( |@exception| ) ) TO chunks.
    APPEND VALUE #( ( |cx_anything       Exception for anything.| )  ) TO chunks.
    APPEND VALUE #( ) TO chunks.

    LOOP AT chunks INTO DATA(chunk).
      cl_abap_unit_assert=>assert_equals( msg = |{ sy-tabix }| exp = chunk act = cut->next_chunk( ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD comment_parsing.
    DATA: ls_meth TYPE zcl_abap2md_main=>method_info.
    DATA(cut) = NEW zcl_abap2md_main( ).

    ls_meth-description = get_code_fragment( ).
    cut->parse_method_docu(
      CHANGING
        cs_method_info = ls_meth
    ).
    DATA lt_desc LIKE ls_meth-description.
    lt_desc = VALUE #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*@EXCEPTION*, *@PARAM*, *@RETURN*, *@THROWS*.| )
                ( || )
                ( || )
     ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = lt_desc act = ls_meth-description ).
    cl_abap_unit_assert=>assert_equals( msg = 'params' exp = 5 act = lines( ls_meth-parameter_infos ) ).

    DATA(param) = ls_meth-parameter_infos[ parameter_name = 'EV_NEXT_TAG' ].
    DATA exp LIKE param-description.
    exp = VALUE #( ( |Next/Last found tag| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'param1' exp = exp  act = param-description ).

    param = ls_meth-parameter_infos[ parameter_name = 'EV_TAG_VALUE' ].
    exp = VALUE #( ( |Tag value| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'param2' exp = exp  act = param-description ).

    param = ls_meth-parameter_infos[ parameter_name = 'ET_DESCRIPTION' ].
    exp = VALUE #( ( |Description found until found tag| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'param3' exp = exp  act = param-description ).

    param = ls_meth-parameter_infos[ parameter_name = 'CV_NEXT_INDENT' ].
    exp = VALUE #( ( |Next indent to be used| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'param4' exp = exp  act = param-description ).

    param = ls_meth-parameter_infos[ parameter_name = 'CT_SOURCE' ].
    exp = VALUE #( ( |Source description| )  ( || ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'param5' exp = exp  act = param-description ).

    exp = VALUE #(  ( |abap_bool *abap_true* if succesfull,| )
                    ( |*abap_false* otherwise| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'returns' exp = exp  act = ls_meth-return_info-description ).

    cl_abap_unit_assert=>assert_equals( msg = 'exceptions' exp = 3 act = lines( ls_meth-exception_infos ) ).

    DATA(exc) = ls_meth-exception_infos[ exception_name = 'CX_NOTHING' ].
    exp = VALUE #( ( |Exception for nothing.| ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'cx_nothing' exp = exp act = exc-description ).


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
                ( |* @param CV_NEXT_INDENT     Next indent to be used| )
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
