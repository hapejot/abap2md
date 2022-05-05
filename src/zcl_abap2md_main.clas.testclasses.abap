CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      comment_parsing FOR TESTING RAISING cx_static_check.
ENDCLASS.
CLASS zcl_abap2md_main DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main IMPLEMENTATION.

  METHOD comment_parsing.
    DATA: ls_meth TYPE zcl_abap2md_main=>crms_method_info.
    DATA(cut) = NEW zcl_abap2md_main( ).

    ls_meth-description = VALUE #(
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

    cut->parse_docu_jd(
      CHANGING
        cs_method_info = ls_meth
    ).
    data lt_desc like ls_meth-description.
    lt_desc = value #(
                ( |This method collects the description until the next JavaDoc-like tag| )
                ( |Known tag values:| )
                ( |*EXCEPTION*, *PARAM*, *RETURN*, *THROWS* started by a leading @| )

     ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = lt_desc act = ls_meth-description ).
  ENDMETHOD.

ENDCLASS.
