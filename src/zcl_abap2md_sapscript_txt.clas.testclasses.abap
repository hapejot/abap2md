CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
        text TYPE zcl_abap2md_sapscript_txt=>t_td_text.
    METHODS:
      convert1 FOR TESTING RAISING cx_static_check,
      fmt1 FOR TESTING RAISING cx_static_check,
      fmt2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD convert1.
    DATA txt TYPE text.
    DATA exp TYPE stringtab.

    txt = VALUE #(
        ( tdformat = 'U1'  tdline = '&USE&' )
        ( tdformat = 'AS'  tdline = 'This table contains all valid process codes in the system. Each process' )
        ( tdformat = '  '  tdline = 'needs an entry here in order to be processed by the main controller.' )
        ( tdformat = 'U1'  tdline = '&MAINTENANCE&' )
        ( tdformat = 'AS'  tdline = 'The table has to be maintained by using the transaction <ZH>ZBC_CUST</>' )
        ( tdformat = 'T1'  tdline = '<ZK>Title</>,,<ZK>Usage</>,,<ZH>Function</>' )
        ( tdformat = 'T1'  tdline = 'Peter,,Test text for more and more and more and much much more .......' )
        ( tdformat = '= '  tdline = 'usage,,Fun' )
     ).
    exp = VALUE #(
        ( `` )
        ( `## Use` )
        ( `` )
        ( `This table contains all valid process codes in the system. Each process` )
        ( ` needs an entry here in order to be processed by the main controller.` )
        ( `` )
        ( `## Maintenance` )
        ( `` )
        ( `The table has to be maintained by using the transaction **ZBC_CUST**` )
        ( `` )
        ( `|*Title*|*Usage*                                                             |**Function**|` )
        ( `|-------|--------------------------------------------------------------------|------------|` )
        ( `|Peter  |Test text for more and more and more and much much more .......usage|Fun         |` )
        ( `` )
    ).





    DATA(cut) = NEW zcl_abap2md_sapscript_txt( ).
    cut->add_text( txt ).
    DATA(act) = cut->get_markdown( ).

    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = exp act = act ).

  ENDMETHOD.


  METHOD fmt1.
    DATA(cut) = NEW zcl_abap2md_sapscript_txt( ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg'
                                        exp = `The table has to be maintained by using the transaction **ZBC_CUST**`
                                        act = cut->cvt_fmt( 'The table has to be maintained by using the transaction <ZH>ZBC_CUST</>' ) ).

  ENDMETHOD.

  METHOD fmt2.
    DATA(cut) = NEW zcl_abap2md_sapscript_txt( ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg'
                                        exp = `*Title*,,*Usage*,,**Function**`
                                        act = cut->cvt_fmt( '<ZK>Title</>,,<ZK>Usage</>,,<ZH>Function</>' ) ).

  ENDMETHOD.

ENDCLASS.
