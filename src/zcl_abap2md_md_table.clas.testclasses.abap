CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA flds TYPE zabap2md_fields.
    DATA: cut TYPE REF TO zcl_abap2md_md_table.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check,
      test_field_selection FOR TESTING RAISING cx_static_check,
      test_field_titles FOR TESTING RAISING cx_static_check,
      pipe_separator FOR TESTING RAISING cx_static_check.
    METHODS setup.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.


  METHOD test_field_selection.
    cut->set_fields( VALUE #( ( name = `NAME` ) ( name = `TEXT` ) ) ).
    DATA text TYPE stringtab.
    text = VALUE #(
    ( `` )
    ( `------------` )
    ( `NAME TEXT   ` )
    ( `---- -------` )
    ( `F1   Zeile 1` )
    ( `     Zeile 2` )
    ( `` )
    ( `F2   Line 1 ` )
    ( `     Line 2 ` )
    ( `------------` )
    ( `` )
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'md' exp = text act = cut->get_markdown( ) ).
  ENDMETHOD.

  METHOD test_field_titles.
    cut->set_fields( VALUE #( ( name = `NAME` title = `Name` ) ( name = `TEXT` title = 'Description' ) ) ).
    DATA text TYPE stringtab.
    text = VALUE #(
    ( `` )
    ( `----------------` )
    ( `Name Description` )
    ( `---- -----------` )
    ( `F1   Zeile 1    ` )
    ( `     Zeile 2    ` )
    ( `` )
    ( `F2   Line 1     ` )
    ( `     Line 2     ` )
    ( `----------------` )
    ( `` )
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'md' exp = text act = cut->get_markdown( ) ).
  ENDMETHOD.

  METHOD pipe_separator.
    DATA text TYPE stringtab.
    text = VALUE #(
    ( `` )
    ( `|NAME|TEXT   |DIRECTION|` )
    ( `|----|-------|---------|` )
    ( `|F1  |Zeile 1|I        |` )
    ( `|    |Zeile 2|         |` )
    ( `|F2  |Line 1 |O        |` )
    ( `|    |Line 2 |         |` )
    ( `` )
    ).
    cut->use_pipe_separator( ).
    cl_abap_unit_assert=>assert_equals( msg = 'md'
                                        exp = text
                                        act = cut->get_markdown( ) ).

  ENDMETHOD.


  METHOD first_test.

    DATA row_count TYPE i.
    DATA col_count TYPE i.
    cut->get_dimensions( IMPORTING e_rows = row_count e_cols = col_count ).

    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = 2 act = row_count ).
    cl_abap_unit_assert=>assert_equals( msg = 'msg' exp = 5 act = col_count ).

    DATA c TYPE REF TO zcl_abap2md_md_cell.
    c = cut->get_content( i_row = 1 i_col = 'NAME' ).
    cl_abap_unit_assert=>assert_equals( msg = 'name-width' exp = 2 act = c->get_width( ) ).
    c = cut->get_content( i_row = 1 i_col = 'TEXT' ).
    cl_abap_unit_assert=>assert_equals( msg = 'text-width' exp = 7 act = c->get_width( ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'text-line1' exp = 'Zeile 1' act = c->get_line( 1 ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'text-line2' exp = 'Zeile 2' act = c->get_line( 2 ) ).

    DATA text TYPE stringtab.
    text = VALUE #(
    ( `` )
    ( `----------------------` )
    ( `NAME TEXT    DIRECTION` )
    ( `---- ------- ---------` )
    ( `F1   Zeile 1 I        ` )
    ( `     Zeile 2          ` )
    ( `` )
    ( `F2   Line 1  O        ` )
    ( `     Line 2           ` )
    ( `----------------------` )
    ( `` )
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'md'
                                        exp = text
                                        act = cut->get_markdown( ) ).


  ENDMETHOD.

  METHOD setup.

    flds = VALUE #(
     (  name = 'F1'
        direction = 'I'
        text = VALUE #( ( `Zeile 1` ) ( `Zeile 2` ) ) )
     (  name = 'F2'
        direction = 'O'
        text = VALUE #( ( `Line 1` ) ( `Line 2` ) ) )
    ).

    cut = NEW zcl_abap2md_md_table( ).

    cut->load( flds ).


  ENDMETHOD.



ENDCLASS.
