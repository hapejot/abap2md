CLASS zcl_abap2md_markdown DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_text_generator.
  PRIVATE SECTION.
    DATA: mt_text TYPE rswsourcet.
    METHODS generate_table
      IMPORTING
        io_type          TYPE REF TO cl_abap_structdescr
        it_tab           TYPE STANDARD TABLE
      RETURNING
        VALUE(r_out_tab) TYPE rswsourcet.
ENDCLASS.



CLASS zcl_abap2md_markdown IMPLEMENTATION.
  METHOD zif_abap2md_text_generator~generate.

  ENDMETHOD.

  METHOD zif_abap2md_text_generator~heading.
**/
* reporting the heading in form of
*      # heading 1
*      ## heading 2
*/

    DATA: txt TYPE string.

    zif_abap2md_text_generator~new_paragraph( ).
    CASE iv_level.
      WHEN 1.
        txt = CONV string( iv_text ).
        APPEND |# {  txt }| TO mt_text.
        " ATEXT variant dropped in favour of the ATX variant
        " APPEND repeat( val = '=' occ = strlen( txt ) ) TO mt_text.
      WHEN 2.
        txt = CONV string( iv_text ).
        APPEND |## {  txt }| TO mt_text.
        " APPEND repeat( val = '-' occ = strlen( txt ) ) TO mt_text.
    ENDCASE.
    " skip line after heading
    APPEND INITIAL LINE TO mt_text.
    ro_gen = me.
  ENDMETHOD.

  METHOD zif_abap2md_text_generator~text.

    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        " see if the table is a table with one column or multiple columns:
        DATA(tab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( iv_text ) ).
        DATA(struct) = tab->get_table_line_type( ).
        IF struct->kind = cl_abap_typedescr=>kind_struct.
          APPEND LINES OF generate_table( io_type = CAST cl_abap_structdescr( struct ) it_tab = iv_text ) TO mt_text.
        ELSE.
          ASSIGN iv_text TO <lt_text>.
          LOOP AT <lt_text> ASSIGNING <lv_text>.
            APPEND CONV string( <lv_text> ) TO mt_text.
          ENDLOOP.
        ENDIF.
      WHEN OTHERS.
        APPEND CONV string( iv_text ) TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD zif_abap2md_text_generator~result.

    r_result = mt_text.

  ENDMETHOD.

  METHOD zif_abap2md_text_generator~code.
    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.

    APPEND INITIAL LINE TO mt_text.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN iv_text TO <lt_text>.
        LOOP AT <lt_text> ASSIGNING <lv_text>.
          APPEND |    { <lv_text> }| TO mt_text.
        ENDLOOP.
      WHEN OTHERS.
        APPEND |    { iv_text }|   TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD zif_abap2md_text_generator~definition.

    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.

* empty line
    APPEND INITIAL LINE TO mt_text.

* Definition term
    APPEND |**{ iv_def }**|  TO mt_text.

* Definition text
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    DATA(lv_first_out) = ':'.
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN iv_text TO <lt_text>.
        LOOP AT <lt_text> ASSIGNING <lv_text>.
          APPEND |{ lv_first_out WIDTH = 1 }  { <lv_text> }| TO mt_text.
          CLEAR lv_first_out.
        ENDLOOP.
      WHEN OTHERS.
        APPEND |:  { iv_text }|   TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD zif_abap2md_text_generator~new_paragraph.
    " skip one line only if there is already text.
    IF mt_text IS NOT INITIAL AND mt_text[ lines( mt_text ) ] IS NOT INITIAL.
      APPEND INITIAL LINE TO mt_text.
    ENDIF.

    ro_gen = me.
  ENDMETHOD.

  METHOD generate_table.
    DATA: col_width TYPE i,
          sep       TYPE string.
    FIELD-SYMBOLS: <ls_row>      TYPE any,
                   <lv_out_line> TYPE string,
                   <lv_value>    TYPE data.
    LOOP AT io_type->components INTO DATA(ls_comp).
      DATA(col_no) = sy-tabix.
      IF col_no = 1.
        " create two empty lines for the table header
        APPEND INITIAL LINE TO r_out_tab.
        APPEND INITIAL LINE TO r_out_tab.
        sep = ``.
      ELSE.
        sep = `|`.
      ENDIF.


      col_width = strlen( ls_comp-name ).
      LOOP AT it_tab ASSIGNING <ls_row>.
        DATA(out_row_no) = sy-tabix + 2. " output row number includes offset for the table header
        IF col_no = 1.
          APPEND INITIAL LINE TO r_out_tab ASSIGNING <lv_out_line>.
        ELSE.
          ASSIGN r_out_tab[ out_row_no ] TO <lv_out_line>.
        ENDIF.
        ASSERT <lv_out_line> IS ASSIGNED.
        ASSIGN COMPONENT col_no OF STRUCTURE <ls_row> TO <lv_value>.

        " calculate column width
        DATA(tmp) = |{ <lv_value> WIDTH = col_width }|.
        IF strlen( tmp ) > col_width.
          col_width = strlen( tmp ).
        ENDIF.
      ENDLOOP.
      IF col_no > 1.
        r_out_tab[ 1 ] = |{ r_out_tab[ 1 ] } |.
*        r_out_tab[ 2 ] = |{ r_out_tab[ 2 ] }-|.
      ENDIF.
      r_out_tab[ 1 ] = |{ r_out_tab[ 1 ] }{ sep } { ls_comp-name WIDTH = col_width }|.
      r_out_tab[ 2 ] = |{ r_out_tab[ 2 ] }{ sep }{ repeat( val = '-' occ = col_width + 2 ) }|.
      LOOP AT it_tab ASSIGNING <ls_row>.
        out_row_no = sy-tabix + 2. " output row number includes offset for the table header
        ASSIGN r_out_tab[ out_row_no ] TO <lv_out_line>.
        ASSERT <lv_out_line> IS ASSIGNED.
        ASSIGN COMPONENT col_no OF STRUCTURE <ls_row> TO <lv_value>.
        IF col_no > 1.
          <lv_out_line> = |{ <lv_out_line> } |.
        ENDIF.
        <lv_out_line> = |{ <lv_out_line> }{ sep } { <lv_value> WIDTH = col_width }|.
        " calculate column width
      ENDLOOP.
    ENDLOOP.
    APPEND INITIAL LINE TO r_out_tab.
  ENDMETHOD.
ENDCLASS.
