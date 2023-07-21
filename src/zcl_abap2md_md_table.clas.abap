CLASS zcl_abap2md_md_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS load
      IMPORTING
        i_tab TYPE STANDARD TABLE.
    METHODS get_dimensions
      EXPORTING
        e_rows TYPE i
        e_cols TYPE i.
    METHODS get_content
      IMPORTING
        VALUE(i_row)    TYPE i
        i_col           TYPE string
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_abap2md_md_cell.
    METHODS get_markdown
      RETURNING
        VALUE(r_result) TYPE stringtab.
    METHODS set_fields
      IMPORTING
        i_names TYPE zif_abap2md_text_generator=>t_field_specs .
    METHODS use_pipe_separator.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS load_row      IMPORTING
                            i_row        TYPE any
                            VALUE(i_idx) TYPE i.
    METHODS add_cell
      IMPORTING
        i_name       TYPE string
        VALUE(i_row) TYPE i
        i_content    TYPE any.
    METHODS dashed_line
      IMPORTING
        i_sep         TYPE string
      RETURNING
        VALUE(r_line) TYPE string.
    METHODS title_line
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS row_lines
      IMPORTING
        VALUE(i_idx)    TYPE i
      RETURNING
        VALUE(r_result) TYPE stringtab.


    DATA:
      BEGIN OF col,
        name  TYPE string,
        width TYPE i,
      END OF col,
      cols LIKE STANDARD TABLE OF col,
      BEGIN OF row,
        number TYPE i,
        height TYPE i,
      END OF row,
      rows LIKE STANDARD TABLE OF row,
      BEGIN OF cell,
        row     TYPE i,
        name    TYPE string,
        content TYPE REF TO zcl_abap2md_md_cell,
      END OF cell,
      cells          LIKE SORTED TABLE OF cell WITH UNIQUE KEY row name,
      fields         TYPE zif_abap2md_text_generator=>t_field_specs,
      pipe_separator TYPE abap_bool.
ENDCLASS.



CLASS zcl_abap2md_md_table IMPLEMENTATION.


  METHOD add_cell.
    cell = VALUE #( name = i_name
                    row = i_row
                    content = NEW zcl_abap2md_md_cell( i_content ) ).
    INSERT cell INTO TABLE cells.

    DATA(col_ref) = REF #( cols[ name = cell-name ] OPTIONAL ).
    IF col_ref IS BOUND.
      DATA(w) = cell-content->get_width( ).
      IF w > col_ref->width.
        col_ref->width = w.
      ENDIF.
    ELSE.
      APPEND VALUE #( name = cell-name
                        width = cell-content->get_width( ) ) TO cols.
    ENDIF.

    DATA(row_ref) = REF #( rows[ number = i_row ] OPTIONAL ).
    IF row_ref IS BOUND.
      DATA(h) = cell-content->get_height( ).
      IF h > row_ref->height.
        row_ref->height = h.
      ENDIF.
    ELSE.
      APPEND VALUE #( number = i_row
                      height = cell-content->get_height( )
                       ) TO rows.
    ENDIF.

  ENDMETHOD.


  METHOD dashed_line.

    DATA sep TYPE string.

    CLEAR r_line.
    CLEAR sep.
    IF pipe_separator = abap_true.
      sep = `|`.
    ENDIF.
    LOOP AT cols INTO col.
      r_line = |{ r_line }{ sep }{ repeat( val = '-' occ = col-width ) }|.
      IF pipe_separator = abap_true.
        sep = `|`.
      ELSE.
        sep = i_sep.
      ENDIF.
    ENDLOOP.
    IF pipe_separator = abap_true.
      r_line = r_line && `|`.
    ENDIF.


  ENDMETHOD.


  METHOD get_content.
    r_result = cells[ name = i_col row = i_row ]-content.
  ENDMETHOD.


  METHOD get_dimensions.
    e_cols = lines( cols ).
    DATA(n) = lines( cells ).
    e_rows = cells[ n ]-row.
  ENDMETHOD.


  METHOD get_markdown.
**/
*generates markdown from the table that has been loaded.
*this markdown first removes all the fields that are empty
*for all rows or otherwise if a field list is given uses the field
*list.
*
*Then width of the columns are arranged so that the titles fit as well.
*
*At last the lines are generated.
*/



    DATA: row_count TYPE i,
          col_count TYPE i,
          tmp_cols  LIKE cols,
          col_ref   LIKE REF TO zcl_abap2md_md_table=>col.

    IF fields IS INITIAL.
      DELETE cols WHERE width = 0.
    ELSE.
      LOOP AT fields INTO DATA(field).
        col_ref = REF #( cols[ name = field-name ] OPTIONAL ).
        IF col_ref IS BOUND.
          APPEND col_ref->* TO tmp_cols.
        ENDIF.
      ENDLOOP.
      cols = tmp_cols.
    ENDIF.

    LOOP AT cols REFERENCE INTO col_ref.
      DATA(n) = strlen( col_ref->name ).
      DATA(fld_ref) = REF #( fields[ name = col_ref->name ] OPTIONAL ).
      IF fld_ref IS BOUND AND fld_ref->title IS NOT INITIAL.
        n = strlen( fld_ref->title ).
      ENDIF.
      IF fld_ref->style = 'CODE'.
        n = col_ref->width + 2.
      ENDIF.
      IF n > col_ref->width.
        col_ref->width = n.
      ENDIF.
    ENDLOOP.

    get_dimensions(
      IMPORTING
        e_rows = row_count
        e_cols = col_count
    ).

    DATA line TYPE string.
    DATA sep TYPE string.

    APPEND INITIAL LINE TO r_result.
    IF pipe_separator = abap_false.
      APPEND dashed_line( `-` ) TO r_result.
    ENDIF.
    APPEND title_line( ) TO r_result.
    APPEND dashed_line( ` ` ) TO r_result.
    DO row_count TIMES.
      IF sy-index > 1 AND pipe_separator = abap_false.
        APPEND INITIAL LINE TO r_result.
      ENDIF.
      APPEND LINES OF row_lines( i_idx = sy-index ) TO r_result.
    ENDDO.
    IF pipe_separator = abap_false.
      APPEND dashed_line( `-` ) TO r_result.
    ENDIF.
    APPEND INITIAL LINE TO r_result.
  ENDMETHOD.


  METHOD load.
    DATA: field_type TYPE sychar01.
    FIELD-SYMBOLS:
    <row> TYPE any.

    LOOP AT i_tab ASSIGNING <row>.
      DATA(idx) = sy-tabix.
      DESCRIBE FIELD <row> TYPE field_type.
      CASE field_type.
        WHEN 'C'. " Char like
        WHEN 'g'. " string
        WHEN 'N'. " Numeric
        WHEN 'D'. " Date
        WHEN 'T'. " Time

        WHEN 'I'. " Integer
        WHEN 'p'. " Packed
        WHEN 'f'. " Float

        WHEN 'x'. " X
        WHEN 'y'. " X-String

        WHEN 'l'. " data reference
        WHEN 'r'. " object reference

        WHEN 'u'. " flat structure
          load_row( i_row = <row> i_idx = idx ).
        WHEN 'v'. " deep structure
          load_row( i_row = <row> i_idx = idx ).
        WHEN 'h'. " table

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD load_row.
    DATA l_type TYPE REF TO cl_abap_structdescr.
    l_type ?= cl_abap_structdescr=>describe_by_data( i_row ).
    FIELD-SYMBOLS: <val> TYPE data.

    LOOP AT l_type->components INTO DATA(comp).
      ASSIGN COMPONENT comp-name OF STRUCTURE i_row TO <val>.
      add_cell( i_name = |{ comp-name }|
                i_row = i_idx
                i_content = <val> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD row_lines.
    DATA line TYPE string.
    DATA sep TYPE string.
    row = rows[ i_idx ].
    DO row-height TIMES.
      DATA(line_idx) = sy-index.
      CLEAR line.
      CLEAR sep.
      IF pipe_separator = abap_true.
        sep = `|`.
      ENDIF.
      LOOP AT cols INTO col.
        DATA(c) = get_content(
                                 i_row    = i_idx
                                 i_col    = col-name
                             ).

        DATA(txt) = c->get_line(  i_idx = line_idx ).
        IF fields[ name = col-name ]-style = 'CODE'.
          txt = |`{ txt }`|.
        ENDIF.
        line = |{ line }{ sep }{ txt WIDTH = col-width }|.
        IF pipe_separator = abap_true.
          sep = `|`.
        ELSE.
          sep = ` `.
        ENDIF.
      ENDLOOP.
      IF pipe_separator = abap_true.
        line = line && `|`.
      ENDIF.
      APPEND line TO r_result.
    ENDDO.
  ENDMETHOD.


  METHOD title_line.
    DATA(sep) = ``.
    IF pipe_separator = abap_true.
      sep = `|`.
    ENDIF.
    LOOP AT cols INTO col.
      DATA(fld_ref) = REF #( fields[ name = col-name ] OPTIONAL ).
      IF fld_ref IS BOUND AND fld_ref->title IS NOT INITIAL.
        r_result = |{ r_result }{ sep }{ fld_ref->title WIDTH = col-width }|.
      ELSE.
        r_result = |{ r_result }{ sep }{ col-name WIDTH = col-width }|.
      ENDIF.
      IF pipe_separator = abap_true.
        sep = `|`.
      ELSE.
        sep = ` `.
      ENDIF.
    ENDLOOP.
    IF pipe_separator = abap_true.
      r_result = r_result && `|`.
    ENDIF.
  ENDMETHOD.

  METHOD set_fields.
    fields = i_names.
  ENDMETHOD.


  METHOD use_pipe_separator.
    pipe_separator = abap_true.
  ENDMETHOD.

ENDCLASS.
