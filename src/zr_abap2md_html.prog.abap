**Please use the pattern Z_BC_PROGRAM_HEAD to create the program head**
REPORT zr_abap2md_html.


DATA: ctab         TYPE stringtab,
      xtab         TYPE STANDARD TABLE OF char80,
      xml_viewer   TYPE REF TO cl_gui_html_viewer,
      gen_url(500) TYPE c,
      c_xmldoc     TYPE string,
      names        TYPE zabap2md_object_names,
      options      TYPE zabap2md_options,
      doc          TYPE stringtab,
      values       TYPE vrm_values,
      elements     TYPE STANDARD TABLE OF zabap2md_docelem,
      field_name   TYPE fieldname VALUE 'P_OBJSET'.

PARAMETERS:
    p_objset TYPE seoclsname AS LISTBOX VISIBLE LENGTH 35.

INITIALIZATION.
  DATA(html) = NEW zcl_abap2md_html( ).


AT SELECTION-SCREEN OUTPUT.
  SELECT * FROM zabap2md_docelem
        INTO TABLE @elements.
  CLEAR values.
  LOOP AT elements INTO DATA(x) GROUP BY x-docset.
    APPEND VALUE #( key = x-docset text = x-docset ) TO values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_OBJSET'
      values = values.

START-OF-SELECTION.
  CLEAR names.
  LOOP AT elements INTO DATA(element) WHERE docset = p_objset.
    APPEND element-name TO names.
  ENDLOOP.

  CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
    EXPORTING
      it_names   = names
      ix_options = options
    IMPORTING
      et_doc     = doc.    " Table with Strings

  html->render_md_as_html( doc ).
