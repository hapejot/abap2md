**Please use the pattern Z_BC_PROGRAM_HEAD to create the program head**
REPORT zr_abap2md_html.


DATA: ctab         TYPE stringtab,
      xtab         TYPE STANDARD TABLE OF char80,
      xml_viewer   TYPE REF TO cl_gui_html_viewer,
      gen_url(500) TYPE c,
      c_xmldoc     TYPE string,
      names        TYPE zabap2md_object_names,
      options      TYPE zabap2md_options,
      doc          TYPE stringtab.


DATA(html) = NEW zcl_abap2md_html( ).


names = VALUE #(
*  ( 'ZR_ABAP2MD_MAIN' )
*  ( 'ZCL_ABAP2MD_*' )
*  ( 'ZCX_ABAP2MD_*' )
*  ( |ZABAP2MD_*| )
*  ( |ZIF_ABAP2MD_*| )
*  ( |Z_ABAP2MD*| )
   ( |ZCL_CORE*_PROCESSING| )
).

CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
  EXPORTING
    it_names   = names
    ix_options = options
  IMPORTING
    et_doc     = doc.    " Table with Strings

html->render_md_as_html( doc ).
