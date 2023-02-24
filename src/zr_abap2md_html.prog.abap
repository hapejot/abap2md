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
( 'ZR_ABAP2MD_MAIN' )
( 'ZCL_ABAP2MD_*' )
*( 'ZCL_ABAP2MD_ADT_READER' )
*( 'ZCL_ABAP2MD_CLASS_INFO' )
*( 'ZCL_ABAP2MD_COMMENT_PARSER' )
*( 'ZCL_ABAP2MD_DOC_GENERATOR' )
*( 'ZCL_ABAP2MD_DOC_PARSER' )
*( 'ZCL_ABAP2MD_FUNCTION_INFO' )
*( 'ZCL_ABAP2MD_LOCAL_FILE' )
*( 'ZCL_ABAP2MD_MAIN' )
*( 'ZCL_ABAP2MD_MARKDOWN' )
*( 'ZCL_ABAP2MD_METHOD_PARSER' )
*( 'ZCL_ABAP2MD_PROGRAM_INFO' )
*( 'ZCL_ABAP2MD_REPORT_PARSER' )
*( 'ZCL_ABAP2MD_TAG_DEF_PARSER' )
( 'ZCX_ABAP2MD_ERROR' )

( |ZABAP2MD_CHANGE| )
( |ZABAP2MD_CLASS_INFO| )
( |ZABAP2MD_COMMON_INFO| )
( |ZABAP2MD_DEPENDENCY| )
( |ZABAP2MD_DOC_STRUCTURE| )
( |ZABAP2MD_FUNCTION_INFO| )
( |ZABAP2MD_METHOD_INFO| )
( |ZABAP2MD_METHOD_OPTIONS| )
( |ZABAP2MD_OPTIONS| )
( |ZABAP2MD_PAGE| )
( |ZABAP2MD_PARAM| )
( |ZABAP2MD_PROGRAM_INFO| )
( |ZABAP2MD_SECTION| )
( |ZABAP2MD_SUBSECTION| )
( |ZABAP2MD_TOKEN| )

( |ZIF_ABAP2MD_DOC_GENERATOR| )
( |ZIF_ABAP2MD_INFO| )
( |ZIF_ABAP2MD_PARSER| )
( |ZIF_ABAP2MD_TEXT_GENERATOR| )

).

CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
  EXPORTING
    it_names   = names
    ix_options = options
  IMPORTING
    et_doc     = doc.    " Table with Strings

html->render_md_as_html( doc ).
