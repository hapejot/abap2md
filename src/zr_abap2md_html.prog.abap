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


ctab = VALUE #(
( `<!DOCTYPE html>` )
( `` )
( `<head>` )
( `    <meta http-equiv="content-type" content="text/html; charset=utf-8">` )
( `    <meta charset="utf-8" />` )
( `    <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />` )
( `    <title>Test</title>` )
( `  <style type="text/css">` )
( `    body { font-family: "Source Sans Pro", Arial, Helvetica, sans-serif; color: rgb(57,74,88) }` )
( `    h1   { font-size: 18px }` )
( `    h2   { font-size: 18px }` )
( `    h3   { font-size: 18px }` )
( `  </style>` )
( `</head>` )
( `` )
( `<body>` )
( `    <div id=main>` )
( `        <pre id="text">` )
( `![Inwerken](https://www.inwerken.de/wp-content/uploads/2018/06/logo_inwerken_2x.png)` )
( `` )
( LINES OF doc )
( `</pre>` )
( `    </div>` )
( `    <script src="https://cdnjs.cloudflare.com/ajax/libs/showdown/2.1.0/showdown.min.js"></script>` )
( `    <script>` )
( `        showdown.setOption('tables', true);` )
( `        showdown.setOption('literalMidWordUnderscores', true);` )
( `    var converter = new showdown.Converter(),` )
( `            m = document.getElementById('main'),` )
( `            t = document.getElementById('text'),` )
( `            text = t.innerText;` )
( `        console.log(text);` )
( `        m.innerHTML = converter.makeHtml(text);` )
( `    </script>` )
( `</body>` )
( `` )
( `</html>` )
).

CONCATENATE LINES OF ctab INTO DATA(s) SEPARATED BY cl_abap_char_utilities=>newline.

CALL FUNCTION 'CONVERT_STRING_TO_TAB'
  EXPORTING
    i_string         = s    " String that is to be cut into the table
    i_tabline_length = 80    " Length of the table lines
  TABLES
    et_table         = xtab.    " Character field lines (pass length in import parameter)

CREATE OBJECT xml_viewer
  EXPORTING
    parent = cl_gui_container=>screen0.

IF NOT xml_viewer IS INITIAL.
  gen_url = ''.
  CALL METHOD xml_viewer->load_data
    EXPORTING
      type         = 'text'
      subtype      = 'html'
      language     = sy-langu
    IMPORTING
      assigned_url = gen_url
    CHANGING
      data_table   = xtab
    EXCEPTIONS
      OTHERS       = 0.
  IF sy-subrc = 0 AND NOT gen_url IS INITIAL.
    CALL METHOD xml_viewer->show_url
      EXPORTING
        url = gen_url.
  ENDIF.
ENDIF.
WRITE: /.
