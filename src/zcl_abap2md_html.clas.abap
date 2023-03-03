CLASS zcl_abap2md_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS string_to_tab
      IMPORTING
                i_string         TYPE string
                i_tabline_length TYPE i
      CHANGING  et_table         TYPE STANDARD TABLE.
    METHODS render_md_as_html
      IMPORTING
        doc TYPE stringtab.
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abap2md_html IMPLEMENTATION.

  METHOD render_md_as_html.
    DATA ctab TYPE stringtab.
    DATA xtab TYPE STANDARD TABLE OF char80.
    DATA xml_viewer TYPE REF TO cl_gui_html_viewer.
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
*    ( `![Inwerken](https://www.inwerken.de/wp-content/uploads/2018/06/logo_inwerken_2x.png)` )
    ( `` )
    ( LINES OF doc )
    ( `</pre>` )
    ( `    </div>` )
    ( `    <script src="https://cdnjs.cloudflare.com/ajax/libs/showdown/2.1.0/showdown.min.js"></script>` )
    ( `    <script>` )
    ( `        showdown.setOption('tables', true);` )
    ( `        showdown.setOption('literalMidWordUnderscores', true);` )
    ( `        var converter = new showdown.Converter(),` )
    ( `            m = document.getElementById('main'),` )
    ( `            t = document.getElementById('text'),` )
    ( `            text = t.innerText;` )
    ( `        m.innerHTML = converter.makeHtml(text);` )
    ( `    </script>` )
    ( `</body>` )
    ( `` )
    ( `</html>` )
    ).

    CONCATENATE LINES OF ctab INTO DATA(s) SEPARATED BY cl_abap_char_utilities=>newline.

    string_to_tab(
      EXPORTING
        i_string         = s
        i_tabline_length = 80
      CHANGING
        et_table         = xtab
    ).

    CREATE OBJECT xml_viewer
      EXPORTING
        parent = cl_gui_container=>screen0.

    IF NOT xml_viewer IS INITIAL.
      DATA gen_url(1000) TYPE c.
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
  ENDMETHOD.


  METHOD string_to_tab.
**/
* this method splits a string at the given length into chunks.
* it calculates the number of full rows and the number of
* characters in the last row.
* then copies everything into the table.
*/
    DATA: l_length      TYPE i,
          l_offset      TYPE i,
          l_full_lines  TYPE i,
          l_last_length TYPE i.


    l_length = strlen( i_string ).
    l_full_lines  = l_length DIV i_tabline_length.
    l_last_length = l_length MOD i_tabline_length.

    DO l_full_lines TIMES.
      APPEND  i_string+l_offset(i_tabline_length) TO et_table.
      l_offset = l_offset + i_tabline_length.
    ENDDO.

    IF l_last_length  > 0.
      APPEND i_string+l_offset(l_last_length) TO et_table.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
