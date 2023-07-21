**/
* @page main ABAP to Markdown
*
* For generating documentation for existing ABAP code one can use several ways.
* running the report for downloading a complete markdown document is one of it.
* another way is using the RFC module using the C-Client for generating the documentation
* in a more automated fashion on the client.
*
*/

**/
* @page p2 General procedure of document setup
* The program is flexible but follows a fixed procedure to create the documentation
* There are basically three phases: reading object info, parsing comments, output markdown
*
* @section s1 Reading Objects
*
* In the initial phase the program is gathering lots of informations from the system
* but only some meta data is read. The sources are not touched in this phase to avoid
* unnecessary memory consumption.
*
* @section s2 Building Documentation Structure
*
* This phase walks through all the gathered informations and also parses the sources.
* THe parsing results are directly fed into the documentation structures. The Meta data
* then is also filled into the documentation structures.
*
* @section s3 Output Markdown
*
* This phase generates a markdown text from the documentation structure.
* This step can easily replaced by some other generating tool since
* all sematinc information should be ready available in the documentation structure.
*
* ```{.graphviz caption="Example of displaying a graph" width=50%}
* digraph G {
*    node [ shape = box ]
*    MAIN -> GENERATE_MULTI
*    GENERATE_MULTI -> GENERATE_DOC_STRUCT
*    GENERATE_SINGLE -> GENERATE_DOC_STRUCT
*    GENERATE_DOC_STRUCT -> ZCL_MAIN
* }
* ```
*
* ```{.plantuml caption="General Flow" width=50%}
* Client -> Main : generate_multi
* Main -> Info : read_main
* Main -> Info : build_doc_structure
* Info -> Generator : add text
* Main -> Generator : get_text
* Main -> Client : return text
* ```
*/


**/
* @param obj is a select option defining a set of objects.
*/
REPORT zr_abap2md_main  MESSAGE-ID zabap2md.



PARAMETERS:
  p_objset TYPE seoclsname AS LISTBOX VISIBLE LENGTH 40 MEMORY ID zabap2md_doc_set,
  p_path   TYPE zcl_abap2md_local_file=>t_dir MEMORY ID zabap2md_path.



SELECTION-SCREEN:
    PUSHBUTTON  /35(17) text-001 USER-COMMAND html,
    PUSHBUTTON   55(17) text-002 USER-COMMAND md.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS:
      init,
      at_sel_scr_out,
      at_start_of_sel,
      handle_command
        IMPORTING
          i_ucomm TYPE syst-ucomm,
      at_value_req_path.
  PRIVATE SECTION.
    DATA:

      html     TYPE REF TO zcl_abap2md_html,
      options  TYPE zabap2md_options,
      doc      TYPE stringtab,
      elements TYPE STANDARD TABLE OF zabap2md_docelem,
      path     TYPE string VALUE '.'.
    METHODS generate_markdown.
    METHODS show_html.
    METHODS save_markdown.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD init.
  ENDMETHOD.

  METHOD at_sel_scr_out.
    DATA: values   TYPE vrm_values.
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
  ENDMETHOD.

  METHOD at_start_of_sel.
    show_html( ).
  ENDMETHOD.

  METHOD show_html.

    generate_markdown( ).
    html->render_md_as_html( doc ).

  ENDMETHOD.



  METHOD generate_markdown.

    DATA: names    TYPE zabap2md_object_names.
    html = NEW zcl_abap2md_html( ).
    CLEAR names.
    LOOP AT elements INTO DATA(element) WHERE docset = p_objset.
      APPEND element-name TO names.
    ENDLOOP.
    options-markdown-use_pipe_tables = abap_true.
    CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
      EXPORTING
        it_names   = names
        ix_options = options
      IMPORTING
        et_doc     = doc.    " Table with Strings

  ENDMETHOD.

  METHOD handle_command.
    IF p_objset IS INITIAL.
      MESSAGE i001.
      RETURN.
    ENDIF.
    IF p_path IS NOT INITIAL.
      path = p_path.
    ENDIF.
    CASE i_ucomm.
      WHEN 'HTML'.
        show_html( ).
      WHEN 'MD'.
        save_markdown( ).
    ENDCASE.

  ENDMETHOD.


  METHOD save_markdown.
    generate_markdown( ).
    DATA(file) = NEW zcl_abap2md_local_file( ).
    file->add_text( doc ).
    IF path IS INITIAL.
      file->save( i_path = |{ p_objset }.md| ).
    ELSE.
      file->save( i_path = |{ path }\\{ p_objset }.md| ).
    ENDIF.
  ENDMETHOD.


  METHOD at_value_req_path.
    DATA: files TYPE filetable,
          rc    TYPE i.

    cl_gui_frontend_services=>directory_browse(
      CHANGING
        selected_folder      = p_path
      EXCEPTIONS
        OTHERS               = 0
    ).

    path = p_path.

  ENDMETHOD.

ENDCLASS.




INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  app->init( ).

AT SELECTION-SCREEN OUTPUT.
  app->at_sel_scr_out( ).

AT SELECTION-SCREEN.
  app->handle_command( sy-ucomm ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  app->at_value_req_path( ).

START-OF-SELECTION.
  app->at_start_of_sel( ).
