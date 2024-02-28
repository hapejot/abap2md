**/
* @page 1 ABAP to Markdown
*
* For generating documentation for existing ABAP code one can use several ways.
* running the report for downloading a complete markdown document is one of it.
* another way is using the RFC module using the C-Client for generating the documentation
* in a more automated fashion on the client.
*
*/

**/
* @page 2 Reference
*
* ABAP2MD allows for documenting details that are not directly related to
* any development object. For example it is possible to explain the overall
* architecture of a solution in a sepearate part of the documentation, since this explains
* several development objects and their interactions or relationships. It would
* be wrong to just put this explanation into the documentation of a single object.
*
* The way we use the documention here is still storing the explanation at some development
* object. Typically a report that is very central to the development.
* In order to bring the architecture descriptions into the front of the resulting
* document one can use the *page*, *section*, and *subsection* commands.
*
* The documentation tool finally assembles all pages with their sections into one
* document. This way it is possible to add sections to one single page with comments
* from different development objects.
*
* For example you are describing The general architecture, but you are adding some aspects
* from different angels to it from individual ABAP-Classes that are written.
*
* Pages:
* ABAP2MD organizes information into pages. Each page can contain multiple sections, each with its own title and content. Pages are created using the
* @@page command followed by a unique name for the page, which acts as an identifier within your documentation. You can then add content to this page using
* the @@section command, which allows you to create named sections on that page.
*
* Sections:
* Sections in ABAP2MD are used to organize information within a page. Each section is identified by a unique name and consists of content between the
* corresponding @@section and @@subsection commands. You can nest subsections within sections to create a hierarchical structure for your documentation.
* Sections are useful for grouping related information together, making it easier for users to find relevant details.
*
* ```
*    @@page *name* (title)
* ```
*
* Indicates that a comment block contains a piece of documentation that
* is not directly related to one specific class, file or member.
*
* Example:
*
* ```
*    **/
*    * @page page1 A documentation page
*    *
*    * Leading text.
*    * @section sec An example section
*    * This page contains the subsections \ref subsection1 and \ref subsection2.
*    * For more info see page \ref page2.
*    * @@subsection subsection1 The first subsection
*    * Text.
*    * @@subsection subsection2 The second subsection
*    * More text.
*    */
*
*    **/
*    * @page page2 Another page
*    *  Even more info.
*    */
* ```
*
*    Note:
*
*    The <name> argument consists of a combination of letters and number digits.
*
*
* All commands in the documentation start with an at-sign (@).
*
* Some commands have one or more arguments. Each argument has a certain range:
*
* If <sharp> braces are used the argument is a single word.
*
* If (round) braces are used the argument extends until the end of the line on
*  which the command was found.
*
* If {curly} braces are used the argument extends until the next paragraph.
* Paragraphs are delimited by a blank line or by a section indicator. Note that
* {curly} braces are also used for command options, here the braces are mandatory
* and just 'normal' characters. The starting curly brace has to directly follow the
* command, so without whitespace.
*
* If in addition to the above argument specifiers [square] brackets are used the
* argument is optional, unless they are placed between quotes in that case they are
* a mandatory part of the command argument.
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
    SELECT * FROM zabap2md_docelem "#EC CI_NOWHERE
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
