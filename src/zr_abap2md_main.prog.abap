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
REPORT zr_abap2md_main.
DATA:
  rc          TYPE i,
  doc         TYPE stringtab,
  json_string TYPE string,
  path        TYPE zcl_abap2md_local_file=>t_dir,
  files       TYPE filetable,
  obj_names   TYPE STANDARD TABLE OF zcl_abap2md_main=>obj_name,
  obj_name    TYPE zcl_abap2md_main=>obj_name,
  last_objset TYPE seoclsname,
  values      TYPE vrm_values,
  elements    TYPE STANDARD TABLE OF zabap2md_docelem,
  field_name  TYPE fieldname VALUE 'P_OBJSET',
  options     TYPE zabap2md_options.

PARAMETERS:
  p_objset TYPE zabap2md_docelem-docset AS LISTBOX VISIBLE LENGTH 35,
  p_json   TYPE abap_bool AS CHECKBOX.

PARAMETERS:
  p_pipe TYPE abap_bool AS CHECKBOX,
  p_path TYPE zcl_abap2md_local_file=>t_dir.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_filename = p_path
    CHANGING
      file_table       = files
      rc               = rc
  ).
  IF lines( files ) > 0.
    p_path = files[ 1 ].
  ENDIF.

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

  IF p_objset IS NOT INITIAL AND last_objset <> p_objset.
    last_objset = p_objset.
    p_path = |{ p_objset }.md|.
  ENDIF.



START-OF-SELECTION.
  options-markdown-use_pipe_tables = p_pipe.
  IF p_json IS INITIAL.
    CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
      EXPORTING
        it_names   = obj_names
        iv_obj_set = p_objset
        ix_options = options
      IMPORTING
        et_doc     = doc.
  ELSE.
    CALL FUNCTION 'Z_ABAP2MD_GENERATE_DOC_STRUCT'
      EXPORTING
        it_names   = obj_names
        iv_obj_set = p_objset
      IMPORTING
*       es_struct  =     " Documentation Structure
        ev_doc     = json_string.    " JSON representation of the doc structure

    doc = VALUE #( ( json_string ) ).
  ENDIF.
  DATA(file) = NEW zcl_abap2md_local_file( ).
  file->add_text( doc ).
  file->save( i_path = p_path ).
