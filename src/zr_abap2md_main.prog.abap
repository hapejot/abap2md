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
* ```graphviz
* digraph G {
*    A -> C
*    A -> D
*    B -> E
*    B -> F
* }
* ```
*
*/


**/
* @param obj is a select option defining a set of objects.
*/
REPORT zr_abap2md_main.
DATA:
  rc        TYPE i,
  doc       TYPE stringtab,
  path      TYPE zcl_abap2md_local_file=>t_dir,
  files     TYPE filetable,
  obj_names TYPE STANDARD TABLE OF zcl_abap2md_main=>obj_name,
  obj_name  TYPE zcl_abap2md_main=>obj_name.
SELECT-OPTIONS:
    s_objs FOR obj_name.
PARAMETERS: p_path  TYPE zcl_abap2md_local_file=>t_dir DEFAULT 'abap-doc.md'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  cl_gui_frontend_services=>file_open_dialog(
    CHANGING
      file_table              = files
      rc                      = rc
  ).
  IF lines( files ) > 0.
    p_path = files[ 1 ].
  ENDIF.

START-OF-SELECTION.

  " first select possible candidates from TADIR
  SELECT obj_name
        FROM tadir
        WHERE obj_name IN @s_objs
        AND pgmid = 'R3TR'
        AND object IN ( 'CLAS', 'PROG' )
        INTO TABLE @obj_names.

  " next try the same with TFDIR
  SELECT funcname
        FROM tfdir
        WHERE funcname IN @s_objs
        APPENDING TABLE @obj_names.


  CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
    EXPORTING
      it_names = obj_names
    IMPORTING
      et_doc   = doc.

  DATA(file) = NEW zcl_abap2md_local_file( ).
  file->add_text( doc ).
  file->save( i_path = P_path ).
