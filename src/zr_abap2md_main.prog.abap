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
* @page p2 Why using this style of documentation?
* There exists already  a style named abapdoc for documentation
* It has some drawbacks:
*   1. it only documents parts of classes
*   2. the documentation has to be on the header so in SE24 this is invisible
*   3. since the documentation is in the header, whenever the body is changed
*      the documentation in the header will likely be forgotten to be updated.
*
* @section s1 First Section
* The text is part of the first section.
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
PARAMETERS: p_path  TYPE zcl_abap2md_local_file=>t_dir.

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
  SELECT obj_name
          FROM tadir
          WHERE obj_name IN @s_objs
          AND pgmid = 'R3TR'
          AND object IN ( 'CLAS', 'PROG' )
  INTO TABLE @obj_names.
  CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'
    EXPORTING
      it_names = obj_names
    IMPORTING
      et_doc   = doc.

  DATA(file) = NEW zcl_abap2md_local_file( ).
  file->add_text( doc ).
  file->save( i_path = P_path ).
