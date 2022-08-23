FUNCTION z_abap2md_generate_multi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
*"  EXPORTING
*"     VALUE(ET_DOC) TYPE  STRINGTAB
*"----------------------------------------------------------------------
**/
* @page p2 Extraction Process
* @section s4 Function module for generating multiple docs
* In order to generate markdown one should use this function
* module. This gives the markdown result as a table of lines.
* this markdown can than be further processed by the client.
*
* The module is desigend as RFC so this can be used by remote
* programs to retrieve the documentation. For example there is a
* docker image that will process the output directly using pandoc
* into PDF and DOCX formats, including processing of graph
* descriptions.
*/


**/
* FM
* Generating the documentation for several sources into one result
* document.
* @param it_names   gives the list of dev objects to be taken into concideration
* @param et_doc     is the resulting documentation as markdown text, line by line
*/

  TRY.
      DATA obj_names TYPE STANDARD TABLE OF tadir-obj_name.
      DATA name_range TYPE RANGE OF tadir-obj_name.
      " first select possible candidates from TADIR

      name_range = VALUE #( FOR <x> IN it_names ( sign = 'I' option = 'CP' low = to_upper( <x> ) ) ).
      SELECT obj_name
            FROM tadir
            WHERE obj_name IN @name_range
            AND pgmid = 'R3TR'
            AND object IN ( 'CLAS', 'PROG' )
            APPENDING TABLE @obj_names.

      " next try the same with TFDIR
      SELECT funcname
            FROM tfdir
            WHERE funcname IN @name_range
            APPENDING TABLE @obj_names.

      DATA(lo_app) = NEW zcl_abap2md_main( ).
      LOOP AT obj_names INTO DATA(name).
        lo_app->add( name ).
      ENDLOOP.
      et_doc[] = lo_app->generate_multiple( ).
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
