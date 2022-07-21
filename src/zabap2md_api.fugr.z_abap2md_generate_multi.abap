FUNCTION z_abap2md_generate_multi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
*"  EXPORTING
*"     VALUE(ET_DOC) TYPE  STRINGTAB
*"----------------------------------------------------------------------
**/
* FM
* Generating the documentation for several sources into one result
* document.
* @param it_names   gives the list of dev objects to be taken into concideration
* @param et_doc     is the resulting documentation as markdown text, line by line
*/

  TRY.

      DATA(lo_app) = NEW zcl_abap2md_main( ).
      LOOP AT it_names INTO DATA(name).
        lo_app->add( name ).
      ENDLOOP.
      et_doc[] = lo_app->generate_multiple( ).
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
