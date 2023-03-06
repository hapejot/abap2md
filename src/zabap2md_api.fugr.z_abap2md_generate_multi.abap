FUNCTION z_abap2md_generate_multi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
*"     VALUE(IV_OBJ_SET) TYPE  ZABAP2MD_DOCELEM-DOCSET OPTIONAL
*"     VALUE(IX_OPTIONS) TYPE  ZABAP2MD_OPTIONS
*"  EXPORTING
*"     VALUE(ET_DOC) TYPE  STRINGTAB
*"----------------------------------------------------------------------
**/
* @page p3 Extraction Process
* @section s1 Function module for generating multiple docs
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
* @param iv_obj_set optional parameter defining the document set that is stored in the configuration table ZABAP2MD_DOCELEM.
*/

  TRY.
      IF iv_obj_set IS NOT INITIAL.
        CLEAR it_names.
        DATA(obj_set) = to_upper( iv_obj_set ).
        SELECT name
                FROM zabap2md_docelem
                WHERE docset = @obj_set
                INTO TABLE @it_names.
      ENDIF.

      IF it_names IS NOT INITIAL.
        DATA(lo_app) = NEW zcl_abap2md_main( ).
        DATA(obj_names) = lo_app->resolve_names( it_names ).

        LOOP AT obj_names INTO DATA(name).
          lo_app->add( name ).
        ENDLOOP.
        et_doc[] = lo_app->generate_multiple( ix_options ).
      ENDIF.
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
