FUNCTION z_abap2md_generate_doc_struct.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
*"     VALUE(IV_OBJ_SET) TYPE  ZABAP2MD_DOCELEM-DOCSET OPTIONAL
*"  EXPORTING
*"     VALUE(ES_STRUCT) TYPE  ZABAP2MD_DOC_STRUCTURE
*"     VALUE(EV_DOC) TYPE  STRING
*"----------------------------------------------------------------------
**/
* Generating the documentation structure only for several sources into one result
* document.
* @param it_names   gives the list of dev objects to be taken into concideration
* @param ev_doc     is the resulting documentation as json
* @param es_struct  is the document structure as is
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
        IF obj_names IS NOT INITIAL.
          LOOP AT obj_names INTO DATA(name).
            lo_app->add( name ).
          ENDLOOP.
          es_struct = lo_app->build_structure( ).
          ev_doc = /ui2/cl_json=>serialize( data = es_struct
                                    compress = abap_true
                                    pretty_name = /ui2/cl_json=>pretty_mode-extended ).
        ENDIF.
      ENDIF.
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
