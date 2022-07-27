FUNCTION z_abap2md_generate_doc_struct.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
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
      DATA(lo_app) = NEW zcl_abap2md_main( ).

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



      LOOP AT obj_names INTO DATA(name).
        lo_app->add( name ).
      ENDLOOP.
      es_struct = lo_app->build_structure( ).
      ev_doc = /ui2/cl_json=>serialize( data = es_struct
                                compress = abap_true
                                pretty_name = /ui2/cl_json=>pretty_mode-extended ).
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
