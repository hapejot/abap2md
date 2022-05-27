FUNCTION z_abap2md_generate_multi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_NAMES) TYPE  ZABAP2MD_OBJECT_NAMES
*"  EXPORTING
*"     VALUE(ET_DOC) TYPE  STRINGTAB
*"----------------------------------------------------------------------

  TRY.

      DATA(lo_app) = NEW zcl_abap2md_main( ).
      LOOP AT it_names INTO DATA(name).
        lo_app->add( name ).
      ENDLOOP.
      et_doc[] = lo_app->generate_multiple( ).
    CATCH zcx_abap2md_error.    "
  ENDTRY.

ENDFUNCTION.
