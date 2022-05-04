FUNCTION z_abap2md_generate_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NAME) TYPE  SEOCLNAME
*"  EXPORTING
*"     VALUE(ET_DOC) TYPE  STRINGTAB
*"----------------------------------------------------------------------


  DATA(lo_app) = NEW zcl_abap2md_main( ).
  TRY.
      et_doc[] = lo_app->generate_single( iv_name = iv_name ).
    CATCH zcx_abap2md_error.    "


  ENDTRY.

ENDFUNCTION.
