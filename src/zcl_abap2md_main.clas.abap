CLASS zcl_abap2md_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
**/
* This class extracts documentation from source code of other classes.
* this source code has to be documented in a special fashion.
* Also the standard document strings of the SE24 are included for documentation.
*
*
*/

  PUBLIC SECTION.
    METHODS generate_single
      IMPORTING
        !iv_name       TYPE seoclname
      RETURNING
        VALUE(rt_text) TYPE stringtab
      RAISING
        zcx_abap2md_error .
    METHODS generate_multiple .
  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS read_object_info
      IMPORTING
        iv_name         TYPE seoclname
      RETURNING
        VALUE(r_result) TYPE REF TO lif_info.
ENDCLASS.


CLASS zcl_abap2md_main IMPLEMENTATION.


  METHOD generate_multiple.
  ENDMETHOD.


  METHOD generate_single.
**/
* @param iv_name is the class name of the class to be documented. This name can be in lower case.
* @return a string table containing the raw mark down description of the class.
*/
    TRY.
        DATA(lo_info) = read_object_info( iv_name ).
        IF lo_info IS BOUND.
          lo_info->read_main( ).
          lo_info->build_doc_structure( ).
          lo_info->generate_markdown( CHANGING ct_text = rt_text ).
        ENDIF.
      CATCH zcx_abap2md_error.
        ASSERT 1 = 0.
    ENDTRY.

  ENDMETHOD.


  METHOD read_object_info.
**/
* Trying to read different object types with the passed name.
* first try is as a class name.
* @param iv_name name of the object
*/
    r_result = lcl_class_info=>try_read( CONV #( to_upper( iv_name ) ) ).
    IF r_result IS INITIAL.
      r_result = lcl_program_info=>try_read( to_upper( iv_name ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
