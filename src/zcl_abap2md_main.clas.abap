CLASS zcl_abap2md_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
**/
* @page p2
* @section s5 Main processor
* This should explain what the main processor does and the steps.
* The steps are important, since the first step doesn't read all the data
* The second step reads the source code and parses the content in order to
* find the relevant tags in the comments. This however is the major action.
*/

**/
* This class extracts documentation from source code of other classes.
* this source code has to be documented in a special fashion.
* Also the standard document strings of the SE24 are included for documentation.
*
*
*/

  PUBLIC SECTION.
    TYPES:
             obj_name TYPE tadir-obj_name.
    METHODS generate_single
      IMPORTING
        !iv_name       TYPE obj_name
      RETURNING
        VALUE(rt_text) TYPE stringtab
      RAISING
        zcx_abap2md_error .
    METHODS generate_multiple
      IMPORTING ix_options    TYPE zabap2md_options OPTIONAL
      RETURNING
                VALUE(r_text) TYPE stringtab.
    METHODS add
      IMPORTING
        i_name TYPE obj_name.
    METHODS build_structure
      RETURNING
        VALUE(r_result) TYPE zabap2md_doc_structure.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_names TYPE STANDARD TABLE OF obj_name WITH DEFAULT KEY.


    METHODS read_object_info
      IMPORTING
        iv_name         TYPE obj_name
      RETURNING
        VALUE(r_result) TYPE REF TO zif_abap2md_info.
ENDCLASS.



CLASS zcl_abap2md_main IMPLEMENTATION.


  METHOD add.
    APPEND i_name TO mt_names.
  ENDMETHOD.


  METHOD build_structure.
**/
* generate documentation from a list of dev objects.
* before this can be run the object needs to be created and dev objects
* have to be added using the *add* method.
*
* @return the document structure generated form the given development objects.
*           this structure can be used to generate different output than the
*           markdown. It contains all details in a more uniform manner.
*/
    DATA: name  TYPE obj_name,
          infos TYPE STANDARD TABLE OF REF TO zif_abap2md_info,
          text  TYPE zabap2md_text.
    DATA(main_gen) = NEW zcl_abap2md_doc_generator( REF #( text ) ).
    LOOP AT mt_names INTO name.
      TRY.
          DATA(lo_info) = read_object_info( name ).
          IF lo_info IS BOUND.
            lo_info->read_main( ).
            lo_info->build_doc_structure( main_gen ).
            APPEND lo_info TO infos.
          ENDIF.
        CATCH zcx_abap2md_error.
*          ASSERT 1 = 0.
      ENDTRY.
    ENDLOOP.
    r_result = main_gen->doc.
  ENDMETHOD.


  METHOD generate_multiple.
**/
* generate documentation from a list of dev objects.
* before this can be run the object needs to be created and dev objects
* have to be added using the *add* method.
*
* @return a complete markdown text with all documented components.
*/
    DATA: name  TYPE obj_name,
          infos TYPE STANDARD TABLE OF REF TO zif_abap2md_info.
    DATA(main_gen) = NEW zcl_abap2md_doc_generator( REF #( r_text ) ).

    main_gen->options( ix_options ).
    LOOP AT mt_names INTO name.
      TRY.
          DATA(lo_info) = read_object_info( name ).
          IF lo_info IS BOUND.
            lo_info->read_main( ).
            lo_info->build_doc_structure( main_gen ).
            APPEND lo_info TO infos.
          ENDIF.
        CATCH zcx_abap2md_error.
*          ASSERT 1 = 0.
      ENDTRY.
    ENDLOOP.

    TRY.
        build_structure( ).
        main_gen->generate_markdown( CHANGING ct_text = r_text ).
*        LOOP AT infos INTO lo_info.
*          APPEND INITIAL LINE TO r_text.
*          lo_info->generate_markdown( CHANGING ct_text = r_text ).
*        ENDLOOP.
      CATCH zcx_abap2md_error.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.


  METHOD generate_single.
**/
* generates documentation for a single development object.
*
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
    r_result = zcl_abap2md_class_info=>try_read( CONV #( to_upper( iv_name ) ) ).
    IF r_result IS INITIAL.
      r_result = zcl_abap2md_program_info=>try_read( to_upper( iv_name ) ).
    ENDIF.
    IF r_result IS INITIAL.
      r_result = zcl_abap2md_function_info=>try_read( to_upper( iv_name ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
