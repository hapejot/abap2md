CLASS zcl_abap2md_method_parser DEFINITION
INHERITING FROM zcl_abap2md_doc_parser
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_class         TYPE rpyclme-clsname
        i_method        TYPE rpyclme-cmpname
        i_code          TYPE rswsourcet
        i_doc           TYPE REF TO zabap2md_doc_structure
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_abap2md_method_parser.

  PROTECTED SECTION.
    METHODS:
      handle_date REDEFINITION.
  PRIVATE SECTION.
    DATA:
      current_class  TYPE REF TO zabap2md_class_info,
      current_method TYPE REF TO zabap2md_method_info.
    METHODS method_name
      IMPORTING
        i_class  TYPE rpyclme-clsname
        i_method TYPE rpyclme-cmpname.
ENDCLASS.



CLASS zcl_abap2md_method_parser IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT r_result
      EXPORTING
        i_code = i_code
        i_doc  = i_doc.
    r_result->method_name( i_class = i_class
                            i_method = i_method ).
  ENDMETHOD.


  METHOD method_name.

    current_class = REF #( doc->classes[ name = i_class ] OPTIONAL ).
    IF current_class IS INITIAL.
      APPEND VALUE #( name = i_class ) TO doc->classes REFERENCE INTO current_class.
    ENDIF.
    current_method = REF #( current_class->methods[ name = i_method ] OPTIONAL ).
    IF current_method IS INITIAL.
      APPEND VALUE #( name = i_method ) TO current_class->methods REFERENCE INTO current_method.
    ENDIF.
    current_text = REF #( current_method->text ).

  ENDMETHOD.



  METHOD handle_date.
    APPEND VALUE #( date = token-value ) TO current_method->changes REFERENCE INTO DATA(current_change).
    current_text = REF #( current_change->text ).
    next_token( ).
  ENDMETHOD.

ENDCLASS.
