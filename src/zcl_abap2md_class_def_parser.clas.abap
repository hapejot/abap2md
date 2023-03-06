CLASS zcl_abap2md_class_def_parser DEFINITION
INHERITING FROM zcl_abap2md_doc_parser
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_code          TYPE stringtab
        i_doc           TYPE REF TO zabap2md_doc_structure
        i_name          TYPE funcname
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_abap2md_class_def_parser.

  PROTECTED SECTION.
    METHODS:
      handle_cmd REDEFINITION,
      handle_end REDEFINITION,
      handle_date REDEFINITION.

  PRIVATE SECTION.
    DATA: m_name TYPE funcname,
          current_info  TYPE REF TO zabap2md_class_info.
    METHODS name IMPORTING i_name type clike.
ENDCLASS.



CLASS zcl_abap2md_class_def_parser IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_result
      EXPORTING
        i_code = i_code
        i_doc  = i_doc.
    r_result->name( i_name ).

  ENDMETHOD.

  METHOD name.
    m_name = i_name.
    current_info = REF #( doc->classes[ name = m_name ] OPTIONAL ).
    IF current_info IS INITIAL.
      APPEND VALUE #( name = m_name ) TO doc->classes REFERENCE INTO current_info.
    ENDIF.
    current_text = REF #( current_info->text ).
  ENDMETHOD.

  METHOD handle_cmd.
    CASE token-value.

      WHEN OTHERS.
        super->handle_cmd( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_end.
    super->handle_end( ).
    current_text = REF #( current_info->text ).
  ENDMETHOD.

  METHOD handle_date.

    APPEND VALUE #( date = token-value ) TO current_info->changes REFERENCE INTO DATA(current_change).
    current_text = REF #( current_change->text ).
    next_token( ).

  ENDMETHOD.

ENDCLASS.
