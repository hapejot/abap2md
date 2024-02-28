CLASS zcl_abap2md_function_parser DEFINITION
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
        VALUE(r_result) TYPE REF TO zcl_abap2md_function_parser.

  PROTECTED SECTION.
    METHODS:
      handle_cmd REDEFINITION,
      handle_end REDEFINITION,
      handle_date REDEFINITION.

  PRIVATE SECTION.
    DATA: m_report_name TYPE funcname,
          current_info  TYPE REF TO zabap2md_function_info.
    METHODS name IMPORTING i_name TYPE clike.
ENDCLASS.



CLASS zcl_abap2md_function_parser IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_result
      EXPORTING
        i_code = i_code
        i_doc  = i_doc.
    r_result->name( i_name ).

  ENDMETHOD.

  METHOD name.
    m_report_name = i_name.
    current_info = REF #( doc->functions[ name = m_report_name ] OPTIONAL ).
    IF current_info IS INITIAL.
      APPEND VALUE #( name = m_report_name ) TO doc->functions REFERENCE INTO current_info.
    ENDIF.
    current_text = NEW zcl_abap2md_text_lines( REF #( current_info->text ) ).
  ENDMETHOD.

  METHOD handle_cmd.
    CASE token-value.
      WHEN 'param'.
        next_token( ).
        DATA(line) = token-line.
        DATA(name) = to_upper( token-value ).
        DATA(param) = REF #( current_info->params[ name = name ] OPTIONAL ).
        IF param IS INITIAL.
          APPEND VALUE #( name = to_upper( token-value ) ) TO current_info->params REFERENCE INTO param.
        ENDIF.
        next_token( ).
        APPEND read_words_in_line( line ) TO param->text.

      WHEN OTHERS.
        super->handle_cmd( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_end.
    super->handle_end( ).
    current_text = NEW zcl_abap2md_text_lines( REF #( current_info->text ) ).
  ENDMETHOD.

  METHOD handle_date.

    APPEND VALUE #( date = token-value ) TO current_info->changes REFERENCE INTO DATA(current_change).
    current_text = NEW zcl_abap2md_text_lines( REF #( current_change->text ) ).
    next_token( ).

  ENDMETHOD.

ENDCLASS.
