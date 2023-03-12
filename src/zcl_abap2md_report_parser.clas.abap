CLASS zcl_abap2md_report_parser DEFINITION
INHERITING FROM zcl_abap2md_doc_parser
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_code          TYPE stringtab
        i_doc           TYPE REF TO zabap2md_doc_structure
        i_report_name   TYPE progn
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_abap2md_report_parser.


  PROTECTED SECTION.
    METHODS handle_cmd REDEFINITION.
    METHODS handle_end REDEFINITION.
    methods handle_date REDEFINITION.
  PRIVATE SECTION.
    DATA: m_report_name  TYPE progn,
          current_report TYPE REF TO zabap2md_program_info.
    METHODS report_name IMPORTING i_name TYPE progn.
ENDCLASS.



CLASS zcl_abap2md_report_parser IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_result
      EXPORTING
        i_code = i_code
        i_doc  = i_doc.
    r_result->report_name( i_report_name ).

  ENDMETHOD.

  METHOD report_name.
    m_report_name = i_name.
    current_report = REF #( doc->programs[ name = m_report_name ] OPTIONAL ).
    IF current_report IS INITIAL.
      APPEND VALUE #( name = m_report_name ) TO doc->programs REFERENCE INTO current_report.
    ENDIF.
    current_text = REF #( current_report->text ).
  ENDMETHOD.

  METHOD handle_cmd.
    CASE token-value.
      WHEN 'param'.
        next_token( ).
        DATA(line) = token-line.
        DATA(name) = to_upper( token-value ).
        DATA(param) = REF #( current_report->params[ name = name ] OPTIONAL ).
        IF param IS INITIAL.
          APPEND VALUE #( name = to_upper( token-value ) ) TO current_report->params REFERENCE INTO param.
        ENDIF.
        next_token( ).
        APPEND read_words( line ) TO param->text.

      WHEN OTHERS.
        super->handle_cmd( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_end.
    super->handle_end( ).
    current_text = REF #( current_report->text ).
  ENDMETHOD.

  METHOD handle_date.

    APPEND VALUE #( date = token-value ) TO current_report->changes REFERENCE INTO DATA(current_change).
    current_text = REF #( current_change->text ).
    next_token( ).

  ENDMETHOD.

ENDCLASS.
