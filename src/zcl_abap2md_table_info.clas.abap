CLASS zcl_abap2md_table_info DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_info.
    TYPES:
      tt_fields      TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY,
      tt_fkeyfields  TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY,
      tt_fkeys       TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
      tt_indexes     TYPE STANDARD TABLE OF dd12v WITH DEFAULT KEY,
      tt_indexfields TYPE STANDARD TABLE OF dd17v WITH DEFAULT KEY,
      tt_shelps      TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
      tt_shelp_alloc TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY.
    CLASS-METHODS try_read
      IMPORTING
        iv_name          TYPE tadir-obj_name
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abap2md_info.
    METHODS constructor
      IMPORTING
        iv_state       TYPE ddgotstate
        iv_hd          TYPE dd02v
        iv_tech        TYPE dd09v
        iv_fields      TYPE tt_fields
        iv_fkeyfields  TYPE tt_fkeyfields
        iv_fkeys       TYPE tt_fkeys
        iv_indexes     TYPE tt_indexes
        iv_indexfields TYPE tt_indexfields
        iv_shelps      TYPE tt_shelps
        iv_shelp_alloc TYPE tt_shelp_alloc.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_pline   TYPE STANDARD TABLE OF tline WITH DEFAULT KEY,
      ty_symbols TYPE STANDARD TABLE OF itcst WITH DEFAULT KEY.
    METHODS sapscript_to_markdown
      IMPORTING
                i_lines       TYPE ty_pline
      RETURNING VALUE(r_text) TYPE zabap2md_text.
    CLASS-DATA: m_name        TYPE ddobjname.
    DATA: m_state       TYPE ddgotstate,
          m_hd          TYPE dd02v,
          m_tech        TYPE dd09v,
          m_fields      TYPE tt_fields,
          m_fkeyfields  TYPE tt_fkeyfields,
          m_fkeys       TYPE tt_fkeys,
          m_indexes     TYPE tt_indexes,
          m_indexfields TYPE tt_indexfields,
          m_shelps      TYPE tt_shelps,
          m_shelp_alloc TYPE tt_shelp_alloc.

ENDCLASS.



CLASS zcl_abap2md_table_info IMPLEMENTATION.


  METHOD constructor.

    me->m_state = iv_state.
    me->m_hd = iv_hd.
    me->m_tech = iv_tech.
    me->m_fields = iv_fields.
    me->m_fkeyfields = iv_fkeyfields.
    me->m_fkeys = iv_fkeys.
    me->m_indexes = iv_indexes.
    me->m_indexfields = iv_indexfields.
    me->m_shelps = iv_shelps.
    me->m_shelp_alloc = iv_shelp_alloc.

  ENDMETHOD.


  METHOD sapscript_to_markdown.
**/
* - remove all the empty sections in the document
* - replace all the symbols in the lines
* - walk through the lines and produce markdown output.
*/
    DATA symbols TYPE ty_symbols.
    DATA replacement TYPE char80.
    DATA: lines TYPE STANDARD TABLE OF tline,
          idx   TYPE i.

    lines = i_lines.

    idx = lines( lines ).
    DATA(delete_heading) = abap_true.
    WHILE idx > 0.
      IF delete_heading = abap_true AND lines[ idx ]-tdformat(1) = 'U'.
        DELETE lines INDEX idx.
      ELSEIF lines[ idx ]-tdformat(1) = 'U'.
        delete_heading = abap_true.
      ELSE.
        CLEAR delete_heading.
      ENDIF.
      idx = idx - 1.
    ENDWHILE.

    CALL FUNCTION 'COLLECT_TEXTSYMBOL'
      TABLES
        lines   = lines
        symbols = symbols.

    LOOP AT symbols INTO DATA(sym).
      DATA(s) = CONV char80(  |&{ sym-name }&| ).
      CALL FUNCTION 'GET_TEXTSYMBOL'
        EXPORTING
          line         = s
          start_offset = 0
        IMPORTING
          value        = replacement.
      IF replacement IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF s IN TABLE lines WITH replacement.
      ENDIF.
    ENDLOOP.
    DATA(num) = 1.
    LOOP AT lines REFERENCE INTO DATA(r_line).
      CASE r_line->tdformat.
        WHEN 'AS'.
          APPEND INITIAL LINE TO r_text.
          APPEND r_line->tdline TO r_text.
        WHEN 'U1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |## { r_line->tdline }| TO r_text.
          num = 1.
        WHEN 'U2'.
          APPEND INITIAL LINE TO r_text.
          APPEND |### { r_line->tdline }| TO r_text.
          num = 1.
        WHEN 'U3'.
          APPEND INITIAL LINE TO r_text.
          APPEND |#### { r_line->tdline }| TO r_text.
          num = 1.
        WHEN 'U4'.
          APPEND INITIAL LINE TO r_text.
          APPEND |##### { r_line->tdline }| TO r_text.
          num = 1.
        WHEN 'N1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |{ num }. { r_line->tdline }| TO r_text.
          num = num + 1.
        WHEN 'B1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |- { r_line->tdline }| TO r_text.
        WHEN '= '.
          idx = lines( r_text ).
          r_text[ idx ] = r_text[ idx ] && r_line->tdline.
        WHEN OTHERS.
          APPEND |{ r_line->tdformat } { r_line->tdline }| TO r_text.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD try_read.
    DATA: l_state       TYPE ddgotstate,
          l_hd          TYPE dd02v,
          l_tech        TYPE dd09v,
          l_fields      TYPE STANDARD TABLE OF dd03p,
          l_fkeyfields  TYPE STANDARD TABLE OF dd05m,
          l_fkeys       TYPE STANDARD TABLE OF dd08v,
          l_indexes     TYPE STANDARD TABLE OF dd12v,
          l_indexfields TYPE STANDARD TABLE OF dd17v,
          l_shelps      TYPE STANDARD TABLE OF dd35v,
          l_shelp_alloc TYPE STANDARD TABLE OF dd36m.
    m_name = iv_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = m_name
*       state         = 'A'
        langu         = sy-langu
      IMPORTING
        gotstate      = l_state
        dd02v_wa      = l_hd
        dd09l_wa      = l_tech
      TABLES
        dd03p_tab     = l_fields
        dd05m_tab     = l_fkeyfields
        dd08v_tab     = l_fkeys
        dd12v_tab     = l_indexes
        dd17v_tab     = l_indexfields
        dd35v_tab     = l_shelps
        dd36m_tab     = l_shelp_alloc
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      DATA(obj) = NEW zcl_abap2md_table_info(
        iv_state        = l_state
        iv_hd           = l_hd
        iv_tech         = l_tech
        iv_fields       = l_fields
        iv_fkeyfields   = l_fkeyfields
        iv_fkeys        = l_fkeys
        iv_indexes      = l_indexes
        iv_indexfields  = l_indexfields
        iv_shelps       = l_shelps
        iv_shelp_alloc  = l_shelp_alloc
       ).
      ro_result = obj.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abap2md_info~build_doc_structure.
    DATA(doc) = i_gen->doc( ).
    DATA text TYPE zabap2md_text.
    DATA: pline       TYPE STANDARD TABLE OF tline,
          l_doc_id    TYPE dokhl-id VALUE 'TB',
          l_object    TYPE dokhl-object,
          symbols     TYPE STANDARD TABLE OF itcst,
          replacement TYPE char80.
    APPEND VALUE zabap2md_table_info( name = m_hd-tabname ) TO  doc->tables REFERENCE INTO DATA(cur_table).
    cur_table->title = m_hd-ddtext.
    l_object = m_name.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = l_doc_id
        langu  = sy-langu
        object = l_object
      TABLES
        line   = pline
      EXCEPTIONS
        OTHERS = 5.
    CALL FUNCTION 'SDU_DOCU_REPLACE'
      TABLES
        lines  = pline
      EXCEPTIONS
        OTHERS = 3.

    IF sy-subrc = 0 AND pline IS NOT INITIAL.
      cur_table->text = sapscript_to_markdown( pline ).
    ENDIF.

    LOOP AT m_fields REFERENCE INTO DATA(fld).
      CLEAR text.
      IF fld->ddtext IS NOT INITIAL.
        APPEND fld->ddtext TO text.
      ELSEIF fld->scrtext_l IS NOT INITIAL.
        APPEND fld->scrtext_l TO text.
      ENDIF.
      APPEND VALUE #( name = fld->fieldname
                      data_type = |{ fld->datatype } { fld->domname } { fld->rollname }|
                      text = text  ) TO cur_table->fields.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abap2md_info~generate_markdown.

  ENDMETHOD.


  METHOD zif_abap2md_info~read_main.

  ENDMETHOD.
ENDCLASS.
