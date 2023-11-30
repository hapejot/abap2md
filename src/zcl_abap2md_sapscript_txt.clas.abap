CLASS zcl_abap2md_sapscript_txt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_tabline,
        f1 TYPE string,
        f2 TYPE string,
        f3 TYPE string,
        f4 TYPE string,
        f5 TYPE string,
        f6 TYPE string,
        f7 TYPE string,
        f8 TYPE string,
        f9 TYPE string,
      END OF t_tabline,
      BEGIN OF t_line,
        tdformat TYPE tline-tdformat,
        tdline   TYPE string,
      END OF t_line,
      t_symbols TYPE STANDARD TABLE OF itcst WITH DEFAULT KEY,
      t_text    TYPE STANDARD TABLE OF t_line WITH EMPTY KEY,
      t_td_text TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
    METHODS add_text
      IMPORTING
        i_txt TYPE t_td_text.
    METHODS constructor.
    METHODS get_markdown
      RETURNING
        VALUE(r_text) TYPE stringtab.
    METHODS cvt_fmt
      IMPORTING
        i_line        TYPE string
      RETURNING
        VALUE(r_line) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA m_lines TYPE t_text.
    DATA: BEGIN OF m,
            BEGIN OF tbl,
              hd    TYPE stringtab,
              flds  TYPE zif_abap2md_text_generator=>t_field_specs,
              lines TYPE STANDARD TABLE OF t_tabline,
            END OF tbl,
          END OF m.
    DATA: lines       TYPE STANDARD TABLE OF t_line,
          idx         TYPE i,
          replacement TYPE char80,
          symbols     TYPE t_symbols,
          m_regex     TYPE REF TO cl_abap_regex.
    METHODS remove_empty_sections.
    METHODS replace_symbols.

    METHODS expand_wrapped_lines.
    METHODS flush_table
      CHANGING c_text TYPE stringtab.
ENDCLASS.



CLASS zcl_abap2md_sapscript_txt IMPLEMENTATION.

  METHOD constructor.
    DATA rules TYPE stringtab.

    rules = VALUE #(    ( `<([A-Za-z0-9]+)>` )      " #1
                        ( `<(/)>` )                 " #2
                        ( `(.[^<]*)` )              " #3
                         ).
    IF m_regex IS INITIAL.
      CONCATENATE LINES OF rules INTO DATA(pattern) SEPARATED BY '|'.
      m_regex = NEW cl_abap_regex( pattern ).
    ENDIF.

  ENDMETHOD.

  METHOD add_text.

    LOOP AT i_txt REFERENCE INTO DATA(line).
      APPEND CORRESPONDING #( line->* ) TO m_lines.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_markdown.
    DATA tabline TYPE t_tabline.
    DATA flush_table TYPE abap_bool.

    lines = m_lines.

    remove_empty_sections( ).

    expand_wrapped_lines( ).

    replace_symbols( ).

    DATA(num) = 1.
    LOOP AT lines REFERENCE INTO DATA(r_line).
      IF flush_table = abap_true.
        flush_table( CHANGING c_text = r_text ).
      ENDIF.
      flush_table = abap_true.
      DATA(line) = cvt_fmt( r_line->tdline ).
      CASE r_line->tdformat.
        WHEN 'AS'.
          APPEND INITIAL LINE TO r_text.
          APPEND line TO r_text.
        WHEN 'U1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |## { line }| TO r_text.
          num = 1.
        WHEN 'U2'.
          APPEND INITIAL LINE TO r_text.
          APPEND |### { line }| TO r_text.
          num = 1.
        WHEN 'U3'.
          APPEND INITIAL LINE TO r_text.
          APPEND |#### { line }| TO r_text.
          num = 1.
        WHEN 'U4'.
          APPEND INITIAL LINE TO r_text.
          APPEND |##### { line }| TO r_text.
          num = 1.
        WHEN 'N1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |{ num }. { line }| TO r_text.
          num = num + 1.
        WHEN 'B1'.
          APPEND INITIAL LINE TO r_text.
          APPEND |- { line }| TO r_text.
        WHEN '= '.
          idx = lines( r_text ).
          r_text[ idx ] = r_text[ idx ] && line.
        WHEN 'T1' OR 'T2' OR 'T3' OR 'T4' OR 'T5' OR 'T6' OR 'T7' OR 'T8' OR 'T9'.
          IF m-tbl-hd IS INITIAL.
            SPLIT line AT ',,' INTO TABLE m-tbl-hd.
            LOOP AT m-tbl-hd INTO DATA(title).
              APPEND VALUE #( name = |F{ sy-tabix }| title = title  ) TO m-tbl-flds.
            ENDLOOP.
          ELSE.
            SPLIT line AT ',,' INTO tabline-f1 tabline-f2 tabline-f3 tabline-f4 tabline-f5 tabline-f6 tabline-f7 tabline-f8 tabline-f9.
            APPEND tabline TO m-tbl-lines.
          ENDIF.
          CLEAR flush_table.
        WHEN OTHERS.
          APPEND |{ r_line->tdformat } { line }| TO r_text.
      ENDCASE.
    ENDLOOP.
    flush_table( CHANGING c_text = r_text ).

  ENDMETHOD.

  METHOD flush_table.

    IF m-tbl-lines IS NOT INITIAL.
      DATA(t) = NEW zcl_abap2md_md_table( ).

      t->load( m-tbl-lines ).
      t->set_fields( m-tbl-flds ).
      t->use_pipe_separator( ).
      APPEND LINES OF t->get_markdown( ) TO c_text.
      CLEAR m-tbl.
    ENDIF.

  ENDMETHOD.



  METHOD replace_symbols.

    DATA td_text TYPE STANDARD TABLE OF tline.

    " we have to compy everything in order to use the FM to collect the symbols.
    td_text = VALUE #( FOR <x> IN lines ( tdformat = <x>-tdformat
                                          tdline = <x>-tdline ) ).

    CALL FUNCTION 'COLLECT_TEXTSYMBOL'
      TABLES
        lines   = td_text
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
        LOOP AT lines REFERENCE INTO DATA(line).
          REPLACE ALL OCCURRENCES OF s IN line->tdline WITH replacement.
        ENDLOOP.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.



  METHOD remove_empty_sections.

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


  ENDMETHOD.

  METHOD cvt_fmt.
    DATA:
      closings TYPE stringtab,
      str      TYPE stringtab,
      fmt      TYPE string.


    DATA(matcher) = m_regex->create_matcher( text = i_line ).
    WHILE matcher->find_next( ).
      IF matcher->get_length( 1 ) > 0.
        fmt = matcher->get_submatch( 1 ).
        CASE fmt.
          WHEN 'ZH'. " highlight
            APPEND |**| TO str.
            INSERT `**` INTO closings INDEX 1.
          WHEN 'ZK'. " cursive
            APPEND `*` TO str.
            INSERT `*` INTO closings INDEX 1.
        ENDCASE.

      ELSEIF matcher->get_length( 2 ) > 0.
        IF closings IS NOT INITIAL.
          APPEND closings[ 1 ] TO str.
          DELETE closings INDEX 1.
        ENDIF.
      ELSEIF matcher->get_length( 3 ) > 0.
        APPEND |{ matcher->get_submatch( 3 ) }| TO str.
      ENDIF.


*  CATCH cx_sy_matcher.    "



    ENDWHILE.
    CONCATENATE LINES OF str INTO r_line.

  ENDMETHOD.


  METHOD expand_wrapped_lines.
    DATA tail TYPE string.
    idx = lines( lines ).

    WHILE idx > 0.
      DATA(line) = REF #( lines[ idx ]  ).
      IF line->tdformat = '='.
        tail = tail && line->tdline.
        DELETE lines INDEX idx.
      ELSE.
        line->tdline = line->tdline && tail.
        CLEAR tail.
      ENDIF.
      idx = idx - 1.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
