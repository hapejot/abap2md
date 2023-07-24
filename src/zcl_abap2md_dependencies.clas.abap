CLASS zcl_abap2md_dependencies DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS constructor.
    METHODS load_for
      IMPORTING
        i_compile_unit TYPE program
        i_include      TYPE program OPTIONAL.
    METHODS get_functions RETURNING VALUE(r_result) TYPE zabap2md_dependencies.
    METHODS get_classes RETURNING VALUE(r_result) TYPE zabap2md_dependencies.
    METHODS get_tables RETURNING VALUE(r_result) TYPE zabap2md_dependencies.
    METHODS get_messages
      RETURNING
        VALUE(r_result) TYPE zabap2md_dependencies.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_refs          TYPE STANDARD TABLE OF scr_glref,
          mt_deps          TYPE zabap2md_dependencies,
          interesting_tags TYPE RANGE OF scr_tag.

    METHODS setup_tags.
    METHODS prepare_deps.
    METHODS get_kind
      IMPORTING
        i_ref         TYPE scr_glref
      RETURNING
        VALUE(r_kind) TYPE string.

ENDCLASS.



CLASS zcl_abap2md_dependencies IMPLEMENTATION.


  METHOD constructor.
    setup_tags( ).
  ENDMETHOD.


  METHOD get_classes.
    DATA obj TYPE REF TO cl_abap_comp_class.
    LOOP AT mt_refs INTO DATA(l_ref) WHERE tag = 'TY'.
      TRY.
          DATA: type_name(100) TYPE c.
          type_name = l_ref-full_name+4.
          CALL METHOD cl_abap_typedescr=>describe_by_name
            EXPORTING
              p_name         = type_name    " Type name
            RECEIVING
              p_descr_ref    = DATA(xref)
            EXCEPTIONS
              type_not_found = 1
              OTHERS         = 2.
          CHECK sy-subrc = 0.
          CHECK xref->kind = cl_abap_typedescr=>kind_class.

          DATA(name) = l_ref-full_name+4.
          IF NOT name CA '\'.
            SELECT * FROM vseoclass
                    WHERE clsname = @name
                    INTO TABLE @DATA(lt_classes).
            DATA(title) = VALUE #( lt_classes[ langu = sy-langu ]-descript OPTIONAL ).
            IF title IS INITIAL.
              title = VALUE #( lt_classes[ 1 ]-descript OPTIONAL ).
            ENDIF.
          ENDIF.
          APPEND VALUE zabap2md_dependency(
                          kind  = 'TY-CLASS'
                          name  = name
                          title = title
                      ) TO r_result.
        CATCH cx_sy_move_cast_error.
          " ignore it.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_functions.
    LOOP AT mt_refs INTO DATA(l_ref)
            WHERE tag = 'FU' " function references local as well as remote. Variable calls are not recognized.
            AND grade > 0.   " the function itself has grade = 0, this should be skipped.

      DATA(name) = l_ref-full_name+4.
      SELECT * FROM tftit
              WHERE funcname = @name
              INTO TABLE @DATA(lt_titles).
      DATA(title) = VALUE #( lt_titles[ spras = sy-langu ]-stext OPTIONAL ).
      IF title IS INITIAL.
        title = VALUE #( lt_titles[ 1 ]-stext OPTIONAL ).
      ENDIF.
      APPEND VALUE zabap2md_dependency(
                      kind  =  'FU-SYMBOL'
                      name  = name
                      title = title
                  ) TO r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_kind.

    DATA(clsname) = cl_abap_classdescr=>get_class_name( i_ref-symbol ).
    r_kind  = |{ i_ref-tag }-{ clsname+20 }|.


  ENDMETHOD.


  METHOD get_messages.

    LOOP AT mt_refs INTO DATA(l_ref) WHERE tag = 'MN'.


      SPLIT l_ref-full_name AT '\' INTO DATA(msgid) DATA(msgno).
      msgno = msgno+3.
      MESSAGE ID msgid TYPE 'S' NUMBER msgno
            WITH '&1' '&2' '&3' '&4'
            INTO DATA(lv_msg).
      APPEND VALUE zabap2md_dependency(
                      kind  =  'MN-SYMBOL'
                      name  = |{ msgid }-{ msgno }|
                      title = lv_msg
                  ) TO r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_tables.
    DATA obj TYPE REF TO cl_abap_comp_ddic_dbtab.
    LOOP AT mt_refs INTO DATA(l_ref) WHERE tag = 'TY'.
      TRY.
          CHECK l_ref-full_name(4) = '\TY:'.
          DATA new_ref TYPE zabap2md_dependency.
          DATA: type_name(100) TYPE c.
          type_name = l_ref-full_name+4.
          CALL METHOD cl_abap_typedescr=>describe_by_name
            EXPORTING
              p_name         = type_name    " Type name
            RECEIVING
              p_descr_ref    = DATA(xref)
            EXCEPTIONS
              type_not_found = 1
              OTHERS         = 2.
          CHECK sy-subrc = 0.
          CHECK xref->kind = cl_abap_typedescr=>kind_struct.
          SELECT SINGLE tabclass
                FROM dd02l
                WHERE tabname = @type_name
                INTO @DATA(tab_class).
          CHECK sy-subrc = 0.
          CHECK tab_class = 'TRANSP'.
          DATA(tab_descr) = CAST cl_abap_structdescr( xref ).
          new_ref-name = type_name.

          IF NOT new_ref-name CA '\'.
            SELECT * FROM dd02t
                    WHERE tabname = @new_ref-name
                    INTO TABLE @DATA(lt_tabs).
            new_ref-title = VALUE #( lt_tabs[ ddlanguage = sy-langu ]-ddtext OPTIONAL ).
            IF new_ref-title IS INITIAL.
              new_ref-title = VALUE #( lt_tabs[ 1 ]-ddtext OPTIONAL ).
            ENDIF.
            new_ref-kind = 'TY-DDIC_DBTAB'.
            APPEND new_ref TO r_result.
          ENDIF.
        CATCH cx_sy_move_cast_error.
          " ignore it.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    load_for( i_compile_unit = 'SAPLZPJL'
                i_include = 'LZPJLU01' ).
    prepare_deps( ).
    DATA(x) = VALUE stringtab( FOR <x> IN mt_refs ( <x>-symbol->full_name ) ).
    out->write( 'done' ).
    out->write( x ).
  ENDMETHOD.


  METHOD load_for.
    DATA:
      result       TYPE scr_glrefs,
      ref          TYPE scr_glref,
      all_result   TYPE scr_refs,
      all_errors   TYPE rsyntmsgs,
      compl_refs   TYPE cl_abap_compiler=>t_all_refs,
      compl_error  TYPE sychar01,
      compl_errors TYPE scr_errors,
      compl_abort  TYPE sychar01,
      comp         TYPE REF TO cl_abap_compiler.


    IF i_include IS SUPPLIED.
      comp = NEW cl_abap_compiler( p_name  = i_compile_unit
                                         p_include = i_include ).
    ELSE.
      comp = NEW cl_abap_compiler( p_name  = i_compile_unit ).
    ENDIF.
    " ignoring all errors, if there are we just have no references.
    comp->get_all_refs(
      EXPORTING
        p_local       = 'X'                 " Local classes too
*        p_extended    = 'X'                 " will populate symbols, this causes an exception in some classes
        p_types       = interesting_tags
      IMPORTING
        p_result      = result
    ).

    mt_refs = result.
    SORT mt_refs BY full_name.
    DELETE ADJACENT DUPLICATES FROM mt_refs COMPARING full_name.
  ENDMETHOD.


  METHOD prepare_deps.
    LOOP AT mt_refs INTO DATA(reference).
      APPEND VALUE zabap2md_dependency(
          kind  = reference-tag
          name  = reference-full_name
      ) TO mt_deps.
    ENDLOOP.
  ENDMETHOD.


  METHOD setup_tags.

    interesting_tags = VALUE #( sign = 'I' option = 'EQ'
                                ( low = 'TY' )
                                ( low = 'FU' )
                                ( low = 'MN' ) ).


  ENDMETHOD.
ENDCLASS.
