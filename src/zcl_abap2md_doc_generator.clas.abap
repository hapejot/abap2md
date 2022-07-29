CLASS zcl_abap2md_doc_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_doc_generator.
    ALIASES: add_text FOR zif_abap2md_doc_generator~add_text,
             main_text FOR zif_abap2md_doc_generator~main_text.

    DATA doc TYPE zabap2md_doc_structure.

    METHODS constructor
      IMPORTING
        i_current_text TYPE REF TO stringtab.


    METHODS generate_markdown
      CHANGING
        ct_text TYPE stringtab.
    TYPES: BEGIN OF subsection,
             name  TYPE string,
             title TYPE string,
             text  TYPE rswsourcet,
           END OF subsection.
    TYPES: BEGIN OF section,
             name        TYPE string,
             title       TYPE string,
             text        TYPE rswsourcet,
             subsections TYPE STANDARD TABLE OF subsection WITH KEY name,
           END OF section.
    TYPES: BEGIN OF page,
             name     TYPE string,
             title    TYPE string,
             text     TYPE rswsourcet,
             sections TYPE STANDARD TABLE OF section WITH KEY name,
           END OF page.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS first_word
      CHANGING
        c_chunk         TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS gen_param_def_list
      IMPORTING
        i_gen    TYPE REF TO zif_abap2md_text_generator
        i_params TYPE zabap2md_params.
    "!

    METHODS explained_name
      IMPORTING i_title         TYPE any

                i_name          TYPE any
      RETURNING VALUE(r_result) TYPE string.
    METHODS exposure_name
      IMPORTING
                i_enum          TYPE seoexpose
      RETURNING VALUE(r_result) TYPE string.
    METHODS gen_class
      IMPORTING
        i_gen TYPE REF TO zif_abap2md_text_generator
        i_cls TYPE zabap2md_class_info.

    DATA: mr_current_page       TYPE REF TO zabap2md_page,
          mr_current_section    TYPE REF TO zabap2md_section,
          mr_current_subsection TYPE REF TO zabap2md_subsection,
          mr_current_text       TYPE REF TO zabap2md_page-text,
          mr_main_text          TYPE REF TO zabap2md_text.
    DATA: BEGIN OF m_cache,
            exposure TYPE dd07v_tab,
          END OF m_cache.
ENDCLASS.



CLASS ZCL_ABAP2MD_DOC_GENERATOR IMPLEMENTATION.


  METHOD constructor.


  ENDMETHOD.


  METHOD explained_name.
    IF i_title IS INITIAL.
      r_result = |{ i_name }|.
    ELSE.
      r_result = |{ i_name } - { i_title }|.
    ENDIF.
  ENDMETHOD.


  METHOD exposure_name.
    IF m_cache-exposure IS INITIAL.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname   = 'SEOEXPOSE'
          text      = 'X'
*         langu     = 'E'
        TABLES
          dd07v_tab = m_cache-exposure
        EXCEPTIONS
          OTHERS    = 0.
    ENDIF.

    r_result = VALUE #( m_cache-exposure[ domvalue_l = i_enum ]-ddtext OPTIONAL ).
  ENDMETHOD.


  METHOD first_word.
    IF c_chunk IS NOT INITIAL.
      DATA(x) = xsdbool( c_chunk[ 1 ] CA space ).
      DATA(idx) = sy-fdpos.
      DATA(left) = substring( val = c_chunk[ 1 ] len = idx ).
      IF strlen( c_chunk[ 1 ] ) > idx.
        DATA(right) = substring(  val = c_chunk[ 1 ] off = idx + 1 ).
        c_chunk[ 1 ] = right.
      ENDIF.
      r_result = left.
    ENDIF.
  ENDMETHOD.


  METHOD generate_markdown.
    DATA: page       TYPE page,
          section    TYPE section,
          subsection TYPE subsection,
          gen        TYPE REF TO zif_abap2md_text_generator.


    gen ?= NEW zcl_abap2md_markdown( ).

    LOOP AT doc-pages INTO page.
      gen->heading(   iv_level = 1
                      iv_text  = page-title
        )->text( page-text ).
      LOOP AT page-sections INTO section.
        gen->heading(   iv_level = 2
                        iv_text  = section-title
          )->text( page-text ).
        LOOP AT section-subsections INTO subsection.
          gen->heading(   iv_level = 3
                          iv_text  = subsection-title
            )->text( page-text ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    LOOP AT doc-programs INTO DATA(prg).
      gen->heading( iv_level = 1
                      iv_text = |{ 'Report'(002) } {
                                    explained_name( i_name = prg-name
                                                    i_title = prg-title ) }|
      )->text( prg-text ).
      gen_param_def_list(     i_gen = gen
                              i_params = prg-params ).
    ENDLOOP.

    LOOP AT doc-functions INTO DATA(fun).
      gen->heading( iv_level = 1
          iv_text = |{ 'Function'(003) } {
                                explained_name( i_name  = fun-name
                                                i_title = fun-title ) }|
       )->text( fun-text ).
      gen_param_def_list(
          i_gen    = gen
          i_params = fun-params
      ).
    ENDLOOP.

    LOOP AT doc-classes INTO DATA(cls).
      gen_class(      i_gen = gen
                      i_cls = cls ).
    ENDLOOP.

    APPEND LINES OF gen->result( ) TO ct_text.
  ENDMETHOD.


  METHOD gen_class.
    DATA code TYPE stringtab.

    i_gen->heading( iv_level    =   1
                    iv_text     =   |{ 'Class'(004) } { i_cls-name }|
     )->text(                       i_cls-text ).

    LOOP AT i_cls-methods INTO DATA(method).
      i_gen->heading( iv_level = 2
            iv_text =  |{ 'Method'(001) } {
                            explained_name( i_name = |{ method-name }|
                                      i_title = method-title ) }| ).
      i_gen->text( method-title ).

      code = VALUE #( ( |METHOD { method-name }| ) ).

*      APPEND |    IMPORTING|    TO code.
      DATA(w) = REDUCE i( INIT s = 5
              FOR <x> IN method-params
              NEXT s = COND #(  WHEN strlen( <x>-name ) > s
                                THEN strlen( <x>-name )
                                ELSE s ) ).
      LOOP AT method-params INTO DATA(param).
        APPEND |    { param-direction WIDTH = 10 }   { param-name WIDTH = w } TYPE { param-data_type }| TO code.
      ENDLOOP.
*      APPEND |    EXPORTING|    TO code.
*      APPEND |    CHANGING|     TO code.
*      APPEND |    RETURNING VALUE() TYPE | TO code.
      IF method-returns-data_type IS NOT INITIAL.
        APPEND |    { 'RETURNING' WIDTH = 10 }   { method-returns-name WIDTH = w } TYPE { method-returns-data_type }| TO code.
      ENDIF.
      i_gen->code( code ).
      i_gen->text( method-text ).
      gen_param_def_list(
          i_gen    = i_gen
          i_params = method-params
      ).
    ENDLOOP.


  ENDMETHOD.


  METHOD gen_param_def_list.

    LOOP AT i_params INTO DATA(param).
      i_gen->definition(    iv_text = param-text
                          iv_def  = param-name    ).
    ENDLOOP.


  ENDMETHOD.


  METHOD zif_abap2md_doc_generator~add_text.
    DATA: name TYPE string.
    DATA(source) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( i_code ) ) ).
    DO.
      DATA(chunk) = source->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.
      CASE chunk[ 1 ].
        WHEN '@page'.
          chunk = source->next_chunk( ).
          name = first_word( CHANGING c_chunk = chunk ).
          APPEND VALUE #( name = name ) TO doc-pages REFERENCE INTO mr_current_page.
          mr_current_page->title = chunk[ 1 ].
          mr_current_text = REF #( mr_current_page->text ).
          DELETE chunk INDEX 1.
          APPEND LINES OF chunk TO mr_current_text->*.
          CLEAR mr_current_section.
        WHEN '@section'.
          chunk = source->next_chunk( ).
          name = first_word( CHANGING c_chunk = chunk ).
          ASSERT mr_current_page IS BOUND.
          APPEND VALUE #( name = name ) TO mr_current_page->sections REFERENCE INTO mr_current_section.
          mr_current_text = REF #( mr_current_section->text ).
          mr_current_section->title = chunk[ 1 ].
          DELETE chunk INDEX 1.
          APPEND LINES OF chunk TO mr_current_text->*.
        WHEN '@subsection'.
          chunk = source->next_chunk( ).
          name = first_word( CHANGING c_chunk = chunk ).
          ASSERT mr_current_section IS BOUND.
          APPEND VALUE #( name = name ) TO mr_current_section->subsections REFERENCE INTO DATA(lr_subsection).
          lr_subsection->title = chunk[ 1 ].
          DELETE chunk INDEX 1.
          mr_current_text = REF #( lr_subsection->text ).
          APPEND LINES OF chunk TO mr_current_text->*.
        WHEN '@@c'. " end of comment chunk is automatically end of pages and sections.
          CLEAR mr_current_page.
          CLEAR mr_current_section.
          mr_current_text = mr_main_text.
        WHEN OTHERS.
          APPEND LINES OF chunk TO mr_current_text->*.
      ENDCASE.
    ENDDO.
  ENDMETHOD.


  METHOD zif_abap2md_doc_generator~doc.
    r_result = REF #( doc ).
  ENDMETHOD.


  METHOD zif_abap2md_doc_generator~main_text.
    mr_main_text = i_text.
    mr_current_text = mr_main_text.
  ENDMETHOD.
ENDCLASS.
