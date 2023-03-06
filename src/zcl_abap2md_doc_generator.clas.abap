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
    METHODS options
      IMPORTING
        ix_options TYPE zabap2md_options.
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
    METHODS write_dependencies
      IMPORTING
        i_cls TYPE zabap2md_class_info
        i_gen TYPE REF TO zif_abap2md_text_generator.
    METHODS generate_method
      IMPORTING
        i_method TYPE zabap2md_method_info
        i_gen    TYPE REF TO zif_abap2md_text_generator.
    METHODS static_name
      IMPORTING
        iv_static      TYPE zabap2md_method_info-static
      RETURNING
        VALUE(rv_name) TYPE string.
    METHODS parse_v1
      IMPORTING
        i_code TYPE stringtab.
    METHODS generate_changes
      IMPORTING
        i_gen     TYPE REF TO zif_abap2md_text_generator
        i_changes TYPE zabap2md_method_info-changes.
    METHODS generate_function
      IMPORTING
        i_fun TYPE zabap2md_function_info
        i_gen TYPE REF TO zif_abap2md_text_generator.
    METHODS generate_report
      IMPORTING
        i_gen TYPE REF TO zif_abap2md_text_generator
        i_prg TYPE zabap2md_program_info.
    METHODS generate_table
      IMPORTING
        i_gen TYPE REF TO zif_abap2md_text_generator
        i_tab TYPE REF TO zabap2md_table_info.

    DATA: mr_current_page       TYPE REF TO zabap2md_page,
          mr_current_section    TYPE REF TO zabap2md_section,
          mr_current_subsection TYPE REF TO zabap2md_subsection,
          mr_current_text       TYPE REF TO zabap2md_page-text,
          mr_main_text          TYPE REF TO zabap2md_text.
    DATA: BEGIN OF m_cache,
            exposure TYPE dd07v_tab,
          END OF m_cache,
          mx_options TYPE zabap2md_options.
ENDCLASS.



CLASS zcl_abap2md_doc_generator IMPLEMENTATION.


  METHOD constructor.


  ENDMETHOD.


  METHOD explained_name.
    IF i_title IS INITIAL.
      r_result = |{ i_name }|.
    ELSE.
      r_result = |{ i_name } - { i_title }|.
    ENDIF.
  ENDMETHOD.

  METHOD static_name.

    IF iv_static = abap_true.
      rv_name = 'static'.
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
      IF x = abap_true.
        DATA(idx) = sy-fdpos.
        DATA(left) = substring( val = c_chunk[ 1 ] len = idx ).
        IF strlen( c_chunk[ 1 ] ) > idx.
          DATA(right) = substring(  val = c_chunk[ 1 ] off = idx + 1 ).
          c_chunk[ 1 ] = right.
        ENDIF.
        r_result = left.
      ELSE.
        r_result = c_chunk[ 1 ].
        CLEAR c_chunk[ 1 ].
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generate_markdown.
    DATA: page       TYPE page,
          section    TYPE section,
          subsection TYPE subsection,
          gen        TYPE REF TO zif_abap2md_text_generator.


    gen ?= NEW zcl_abap2md_markdown( ).

    SORT doc-pages BY name.
    LOOP AT doc-pages INTO page.
      gen->heading(   iv_level = 1
                      iv_text  = page-title
        )->text( page-text ).
      SORT page-sections BY name.
      LOOP AT page-sections INTO section.
        gen->heading(   iv_level = 2
                        iv_text  = section-title
          )->text( section-text ).
        SORT section-subsections BY name.
        LOOP AT section-subsections INTO subsection.
          gen->heading(   iv_level = 3
                          iv_text  = subsection-title
            )->text( subsection-text ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    LOOP AT doc-tables REFERENCE INTO DATA(tab).
      generate_table( i_gen = gen i_tab = tab ).
    ENDLOOP.


    LOOP AT doc-programs INTO DATA(prg).
      generate_report(    i_gen = gen
                          i_prg = prg ).
    ENDLOOP.

    LOOP AT doc-functions INTO DATA(fun).
      generate_function(  i_fun = fun
                          i_gen = gen ).
    ENDLOOP.

    LOOP AT doc-classes INTO DATA(cls).
      gen_class(      i_gen = gen
                      i_cls = cls ).
    ENDLOOP.

    APPEND LINES OF gen->result( ) TO ct_text.
  ENDMETHOD.

  METHOD generate_report.

    i_gen->heading( iv_level = 1
                    iv_text = |{ 'Report'(002) } {
                                  explained_name( i_name = i_prg-name
                                                  i_title = i_prg-title ) }|
    )->text( i_prg-text ).
    gen_param_def_list(     i_gen = i_gen
                            i_params = i_prg-params ).

    IF i_prg-changes IS NOT INITIAL.
      generate_changes( i_gen     = i_gen
                        i_changes = i_prg-changes ).
    ENDIF.

  ENDMETHOD.



  METHOD generate_function.

    i_gen->heading( iv_level = 1
        iv_text = |{ 'Function'(003) } {
                              explained_name( i_name  = i_fun-name
                                              i_title = i_fun-title ) }|
     )->text( i_fun-text ).
    gen_param_def_list(
        i_gen    = i_gen
        i_params = i_fun-params
    ).

    IF i_fun-changes IS NOT INITIAL.
      generate_changes( i_gen     = i_gen
                        i_changes = i_fun-changes ).
    ENDIF.

  ENDMETHOD.




  METHOD gen_class.
    DATA code TYPE stringtab.

    i_gen->heading( iv_level    =   1
                    iv_text     =   |{ 'Class'(004) } { i_cls-name }|
     )->text(                       i_cls-text ).

    write_dependencies(   i_cls = i_cls
                          i_gen = i_gen ).

    LOOP AT i_cls-methods INTO DATA(method).
      IF    method-title IS NOT INITIAL
            OR method-changes IS NOT INITIAL
            OR method-text IS NOT INITIAL.
        generate_method(         i_method = method
                            i_gen    = i_gen ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD generate_method.

    DATA code TYPE stringtab.

    CASE mx_options-methods-hdr_include_description.
      WHEN abap_true.
        i_gen->heading( iv_level = 2
                iv_text =  |{ exposure_name(  i_method-exposure ) } {
                                static_name( i_method-static ) } { 'Method'(001) } {
                                explained_name( i_name = |{ i_method-name }|
                                                i_title = i_method-title ) }| ).
      WHEN OTHERS.
        i_gen->heading( iv_level = 2
                iv_text =  |{ exposure_name(  i_method-exposure ) } {
                                static_name( i_method-static ) } { 'Method'(001) } { i_method-name }| ).
    ENDCASE.
*    i_gen->text( i_method-title ).

    code = VALUE #( ( |METHOD { i_method-name }| ) ).

*      APPEND |    IMPORTING|    TO code.
    DATA(w) = REDUCE i( INIT s = 5
            FOR <x> IN i_method-params
            NEXT s = COND #(  WHEN strlen( <x>-name ) > s
                              THEN strlen( <x>-name )
                              ELSE s ) ).
    LOOP AT i_method-params INTO DATA(param).
      APPEND |    { param-direction WIDTH = 10 }   { param-name WIDTH = w } TYPE { param-data_type }| TO code.
    ENDLOOP.
*      APPEND |    EXPORTING|    TO code.
*      APPEND |    CHANGING|     TO code.
*      APPEND |    RETURNING VALUE() TYPE | TO code.
    IF i_method-returns-data_type IS NOT INITIAL.
      APPEND |    { 'RETURNING' WIDTH = 10 }   { i_method-returns-name WIDTH = w } TYPE { i_method-returns-data_type }| TO code.
    ENDIF.
    i_gen->code( code ).
    i_gen->new_paragraph( ).
    i_gen->text( i_method-text ).
    gen_param_def_list(     i_gen    = i_gen
                            i_params = i_method-params    ).
    IF i_method-returns-text IS NOT INITIAL.
      i_gen->definition(    iv_def  = 'Returns'
                            iv_text = i_method-returns-text      ).
    ENDIF.


    IF i_method-changes IS NOT INITIAL.
      generate_changes( i_gen     = i_gen
                        i_changes = i_method-changes ).
    ENDIF.

  ENDMETHOD.

  METHOD generate_changes.

    i_gen->heading( iv_level = 2 iv_text = |Changes| ).
    DATA(changes) = i_changes.
    SORT changes BY date DESCENDING.
    LOOP AT changes REFERENCE INTO DATA(change).
      i_gen->definition(  iv_text = change->text
                          iv_def  = |{ change->date DATE = USER }|    ).
    ENDLOOP.


  ENDMETHOD.





  METHOD write_dependencies.

    DATA: dep           LIKE LINE OF i_cls-dependencies,
          title_written TYPE abap_bool.
    CLEAR title_written.
    LOOP AT i_cls-dependencies INTO dep WHERE kind = 'FU-SYMBOL'.
      IF title_written IS INITIAL.
        i_gen->heading( iv_level = 2 iv_text = 'Referenced Function Modules' ).
        title_written = abap_true.
      ENDIF.
      i_gen->definition(    iv_text = |{ dep-title }|
                                  iv_def  = dep-name ).
    ENDLOOP.

    CLEAR title_written.
    LOOP AT i_cls-dependencies INTO dep WHERE kind = 'TY-CLASS'.
      CHECK dep-name(1) = 'Z'.
      IF title_written IS INITIAL.
        i_gen->heading( iv_level = 2 iv_text = 'Referenced Custom Classes' ).
        title_written = abap_true.
      ENDIF.
      i_gen->definition(    iv_text = |{ dep-title }|
                                  iv_def  = dep-name ).
    ENDLOOP.



  ENDMETHOD.




  METHOD gen_param_def_list.

    LOOP AT i_params INTO DATA(param).
      DATA(txt) = VALUE stringtab( ).
      IF param-title IS NOT INITIAL.
        APPEND param-title TO txt.
        APPEND `` TO txt.
      ENDIF.
      IF param-text IS NOT INITIAL.
        APPEND LINES OF param-text TO txt.
      ENDIF.
      IF txt IS NOT INITIAL.
        i_gen->definition(  iv_text = txt
                            iv_def  = param-name    ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD zif_abap2md_doc_generator~add_text.
**/
* Reads a complete source file and interprets the comment sections.
*
* this method concentrates on the content in the file that might got into
* the general document sections. This will ignore specific information about
* the documented object but sort the pages and sections into the right place.
*/

    parse_v1( i_code ).

  ENDMETHOD.

  METHOD parse_v1.

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
          mr_current_page = REF #( doc-pages[ name = name ] OPTIONAL ).
          DATA(title) = VALUE #( chunk[ 1 ] OPTIONAL ).
          IF mr_current_page IS INITIAL.
            APPEND VALUE #( name = name ) TO doc-pages REFERENCE INTO mr_current_page.
            mr_current_page->title = title.
          ELSE.
            IF mr_current_page->title IS INITIAL OR mr_current_page->title = mr_current_page->name.
              mr_current_page->title = title.
            ENDIF.
          ENDIF.
          DELETE chunk INDEX 1.

          mr_current_text = REF #( mr_current_page->text ).
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

  METHOD options.
    mx_options = ix_options.
  ENDMETHOD.


  METHOD generate_table.
    i_gen->heading( iv_level = 1
                    iv_text = |{ 'Table' } {
                                  explained_name( i_name = i_tab->name
                                                  i_title = i_tab->title ) }|
    )->text( i_tab->text ).

  ENDMETHOD.

ENDCLASS.
