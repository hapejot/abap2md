CLASS zcl_abap2md_doc_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_doc_generator.
    ALIASES: add_text FOR zif_abap2md_doc_generator~add_text,
             main_text FOR zif_abap2md_doc_generator~main_text.
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

    DATA:
            m_pages TYPE STANDARD TABLE OF page WITH KEY name READ-ONLY.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mr_current_page    TYPE REF TO page,
      mr_current_section TYPE REF TO section,
      mr_current_text    TYPE REF TO stringtab,
      mr_main_text       TYPE REF TO stringtab.
    METHODS first_word
      CHANGING
        c_chunk         TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.

ENDCLASS.



CLASS zcl_abap2md_doc_generator IMPLEMENTATION.
  METHOD constructor.

    me->mr_current_text = i_current_text.
    me->mr_main_text    = i_current_text.

  ENDMETHOD.


  METHOD zif_abap2md_doc_generator~main_text.
    mr_main_text = i_text.
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
          APPEND VALUE #( name = name ) TO m_pages REFERENCE INTO mr_current_page.
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
          subsection TYPE subsection.
    DATA(gen) = CAST zif_abap2md_text_generator( NEW zcl_abap2md_markdown( ) ).

    LOOP AT m_pages INTO page.
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

    APPEND LINES OF gen->result( ) TO ct_text.
  ENDMETHOD.
ENDCLASS.
