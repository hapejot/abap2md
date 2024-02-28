CLASS zcl_abap2md_doc_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_code TYPE stringtab
        i_doc  TYPE REF TO zabap2md_doc_structure.
    METHODS parse.
  PROTECTED SECTION.
    DATA: code            TYPE stringtab,
          doc             TYPE REF TO zabap2md_doc_structure,
          source          TYPE REF TO zif_abap2md_parser,
          token           TYPE zabap2md_token,
          current_page    TYPE REF TO zabap2md_page,
          current_section TYPE REF TO zabap2md_section,
          subsection      TYPE REF TO zabap2md_subsection,
          current_text    TYPE REF TO zcl_abap2md_text_lines.
    METHODS next_token.
    METHODS require_current_text.
    METHODS read_words_in_line
      IMPORTING
        i_line          TYPE i OPTIONAL
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS handle_cmd.
    METHODS handle_end.
    METHODS handle_word.
    METHODS handle_sep.
    METHODS handle_parsep.
    METHODS handle_start.
    METHODS handle_date.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abap2md_doc_parser IMPLEMENTATION.

  METHOD parse.
    DATA: name       TYPE string,
          line       TYPE i,
          save_token TYPE zabap2md_token.
    source = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( code ) ) ).
    CLEAR save_token.
    next_token( ).
    WHILE token IS NOT INITIAL.
      ASSERT token <> save_token.
      save_token = token.
      CASE token-type.
        WHEN 'START'.
          handle_start( ).
        WHEN 'PARSEP'.
          handle_parsep( ).

        WHEN 'CMD'.
          handle_cmd( ).
          " if the subclass didn't handle the command we still just skip it
          IF token = save_token.
            next_token( ).
          ENDIF.
        WHEN 'END'. " end of comment chunk is automatically end of pages and sections.
          handle_end( ).

        WHEN 'WORD'.
          handle_word( ).

        WHEN 'SEP'.
          handle_sep( ).

        WHEN 'DATE'.
          handle_date( ).

      ENDCASE.
    ENDWHILE.

  ENDMETHOD.

  METHOD handle_date.

    DATA date TYPE d.
    date = token-value.
    token-value = |{ date DATE = USER }|.
    token-type = 'WORD'.
    handle_word( ).

  ENDMETHOD.



  METHOD handle_start.

    next_token( ).


  ENDMETHOD.

  METHOD handle_parsep.

    next_token( ).
    require_current_text( ).
    current_text->new_paragraph( ).

  ENDMETHOD.


  METHOD handle_sep.

    require_current_text( ).
    current_text->add_string( token-value ).
    next_token( ).

  ENDMETHOD.

  METHOD handle_word.

    require_current_text( ).
*    APPEND read_words_in_line( ) TO current_text->*.
    current_text->add_word( token-value ).
    next_token( ).

  ENDMETHOD.

  METHOD handle_end.

    CLEAR current_page.
    CLEAR current_section.
    next_token( ).

  ENDMETHOD.

  METHOD handle_cmd.

    DATA line TYPE i.
    DATA name TYPE string.

    CASE token-value.

      WHEN '@'.
        current_text->add_space( ).
        current_text->add_string( `@` ).
        next_token( ).

      WHEN 'page'.
        next_token( ).
        name = token-value.
        line = token-line.
        next_token( ).
        DATA(title) = read_words_in_line( line ).
        current_page = REF #( doc->pages[ name = name ] OPTIONAL ).
        IF current_page IS INITIAL.
          APPEND VALUE #( name = name ) TO doc->pages REFERENCE INTO current_page.
          SORT doc->pages BY name.
        ENDIF.
        IF current_page->title IS INITIAL.
          current_page->title = title.
        ENDIF.
        current_text = NEW zcl_abap2md_text_lines( i_txt =  REF #( current_page->text ) ).

      WHEN 'section'.
        token = source->next_token( ).
        name = token-value.
        line = token-line.
        token = source->next_token( ).
        current_section = REF #( current_page->sections[ name = name ] OPTIONAL ).
        IF current_section IS INITIAL.
          APPEND VALUE #( name = name ) TO current_page->sections REFERENCE INTO current_section.
          SORT current_page->sections BY name.
          current_section->title = read_words_in_line( line ).
        ENDIF.
        current_text = NEW zcl_abap2md_text_lines( i_txt = REF #( current_section->text ) ).

      WHEN 'subsection'.
        token = source->next_token( ).
        name = token-value.
        line = token-line.
        token = source->next_token( ).
        subsection = REF #( current_section->subsections[ name = name ] OPTIONAL ).
        IF subsection IS INITIAL.
          APPEND VALUE #( name = name ) TO current_section->subsections REFERENCE INTO subsection.
          subsection->title = read_words_in_line( line ).
        ENDIF.
        current_text = NEW zcl_abap2md_text_lines( REF #( subsection->text ) ).
    ENDCASE.


  ENDMETHOD.

  METHOD read_words_in_line.
**/
* reads all the remaining words on the same line.
*/

    DATA(line) = i_line.
    IF line IS INITIAL.
      line = token-line.
    ENDIF.
    WHILE token-line = line AND ( token-type = 'WORD' OR token-type = 'SEP' ).
      IF r_result IS INITIAL.
        r_result = token-value.
      ELSE.
        IF token-type = 'SEP'.
          r_result = r_result && token-value.
        ELSE.
          r_result = |{ r_result } { token-value }|.
        ENDIF.
      ENDIF.
      next_token( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD require_current_text.

  ENDMETHOD.

  METHOD next_token.

    token = source->next_token( ).

  ENDMETHOD.

  METHOD constructor.

    me->code = i_code.
    me->doc = i_doc.

  ENDMETHOD.


ENDCLASS.
