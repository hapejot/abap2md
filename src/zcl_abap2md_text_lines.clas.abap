CLASS zcl_abap2md_text_lines DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_txt TYPE REF TO stringtab.
    METHODS new_paragraph.
    METHODS add_string
      IMPORTING
        i_string TYPE any.
    METHODS add_word
      IMPORTING
        i_string TYPE any .
    METHODS add_space.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      txt           TYPE REF TO stringtab,
      add_new_line  TYPE abap_bool,
      next_no_space TYPE abap_bool.
    METHODS handle_new_line_if_needed.
    " don't add a space for the next word that is added.

ENDCLASS.



CLASS zcl_abap2md_text_lines IMPLEMENTATION.

  METHOD constructor.

    me->txt = i_txt.

  ENDMETHOD.

  METHOD new_paragraph.
    IF lines( txt->* ) > 0.
      APPEND INITIAL LINE TO txt->*.
      add_new_line = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_space.
**/
* Adds a space to the last line unless it would be the first character in the line.
*/
    DATA(idx) = lines( txt->* ).
    IF idx > 0.
      DATA(line) = REF #( txt->*[ idx ] ).
      IF strlen( line->* ) > 0.
        line->* = line->* && ` `.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD add_string.
**/
* adding a given string at the current position of the current text.
*/
    handle_new_line_if_needed( ).
    DATA(idx) = lines( txt->* ).
    IF idx > 0.
      txt->*[ idx ] = txt->*[ idx ] && i_string.
    ELSE.
      APPEND i_string TO txt->*.
    ENDIF.
    next_no_space = abap_true.

  ENDMETHOD.

  METHOD handle_new_line_if_needed.

    IF add_new_line = abap_true.
      APPEND INITIAL LINE TO txt->*.
    ENDIF.
    CLEAR add_new_line.


  ENDMETHOD.



  METHOD add_word.
**/
* adding a given string at the current position of the current text.
* in contrast to the *add_string* method this method checks if there is any previous
* text in front of the new one. And if there is, it will insert a space before.
*/
    DATA(max_line) = 70.
    handle_new_line_if_needed( ).
    DATA(idx) = lines( txt->* ).
    IF idx > 0.
      DATA(current_line) = REF #( txt->*[ idx ] ).
      DATA(line_len) = strlen( current_line->* ).
      DATA(word_len) = strlen( i_string ).
      " if we should not suppress space.
      IF next_no_space IS INITIAL.
        IF line_len  = 0.
          " do nothing...
        ELSEIF line_len + word_len + 1 > max_line.
          " add new line insert no space.
          APPEND INITIAL LINE TO txt->* REFERENCE INTO current_line.
          CLEAR line_len.
        ELSE.
          current_line->* = current_line->* && ` `.
        ENDIF.
      ELSE.

      ENDIF.
      current_line->* = current_line->* && i_string.
    ELSE.
      txt->* = VALUE #( ( i_string ) ).
    ENDIF.
    CLEAR next_no_space.

  ENDMETHOD.



ENDCLASS.
