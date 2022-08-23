CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_word FOR TESTING RAISING cx_static_check,
      first_word_single FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS zcl_abap2md_doc_generator DEFINITION LOCAL FRIENDS ltcl_main.

CLASS ltcl_main IMPLEMENTATION.

  METHOD first_word.
    DATA: lt_chunk TYPE stringtab.

    lt_chunk = VALUE #( ( |p1 Test Text| ) ).

    DATA(cut) = NEW zcl_abap2md_doc_generator( i_current_text = VALUE #(  ) ).
    DATA(lt_result) = cut->first_word(
      CHANGING
        c_chunk  = lt_chunk
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = 'p1' act = lt_result ).
    cl_abap_unit_assert=>assert_equals( msg = 'remaining text' exp = VALUE stringtab( ( |Test Text| ) ) act = lt_chunk ).
  ENDMETHOD.

  METHOD first_word_single.
    DATA: lt_chunk TYPE stringtab.

    lt_chunk = VALUE #( ( |p1| ) ).

    DATA(cut) = NEW zcl_abap2md_doc_generator( i_current_text = VALUE #(  ) ).
    DATA(lt_result) = cut->first_word(
      CHANGING
        c_chunk  = lt_chunk
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = 'p1' act = lt_result ).
    cl_abap_unit_assert=>assert_equals( msg = 'remaining text' exp = VALUE stringtab( ( || ) ) act = lt_chunk ).
  ENDMETHOD.

ENDCLASS.
