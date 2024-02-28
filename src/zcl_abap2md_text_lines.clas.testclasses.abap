CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      words FOR TESTING RAISING cx_static_check,
      words_and_strings FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD words.
    DATA txt TYPE stringtab.
    DATA(cut) = NEW zcl_abap2md_text_lines( REF #( txt ) ).
    cut->add_word( `One` ).
    cut->add_word( `first` ).
    cut->add_word( `test` ).
    cut->add_word( `with` ).
    cut->add_word( `a` ).
    cut->add_word( `couple` ).
    cut->add_word( `of` ).
    cut->add_word( `words` ).

    DATA(exp) = `One first test with a couple of words`.
    DATA(act) = txt[ 1 ].
    cl_abap_unit_assert=>assert_equals( msg = 'line' exp = exp act = act ).
  ENDMETHOD.


method words_and_strings.

    DATA txt TYPE stringtab.
    DATA(cut) = NEW zcl_abap2md_text_lines( REF #( txt ) ).

    cut->add_string( '*' ).
    cut->add_word( 'Test' ).
    cut->add_string( '*' ).

    DATA(exp) = `*Test*`.
    DATA(act) = txt[ 1 ].
    cl_abap_unit_assert=>assert_equals( msg = 'line' exp = exp act = act ).

endmethod.

ENDCLASS.
