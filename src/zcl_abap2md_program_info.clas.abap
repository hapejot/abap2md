CLASS zcl_abap2md_program_info DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_info.
    CLASS-METHODS try_read
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abap2md_info.
    METHODS constructor
      IMPORTING
        is_tadir TYPE tadir.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      ms_tadir       TYPE tadir,
      ms_text        TYPE trdirt,
      mv_description TYPE rswsourcet,
      ms_hd          TYPE trdir,
      mr_info        TYPE REF TO zabap2md_program_info,
      m_src          TYPE stringtab.
    METHODS        user_name
      IMPORTING
                iv_uname       TYPE syst_uname
      RETURNING VALUE(rv_name) TYPE string.
    METHODS find_param
      IMPORTING i_name          TYPE string
      RETURNING
                VALUE(rr_param) TYPE REF TO zabap2md_param.
    METHODS parse_docu
      IMPORTING
        i_text          TYPE REF TO zabap2md_text
      RETURNING
        VALUE(r_result) TYPE zabap2md_text.
    METHODS extract_word
      CHANGING
                text            TYPE rswsourcet
      RETURNING VALUE(r_result) TYPE string.
ENDCLASS.



CLASS ZCL_ABAP2MD_PROGRAM_INFO IMPLEMENTATION.


  METHOD constructor.

    me->ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD try_read.
    DATA: ls_tadir TYPE tadir.
    SELECT SINGLE *
                FROM tadir
                WHERE pgmid = 'R3TR'
                AND object = 'PROG'
                AND obj_name = @iv_name
                INTO @ls_tadir.
    IF sy-subrc = 0. " found this to be a class name...
      ro_result = NEW zcl_abap2md_program_info( ls_tadir ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abap2md_info~build_doc_structure.
    DATA: pars TYPE STANDARD TABLE OF rsel_paras.
    SELECT SINGLE *
                FROM trdirt
                WHERE name = @ms_tadir-obj_name
                AND sprsl = @sy-langu
                INTO @ms_text.


    SELECT SINGLE *
                FROM trdir
                WHERE name = @ms_tadir-obj_name
                INTO @ms_hd.

    IF i_gen IS BOUND.
      DATA(doc) = i_gen->doc( ).
      APPEND VALUE #( name = ms_tadir-obj_name title = ms_text-text ) TO  doc->programs REFERENCE INTO mr_info.

      CALL FUNCTION 'SELOPTS_AND_PARAMS'
        EXPORTING
          program = ms_tadir-obj_name
        TABLES
          selpars = pars
        EXCEPTIONS
          OTHERS  = 4.
      IF 0 = sy-subrc.

        mr_info->params = VALUE #( FOR <x> IN pars
                                    ( name = <x>-name ) ).
      ENDIF.


      DATA lt_text TYPE zabap2md_text.
      i_gen->main_text( REF #( lt_text ) ).
      i_gen->add_text( m_src ).

      mr_info->text = parse_docu( REF #( lt_text ) ).
    ENDIF.

  ENDMETHOD.


  METHOD find_param.

    rr_param  = REF #( mr_info->params[ name = i_name ] OPTIONAL ).
    IF rr_param IS INITIAL.
      APPEND VALUE #( name  = i_name ) TO mr_info->params REFERENCE INTO rr_param.
    ENDIF.


  ENDMETHOD.


  METHOD zif_abap2md_info~generate_markdown.
    TYPES: BEGIN OF row,
             when TYPE string,
             date TYPE string,
             user TYPE string,
           END OF row.
    DATA: times TYPE STANDARD TABLE OF row.


    times = VALUE #(
                    ( when = `created`      date = |{ ms_hd-cdat DATE = USER }|  user = user_name( ms_hd-cnam ) )
                    ( when = `last changed` date = |{ ms_hd-udat DATE = USER }|  user = user_name( ms_hd-unam ) )
     ).

    DATA(lo_markdown) = CAST zif_abap2md_text_generator( NEW zcl_abap2md_markdown( ) ).

    lo_markdown->heading( iv_level = 1 iv_text = ms_tadir-obj_name
                )->text( ms_text-text
                )->new_paragraph(
                )->text( mv_description
                )->heading( iv_level = 2 iv_text = 'AUTHOR'
                )->text( times ).

    APPEND LINES OF lo_markdown->result( ) TO ct_text.
  ENDMETHOD.


  METHOD zif_abap2md_info~read_main.

    DATA: texttab TYPE STANDARD TABLE OF textpool,
          pars    TYPE STANDARD TABLE OF rsel_paras.
    READ TEXTPOOL ms_tadir-obj_name LANGUAGE sy-langu INTO texttab.

    READ REPORT ms_tadir-obj_name INTO m_src.

    DATA(tokens) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( m_src ) ) ).

* Scan for tags

  ENDMETHOD.


  METHOD user_name.
    DATA: ls_address TYPE bapiaddr3,
          lt_ret     TYPE STANDARD TABLE OF bapiret2,
          ls_company TYPE bapiuscomp.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_uname
      IMPORTING
        address  = ls_address
        company  = ls_company
      TABLES
        return   = lt_ret.

    rv_name = |{ ls_address-firstname } { ls_address-lastname }|.
    IF ls_company-company IS NOT INITIAL.
      rv_name = |{ rv_name } ({ ls_company-company })|.
    ENDIF.
  ENDMETHOD.


  METHOD parse_docu.
    DATA: lv_tag_value TYPE string.

    DATA(tokens) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( i_text = i_text ) ).
    DO.
      DATA(chunk) = tokens->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.

      CASE to_upper( chunk[ 1 ] ).

*     Returning docu

*     Parameter docu
        WHEN '@PARAM'.
          chunk = tokens->next_chunk( ).
          lv_tag_value = to_upper( extract_word( CHANGING text = chunk ) ).
          DATA(lr_param) = find_param( lv_tag_value ).
          APPEND LINES OF chunk TO lr_param->text.
        WHEN '@@C'.
          EXIT.

        WHEN OTHERS.
          APPEND LINES OF chunk TO mr_info->text.
      ENDCASE.
    ENDDO.
  ENDMETHOD.


  METHOD extract_word.
**/
* extract the initial word of the text separated by space and remove this from the first line.
* leading and traling spaces will be removed from the resulting first line.
*
* @param text contains the text lines.
* @return the first word of the first line.
*/
    WHILE text IS NOT INITIAL.
      IF text[ 1 ] IS INITIAL.
        DELETE text INDEX 1.
      ENDIF.
      IF text[ 1 ] CA space.
        DATA(idx) = sy-fdpos.
        r_result = substring( val = text[ 1 ] len = idx ).
        text[ 1 ] = condense( substring( val = text[ 1 ] off = idx ) ).
        EXIT.
      ELSE.
        r_result = text[ 1 ].
        DELETE text INDEX 1.
        EXIT.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
