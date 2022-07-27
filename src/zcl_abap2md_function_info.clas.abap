CLASS zcl_abap2md_function_info DEFINITION
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
        is_tfdir TYPE tfdir.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF parameter_info,
             parameter_name TYPE seocmpname,
             direction      TYPE char20,
             typ_type       TYPE seotyptype,
             data_type      TYPE rs38l_typ,
             description    TYPE rswsourcet,
           END OF parameter_info.
    TYPES parameter_info_t TYPE STANDARD TABLE OF parameter_info WITH EMPTY KEY.

    DATA:
      ms_tfdir       TYPE tfdir,
      ms_text        TYPE trdirt,
      mt_params      TYPE parameter_info_t,
      mv_description TYPE rswsourcet,
      ms_hd          TYPE trdir,
      mr_info        TYPE REF TO zabap2md_function_info,
      m_src          TYPE stringtab.
    METHODS        user_name
      IMPORTING
                iv_uname       TYPE syst_uname
      RETURNING VALUE(rv_name) TYPE string.
    METHODS parse_docu.
    METHODS extract_word CHANGING  text            TYPE rswsourcet
                         RETURNING
                                   VALUE(r_result) TYPE string.
    METHODS find_param
      IMPORTING
        iv_tag_value    TYPE clike
      RETURNING
        VALUE(rr_param) TYPE REF TO zabap2md_param.
ENDCLASS.



CLASS zcl_abap2md_function_info IMPLEMENTATION.
  METHOD constructor.

    me->ms_tfdir = is_tfdir.

  ENDMETHOD.


  METHOD try_read.
    DATA: ls_tfdir TYPE tfdir.
    SELECT SINGLE *
                FROM tfdir
                WHERE funcname = @iv_name
                INTO @ls_tfdir.
    IF sy-subrc = 0. " found this to be a class name...
      ro_result = NEW zcl_abap2md_function_info( ls_tfdir ).
    ENDIF.

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

  METHOD zif_abap2md_info~build_doc_structure.
    DATA: funcname                TYPE rs38l-name,
          language                TYPE sy-langu,
          with_enhancement        TYPE char1,
          ignore_switches         TYPE char1,
          global_flag             TYPE rs38l-global,
          remote_call             TYPE rs38l-remote,
          update_task             TYPE rs38l-utask,
          short_text              TYPE tftit-stext,
          freedate                TYPE enlfdir-freedate,
          exception_class         TYPE enlfdir-exten3,
          remote_basxml_supported TYPE rs38l-basxml_enabled,
          dokumentation           TYPE STANDARD TABLE OF funct,
          exception_list          TYPE STANDARD TABLE OF rsexc,
          export_parameter        TYPE STANDARD TABLE OF rsexp,
          import_parameter        TYPE STANDARD TABLE OF rsimp,
          changing_parameter      TYPE STANDARD TABLE OF rscha,
          tables_parameter        TYPE STANDARD TABLE OF rstbl,
          enha_exp_parameter      TYPE STANDARD TABLE OF rsexc,
          enha_imp_parameter      TYPE STANDARD TABLE OF rsimp,
          enha_cha_parameter      TYPE STANDARD TABLE OF rscha,
          enha_tbl_parameter      TYPE STANDARD TABLE OF rstbl,
          enha_dokumentation      TYPE STANDARD TABLE OF funct,
          lt_source               TYPE rswsourcet,
          ls_title                TYPE dsyst-doktitle,
          ls_doc_hd               TYPE thead,
          lt_doc_lines            TYPE STANDARD TABLE OF tline.

    DATA(lr_doc) = i_gen->doc( ).
    APPEND INITIAL LINE TO lr_doc->functions REFERENCE INTO mr_info.


    CALL FUNCTION 'FUNCTION_IMPORT_DOKU'
      EXPORTING
        funcname                = ms_tfdir-funcname    " Name of the function module
        language                = sy-langu
        with_enhancements       = with_enhancement "'X'
        ignore_switches         = ignore_switches " SPACE
      IMPORTING
        global_flag             = global_flag    " Global interface
        remote_call             = remote_call                " Function module can be called with
        update_task             = update_task                " Function module luft in the update
        short_text              = short_text                 " Short text for function module
        freedate                = freedate
        exception_class         = exception_class
        remote_basxml_supported = remote_basxml_supported
      TABLES
        dokumentation           = dokumentation          " Short description of parameters
        exception_list          = exception_list         " Table of exceptions
        export_parameter        = export_parameter       " Table of export parameters
        import_parameter        = import_parameter       " Table of import parameters
        changing_parameter      = changing_parameter
        tables_parameter        = tables_parameter       " Table of tables
        enha_exp_parameter      = enha_exp_parameter
        enha_imp_parameter      = enha_imp_parameter
        enha_cha_parameter      = enha_cha_parameter
        enha_tbl_parameter      = enha_tbl_parameter
        enha_dokumentation      = enha_dokumentation
      EXCEPTIONS
        error_message           = 1
        function_not_found      = 2
        invalid_name            = 3
        OTHERS                  = 4.

    mr_info->name = ms_tfdir-funcname.
    mr_info->title = short_text.

*    LOOP AT export_parameter INTO DATA(epar).
*    ENDLOOP.
*    LOOP AT import_parameter INTO DATA(ipar).
*    ENDLOOP.
*    LOOP AT changing_parameter INTO DATA(cpar).
*    ENDLOOP.
*    LOOP AT tables_parameter INTO DATA(tpar).
*    ENDLOOP.
    LOOP AT dokumentation INTO DATA(dok).
      DATA(par) = find_param( dok-parameter ).
      par->title = dok-stext.
    ENDLOOP.

    DATA lv_include TYPE tadir-obj_name.
    lv_include = |{ ms_tfdir-pname+3 }U{ ms_tfdir-include }|.
    READ REPORT lv_include INTO m_src.

    parse_docu( ).

    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id                = 'FU'
        langu             = sy-langu
        object            = CONV doku_obj( ms_tfdir-funcname )
        typ               = 'T'
        version           = '0001'
        suppress_template = 'X'
      IMPORTING
        doktitle          = ls_title
        head              = ls_doc_hd
      TABLES
        line              = lt_doc_lines.


*    IF i_gen IS BOUND.
*      i_gen->main_text( REF #( mv_description ) ).
*      i_gen->add_text( m_src ).
*    ENDIF.

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

    lo_markdown->heading( iv_level = 1 iv_text = 'ms_tadir-obj_name'
                )->text( ms_text-text
                )->new_paragraph(
                )->text( mv_description
                )->heading( iv_level = 2 iv_text = 'AUTHOR'
                )->text( times ).

    APPEND LINES OF lo_markdown->result( ) TO ct_text.
  ENDMETHOD.

  METHOD zif_abap2md_info~read_main.

* Scan for tags

  ENDMETHOD.
  METHOD extract_word.
**/
* extract the initial word of the text separated by space and remove this from the first line.
* leading and traling spaces will be removed from the resulting first line.
*
* @param text contains the text lines.
* @return the first word of the first line.
*/
    IF text IS NOT INITIAL.
      IF text[ 1 ] CA space.
        DATA(idx) = sy-fdpos.
        r_result = substring( val = text[ 1 ] len = idx ).
        text[ 1 ] = condense( substring( val = text[ 1 ] off = idx ) ).
      ELSE.
        r_result = text[ 1 ].
        CLEAR text[ 1 ].
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD parse_docu.
    DATA: lv_tag_value TYPE string.

    DATA(tokens) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( m_src ) ) ).
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

  METHOD find_param.

    rr_param  = REF #( mr_info->params[ name = iv_tag_value ] OPTIONAL ).
    IF rr_param IS INITIAL.
      APPEND VALUE #( name  = iv_tag_value ) TO mr_info->params REFERENCE INTO rr_param.
    ENDIF.


  ENDMETHOD.



ENDCLASS.
