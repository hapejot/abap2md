CLASS zcl_abap2md_class_info DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap2md_info.
    CLASS-METHODS try_read
      IMPORTING
        iv_name          TYPE tadir-obj_name
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abap2md_info.
    METHODS constructor
      IMPORTING
        iv_tadir TYPE tadir.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: ms_tadir                    TYPE tadir,
          mt_class_interface_info_set TYPE STANDARD TABLE OF rpyclci,
          mt_attribute_set            TYPE STANDARD TABLE OF rpyclat,
          mt_method_set               TYPE STANDARD TABLE OF rpyclme,
          mt_parameter_set            TYPE STANDARD TABLE OF rpyclpa,
          mt_meta_relation_set        TYPE STANDARD TABLE OF rpyclmr,
          mt_exception_set            TYPE STANDARD TABLE OF rpyclex,
          mt_friends_relation_set     TYPE STANDARD TABLE OF rpyclfr,
          mt_method_include_set       TYPE seop_methods_w_include,
          mt_sub_class_set            TYPE STANDARD TABLE OF vseoclif,
          mt_redefinition_set         TYPE seor_redefinitions_r,
          mr_info                     TYPE REF TO zabap2md_class_info,
          mv_class_include            TYPE programm,
          mv_classpool                TYPE programm,
          mv_refs                     TYPE STANDARD TABLE OF scr_glref,
          doc                         TYPE REF TO zabap2md_doc_structure.

    METHODS build_class_info
      IMPORTING
        !i_gen TYPE REF TO zif_abap2md_doc_generator OPTIONAL .
    METHODS build_method_info
      IMPORTING
        !i_gen TYPE REF TO zif_abap2md_doc_generator OPTIONAL .
    METHODS extract_word
      CHANGING
        !text           TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string .
    METHODS parse_method_docu
      CHANGING
        !cs_method_info TYPE zabap2md_method_info .
    METHODS read_dependencies .
    METHODS write_definition
      IMPORTING
        !i_description TYPE rswsourcet
      CHANGING
        !i_out         TYPE stringtab .
    METHODS write_out_params
      IMPORTING
        !i_method      TYPE zabap2md_method_info
      RETURNING
        VALUE(rt_text) TYPE stringtab .
    METHODS write_out_param_dir
      IMPORTING
        !i_method TYPE zabap2md_method_info
        !iv_dir   TYPE string
      CHANGING
        !ct_text  TYPE stringtab .
ENDCLASS.



CLASS zcl_abap2md_class_info IMPLEMENTATION.


  METHOD build_class_info.
**/
* Building the class header info.
*
*/
    DATA: lt_source               TYPE rswsourcet,
          ls_class_descr          TYPE zabap2md_common_info,
          ls_class_interface_info TYPE rpyclci.

    doc = i_gen->doc( ).

    ls_class_interface_info = mt_class_interface_info_set[ 1 ].

    APPEND VALUE #( name = ls_class_interface_info-clsname ) TO doc->classes REFERENCE INTO mr_info.

    mr_info->title = ls_class_interface_info-descript.

    mr_info->exposure = ls_class_interface_info-exposure.


* Super class
    READ TABLE mt_meta_relation_set
      INTO DATA(ls_meta_relation)
      WITH KEY reltype = '2'.
    mr_info->super_class = VALUE #( name = ls_meta_relation-refclsname ) .


* Interfaces
    LOOP AT mt_meta_relation_set INTO ls_meta_relation WHERE reltype = '1'.
      CLEAR ls_class_descr.
      ls_class_descr-name = ls_meta_relation-refclsname.
      APPEND ls_class_descr TO mr_info->interfaces.
    ENDLOOP.


* Friends
    LOOP AT mt_friends_relation_set INTO DATA(ls_friend).
      CLEAR ls_class_descr.
      ls_class_descr-name = ls_friend-refclsname.
      APPEND ls_class_descr TO mr_info->friend_classes.
    ENDLOOP.


* Sub classes
    LOOP AT mt_sub_class_set INTO DATA(ls_sub_class).
      CLEAR ls_class_descr.
      ls_class_descr-name = ls_sub_class-clsname.
      APPEND ls_class_descr TO mr_info->sub_classes.
    ENDLOOP.

* Dependencies
    LOOP AT mv_refs INTO DATA(ref).
      DATA(name) = ref-full_name+4.
      DATA(kind) = cl_abap_classdescr=>get_class_name( ref-symbol ).
      kind = |{ ref-tag }-{ kind+20 }|.
      DATA(title) = ``.
      CASE kind.
        WHEN 'FU-SYMBOL'.
          SELECT * FROM tftit
                  WHERE funcname = @name
                  INTO TABLE @DATA(lt_titles).
          title = VALUE #( lt_titles[ spras = sy-langu ]-stext OPTIONAL ).
          IF title IS INITIAL.
            title = VALUE #( lt_titles[ 1 ]-stext OPTIONAL ).
          ENDIF.

        WHEN 'TY-CLASS'.
          SELECT * FROM vseoclass
                  WHERE clsname = @name
                  INTO TABLE @DATA(lt_classes).
          title = VALUE #( lt_classes[ langu = sy-langu ]-descript OPTIONAL ).
          IF title IS INITIAL.
            title = VALUE #( lt_classes[ 1 ]-descript OPTIONAL ).
          ENDIF.

      ENDCASE.
      APPEND VALUE #(   name = name
                        kind = kind
                        title = title )
        TO mr_info->dependencies.
    ENDLOOP.
    SORT mr_info->dependencies BY kind name.

* Source Code
    READ REPORT mv_class_include INTO lt_source.

*    i_gen->main_text( REF #( mr_info->text ) ).
*    i_gen->add_text( lt_source ).

*      mr_info->text = parse_docu( REF #( lt_text ) ).

    DATA(parser) = zcl_abap2md_class_def_parser=>create(
                i_code   = lt_source
                i_doc    = i_gen->doc( )
                i_name   = |{ ls_class_interface_info-clsname }|
            ).
    parser->parse( ).


  ENDMETHOD.


  METHOD build_method_info.
**/
* This method builds the method infos
*
*/

    DATA lt_source            TYPE rswsourcet.
    DATA ls_parameter_info    TYPE zabap2md_param.
    DATA ls_exception_info    TYPE zabap2md_common_info.
    DATA lv_cpdname           TYPE seocpdname.
    DATA: lr_meth TYPE REF TO zabap2md_method_info.


* Get header docu of all methods
    LOOP AT mt_method_set INTO DATA(ls_method).

      APPEND INITIAL LINE TO mr_info->methods REFERENCE INTO lr_meth.

      REFRESH lt_source.

*   Create composite component name (for interface methods)
*      IF ls_method-refclsname CP 'ZIF*' OR ls_method-refclsname CP 'IF*'.
      IF ls_method-refclsname IS NOT INITIAL.

        CONCATENATE ls_method-refclsname '~' ls_method-cmpname INTO lv_cpdname.
      ELSE.
        lv_cpdname = ls_method-cmpname.
      ENDIF.
      lr_meth->name = lv_cpdname.

      lr_meth->title      = |{ ls_method-descript }|.
      lr_meth->exposure   = ls_method-exposure.
      lr_meth->redefined  = ls_method-redefin.

      IF ls_method-mtddecltyp = '1'.
        lr_meth->static = abap_true.
      ENDIF.

      DATA(ls_method_include)  = VALUE #( mt_method_include_set[ cpdkey-cpdname = lv_cpdname ] OPTIONAL ).

*   No method include -> abstract
      IF ls_method_include IS INITIAL.
        lr_meth->abstract = abap_true.
      ENDIF.



      IF ls_method-descript IS NOT INITIAL.
        lr_meth->text = VALUE #( ( CONV #( ls_method-descript ) ) ).
      ENDIF.

*   Create tag infos for parameters/returns
      LOOP AT mt_parameter_set INTO DATA(ls_parameter)
                WHERE cmpname = ls_method-cmpname.

*     Add tag per parameter
        CLEAR ls_parameter_info.
        CASE ls_parameter-pardecltyp.
          WHEN '0'.
            ls_parameter_info-direction = 'IMPORTING'.
          WHEN '1'.
            ls_parameter_info-direction = 'EXPORTING'.
          WHEN '2'.
            ls_parameter_info-direction = 'CHANGING'.
          WHEN '3'.
*          no direction
        ENDCASE.
        ls_parameter_info-name = ls_parameter-sconame.
        ls_parameter_info-data_type  = ls_parameter-type.
        IF ls_parameter-pardecltyp = 3.
          lr_meth->returns = ls_parameter_info.
        ELSE.
          APPEND ls_parameter_info TO lr_meth->params.
        ENDIF.

      ENDLOOP.


*   Create tag infos for exceptions
      LOOP AT mt_exception_set INTO DATA(ls_exception)
        WHERE cmpname = ls_method-cmpname.

*     Add tag per parameter
        CLEAR ls_exception_info.
        ls_exception_info-name = ls_exception-sconame.
*        ls_exception_info-data_type      = ls_exception-sconame.
        APPEND ls_exception_info TO lr_meth->exceptions.

      ENDLOOP.


*   Get source code
      IF lr_meth->abstract = abap_false.
        READ REPORT ls_method_include-incname
          INTO lt_source.
      ENDIF.


*   Parse docu
      IF lt_source IS NOT INITIAL.
*        lr_meth->text = lt_source.
        DATA(method_parser) = zcl_abap2md_method_parser=>create(    i_class = ls_method-clsname
                                                                    i_method = ls_method-cmpname
                                                                    i_code = lt_source
                                                                    i_doc  = doc         ).
        method_parser->parse( ).
*        parse_method_docu( CHANGING cs_method_info = lr_meth->* ).
      ENDIF.


*      APPEND ls_method_info TO ms_class_docu_structure-methods.

    ENDLOOP.

* Raise message for missing class description
*    IF ms_class_docu_structure-descr_found = abap_false.
*   Raise message
*      add_message_symsg( ).
*    ENDIF.


    SORT mr_info->methods BY exposure DESCENDING static DESCENDING name.

  ENDMETHOD.


  METHOD constructor.

    me->ms_tadir = iv_tadir.

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


  METHOD parse_method_docu.
**/
* This method parses the comments using doxygen like tags.
*
* @param CS_METHOD_INFO   Method info structure
*
*/


    DATA ls_parameter_info              TYPE zabap2md_param.
    DATA ls_exception_info              TYPE zabap2md_common_info.
    DATA lv_tag_value                   TYPE name_komp.
    FIELD-SYMBOLS <fs_parameter_info>     TYPE zabap2md_param.
    FIELD-SYMBOLS <fs_exception_info>     TYPE zabap2md_common_info.
    FIELD-SYMBOLS <ft_description>  TYPE rswsourcet.

    DATA(tokens) = CAST zif_abap2md_parser( NEW zcl_abap2md_tag_def_parser( NEW zcl_abap2md_comment_parser( cs_method_info-text ) ) ).

    CLEAR cs_method_info-text[].

* Scan for tags
    DO.

      DATA(chunk) = tokens->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.

*   Find tag info entry
      UNASSIGN: <fs_parameter_info>, <fs_exception_info>, <ft_description>.

      CASE to_upper( chunk[ 1 ] ).

*     Returning docu
        WHEN '@RETURN'.
          cs_method_info-returns-text = tokens->next_chunk( ).

*     Parameter docu
        WHEN '@PARAM'.
          chunk = tokens->next_chunk( ).
          lv_tag_value = to_upper( extract_word( CHANGING text = chunk ) ).
*       Get parameter info
          READ TABLE cs_method_info-params ASSIGNING <fs_parameter_info>
                WITH KEY name = lv_tag_value.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_parameter_info.
            ls_parameter_info-name = lv_tag_value.
            ls_parameter_info-direction      = 'UNKNOWN'.
            APPEND ls_parameter_info TO cs_method_info-params ASSIGNING <fs_parameter_info>.
*            add_message_symsg( ).
          ENDIF.

          <fs_parameter_info>-text = chunk.

*     Exception docu
        WHEN '@EXCEPTION' OR '@THROWS' OR '@RAISING'.
          chunk = tokens->next_chunk( ).
          lv_tag_value = to_upper( extract_word( CHANGING text = chunk ) ).

          READ TABLE cs_method_info-exceptions ASSIGNING <fs_exception_info>
                    WITH KEY name = lv_tag_value.

          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_exception_info.
            ls_exception_info-name = lv_tag_value.
            APPEND ls_exception_info TO cs_method_info-exceptions ASSIGNING <fs_exception_info>.
*            add_message_symsg( ).
          ENDIF.

          <fs_exception_info>-text = chunk.

        WHEN OTHERS.
          IF cs_method_info-text IS INITIAL.
            cs_method_info-text = chunk.
          ENDIF.
      ENDCASE.

    ENDDO.


  ENDMETHOD.


  METHOD read_dependencies.

    DATA(comp) = NEW cl_abap_compiler( p_name  = mv_classpool ).

    " ignoring all errors, if there are we just have no references.
    comp->get_all_refs(
      EXPORTING
        p_local       = 'X'                 " Local classes too
      p_extended    = 'X'                 " Enhanced Breakdown of Where-Used List
      IMPORTING
        p_result      = DATA(result)
    ).
    DATA interesting_tags TYPE RANGE OF scr_tag.
    interesting_tags = VALUE #( sign = 'I' option = 'EQ'
                                ( low = 'TY' )
                                ( low = 'FU' )
                                ( low = 'MN' ) ).
    LOOP AT result INTO DATA(ref) WHERE tag IN interesting_tags.
      IF ref-tag = 'MN'.
        APPEND ref TO mv_refs.
      ELSE.
        DATA(name) = ref-full_name+3.
        " skip
        " - substructures
        " - this class (no use in reporting it)
        IF NOT name CS '\' AND NOT ref-full_name = |\\TY:{ ms_tadir-obj_name }|.
          APPEND ref TO mv_refs.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT mv_refs BY full_name.
    DELETE ADJACENT DUPLICATES FROM mv_refs COMPARING full_name.

  ENDMETHOD.


  METHOD try_read.
    DATA: ls_tadir TYPE tadir.
    SELECT SINGLE *
                FROM tadir
                WHERE pgmid = 'R3TR'
                AND object = 'CLAS'
                AND obj_name = @iv_name
                INTO @ls_tadir.
    IF sy-subrc = 0. " found this to be a class name...
      ro_result = NEW zcl_abap2md_class_info( ls_tadir ).
    ENDIF.
  ENDMETHOD.


  METHOD write_definition.

  ENDMETHOD.


  METHOD write_out_params.
**/
* writes all parameter definitions first and then all the descriptions to
* the output text.
* @param ct_text output is appended to this.
* @param i_method info structure describing the method.
*/
    DATA: hd  TYPE stringtab.

    IF abap_true = i_method-static.
      APPEND `STATIC` TO hd.
    ENDIF.
    CASE i_method-exposure.
      WHEN 0.
        APPEND `PRIVATE`      TO hd.
      WHEN 1.
        APPEND `PROTECTED`    TO hd.
      WHEN 2.
        APPEND `PUBLIC`       TO hd.
    ENDCASE.
    APPEND `METHOD` TO hd.
    APPEND i_method-name TO hd.
    CONCATENATE LINES OF hd INTO DATA(line) SEPARATED BY space.
    APPEND  line TO rt_text.

    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'EXPORTING'
                          CHANGING
                            ct_text = rt_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'IMPORTING'
                          CHANGING
                            ct_text = rt_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'CHANGING'
                          CHANGING
                            ct_text = rt_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'RETURNING'
                          CHANGING
                            ct_text = rt_text ).
    IF i_method-returns IS NOT INITIAL.
      APPEND |    RETURNING| TO rt_text.
      DATA(val_str) = |VALUE({ i_method-returns-name })|.
      APPEND |        { val_str  WIDTH = 35 } TYPE { i_method-returns-data_type }| TO rt_text.
    ENDIF.



  ENDMETHOD.


  METHOD write_out_param_dir.
    DATA(lv_first) = abap_true.
    LOOP AT i_method-params INTO DATA(par) WHERE direction = iv_dir.
      IF lv_first = abap_true.
        APPEND |    { iv_dir }| TO ct_text.
        CLEAR lv_first.
      ENDIF.
      APPEND |        { par-name  WIDTH = 35 } TYPE { par-data_type }| TO ct_text.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abap2md_info~build_doc_structure.
**/
* This method builds the whole class documentation structure
*
*/



* Build class info
    build_class_info( i_gen ).


* Build method info
    build_method_info( i_gen ).


  ENDMETHOD.


  METHOD zif_abap2md_info~generate_markdown.
    ASSERT 1 = 2. " check if this is called somewhere
    DATA(lo_markdown) = CAST zif_abap2md_text_generator( NEW zcl_abap2md_markdown( ) ).

    lo_markdown->heading( iv_level = 1 iv_text = mr_info->name
                )->text( mr_info->title
                )->new_paragraph(
                )->text( mr_info->text ).

    lo_markdown->heading( iv_level = 2 iv_text = 'Referenced Function Modules' ).

    LOOP AT mr_info->methods INTO DATA(method).

      lo_markdown->heading( iv_level = 2 iv_text = method-name
                  )->new_paragraph(
                  )->text( method-title
                  )->new_paragraph(
                  )->text( method-text
                  )->new_paragraph(
*                  )->code( write_out_params( method )
                  ).

*      LOOP AT method-parameter_infos INTO DATA(par).
*        IF par-description IS NOT INITIAL.
*          lo_markdown->definition(
*              iv_text = par-description
*              iv_def  = par-parameter_name
*          ).
*        ENDIF.
*      ENDLOOP.
*
*      IF method-return_info-description IS NOT INITIAL.
*        lo_markdown->definition(
*            iv_text = method-return_info-description
*            iv_def  = method-return_info-parameter_name
*        ).
*      ENDIF.
*
*      LOOP AT method-exception_infos INTO DATA(exc).
*        IF exc-description IS NOT INITIAL.
*          lo_markdown->definition(
*              iv_text = exc-description
*              iv_def  = exc-exception_name
*          ).
*        ENDIF.
*      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF lo_markdown->result( ) TO ct_text.

  ENDMETHOD.


  METHOD zif_abap2md_info~read_main.
**/
* This method reads the class information
*
* @param  IV_CLASS_NAME             Class name the info should be read for
*
* @throws CX_CLASS_DOCU_GENERATOR   Class documentation exception (e.g. class does not exist)
*
*/


    DATA: lr_cifref              TYPE REF TO if_oo_clif_incl_naming,
          lr_clsref              TYPE REF TO if_oo_class_incl_naming,
          lt_clskeys             TYPE TABLE OF seoclskey,
          lv_method_name         TYPE seocmpname,
          lv_interface_name      TYPE seoclsname,
          ls_class_interface_id  TYPE seoclskey,
          lt_class_interface_ids TYPE STANDARD TABLE OF seoclskey WITH EMPTY KEY,
          ls_redefintion         LIKE LINE OF mt_redefinition_set,
          lt_rdmethods           TYPE STANDARD TABLE OF rpyclme,
          lt_rdparameters        TYPE STANDARD TABLE OF rpyclpa,
          lt_rdexceptions        TYPE STANDARD TABLE OF rpyclex,
          lt_rdmetarelations     TYPE STANDARD TABLE OF rpyclmr,
          ls_rdmethod            LIKE LINE OF lt_rdmethods,
          ls_rdmetarelation      LIKE LINE OF lt_rdmetarelations,
          ls_rdparameter         LIKE LINE OF lt_rdparameters,
          ls_rdexception         LIKE LINE OF lt_rdexceptions,
          ls_method_include      TYPE seop_method_w_include,
          ls_method              LIKE LINE OF mt_method_set,
          lt_ifmethods           TYPE STANDARD TABLE OF rpyclme,
          lt_ifparameters        TYPE STANDARD TABLE OF rpyclpa,
          lt_ifexceptions        TYPE STANDARD TABLE OF rpyclex,
          ls_ifmethod            LIKE LINE OF lt_ifmethods,
          ls_ifparameter         LIKE LINE OF lt_ifparameters,
          ls_ifexception         LIKE LINE OF lt_ifexceptions.


* Get class descriptor
    ls_class_interface_id-clsname = ms_tadir-obj_name.

    lr_cifref = cl_oo_include_naming=>get_instance_by_cifkey( ls_class_interface_id ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap2md_error.
    ELSE.
      lr_clsref ?= lr_cifref.
    ENDIF.

    mv_classpool = lr_cifref->pool.
    read_dependencies( ).
* Read general class infos
    REFRESH lt_class_interface_ids.
    ls_class_interface_id-clsname = ms_tadir-obj_name.

    APPEND ls_class_interface_id TO lt_class_interface_ids.

    CALL FUNCTION 'RPY_CLIF_MULTI_READ'
      EXPORTING
        language                 = sy-langu
      TABLES
        class_interface_ids      = lt_class_interface_ids
        class_interface_info_set = mt_class_interface_info_set
        attribute_set            = mt_attribute_set
        method_set               = mt_method_set
        parameter_set            = mt_parameter_set
        meta_relation_set        = mt_meta_relation_set
        exception_set            = mt_exception_set
        friends_relation_set     = mt_friends_relation_set.


* Read method includes
    mt_method_include_set = lr_clsref->get_all_method_includes( ).
    mv_class_include = lr_clsref->public_section.

* Read sub classes
    APPEND ms_tadir-obj_name TO lt_clskeys.
    CALL FUNCTION 'SEO_CLIFS_ENVIRONMENT_SELECT'
      EXPORTING
        predecessors      = seox_false
        successors        = seox_true
        inheritances      = seox_true
        associations      = seox_false
        compositions      = seox_false
        inh_pre           = seox_false
        inh_suc           = seox_true
        ass_pre           = seox_false
        ass_suc           = seox_false
        com_pre           = seox_false
        com_suc           = seox_true
        inh_pre_cls_cls   = seox_false
        inh_pre_cls_int   = seox_false
        inh_pre_int_int   = seox_false
        inh_suc_cls_cls   = seox_true
        inh_suc_cls_int   = seox_true
        inh_suc_int_int   = seox_true
        with_info         = seox_true
      TABLES
        clif_keys         = lt_clskeys
        clifs_environment = mt_sub_class_set.

* Delete local subclasses
    DELETE mt_sub_class_set WHERE clsname CP 'Z*' OR clsname CP 'Y*'. "delete temporary subclasses


* Read method redefinitions
    CALL FUNCTION 'SEO_INHERITANC_READ'
      EXPORTING
        clskey        = ls_class_interface_id
      IMPORTING
        redefinitions = mt_redefinition_set.

* Add method redefintion infos
    CLEAR ls_class_interface_id.
    LOOP AT mt_redefinition_set INTO ls_redefintion.

      IF ls_class_interface_id <> ls_redefintion-refclsname.
        CLEAR: ls_class_interface_id, lt_rdmethods, lt_rdparameters, lt_rdexceptions, lt_rdmetarelations.

        REFRESH lt_class_interface_ids.
        ls_class_interface_id = ls_redefintion-refclsname.
        APPEND ls_class_interface_id TO lt_class_interface_ids.

        CALL FUNCTION 'RPY_CLIF_MULTI_READ'
          EXPORTING
            language            = sy-langu
          TABLES
            class_interface_ids = lt_class_interface_ids
            method_set          = lt_rdmethods
            parameter_set       = lt_rdparameters
            exception_set       = lt_rdexceptions
            meta_relation_set   = lt_rdmetarelations.
      ENDIF.

      READ TABLE lt_rdmethods INTO ls_rdmethod WITH KEY cmpname = ls_redefintion-mtdname.
      WHILE sy-subrc <> 0.
*     The method was not defined in direct superclass, but further up the class hierarchy
*     Gor further up
        READ TABLE lt_rdmetarelations INTO ls_rdmetarelation WITH KEY reltype = '2'.
        IF sy-subrc = 0.
          CLEAR: ls_class_interface_id, lt_rdmethods, lt_rdparameters, lt_rdexceptions, lt_rdmetarelations.
          REFRESH lt_class_interface_ids.
          ls_class_interface_id = ls_rdmetarelation-refclsname.
          APPEND ls_class_interface_id TO lt_class_interface_ids.
          CALL FUNCTION 'RPY_CLIF_MULTI_READ'
            EXPORTING
              language            = sy-langu
            TABLES
              class_interface_ids = lt_class_interface_ids
              method_set          = lt_rdmethods
              parameter_set       = lt_rdparameters
              exception_set       = lt_rdexceptions
              meta_relation_set   = lt_rdmetarelations.
          READ TABLE lt_rdmethods INTO ls_rdmethod WITH KEY cmpname = ls_redefintion-mtdname.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.

      IF sy-subrc = 0.
        ls_rdmethod-redefin = abap_true.
        APPEND ls_rdmethod TO mt_method_set.


        LOOP AT lt_rdparameters INTO ls_rdparameter WHERE cmpname = ls_rdmethod-cmpname.
          ls_rdparameter-clsname = ms_tadir-obj_name.
          APPEND ls_rdparameter TO mt_parameter_set.
        ENDLOOP.

        LOOP AT lt_rdexceptions INTO ls_rdexception WHERE cmpname = ls_rdmethod-cmpname.
          ls_rdexception-clsname = ms_tadir-obj_name.
          APPEND ls_rdexception TO mt_exception_set.
        ENDLOOP.
      ENDIF.

    ENDLOOP.


* Sort methods
    SORT mt_method_set BY cmpname ASCENDING.  "exposure DESCENDING

* Workaround: Put interface methods into method table
* - remove alias definitions first in order to avoid duplicates
    DELETE mt_method_set WHERE alias = abap_true.

* - sort include set by method name
    SORT mt_method_include_set BY cpdkey-cpdname DESCENDING.

* - find all interface definitions
    LOOP AT mt_method_include_set INTO ls_method_include.

      SPLIT ls_method_include-cpdkey-cpdname AT '~' INTO lv_interface_name lv_method_name.
      CHECK lv_method_name IS NOT INITIAL. "otherwise no interface

      READ TABLE mt_method_set TRANSPORTING NO FIELDS WITH KEY cmpname = lv_method_name.

      IF sy-subrc IS NOT INITIAL.

        CLEAR ls_method.
        ls_method-clsname    = ls_method_include-cpdkey-clsname.
        SPLIT ls_method_include-cpdkey-cpdname AT '~' INTO ls_method-refclsname ls_method-cmpname.
*      ls_method-cmpname    = ls_method_include-cpdkey-cpdname.
        ls_method-exposure   = 2. "public
        ls_method-redefin    = abap_false.
        ls_method-mtddecltyp = 0. "instance (1 = static)
        ls_method-descript   = ''.
        INSERT ls_method INTO mt_method_set INDEX 1.

*     Get parameter
        IF ls_method-refclsname IS NOT INITIAL.

*       Check if definition has already been read
          IF ls_class_interface_id-clsname <> ls_method-refclsname.

            REFRESH lt_class_interface_ids.
            ls_class_interface_id-clsname = ls_method-refclsname.
            APPEND ls_class_interface_id TO lt_class_interface_ids.

            CALL FUNCTION 'RPY_CLIF_MULTI_READ'
              EXPORTING
                language            = sy-langu
              TABLES
                class_interface_ids = lt_class_interface_ids
                method_set          = lt_ifmethods
                parameter_set       = lt_ifparameters
                exception_set       = lt_ifexceptions.

            READ TABLE lt_ifmethods INTO ls_ifmethod WITH KEY cmpname = ls_method-cmpname.
            ls_method-exposure   = ls_ifmethod-exposure.
            ls_method-mtddecltyp = ls_ifmethod-mtddecltyp.

          ENDIF.

          LOOP AT lt_ifparameters INTO ls_ifparameter WHERE cmpname = ls_method-cmpname.
            ls_ifparameter-clsname = ms_tadir-obj_name.
            APPEND ls_ifparameter TO mt_parameter_set.
          ENDLOOP.

          LOOP AT lt_ifexceptions INTO ls_ifexception WHERE cmpname = ls_method-cmpname.
            ls_ifexception-clsname = ms_tadir-obj_name.
            APPEND ls_ifexception TO mt_exception_set.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.


* Sort parameters
    SORT mt_parameter_set BY pardecltyp ASCENDING.

  ENDMETHOD.
ENDCLASS.
