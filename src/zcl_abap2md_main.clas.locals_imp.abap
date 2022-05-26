*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_comment_parser IMPLEMENTATION.

  METHOD constructor.

    me->mt_text = i_text.

  ENDMETHOD.

  METHOD lif_parser~next_chunk.
    DATA: in_comment TYPE abap_bool,
          lv_text    TYPE string.

    WHILE has_more_lines( ).
      lv_text = read_next_line( ).
      CASE in_comment.
        WHEN abap_true.
          IF lv_text = '*/'.
            in_comment = abap_false.
            EXIT.
          ENDIF.
          IF strlen( lv_text ) > 0 AND lv_text(1) = '*'.
            IF strlen( lv_text ) > 2.
              APPEND |{ substring( val = lv_text off = 2 ) }| TO r_chunk.
            ELSE.
              APPEND || TO r_chunk.
            ENDIF.
          ELSE.
            in_comment = abap_false.
            EXIT.
          ENDIF.

        WHEN abap_false.
          IF 0 <= find( val   = lv_text
                        regex = '^\*\*/').
            IF strlen( lv_text ) > 3.
              APPEND lv_text+3 TO r_chunk.
            ENDIF.
            in_comment = abap_true.
          ENDIF.
      ENDCASE.
    ENDWHILE.
  ENDMETHOD.

  METHOD read_next_line.

    rv_text = mt_text[ 1 ].
    DELETE mt_text INDEX 1.


  ENDMETHOD.



  METHOD has_more_lines.
    r_result = boolc( mt_text IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_tag_def_parser IMPLEMENTATION.

  METHOD constructor.

    me->src = i_src.

  ENDMETHOD.

  METHOD lif_parser~next_chunk.
    DATA out TYPE REF TO string.


    " we have pairs to work on, so we return those otherwise
    " we extract the pairs form *mt_chunk*
    IF pairs IS INITIAL.
      " read raw data if nothing is there to work on.
      IF mt_chunk IS INITIAL.
        mt_chunk = src->next_chunk( ).
        CLEAR mode.
      ENDIF.
      LOOP AT mt_chunk INTO DATA(line).
        SPLIT line AT '@' INTO TABLE DATA(lt_parts).
        LOOP AT lt_parts INTO DATA(part).
          IF sy-tabix > 1.
            IF strlen( part ) = 0.
              DATA(part_idx) = sy-tabix.
              APPEND VALUE #( text = |@{ lt_parts[ part_idx + 1 ] }| )
                    TO pairs.
              DELETE lt_parts INDEX part_idx + 1.
            ELSE.
              " finds the first word in *part* by searching for a space.
              DATA(x) = xsdbool( part CA space ).
              DATA(idx) = sy-fdpos.
              IF idx >= strlen( part ).
                APPEND VALUE #( keyword = part )
                      TO pairs.
              ELSE.
                APPEND VALUE #( keyword = part(idx) text = condense( val = substring( val = part off = idx + 1 ) from = `` ) ) " from -  empty should remove only leading and trailing blanks.
                      TO pairs.
              ENDIF.
            ENDIF.
          ELSE.
            " if there are @ signs, lines are > 1 and we do not add empty lines
            CONDENSE part.
            IF lines( lt_parts ) = 1 OR part IS NOT INITIAL.
              APPEND VALUE #( keyword = '@n' text = part ) TO pairs.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF sy-subrc = 4. " no parts means empty line... they should be preserved.
          APPEND VALUE #( keyword = '@n' ) TO pairs.
        ENDIF.
      ENDLOOP.
      CLEAR mt_chunk[].
    ENDIF.

    IF mode = 'K'.
      mode = 'T'.
      APPEND INITIAL LINE TO r_chunk REFERENCE INTO out.
      out->* = p-text.
      DELETE pairs INDEX 1.
    ENDIF.

    WHILE pairs IS NOT INITIAL.
      p = pairs[ 1 ].
      CASE p-keyword.
        WHEN '@n'.
          mode = 'T'.
          APPEND INITIAL LINE TO r_chunk REFERENCE INTO out.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN ``.
          mode = 'T'.
          out->* = |{ out->* }{ p-text }|.
          DELETE pairs INDEX 1.
        WHEN OTHERS.
          IF mode IS INITIAL.
            r_chunk = VALUE #( ( |@{ p-keyword }| ) ).
            mode = 'K'.
          ELSE.
            CLEAR mode.
          ENDIF.
          RETURN.
      ENDCASE.
    ENDWHILE.



  ENDMETHOD.

ENDCLASS.

CLASS lcl_class_info DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_info.
    CLASS-METHODS try_read
      IMPORTING
        iv_name          TYPE tadir-obj_name
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_class_info.
    METHODS constructor
      IMPORTING
        iv_tadir TYPE tadir.


  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF class_descr,
             class_name TYPE seoclsname,
             brief      TYPE string,
           END OF class_descr.
    TYPES class_descr_t TYPE STANDARD TABLE OF class_descr.

    TYPES: BEGIN OF parameter_info,
             parameter_name TYPE seocmpname,
             direction      TYPE char20,
             typ_type       TYPE seotyptype,
             data_type      TYPE rs38l_typ,
             description    TYPE rswsourcet,
           END OF parameter_info.
    TYPES parameter_info_t TYPE STANDARD TABLE OF parameter_info WITH EMPTY KEY.
    TYPES: BEGIN OF exception_info,
             exception_name TYPE seoclsname,
             data_type      TYPE rs38l_typ,
             description    TYPE rswsourcet,
           END OF exception_info.
    TYPES exception_info_t TYPE STANDARD TABLE OF exception_info WITH EMPTY KEY.
    TYPES: BEGIN OF method_info,
             method_name     TYPE seocpdname,
             docu_style      TYPE char10,
             exposure        TYPE seoexpose,
             abstract        TYPE char1,
             redefined       TYPE char1,
             static          TYPE char1,
             brief           TYPE rswsourcet,
             description     TYPE rswsourcet,
             return_info     TYPE parameter_info,
             parameter_infos TYPE parameter_info_t,
             exception_infos TYPE exception_info_t,
           END OF method_info.
    TYPES method_info_t TYPE STANDARD TABLE OF method_info.


    DATA ms_tadir TYPE tadir.
    DATA: mt_class_interface_info_set TYPE STANDARD TABLE OF rpyclci,
          mt_attribute_set            TYPE STANDARD TABLE OF rpyclat,
          mt_method_set               TYPE STANDARD TABLE OF rpyclme,
          mt_parameter_set            TYPE STANDARD TABLE OF rpyclpa,
          mt_meta_relation_set        TYPE STANDARD TABLE OF rpyclmr,
          mt_exception_set            TYPE STANDARD TABLE OF rpyclex,
          mt_friends_relation_set     TYPE STANDARD TABLE OF rpyclfr,
          mt_method_include_set       TYPE seop_methods_w_include,
          mt_sub_class_set            TYPE STANDARD TABLE OF vseoclif,
          mt_redefinition_set         TYPE seor_redefinitions_r.

    DATA: BEGIN OF ms_class_docu_structure,
            class_name     TYPE seoclsname,
            exposure       TYPE seoexpose,
            super_class    TYPE class_descr,
            interfaces     TYPE class_descr_t,
            friend_classes TYPE class_descr_t,
            sub_classes    TYPE class_descr_t,
            methods        TYPE method_info_t,
            descr_found    TYPE crmt_boolean,
            brief          TYPE rswsourcet,
            description    TYPE rswsourcet,
          END OF ms_class_docu_structure,
          mv_class_include TYPE programm.




    METHODS:
      write_out_param_dir       IMPORTING i_method TYPE lcl_class_info=>method_info
                                          iv_dir   TYPE string
                                CHANGING  ct_text  TYPE stringtab,
      write_out_params          IMPORTING i_method       TYPE lcl_class_info=>method_info
                                RETURNING VALUE(rt_text) TYPE stringtab,
      parse_method_docu
        CHANGING
          cs_method_info TYPE lcl_class_info=>method_info.
    METHODS write_definition
      IMPORTING
        i_description TYPE rswsourcet
      CHANGING
        i_out         TYPE stringtab.
    METHODS build_class_info.
    METHODS build_method_info.
    METHODS extract_word
      CHANGING
        text            TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.




ENDCLASS.

CLASS lcl_class_info IMPLEMENTATION.

  METHOD constructor.

    me->ms_tadir = iv_tadir.

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
      ro_result = NEW lcl_class_info( ls_tadir ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_info~read_main.
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
    CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
      EXPORTING
        cifkey = ls_class_interface_id
      RECEIVING
        cifref = lr_cifref
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap2md_error.
    ELSE.
      lr_clsref ?= lr_cifref.
    ENDIF.


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

  METHOD lif_info~build_doc_structure.
**/
* This method builds the whole class documentation structure
*
*/



* Build class info
    build_class_info( ).


* Build method info
    build_method_info( ).


  ENDMETHOD.

  METHOD lif_info~generate_markdown.

    DATA(lo_markdown) = CAST lif_text_generator( NEW lcl_markdown( ) ).

    lo_markdown->heading( iv_level = 1 iv_text = ms_class_docu_structure-class_name
                )->text( ms_class_docu_structure-brief
                )->new_paragraph(
                )->text( ms_class_docu_structure-description ).

    LOOP AT ms_class_docu_structure-methods INTO DATA(method).

      lo_markdown->heading( iv_level = 2 iv_text = method-method_name
                  )->new_paragraph(
                  )->text( method-brief
                  )->new_paragraph(
                  )->text( method-description
                  )->new_paragraph(
                  )->code( write_out_params( method )
                  ).

      LOOP AT method-parameter_infos INTO DATA(par).
        IF par-description IS NOT INITIAL.
          lo_markdown->definition(
              iv_text = par-description
              iv_def  = par-parameter_name
          ).
        ENDIF.
      ENDLOOP.

      IF method-return_info-description IS NOT INITIAL.
        lo_markdown->definition(
            iv_text = method-return_info-description
            iv_def  = method-return_info-parameter_name
        ).
      ENDIF.

      LOOP AT method-exception_infos INTO DATA(exc).
        IF exc-description IS NOT INITIAL.
          lo_markdown->definition(
              iv_text = exc-description
              iv_def  = exc-exception_name
          ).
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF lo_markdown->result( ) TO ct_text.

  ENDMETHOD.

  METHOD write_out_params.
**/
* writes all parameter definitions first and then all the descriptions to
* the output text.
* @param ct_text output is appended to this.
* @param i_method info structure describing the method.
*/
    DATA: par TYPE parameter_info,
          hd  TYPE stringtab.

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
    APPEND i_method-method_name TO hd.
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
    IF i_method-return_info IS NOT INITIAL.
      APPEND |    RETURNING| TO rt_text.
      DATA(val_str) = |VALUE({ i_method-return_info-parameter_name })|.
      APPEND |        { val_str  WIDTH = 35 } TYPE { i_method-return_info-data_type }| TO rt_text.
    ENDIF.



  ENDMETHOD.


  METHOD write_out_param_dir.
    DATA(lv_first) = abap_true.
    LOOP AT i_method-parameter_infos INTO DATA(par) WHERE direction = iv_dir.
      IF lv_first = abap_true.
        APPEND |    { iv_dir }| TO ct_text.
        CLEAR lv_first.
      ENDIF.
      APPEND |        { par-parameter_name  WIDTH = 35 } TYPE { par-data_type }| TO ct_text.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_definition.

  ENDMETHOD.


  METHOD build_class_info.
**/
* Building the class header info.
*
*/
    DATA: lt_source               TYPE rswsourcet,
          ls_class_descr          TYPE class_descr,
          ls_class_interface_info TYPE rpyclci.

    ls_class_interface_info = mt_class_interface_info_set[ 1 ].

    ms_class_docu_structure-class_name = ls_class_interface_info-clsname.

    ms_class_docu_structure-brief = VALUE #( ( CONV #( ls_class_interface_info-descript ) ) ).

    ms_class_docu_structure-exposure = ls_class_interface_info-exposure.


* Super class
    READ TABLE mt_meta_relation_set
      INTO DATA(ls_meta_relation)
      WITH KEY reltype = '2'.
    ms_class_docu_structure-super_class = VALUE #( class_name = ls_meta_relation-refclsname ) .


* Interfaces
    LOOP AT mt_meta_relation_set INTO ls_meta_relation WHERE reltype = '1'.
      CLEAR ls_class_descr.
      ls_class_descr-class_name = ls_meta_relation-refclsname.
      APPEND ls_class_descr TO ms_class_docu_structure-interfaces.
    ENDLOOP.


* Friends
    LOOP AT mt_friends_relation_set INTO DATA(ls_friend).
      CLEAR ls_class_descr.
      ls_class_descr-class_name = ls_friend-refclsname.
      APPEND ls_class_descr TO ms_class_docu_structure-friend_classes.
    ENDLOOP.


* Sub classes
    LOOP AT mt_sub_class_set INTO DATA(ls_sub_class).
      CLEAR ls_class_descr.
      ls_class_descr-class_name = ls_sub_class-clsname.
      APPEND ls_class_descr TO ms_class_docu_structure-sub_classes.
    ENDLOOP.


    READ REPORT mv_class_include INTO lt_source.

    DATA(tokens) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( lt_source ) ) ).

* Scan for tags
    DO.

      DATA(chunk) = tokens->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.

      ms_class_docu_structure-description = chunk.

    ENDDO.


  ENDMETHOD.


  METHOD build_method_info.
**/
* This method builds the method infos
*
*/

    DATA lt_source            TYPE rswsourcet.
    DATA lv_source            TYPE string.
    DATA ls_parameter_info    TYPE parameter_info.
    DATA ls_exception_info    TYPE exception_info.
    DATA ls_method_info       TYPE method_info.
    DATA lv_cpdname           TYPE seocpdname.
    DATA lv_coding_started    TYPE abap_bool.
    DATA lv_c1                TYPE c.


* Get header docu of all methods
    LOOP AT mt_method_set INTO DATA(ls_method).

      CLEAR ls_method_info.
      REFRESH lt_source.

*   Create composite component name (for interface methods)
      IF ls_method-refclsname CP 'ZIF*' OR ls_method-refclsname CP 'IF*'.
        CONCATENATE ls_method-refclsname '~' ls_method-cmpname INTO lv_cpdname.
      ELSE.
        lv_cpdname = ls_method-cmpname.
      ENDIF.
      ls_method_info-method_name = lv_cpdname.


      ls_method_info-exposure = ls_method-exposure.
      ls_method_info-redefined = ls_method-redefin.

      IF ls_method-mtddecltyp = '1'.
        ls_method_info-static = abap_true.
      ENDIF.

      DATA(ls_method_include)  = VALUE #( mt_method_include_set[ cpdkey-cpdname = lv_cpdname ] OPTIONAL ).

*   No method include -> abstract
      IF ls_method_include IS INITIAL.
        ls_method_info-abstract = abap_true.
      ENDIF.



      IF ls_method-descript IS NOT INITIAL.
        ls_method_info-brief = VALUE #( ( CONV #( ls_method-descript ) ) ).
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
        ls_parameter_info-parameter_name = ls_parameter-sconame.
        ls_parameter_info-data_type  = ls_parameter-type.
        IF ls_parameter-pardecltyp = 3.
          ls_method_info-return_info = ls_parameter_info.
        ELSE.
          APPEND ls_parameter_info TO ls_method_info-parameter_infos.
        ENDIF.

      ENDLOOP.


*   Create tag infos for exceptions
      LOOP AT mt_exception_set INTO DATA(ls_exception)
        WHERE cmpname = ls_method-cmpname.

*     Add tag per parameter
        CLEAR ls_exception_info.
        ls_exception_info-exception_name = ls_exception-sconame.
        ls_exception_info-data_type      = ls_exception-sconame.
        APPEND ls_exception_info TO ls_method_info-exception_infos.

      ENDLOOP.


*   Get source code
      IF ls_method_info-abstract = abap_false.
        READ REPORT ls_method_include-incname
          INTO lt_source.
      ENDIF.


*   Parse docu
      IF lt_source IS NOT INITIAL.
        ls_method_info-description = lt_source.
        parse_method_docu( CHANGING cs_method_info = ls_method_info ).
      ENDIF.


      APPEND ls_method_info TO ms_class_docu_structure-methods.

    ENDLOOP.

* Raise message for missing class description
    IF ms_class_docu_structure-descr_found = abap_false.
*   Raise message
*      add_message_symsg( ).
    ENDIF.


  ENDMETHOD.


  METHOD parse_method_docu.
**/
* This method parses the comments using doxygen like tags.
*
* @param CS_METHOD_INFO   Method info structure
*
*/


    DATA ls_parameter_info              TYPE parameter_info.
    DATA ls_exception_info              TYPE exception_info.
    DATA lv_tag_value                   TYPE name_komp.
    FIELD-SYMBOLS <fs_parameter_info>     TYPE parameter_info.
    FIELD-SYMBOLS <fs_exception_info>     TYPE exception_info.
    FIELD-SYMBOLS <ft_description>  TYPE rswsourcet.

    DATA(tokens) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( cs_method_info-description ) ) ).

    CLEAR cs_method_info-description[].

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
          cs_method_info-return_info-description = tokens->next_chunk( ).

*     Parameter docu
        WHEN '@PARAM'.
          chunk = tokens->next_chunk( ).
          lv_tag_value = to_upper( extract_word( CHANGING text = chunk ) ).
*       Get parameter info
          READ TABLE cs_method_info-parameter_infos ASSIGNING <fs_parameter_info>
                WITH KEY parameter_name = lv_tag_value.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_parameter_info.
            ls_parameter_info-parameter_name = lv_tag_value.
            ls_parameter_info-direction      = 'UNKNOWN'.
            APPEND ls_parameter_info TO cs_method_info-parameter_infos ASSIGNING <fs_parameter_info>.
*            add_message_symsg( ).
          ENDIF.

          <fs_parameter_info>-description = chunk.

*     Exception docu
        WHEN '@EXCEPTION' OR '@THROWS' OR '@RAISING'.
          chunk = tokens->next_chunk( ).
          lv_tag_value = to_upper( extract_word( CHANGING text = chunk ) ).

          READ TABLE cs_method_info-exception_infos ASSIGNING <fs_exception_info>
                    WITH KEY exception_name = lv_tag_value.

          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_exception_info.
            ls_exception_info-exception_name = lv_tag_value.
            APPEND ls_exception_info TO cs_method_info-exception_infos ASSIGNING <fs_exception_info>.
*            add_message_symsg( ).
          ENDIF.

          <fs_exception_info>-description = chunk.

        WHEN OTHERS.
          IF cs_method_info-description IS INITIAL.
            cs_method_info-description = chunk.
          ENDIF.
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

ENDCLASS.

CLASS lcl_markdown IMPLEMENTATION.

  METHOD lif_text_generator~generate.

  ENDMETHOD.

  METHOD lif_text_generator~heading.
**/
* reporting the heading in form of
*      # heading 1
*      ## heading 2
*/

    DATA: txt TYPE string.

    lif_text_generator~new_paragraph( ).
    CASE iv_level.
      WHEN 1.
        txt = CONV string( iv_text ).
        APPEND |# {  txt }| TO mt_text.
        " ATEXT variant dropped in favour of the ATX variant
        " APPEND repeat( val = '=' occ = strlen( txt ) ) TO mt_text.
      WHEN 2.
        txt = CONV string( iv_text ).
        APPEND |## {  txt }| TO mt_text.
        " APPEND repeat( val = '-' occ = strlen( txt ) ) TO mt_text.
    ENDCASE.
    " skip line after heading
    APPEND INITIAL LINE TO mt_text.
    ro_gen = me.
  ENDMETHOD.

  METHOD lif_text_generator~text.

    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN iv_text TO <lt_text>.
        LOOP AT <lt_text> ASSIGNING <lv_text>.
          APPEND CONV string( <lv_text> ) TO mt_text.
        ENDLOOP.
      WHEN OTHERS.
        APPEND CONV string( iv_text ) TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD lif_text_generator~result.

    r_result = mt_text.

  ENDMETHOD.

  METHOD lif_text_generator~code.
    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.

    APPEND INITIAL LINE TO mt_text.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN iv_text TO <lt_text>.
        LOOP AT <lt_text> ASSIGNING <lv_text>.
          APPEND |    { <lv_text> }| TO mt_text.
        ENDLOOP.
      WHEN OTHERS.
        APPEND |    { iv_text }|   TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD lif_text_generator~definition.

    FIELD-SYMBOLS: <lt_text> TYPE STANDARD TABLE,
                   <lv_text> TYPE any.

* empty line
    APPEND INITIAL LINE TO mt_text.

* Definition term
    APPEND |**{ iv_def }**|  TO mt_text.

* Definition text
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( iv_text ).
    DATA(lv_first_out) = ':'.
    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN iv_text TO <lt_text>.
        LOOP AT <lt_text> ASSIGNING <lv_text>.
          APPEND |{ lv_first_out WIDTH = 1 }  { <lv_text> }| TO mt_text.
          CLEAR lv_first_out.
        ENDLOOP.
      WHEN OTHERS.
        APPEND |:  { iv_text }|   TO mt_text.
    ENDCASE.

    ro_gen = me.

  ENDMETHOD.

  METHOD lif_text_generator~new_paragraph.
    " skip one line only if there is already text.
    IF mt_text IS NOT INITIAL AND mt_text[ lines( mt_text ) ] IS NOT INITIAL.
      APPEND INITIAL LINE TO mt_text.
    ENDIF.

    ro_gen = me.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_program_info DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_info.
    CLASS-METHODS try_read
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO lif_info.
    METHODS constructor
      IMPORTING
        is_tadir TYPE tadir.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      ms_tadir       TYPE tadir,
      ms_text        TYPE trdirt,
      mv_description TYPE rswsourcet,
      ms_hd          TYPE trdir.
    METHODS        user_name
      IMPORTING
                iv_uname       TYPE syst_uname
      RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS lcl_program_info IMPLEMENTATION.

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
      ro_result = NEW lcl_program_info( ls_tadir ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_info~build_doc_structure.
    SELECT SINGLE *
                FROM trdirt
                WHERE name = @ms_tadir-obj_name
                AND sprsl = @sy-langu
                INTO @ms_text.


    SELECT SINGLE *
                FROM trdir
                WHERE name = @ms_tadir-obj_name
                INTO @ms_hd.
  ENDMETHOD.

  METHOD lif_info~generate_markdown.
    DATA(lo_markdown) = CAST lif_text_generator( NEW lcl_markdown( ) ).

    lo_markdown->heading( iv_level = 1 iv_text = ms_tadir-obj_name
                )->text( ms_text-text
                )->new_paragraph(
                )->text( mv_description
                )->heading( iv_level = 2 iv_text = 'AUTHOR'
                )->text( VALUE stringtab(   ( |created      { ms_hd-cdat DATE = USER }: { user_name( ms_hd-cnam ) }| )
                                            (  )
                                            ( |last changed { ms_hd-udat DATE = USER }: { user_name( ms_hd-unam ) }| ) ) ).



    APPEND LINES OF lo_markdown->result( ) TO ct_text.
  ENDMETHOD.

  METHOD lif_info~read_main.

    DATA: lt_src TYPE stringtab.


    READ REPORT ms_tadir-obj_name INTO lt_src.

    DATA(tokens) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( lt_src ) ) ).

* Scan for tags
    DO.

      DATA(chunk) = tokens->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.

      mv_description = chunk.

    ENDDO.

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

ENDCLASS.

CLASS lcl_doc_generator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS add_text
      IMPORTING
        i_code TYPE stringtab.
    TYPES: BEGIN OF page,
             name  TYPE string,
             title TYPE string,
             text  TYPE stringtab,
           END OF page.

    DATA:
            m_pages TYPE STANDARD TABLE OF page WITH KEY name READ-ONLY.
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS first_word
      IMPORTING
        i_chunk         TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.

ENDCLASS.

CLASS lcl_doc_generator IMPLEMENTATION.


  METHOD add_text.
    DATA(source) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( i_code ) ) ).
    DO.
      DATA(chunk) = source->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.
      CASE chunk[ 1 ].
        WHEN '@page'.
          DATA(name) = first_word( chunk ).
          APPEND VALUE #( ) TO m_pages.
      ENDCASE.
    ENDDO.
  ENDMETHOD.


  METHOD first_word.
    IF i_chunk IS NOT INITIAL.
      DATA(x) = xsdbool( i_chunk[ 1 ] CA space ).
      DATA(idx) = sy-fdpos.
      DATA(left) = substring( val = i_chunk[ 1 ] len = sy-fdpos ).
      DATA(right) = substring(  val = i_chunk[ 1 ] off = sy-fdpos + 1 ).
      r_result = left.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
