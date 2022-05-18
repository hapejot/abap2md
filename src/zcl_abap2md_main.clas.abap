CLASS zcl_abap2md_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
**/
* This class extracts documentation from source code of other classes.
* this source code has to be documented in a special fashion.
* Also the standard document strings of the SE24 are included for documentation.
*
*
*/

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    METHODS generate_single
      IMPORTING
        !iv_name       TYPE seoclname
      RETURNING
        VALUE(rt_text) TYPE stringtab
      RAISING
        zcx_abap2md_error .
    METHODS generate_multiple .
  PROTECTED SECTION.
  PRIVATE SECTION.
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

    METHODS read_class_info
      IMPORTING
        iv_class_name TYPE seoclname
      RAISING
        zcx_abap2md_error.
    METHODS parse_method_docu
      CHANGING
        cs_method_info TYPE zcl_abap2md_main=>method_info.

    METHODS build_class_docu_structure.
    METHODS build_class_info.
    METHODS build_method_info.
    METHODS add_message_symsg.

    METHODS write_description
      IMPORTING i_description TYPE zcl_abap2md_main=>parameter_info-description
      CHANGING  i_out         TYPE stringtab.
    METHODS write_definition
      IMPORTING i_description TYPE zcl_abap2md_main=>parameter_info-description
      CHANGING  i_out         TYPE stringtab.
    METHODS write_out_params
      IMPORTING
        i_method TYPE zcl_abap2md_main=>method_info
      CHANGING
        ct_text  TYPE stringtab.
    METHODS write_out_param_dir
      IMPORTING
        i_method TYPE zcl_abap2md_main=>method_info
        iv_dir   TYPE zcl_abap2md_main=>parameter_info-direction
      CHANGING
        ct_text  TYPE stringtab.
    METHODS extract_word
      CHANGING
        text            TYPE rswsourcet
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS parse_class_docu
      IMPORTING
        i_source TYPE rswsourcet.
ENDCLASS.



CLASS ZCL_ABAP2MD_MAIN IMPLEMENTATION.


  METHOD add_message_symsg.

  ENDMETHOD.


  METHOD build_class_docu_structure.
**/
* This method builds the whole class documentation structure
*
*/



* Build class info
    build_class_info( ).


* Build method info
    build_method_info( ).


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

    parse_class_docu( lt_source ).

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
      add_message_symsg( ).
    ENDIF.

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


  METHOD generate_multiple.
  ENDMETHOD.


  METHOD generate_single.
**/
* @param iv_name is the class name of the class to be documented. This name can be in lower case.
* @return a string table containing the raw mark down description of the class.
*/
    read_class_info( iv_name ).

    build_class_docu_structure( ).
    APPEND |NAME| TO rt_text.
    APPEND |====| TO rt_text.

    APPEND |{ ms_class_docu_structure-class_name } - { VALUE #( ms_class_docu_structure-brief[ 1 ] OPTIONAL ) } |  TO rt_text.
    APPEND INITIAL LINE TO rt_text.
    write_description(    EXPORTING i_description = ms_class_docu_structure-description
                          CHANGING i_out         = rt_text ).


    LOOP AT ms_class_docu_structure-methods INTO DATA(method).
      APPEND INITIAL LINE TO rt_text.
      APPEND INITIAL LINE TO rt_text.

      DATA(lv_name) = |{ method-method_name }|.

      APPEND lv_name TO rt_text.
      APPEND repeat( val = '-' occ = strlen( lv_name ) ) TO rt_text.
      write_description(    EXPORTING   i_description   = method-brief
                            CHANGING    i_out           = rt_text ).
      APPEND INITIAL LINE TO rt_text.
      write_description(    EXPORTING   i_description   = method-description
                            CHANGING    i_out           = rt_text ).
      APPEND INITIAL LINE TO rt_text.
      write_out_params(     EXPORTING   i_method        = method
                            CHANGING    ct_text         = rt_text ).


    ENDLOOP.



  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA: req TYPE sadt_rest_request,
          res TYPE sadt_rest_response.

    req = VALUE sadt_rest_request(
        request_line  = VALUE sadt_rest_request_line(
        method  = |GET|
        uri     = |/sap/bc/adt/oo/classes/zcl_abap2md_main/source/test|
        version = |HTTP/1.1| )
        header_fields = VALUE #( )
        message_body  = VALUE #( )
    ).


    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING
        request  = req    " Rest Request
      IMPORTING
        response = res.    " Rest Request

    DATA(lv_string) =    cl_bcs_convert=>xstring_to_string(
                              iv_xstr   = res-message_body
                              iv_cp     =  1100                " SAP character set identification
                                      ).


  ENDMETHOD.


  METHOD parse_class_docu.
    DATA(tokens) = CAST lif_parser( NEW lcl_tag_def_parser( NEW lcl_comment_parser( i_source ) ) ).

* Scan for tags
    DO.

      DATA(chunk) = tokens->next_chunk( ).
      IF chunk IS INITIAL.
        EXIT.
      ENDIF.

      ms_class_docu_structure-description = chunk.

    ENDDO.

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
            add_message_symsg( ).
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
            add_message_symsg( ).
          ENDIF.

          <fs_exception_info>-description = chunk.

        WHEN OTHERS.
          IF cs_method_info-description IS INITIAL.
            cs_method_info-description = chunk.
          ENDIF.
      ENDCASE.

    ENDDO.


  ENDMETHOD.


  METHOD read_class_info.
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
    ls_class_interface_id-clsname = to_upper( iv_class_name ).
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
    ls_class_interface_id-clsname = to_upper( iv_class_name ).

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
    APPEND to_upper( iv_class_name ) TO lt_clskeys.
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
          ls_rdparameter-clsname = to_upper( iv_class_name ).
          APPEND ls_rdparameter TO mt_parameter_set.
        ENDLOOP.

        LOOP AT lt_rdexceptions INTO ls_rdexception WHERE cmpname = ls_rdmethod-cmpname.
          ls_rdexception-clsname = to_upper( iv_class_name ).
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
            ls_ifparameter-clsname = to_upper( iv_class_name ).
            APPEND ls_ifparameter TO mt_parameter_set.
          ENDLOOP.

          LOOP AT lt_ifexceptions INTO ls_ifexception WHERE cmpname = ls_method-cmpname.
            ls_ifexception-clsname = to_upper( iv_class_name ).
            APPEND ls_ifexception TO mt_exception_set.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.


* Sort parameters
    SORT mt_parameter_set BY pardecltyp ASCENDING.


  ENDMETHOD.


  METHOD write_definition.
    DATA text_line TYPE string.
    DATA start TYPE c VALUE ':'.
    LOOP AT i_description INTO text_line.
      IF text_line IS NOT INITIAL.
        APPEND |{ start }  { text_line }| TO i_out.
        CLEAR start.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD write_description.

    DATA text_line TYPE string.

    LOOP AT i_description INTO text_line.
      IF text_line IS NOT INITIAL.
        APPEND text_line TO i_out.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD write_out_params.
**/
* writes all parameter definitions first and then all the descriptions to
* the output text.
* @param ct_text output is appended to this.
* @param i_method info structure describing the method.
*/
    DATA: par TYPE zcl_abap2md_main=>parameter_info,
          hd  TYPE stringtab.

    IF abap_true = i_method-static.
      APPEND `STATIC` TO hd.
    ENDIF.
    CASE i_method-exposure.
      WHEN 0. APPEND `PRIVATE` TO hd.
      WHEN 1. APPEND `PROTECTED` TO hd.
      WHEN 2.	APPEND `PUBLIC` TO hd.
    ENDCASE.
    APPEND `METHOD` TO hd.
    APPEND i_method-method_name TO hd.
    CONCATENATE LINES OF hd INTO DATA(line) SEPARATED BY space.
    APPEND |    { line }| TO ct_text.

    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'EXPORTING'
                          CHANGING
                            ct_text = ct_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'IMPORTING'
                          CHANGING
                            ct_text = ct_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'CHANGING'
                          CHANGING
                            ct_text = ct_text ).
    write_out_param_dir(  EXPORTING
                            i_method = i_method
                            iv_dir   = 'RETURNING'
                          CHANGING
                            ct_text = ct_text ).
    IF i_method-return_info IS NOT INITIAL.
      APPEND |    RETURNING| TO ct_text.
      DATA(val_str) = |VALUE({ i_method-return_info-parameter_name })|.
      APPEND |        { val_str  WIDTH = 35 } TYPE { i_method-return_info-data_type }| TO ct_text.
    ENDIF.

    LOOP AT i_method-parameter_infos INTO par.
      IF par-description IS NOT INITIAL.
        APPEND INITIAL LINE TO ct_text.
        APPEND |**{ par-parameter_name }**|  TO ct_text.
        write_definition(     EXPORTING i_description = par-description
                              CHANGING i_out         = ct_text ).
      ENDIF.
    ENDLOOP.

    IF i_method-return_info-description IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_text.
      APPEND |**{ i_method-return_info-parameter_name }**| TO ct_text.
      write_definition(     EXPORTING i_description = i_method-return_info-description
                            CHANGING i_out         = ct_text ).
    ENDIF.

    LOOP AT i_method-exception_infos INTO DATA(exc).
      IF exc-description IS NOT INITIAL.
        APPEND INITIAL LINE TO ct_text.
        APPEND |**{ exc-exception_name }**| TO ct_text.
        write_definition(   EXPORTING   i_description   = exc-description
                            CHANGING    i_out           = ct_text ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD write_out_param_dir.

    DATA par TYPE zcl_abap2md_main=>parameter_info.

    IF line_exists( i_method-parameter_infos[ direction = iv_dir ] ).
      APPEND |    { iv_dir }| TO ct_text.
      LOOP AT i_method-parameter_infos INTO par WHERE direction = iv_dir.
        APPEND |        { par-parameter_name WIDTH = 35 } TYPE { par-data_type }|  TO ct_text.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
