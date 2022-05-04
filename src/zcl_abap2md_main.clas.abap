CLASS zcl_abap2md_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS generate_single     IMPORTING iv_name        TYPE seoclname
                                RETURNING VALUE(rt_text) TYPE stringtab
                                RAISING
                                          zcx_abap2md_error.
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
    TYPES: BEGIN OF crms_class_descr,
             class_name  TYPE seoclsname,
             description TYPE seodescr,
           END OF crms_class_descr.
    TYPES crmt_class_descr TYPE STANDARD TABLE OF crms_class_descr.

    TYPES: BEGIN OF crms_parameter_info,
             parameter_name TYPE seocmpname,
             direction      TYPE char20,
             typ_type       TYPE seotyptype,
             data_type      TYPE rs38l_typ,
             descr_found    TYPE abap_bool,
             description    TYPE rswsourcet,
           END OF crms_parameter_info.
    TYPES crmt_parameter_info TYPE STANDARD TABLE OF crms_parameter_info WITH EMPTY KEY.
    TYPES: BEGIN OF crms_exception_info,
             exception_name TYPE seoclsname,
             data_type      TYPE rs38l_typ,
             descr_found    TYPE abap_bool,
             description    TYPE rswsourcet,
           END OF crms_exception_info.
    TYPES crmt_exception_info TYPE STANDARD TABLE OF crms_exception_info WITH EMPTY KEY.
    TYPES: BEGIN OF crms_method_info,
             method_name     TYPE  seocpdname,
             docu_style      TYPE  char10,
             exposure        TYPE  seoexpose,
             abstract        TYPE  char1,
             redefined       TYPE  char1,
             static          TYPE  char1,
             descr_found     TYPE  abap_bool,
             description     TYPE  rswsourcet,
             return_info     TYPE  crms_parameter_info,
             parameter_infos TYPE  crmt_parameter_info,
             exception_infos TYPE  crmt_exception_info,
           END OF crms_method_info.
    TYPES crmt_method_info TYPE STANDARD TABLE OF crms_method_info.

    DATA: BEGIN OF ms_class_docu_structure,
            class_name     TYPE seoclsname,
            exposure       TYPE seoexpose,
            super_class    TYPE crms_class_descr,
            interfaces     TYPE crmt_class_descr,
            friend_classes TYPE crmt_class_descr,
            sub_classes    TYPE crmt_class_descr,
            methods        TYPE crmt_method_info,
            descr_found    TYPE crmt_boolean,
            description    TYPE rswsourcet,
          END OF ms_class_docu_structure.

    METHODS read_class_info
      IMPORTING
        iv_class_name TYPE seoclname
      RAISING
        zcx_abap2md_error.
    METHODS parse_docu_jd
      CHANGING
        cs_method_info TYPE zcl_abap2md_main=>crms_method_info.
    METHODS parse_docu_xml
      CHANGING
        cs_method_info TYPE zcl_abap2md_main=>crms_method_info.
    METHODS build_class_docu_structure.
    METHODS build_class_info.
    METHODS build_method_info.
    METHODS add_message_symsg.
    METHODS find_next_jd_tag
      EXPORTING
        ev_next_tag    TYPE char20
        ev_tag_value   TYPE name_komp
        et_description TYPE rswsourcet
      CHANGING
        ct_source      TYPE rswsourcet
        cv_next_indent TYPE i.
    METHODS write_description
      IMPORTING i_description TYPE zcl_abap2md_main=>crms_parameter_info-description
      CHANGING  i_out         TYPE stringtab.
    METHODS write_definition
      IMPORTING i_description TYPE zcl_abap2md_main=>crms_parameter_info-description
      CHANGING  i_out         TYPE stringtab.
    METHODS write_out_params
      IMPORTING
        i_method TYPE zcl_abap2md_main=>crms_method_info
      CHANGING
        ct_text  TYPE stringtab.
    METHODS write_out_param_dir
      IMPORTING
        i_method TYPE zcl_abap2md_main=>crms_method_info
        iv_dir   TYPE zcl_abap2md_main=>crms_parameter_info-direction
      CHANGING
        ct_text  TYPE stringtab.
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
    DATA: ls_class_descr TYPE crms_class_descr.
**/
* This method builds the class header info
*
*/




* Get class interface info
    READ TABLE mt_class_interface_info_set INTO DATA(ls_class_interface_info) INDEX 1.


* Set class name
    ms_class_docu_structure-class_name = ls_class_interface_info-clsname.


* Set default description
    APPEND ls_class_interface_info-descript TO ms_class_docu_structure-description.


* Set exposure
    ms_class_docu_structure-exposure = ls_class_interface_info-exposure.


* Super class
    READ TABLE mt_meta_relation_set
      INTO DATA(ls_meta_relation)
      WITH KEY reltype = '2'.
    ms_class_docu_structure-super_class = ls_meta_relation-refclsname.


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



  ENDMETHOD.


  METHOD build_method_info.
**/
* This method builds the method infos
*
*/


    DATA lt_source            TYPE rswsourcet.
    DATA lv_source            TYPE string.
    DATA ls_parameter_info    TYPE crms_parameter_info.
    DATA ls_exception_info    TYPE crms_exception_info.
    DATA ls_method_info       TYPE crms_method_info.
    DATA lv_cpdname           TYPE seocpdname.
    DATA lv_coding_started    TYPE abap_bool.
    DATA lv_c1                TYPE c.


* Get header docu of all methods
    LOOP AT mt_method_set INTO DATA(ls_method).

      CLEAR ls_method_info.
      REFRESH lt_source.

*   Create composite component name (for interface methods)
      IF ls_method-refclsname CP 'IF*'.
        CONCATENATE ls_method-refclsname '~' ls_method-cmpname INTO lv_cpdname.
      ELSE.
        lv_cpdname = ls_method-cmpname.
      ENDIF.


*   Exposure
      ls_method_info-exposure = ls_method-exposure.

*   Redefined
      ls_method_info-redefined = ls_method-redefin.

*   Static
      IF ls_method-mtddecltyp = '1'.
        ls_method_info-static = abap_true.
      ENDIF.


*   Get include name
      READ TABLE mt_method_include_set INTO DATA(ls_method_include)
        WITH KEY cpdkey-cpdname = lv_cpdname.

*   No method include -> abstract
      IF sy-subrc IS NOT INITIAL.
        ls_method_info-abstract = abap_true.
      ENDIF.


*   Create method info entry
      ls_method_info-method_name = lv_cpdname.


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


*   Collect all comments until method statement
      CLEAR lv_coding_started.
      LOOP AT lt_source INTO lv_source.

*     Collect header comments
        lv_c1 = lv_source.
        IF lv_c1 = '*'.

*       Check style
          IF ls_method_info-docu_style IS INITIAL OR ls_method_info-docu_style = 'UNKNOWN'.

*         XML-like
            IF lv_source CS '<CLASS_DOCU>' OR lv_source CS '<METHOD_DOCU>'.
              ls_method_info-docu_style = 'XML'.
              REFRESH ls_method_info-description.   "delete already collected docu
*         JD-like
            ELSEIF lv_source CS '*/' OR lv_source CS '@PARAM' OR lv_source CS '@RETURN'.
              ls_method_info-docu_style = 'JD'.
              REFRESH ls_method_info-description.   "delete already collected docu
            ELSE.
              ls_method_info-docu_style = 'UNKNOWN'.
            ENDIF.

          ENDIF.

*       Do only collect comments when coding has not started yet or former XML style
          CHECK lv_coding_started = abap_false OR ls_method_info-docu_style = 'XML'.

*       Remove comment tag
          SHIFT lv_source BY 1 PLACES.

*       Append to description table
          IF lv_source IS NOT INITIAL.
            APPEND lv_source TO ls_method_info-description.
          ENDIF.

*     Exit when coding starts
        ELSEIF lv_source CS 'METHOD'.

*          lv_coding_started = abap_true.

        ELSE.
*       Exit if documentation has been found
          EXIT.

        ENDIF.


      ENDLOOP.

*   Parse docu
      IF lt_source IS NOT INITIAL.
*     - XML
        IF ls_method_info-docu_style = 'XML'.
          parse_docu_xml( CHANGING cs_method_info = ls_method_info ).
*     - JD
        ELSEIF ls_method_info-docu_style = 'JD'.
          parse_docu_jd( CHANGING cs_method_info = ls_method_info ).
*     - Unknown
        ELSEIF ls_method_info-docu_style = 'UNKNOWN'.
          "... do nothing
*     - not commented, take the method description
        ELSE.
          APPEND ls_method-descript TO ls_method_info-description.
        ENDIF.
      ENDIF.


*   Append to method info table
      IF ls_method_info-method_name <> 'CLASS_DOCU'.
        APPEND ls_method_info TO ms_class_docu_structure-methods.
      ELSE.
        IF ls_method_info-descr_found = abap_true.
          ms_class_docu_structure-description = ls_method_info-description.
          ms_class_docu_structure-descr_found = abap_true.
        ENDIF.
      ENDIF.


*   Raise warning message for missing docu
*   - method docu
      IF ls_method_info-descr_found = abap_false.
*     ... but not for abstract methods
        IF ls_method_info-abstract = abap_false.
*       Raise message
          MESSAGE i113(crm_mktgs_docugen) WITH ms_class_docu_structure-class_name ls_method_info-method_name INTO DATA(mv_message_text).
          add_message_symsg( ).
        ENDIF.
*   - check parameter only if method docu exists
      ELSE.
*     - return docu
        IF ls_method_info-return_info-parameter_name IS NOT INITIAL AND
           ls_method_info-return_info-descr_found    = abap_false.
*       Raise message
          MESSAGE i110(crm_mktgs_docugen) WITH
              ms_class_docu_structure-class_name
              ls_method_info-method_name
              ls_method_info-return_info-parameter_name
            INTO mv_message_text.
          add_message_symsg( ).
        ENDIF.
*     - parameter docu
        LOOP AT ls_method_info-parameter_infos INTO ls_parameter_info WHERE descr_found = abap_false.
*       Raise message
          MESSAGE i111(crm_mktgs_docugen) WITH
              ms_class_docu_structure-class_name
              ls_method_info-method_name
              ls_parameter_info-parameter_name
            INTO mv_message_text.
          add_message_symsg( ).
        ENDLOOP.
*     - exception docu
        LOOP AT ls_method_info-exception_infos INTO ls_exception_info WHERE descr_found = abap_false.
*       Raise message
          MESSAGE i112(crm_mktgs_docugen) WITH
              ms_class_docu_structure-class_name
              ls_method_info-method_name
              ls_exception_info-exception_name
            INTO mv_message_text.
          add_message_symsg( ).
        ENDLOOP.
      ENDIF.


    ENDLOOP.


* Raise message for missing class description
    IF ms_class_docu_structure-descr_found = abap_false.
*   Raise message
      MESSAGE i114(crm_mktgs_docugen) WITH ms_class_docu_structure-class_name INTO mv_message_text.
      add_message_symsg( ).
    ENDIF.



  ENDMETHOD.


  METHOD find_next_jd_tag.
**/
* This method collects the description until the next JavaDoc-like tag
*  Known tag values:
*   - Method docu starts:   CL_CLASS_DOCU_GENERATOR=>CO_JD_DOCU_START
*   - Method docu end:      CL_CLASS_DOCU_GENERATOR=>CO_JD_DOCU_END
*   - Parameter:            CL_CLASS_DOCU_GENERATOR=>CO_JD_PARAM
*   - Return:               CL_CLASS_DOCU_GENERATOR=>CO_JD_RETURN
*   - Exception:            CL_CLASS_DOCU_GENERATOR=>CO_JD_THROWS
*
* @param EV_NEXT_TAG        Next/Last found tag
* @param EV_TAG_VALUE       Tag value
* @param ET_DESCRIPTION     Description found until found tag
* @param CV_NEXT_INDENT     Next indent to be used
* @param CT_SOURCE          Source description
*
*/


    DATA lt_source            TYPE rswsourcet.
    DATA lv_source            TYPE string.
    DATA lv_source_tmp        TYPE string.
    DATA lv_source_before     TYPE string.
    DATA lv_source_after      TYPE string.
    DATA lv_current_tag       TYPE char20.
    DATA lv_tag               TYPE char20.
    DATA lv_tag_alt           TYPE char20.
    DATA ls_match_result      TYPE match_result.
    DATA lv_shift             TYPE i.
    DATA lv_length            TYPE i.
    DATA lv_length_tmp        TYPE i.
    DATA lv_no_spaces         TYPE i.

    FIELD-SYMBOLS <fv_source> TYPE string.


* Initialize exporting parameter
    REFRESH et_description.
    CLEAR: ev_next_tag.

* Get source
    lt_source = ct_source.

* Scan for tags
    LOOP AT lt_source INTO lv_source.

*   Check for jd tag
*   - class docu start
      lv_tag = '*/'.
      ev_next_tag = lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.

*   - class docu end
      lv_tag = '/'.
      ev_next_tag = lv_tag.
*   - at the beginnig of the line (* has been removed)
      IF lv_source(1) = lv_tag.
        ls_match_result-offset = 0.
        ls_match_result-length = 1.
        EXIT.
      ENDIF.
*   - in the middle of the line (with *)
      CONCATENATE '*' lv_tag INTO lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.

*   - return
      lv_tag = '@RETURN'.
      ev_next_tag = lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.

*   - param
      lv_tag = '@PARAM'.
      ev_next_tag = lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.

*   - exception
      lv_tag = '@EXCEPTION'.
      ev_next_tag = lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.

*   - throws
      lv_tag = '@THROWS'.
      ev_next_tag = lv_tag.
      FIND lv_tag IN lv_source IGNORING CASE RESULTS ls_match_result.
      IF sy-subrc = 0. EXIT. ENDIF.


*   Remove leading spaces
      IF sy-tabix = 1.
        CONDENSE lv_source .
      ELSE.
*     Consider indent starting with second line
        lv_source_tmp = lv_source.
        CONDENSE lv_source_tmp.
        lv_length_tmp = strlen( lv_source_tmp ).
        lv_length     = strlen( lv_source ) - cv_next_indent.
        IF lv_length_tmp < lv_length.
          SHIFT lv_source BY cv_next_indent PLACES.
        ELSE.
          cv_next_indent = strlen( lv_source ) - lv_length_tmp.
          lv_source = lv_source_tmp.
        ENDIF.
      ENDIF.


*   Append description
      APPEND lv_source TO et_description.


*   Remove line from source
      DELETE ct_source INDEX 1.


    ENDLOOP.


* Get current line
    READ TABLE ct_source ASSIGNING <fv_source> INDEX 1.
    IF <fv_source> IS ASSIGNED.
* Get length
      lv_length = strlen( <fv_source> ).

* Check result
      lv_source_before = <fv_source>(ls_match_result-offset).
      lv_shift = ls_match_result-offset + ls_match_result-length.
      SHIFT <fv_source> BY lv_shift PLACES.

* Check if tag value needs to be processed
      IF ev_next_tag = '@PARAM' OR ev_next_tag = '@EXCEPTION' OR ev_next_tag = '@THROWS'.

*   Get tag value
        CONDENSE <fv_source>.
        SPLIT <fv_source> AT space INTO ev_tag_value <fv_source>.
        CONDENSE ev_tag_value.

      ENDIF.

* Calculate next indent
      cv_next_indent = lv_length - strlen( <fv_source> ).
* Add source before to description
      lv_source_tmp = lv_source_before.
      CONDENSE lv_source_tmp.
      IF lv_source_tmp IS NOT INITIAL.
        APPEND lv_source_before TO et_description.
      ENDIF.

* Delete source line if empty
      IF <fv_source> IS INITIAL.
        DELETE ct_source INDEX 1.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD generate_multiple.
  ENDMETHOD.


  METHOD generate_single.
    DATA: text_line TYPE string.

    read_class_info( iv_name ).

    build_class_docu_structure( ).
    APPEND |NAME| TO rt_text.
    APPEND |====| TO rt_text.

    APPEND |{ ms_class_docu_structure-class_name } - |  TO rt_text.

    write_description(    EXPORTING i_description = ms_class_docu_structure-description
                          CHANGING i_out         = rt_text ).


    LOOP AT ms_class_docu_structure-methods INTO DATA(method).
      APPEND INITIAL LINE TO rt_text.
      APPEND INITIAL LINE TO rt_text.

      APPEND |{ method-method_name }| TO rt_text.
      APPEND repeat( val = '-' occ = strlen( method-method_name ) ) TO rt_text.
      APPEND INITIAL LINE TO rt_text.
      write_description(    EXPORTING   i_description = method-description
                            CHANGING    i_out         = rt_text ).

      write_out_params(     EXPORTING   i_method = method
                            CHANGING    ct_text = rt_text ).
    ENDLOOP.



  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA: text_line TYPE string.

    TRY.
        DATA(text) = generate_single( 'zcl_abap2md_main' ).
        out->write( text ).
      CATCH zcx_abap2md_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.


  METHOD parse_docu_jd.
**/
* This method parses the comments using JavaDoc-like tags
*
* @param CS_METHOD_INFO   Method info structure
*
*/


    DATA ls_parameter_info              TYPE crms_parameter_info.
    DATA ls_exception_info              TYPE crms_exception_info.
    DATA lt_source                      TYPE rswsourcet.
    DATA lt_description                 TYPE rswsourcet.
    DATA lv_source                      TYPE string.
    DATA lv_tag_type                    TYPE char20.
    DATA lv_current_tag                 TYPE char20.
    DATA lv_tag_value                   TYPE name_komp.
    DATA lv_finished                    TYPE abap_bool.
    DATA lv_next_indent                 TYPE i.
    DATA ls_message                     TYPE bapiret2.
    DATA: mv_message_text TYPE string.

    FIELD-SYMBOLS <fs_parameter_info>     TYPE crms_parameter_info.
    FIELD-SYMBOLS <fs_exception_info>     TYPE crms_exception_info.
    FIELD-SYMBOLS <ft_description>  TYPE rswsourcet.


* Get method raw header docu
    lt_source = cs_method_info-description.
    REFRESH cs_method_info-description.

* Scan for tags
    WHILE lv_finished = abap_false.

*   Get next tag
      find_next_jd_tag( IMPORTING ev_next_tag    = lv_current_tag
                                  ev_tag_value   = lv_tag_value
                                  et_description = lt_description
                        CHANGING  ct_source      = lt_source
                                  cv_next_indent = lv_next_indent ).

*   Add description to formerly selected tag
      IF <ft_description> IS ASSIGNED.
        APPEND LINES OF lt_description TO <ft_description>.
      ENDIF.

*   Find tag info entry
      UNASSIGN: <fs_parameter_info>, <fs_exception_info>, <ft_description>.
      TRANSLATE lv_current_tag TO UPPER CASE.            "#EC TRANSLANG
      TRANSLATE lv_tag_value   TO UPPER CASE.            "#EC TRANSLANG
      CASE lv_current_tag.

*     Method docu start
        WHEN '*/'.

*       Get description table
          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE cs_method_info              TO <ft_description>.
          cs_method_info-descr_found = abap_true.


*     Method docu start
        WHEN '/'.

*       Exit loop
          lv_finished = abap_true.


*     Returning docu
        WHEN '@RETURN'.

*       Get description table
          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE cs_method_info-return_info  TO <ft_description>.
          cs_method_info-return_info-descr_found = abap_true.


*     Parameter docu
        WHEN '@PARAM'.

*       Get parameter info
          READ TABLE cs_method_info-parameter_infos ASSIGNING <fs_parameter_info> WITH KEY parameter_name = lv_tag_value.

*       Not found
          IF sy-subrc IS NOT INITIAL.

*         Create new entry if not found
            CLEAR ls_parameter_info.
            ls_parameter_info-parameter_name = lv_tag_value.
            ls_parameter_info-direction      = 'UNKNOWN'.
            APPEND ls_parameter_info TO cs_method_info-parameter_infos.
            READ TABLE cs_method_info-parameter_infos ASSIGNING <fs_parameter_info> WITH KEY parameter_name = lv_tag_value.

*         Raise message
            MESSAGE w101(crm_mktgs_docugen) WITH ms_class_docu_structure-class_name cs_method_info-method_name lv_tag_value INTO mv_message_text.
            add_message_symsg( ).

          ENDIF.

*       Get description table
          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_parameter_info> TO <ft_description>.
          <fs_parameter_info>-descr_found = abap_true.


*     Exception docu
        WHEN '@EXCEPTION' OR '@THROWS'.

*       Get exception info
          READ TABLE cs_method_info-exception_infos ASSIGNING <fs_exception_info> WITH KEY exception_name = lv_tag_value.

*       Not found
          IF sy-subrc IS NOT INITIAL.

*         Create new entry if not found
            CLEAR ls_exception_info.
            ls_exception_info-exception_name = lv_tag_value.
            APPEND ls_exception_info TO cs_method_info-exception_infos.
            READ TABLE cs_method_info-exception_infos ASSIGNING <fs_exception_info> WITH KEY exception_name = lv_tag_value.

*         Raise message
            MESSAGE w102(crm_mktgs_docugen) WITH ms_class_docu_structure-class_name cs_method_info-method_name lv_tag_value INTO mv_message_text.
            add_message_symsg( ).

          ENDIF.

*       Get description table
          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_exception_info> TO <ft_description>.
          <fs_exception_info>-descr_found = abap_true.


      ENDCASE.


    ENDWHILE.



  ENDMETHOD.


  METHOD parse_docu_xml.

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
        APPEND |{ start }       { text_line }| TO i_out.
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
    DATA: par    TYPE zcl_abap2md_main=>crms_parameter_info,
          lv_dir TYPE zcl_abap2md_main=>crms_parameter_info-direction.


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

    LOOP AT i_method-parameter_infos INTO par.
      APPEND INITIAL LINE TO ct_text.
      APPEND |**{ par-parameter_name }**|  TO ct_text.
      write_definition(     EXPORTING i_description = par-description
                            CHANGING i_out         = ct_text ).
    ENDLOOP.


  ENDMETHOD.


  METHOD write_out_param_dir.

    DATA par TYPE zcl_abap2md_main=>crms_parameter_info.

    IF line_exists( i_method-parameter_infos[ direction = iv_dir ] ).
      APPEND iv_dir TO ct_text.
      LOOP AT i_method-parameter_infos INTO par WHERE direction = iv_dir.
        APPEND |{ par-parameter_name } TYPE { par-data_type }|  TO ct_text.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
