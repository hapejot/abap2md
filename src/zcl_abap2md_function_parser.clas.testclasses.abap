CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      parse_report FOR TESTING RAISING cx_static_check,
      test_read_report FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD parse_report.
    DATA src TYPE stringtab.
    DATA doc TYPE zabap2md_doc_structure.

    src = VALUE #(
        ( |**/| )
        ( |* @page main ABAP to Markdown| )
        ( |*| )
        ( |* For generating documentation for existing ABAP code one can use several ways.| )
        ( |* running the report for downloading a complete markdown document is one of it.| )
        ( |* another way is using the RFC module using the C-Client for generating the documentation| )
        ( |* in a more automated fashion on the client.| )
        ( |*| )
        ( |*/| )
        ( || )
        ( |**/| )
        ( |* @page p2 General procedure of document setup| )
        ( |* The program is flexible but follows a fixed procedure to create the documentation| )
        ( |* There are basically three phases: reading object info, parsing comments, output markdown| )
        ( |*| )
        ( |* @section s1 Reading Objects| )
        ( |*| )
        ( |* In the initial phase the program is gathering lots of informations from the system| )
        ( |* but only some meta data is read. The sources are not touched in this phase to avoid| )
        ( |* unnecessary memory consumption.| )
        ( |*| )
        ( |* @section s2 Building Documentation Structure| )
        ( |*| )
        ( |* This phase walks through all the gathered informations and also parses the sources.| )
        ( |* THe parsing results are directly fed into the documentation structures. The Meta data| )
        ( |* then is also filled into the documentation structures.| )
        ( |*| )
        ( |* @section s3 Output Markdown| )
        ( |*| )
        ( |* This phase generates a markdown text from the documentation structure.| )
        ( |* This step can easily replaced by some other generating tool since| )
        ( |* all sematinc information should be ready available in the documentation structure.| )
        ( |*| )
        ( |* ```\{.graphviz caption="Example of displaying a graph" width=50%\}| )
        ( |* digraph G \{| )
        ( |*    node [ shape = box ]| )
        ( |*    MAIN -> GENERATE_MULTI| )
        ( |*    GENERATE_MULTI -> GENERATE_DOC_STRUCT| )
        ( |*    GENERATE_SINGLE -> GENERATE_DOC_STRUCT| )
        ( |*    GENERATE_DOC_STRUCT -> ZCL_MAIN| )
        ( |* \}| )
        ( |* ```| )
        ( |*| )
        ( |* ```\{.plantuml caption="General Flow" width=50%\}| )
        ( |* Client -> Main : generate_multi| )
        ( |* Main -> Info : read_main| )
        ( |* Main -> Info : build_doc_structure| )
        ( |* Info -> Generator : add text| )
        ( |* Main -> Generator : get_text| )
        ( |* Main -> Client : return text| )
        ( |* ```| )
        ( |*/| )
        ( || )
        ( || )
        ( |**/| )
        ( |* Description of the report itself.| )
        ( |* @param obj is a select option defining a set of objects.| )
        ( |*/| )
        ( |REPORT zr_abap2md_main.| )
        ( |DATA:| )
        ( |  rc        TYPE i,| )
        ( |  doc       TYPE stringtab,| )
        ( |  path      TYPE zcl_abap2md_local_file=>t_dir,| )
        ( |  files     TYPE filetable,| )
        ( |  obj_names TYPE STANDARD TABLE OF zcl_abap2md_main=>obj_name,| )
        ( |  obj_name  TYPE zcl_abap2md_main=>obj_name,| )
        ( |  options   TYPE zabap2md_options.| )
        ( |SELECT-OPTIONS:| )
        ( |    s_objs FOR obj_name.| )
        ( |PARAMETERS: p_path  TYPE zcl_abap2md_local_file=>t_dir DEFAULT 'abap-doc.md'.| )
        ( || )
        ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.| )
        ( |  cl_gui_frontend_services=>file_open_dialog(| )
        ( |    CHANGING| )
        ( |      file_table              = files| )
        ( |      rc                      = rc| )
        ( |  ).| )
        ( |  IF lines( files ) > 0.| )
        ( |    p_path = files[ 1 ].| )
        ( |  ENDIF.| )
        ( || )
        ( |START-OF-SELECTION.| )
        ( || )
        ( |  " first select possible candidates from TADIR| )
        ( |  SELECT obj_name| )
        ( |        FROM tadir| )
        ( |        WHERE obj_name IN @s_objs| )
        ( |        AND pgmid = 'R3TR'| )
        ( |        AND object IN ( 'CLAS', 'PROG' )| )
        ( |        INTO TABLE @obj_names.| )
        ( || )
        ( |  " next try the same with TFDIR| )
        ( |  SELECT funcname| )
        ( |        FROM tfdir| )
        ( |        WHERE funcname IN @s_objs| )
        ( |        APPENDING TABLE @obj_names.| )
        ( || )
        ( || )
        ( |  CALL FUNCTION 'Z_ABAP2MD_GENERATE_MULTI'| )
        ( |    EXPORTING| )
        ( |      it_names   = obj_names| )
        ( |      ix_options = options| )
        ( |    IMPORTING| )
        ( |      et_doc     = doc.| )
        ( || )
        ( |  DATA(file) = NEW zcl_abap2md_local_file( ).| )
        ( |  file->add_text( doc ).| )
        ( |  file->save( i_path = p_path ).| )
    ).

    DATA(cut) = zcl_abap2md_function_parser=>create(
                i_code   = src
                i_doc    = REF #( doc )
                i_name = 'TEST_REPORT'
            ).
    cut->parse( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( doc-pages ) ).
    DATA(r) = REF #( doc-programs[ 1 ] ).
    DATA(d) = r->text[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = 'Description of the report itself.' act = d ).
  ENDMETHOD.


  METHOD test_read_report.
    DATA: src TYPE stringtab,
          prg TYPE progn,
          doc TYPE zabap2md_doc_structure.
    prg = 'ZR_ABAP2MD_MAIN'.
    READ REPORT prg INTO src.
    DATA(cut) = zcl_abap2md_function_parser=>create(
                i_code   = src
                i_doc    = REF #( doc )
                i_name = |{ prg }|
            ).
    cut->parse( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( doc-pages ) ).

  ENDMETHOD.

ENDCLASS.
