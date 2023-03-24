CLASS zcl_abap2md_adt_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
             t_nodes TYPE STANDARD TABLE OF REF TO if_ixml_node WITH EMPTY KEY.
    TYPES: BEGIN OF link,
             kind       TYPE string,
             start_line TYPE i,
             start_pos  TYPE i,
             end_line   TYPE i,
             end_pos    TYPE i,
           END OF link.
    "! parsing the ADT result xml
    METHODS parse_xml
      IMPORTING
        cdata TYPE string.

    METHODS parse_result_set
      IMPORTING
        i_doc TYPE REF TO if_ixml_node.

    "! instatiate the parser with the given string
    METHODS create_xml_parser
      IMPORTING cdata           TYPE string
      RETURNING VALUE(r_result) TYPE REF TO if_ixml_document.
    "! read source of an interface definition
    METHODS read_interface_source
      IMPORTING
        interface_name TYPE string.
    "! V2 of reading interface def source
    METHODS read_interface_source2
      IMPORTING
        interface_name TYPE string.
    "! recursive extrad values
    METHODS extract_children
      IMPORTING i_node          TYPE REF TO if_ixml_node
      RETURNING VALUE(r_result) TYPE t_nodes.
    "! name
    METHODS adt_name
      IMPORTING
        i_doc         TYPE REF TO if_ixml_node
      RETURNING
        VALUE(r_name) TYPE string.
    METHODS parse_element IMPORTING i_node   TYPE REF TO if_ixml_node
                          RETURNING
                                    VALUE(l) TYPE zcl_abap2md_adt_reader=>link.
    METHODS extract_link_info
      IMPORTING
        i_child       TYPE REF TO if_ixml_node
      RETURNING
        VALUE(r_link) TYPE link.

    DATA: log TYPE stringtab,
          src TYPE stringtab.
ENDCLASS.



CLASS ZCL_ABAP2MD_ADT_READER IMPLEMENTATION.


  METHOD adt_name.
    TRY.
        r_name =
            i_doc->get_attributes(
                )->get_named_item(
                      name      = 'name'
                      namespace = 'adtcore'
                )->get_value( ).
      CATCH cx_sy_ref_is_initial.
        " ignore this.
    ENDTRY.
  ENDMETHOD.


  METHOD create_xml_parser.

    DATA(xml) = cl_ixml=>create( ).
    DATA(sf) = xml->create_stream_factory( ).
    DATA(istream) = sf->create_istream_string( cdata ).
    DATA(doc)  = xml->create_document( ).
    DATA(p)  = xml->create_parser(
           document       = doc
           istream        = istream
           stream_factory = sf        ).
    IF p->parse( ) = 0.
      r_result = doc.
    ENDIF.

  ENDMETHOD.


  METHOD extract_children.
    DATA(children) = i_node->get_children( ).
    DATA(n) = children->get_length( ).
    DO n TIMES.
      DATA(idx) = sy-index - 1.
      APPEND children->get_item( idx ) TO r_result.
    ENDDO.
  ENDMETHOD.


  METHOD extract_link_info.

    DATA: href  TYPE string,
          dummy TYPE string,
          tail  TYPE string,
          start TYPE string,
          end   TYPE string,
          pair  TYPE string,
          line  TYPE string,
          pos   TYPE string,
          rel   TYPE string,
          parts TYPE string_table,
          n     TYPE i.
    href = i_child->get_attributes( )->get_named_item( 'href' )->get_value( ).

    SPLIT href AT '#' INTO dummy tail.
    SPLIT tail AT ';' INTO start end.
    SPLIT start AT '=' INTO dummy pair.
    SPLIT pair AT ',' INTO line pos.
    r_link-start_line = line.
    r_link-start_pos = pos.

    SPLIT end AT '=' INTO dummy pair.
    SPLIT pair AT ',' INTO line pos.
    r_link-end_line = line.
    r_link-end_pos = pos.


    rel = i_child->get_attributes( )->get_named_item( 'rel' )->get_value( ).
    SPLIT rel AT '/' INTO TABLE parts.
    n = lines( parts ).
    r_link-kind = parts[ n ].

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    read_interface_source2( 'ZIF_ABAP2MD_INFO' ).
    read_interface_source( 'ZIF_ABAP2MD_INFO' ).
    out->write( log ).
  ENDMETHOD.


  METHOD parse_element.

    LOOP AT extract_children( i_node ) INTO DATA(child).
      DATA(name) = |{ child->get_namespace( ) }:{ child->get_name( ) }|.
      CASE name.
        WHEN 'atom:link'.
          l = extract_link_info( child ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_result_set.

    LOOP AT extract_children( i_doc ) INTO DATA(child).
      DATA(name) = |{ child->get_namespace( ) }:{ child->get_name( ) }|.
      CASE name.
        WHEN 'abapsource:objectStructureElement'.
          DATA(link) = parse_element( child ).
          APPEND |{ link-start_line }: { adt_name( child ) }|  TO log.
          DATA(idx) = link-start_line - 1.
          DATA(tabidx) = sy-tabix + 1.
          WHILE 1 < idx.
            IF match( val = src[ idx ] regex = ' *"!.*' ) <> ``.
              INSERT src[ idx ] INTO log INDEX tabidx.
            ELSE.
              EXIT.
            ENDIF.
            idx = idx - 1.
          ENDWHILE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_xml.


    DATA(doc) = create_xml_parser( cdata ).
    IF doc IS BOUND.
      parse_result_set( doc->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD read_interface_source.
    DATA req TYPE sadt_rest_request.
    DATA res TYPE sadt_rest_response.
    DATA txt TYPE STANDARD TABLE OF tline.
    DATA(line_len) = 132.

    req-request_line-method = 'GET'.
    req-request_line-uri = |/sap/bc/adt/oo/interfaces/{ interface_name }/objectstructure|.
    req-request_line-version = 'HTTP/1.1'.
    req-header_fields = VALUE #(
    ( name = 'Accept'             value = 'application/vnd.sap.adt.elementinfo+xml;q=0.9, text/plain;q=0.1' )
    ( name = 'User-Agent'          value = 'Eclipse/4.22.0.v20211124-1800 (win32; x86_64; Java 17.0.1) ADT/3.22.1 (devedition)' )
      ).
    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING
        request  = req    " Rest Request
      IMPORTING
        response = res.    " Rest Request


    DATA(s) = cl_bcs_convert=>xstring_to_string(
        iv_xstr   = res-message_body
        iv_cp     = '4110' " UTF-8
    ).

    parse_xml( s ).

  ENDMETHOD.


  METHOD read_interface_source2.
    DATA req TYPE sadt_rest_request.
    DATA res TYPE sadt_rest_response.
    DATA txt TYPE STANDARD TABLE OF tline.
    DATA(line_len) = 132.

    req-request_line-method = 'GET'.
    req-request_line-uri = |/sap/bc/adt/oo/interfaces/{ interface_name }/source/main|.
    req-request_line-version = 'HTTP/1.1'.
    req-header_fields = VALUE #(
    ( name = 'Accept'             value = 'text/plain' )
    ( name = 'User-Agent'          value = 'Eclipse/4.22.0.v20211124-1800 (win32; x86_64; Java 17.0.1) ADT/3.22.1 (devedition)' )
      ).
    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING
        request  = req    " Rest Request
      IMPORTING
        response = res.    " Rest Request


    DATA(s) = cl_bcs_convert=>xstring_to_string(
        iv_xstr   = res-message_body
        iv_cp     = '4110' " UTF-8
    ).

    SPLIT s AT |\n| INTO TABLE src.

  ENDMETHOD.
ENDCLASS.
