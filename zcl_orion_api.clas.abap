CLASS zcl_orion_api DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ii_client TYPE REF TO if_http_client .
  PROTECTED SECTION.

    DATA mi_client TYPE REF TO if_http_client .

    METHODS xpath
      IMPORTING
        !iv_xml         TYPE string
        !iv_xpath       TYPE string
      RETURNING
        VALUE(ri_nodes) TYPE REF TO if_ixml_node_collection .
    METHODS send_and_receive
      RETURNING
        VALUE(rv_data) TYPE string .
    METHODS set_uri
      IMPORTING
        !iv_path TYPE string .
    METHODS json_to_xml
      IMPORTING
        !iv_json      TYPE string
        !iv_display   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_xml) TYPE string .
    METHODS token
      RETURNING
        VALUE(rv_token) TYPE string .
    METHODS json_pretty
      IMPORTING
        !iv_json         TYPE string
      RETURNING
        VALUE(rv_pretty) TYPE string .
  PRIVATE SECTION.

    DATA mv_token TYPE string .
ENDCLASS.



CLASS ZCL_ORION_API IMPLEMENTATION.


  METHOD constructor.

    mi_client = ii_client.

  ENDMETHOD.


  METHOD json_pretty.

    DATA(lv_xjson) = cl_abap_codepage=>convert_to( iv_json ).
    DATA(li_reader) = cl_sxml_string_reader=>create( lv_xjson ).
    DATA(li_writer) = CAST if_sxml_writer(
                        cl_sxml_string_writer=>create( if_sxml=>co_xt_json ) ).
    li_writer->set_option( if_sxml_writer=>co_opt_linebreaks ).
    li_writer->set_option( if_sxml_writer=>co_opt_indent ).
    li_reader->next_node( ).
    li_reader->skip_node( li_writer ).
    rv_pretty = cl_abap_codepage=>convert_from(
                  CAST cl_sxml_string_writer( li_writer )->get_output( ) ).

  ENDMETHOD.


  METHOD json_to_xml.

    DATA(lv_xstr) = cl_abap_codepage=>convert_to( iv_json ).
    DATA(li_reader) = cl_sxml_string_reader=>create( lv_xstr ).
    DATA(li_writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( ) ).

    DATA(li_node) = li_reader->read_next_node( ).
    WHILE NOT li_node IS INITIAL.
      li_writer->write_node( li_node ).
      li_node = li_reader->read_next_node( ).
    ENDWHILE.

    lv_xstr = CAST cl_sxml_string_writer( li_writer )->get_output( ).
    rv_xml = cl_abap_codepage=>convert_from( lv_xstr ).

    IF iv_display = abap_true.
      DATA(li_out) = cl_demo_output=>new( ).
      li_out->write_xml( lv_xstr ).
      li_out->display( ).
    ENDIF.

  ENDMETHOD.


  METHOD send_and_receive.

    mi_client->send( ).
    mi_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS = 4 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    mi_client->response->get_status( IMPORTING code = DATA(lv_code) ).
    IF lv_code <> 200
        AND lv_code <> 201
        AND lv_code <> 204.
      BREAK-POINT.
    ENDIF.

    rv_data = mi_client->response->get_cdata( ).

  ENDMETHOD.


  METHOD set_uri.

    DATA: lv_uri TYPE string.


    CONCATENATE '/sap/hana/xs/dt/base/' iv_path INTO lv_uri.

    IF lv_uri CP '*//*'.
      BREAK-POINT.
    ENDIF.

    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

  ENDMETHOD.


  METHOD token.

    IF NOT mv_token IS INITIAL.
      rv_token = mv_token.
      RETURN.
    ENDIF.

    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = '/sap/hana/xs/dt/base/server/csrf.xsjs' ).

    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'HEAD' ).

    mi_client->request->set_header_field(
        name  = 'X-CSRF-Token'
        value = 'Fetch' ) ##NO_TEXT.

    send_and_receive( ).

    rv_token = mi_client->response->get_header_field( 'x-csrf-token' ) ##NO_TEXT.
    mv_token = rv_token.

* reset to default request method
    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).

  ENDMETHOD.


  METHOD xpath.

    DATA(lo_xslt) = NEW cl_xslt_processor( ).
    lo_xslt->set_source_string( iv_xml ).

    lo_xslt->set_expression( iv_xpath ).
    lo_xslt->run( '' ).
    ri_nodes = lo_xslt->get_nodes( ).

  ENDMETHOD.
ENDCLASS.