REPORT zorion.

*https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/file/p13939179trial/foobar
*CONSTANTS: c_url TYPE string VALUE 'https://s7hanaxs.hanatrial.ondemand.com/p13939179trial/foobar/0_NewPackage/index.html'.

* TODO:
* package = SAML2_CORE
* CL_ABAP_X509_CERTIFICATE
* CL_SAML20_ENTITY
* CL_SEC_SXML_DSIGNATURE looks good

* Links:
* http://www.ssocircle.com/en/1203/saml-request-online-decoder-encoder/
* https://www.samltool.com/fingerprint.php
* https://simplesamlphp.org/
* http://www.w3.org/TR/xmldsig-core/

PARAMETERS: p_url   TYPE text100 OBLIGATORY,
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS lcl_saml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_saml DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS redirect_encode
      IMPORTING iv_xml            TYPE string
      RETURNING value(rv_encoded) TYPE string.

    CLASS-METHODS redirect_decode
      IMPORTING iv_encoded    TYPE string
                iv_url TYPE abap_bool DEFAULT abap_true
      RETURNING value(rv_xml) TYPE string.

    CLASS-METHODS post_encode
      IMPORTING iv_xml            TYPE string
      RETURNING value(rv_encoded) TYPE string.

    CLASS-METHODS post_decode
      IMPORTING iv_encoded    TYPE string
      RETURNING value(rv_xml) TYPE string.

    CLASS-METHODS response.

  PRIVATE SECTION.

    CLASS-METHODS:
      base64_decode
        IMPORTING iv_string         TYPE string
        RETURNING value(rv_xstring) TYPE xstring,
      base64_encode
        IMPORTING iv_xstring       TYPE xstring
        RETURNING value(rv_string) TYPE string,
      deflate
        IMPORTING iv_data            TYPE string
        RETURNING value(rv_deflated) TYPE xstring,
      enflate
        IMPORTING iv_data            TYPE xstring
        RETURNING value(rv_enflated) TYPE string,
      url_encode
        IMPORTING iv_data           TYPE string
        RETURNING value(rv_encoded) TYPE string,
      url_decode
        IMPORTING iv_data           TYPE string
        RETURNING value(rv_decoded) TYPE string,
      string_to_xstring_utf8
        IMPORTING iv_string         TYPE string
        RETURNING value(rv_xstring) TYPE xstring,
      xstring_to_string_utf8
        IMPORTING iv_data          TYPE xstring
        RETURNING value(rv_string) TYPE string.

ENDCLASS.                    "lcl_saml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS download
      IMPORTING value(iv_data) TYPE string.
    CLASS-METHODS parse
      IMPORTING value(iv_html) TYPE string
                ii_client TYPE REF TO if_http_client.
    CLASS-METHODS output_string
      IMPORTING value(iv_str) TYPE string.
    CLASS-METHODS output_response_headers
      IMPORTING ii_client TYPE REF TO if_http_client.
    CLASS-METHODS output_request_headers
      IMPORTING ii_client TYPE REF TO if_http_client.
    CLASS-METHODS send_and_receive
      IMPORTING ii_client TYPE REF TO if_http_client.

ENDCLASS.                    "lcl_util DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.

  METHOD send_and_receive.

    DATA: lv_code TYPE i.


    ii_client->send( ).
    ii_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
* make sure SSL certificates is installed in STRUST
      ASSERT 1 = 1 + 1.
    ENDIF.
    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    IF lv_code <> 200.
      WRITE: / lv_code.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.                    "send_and_receive

  METHOD output_response_headers.

    DATA: lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    WRITE: / 'RESPONSE HEADERS'.
    ii_client->response->get_header_fields( CHANGING fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING <ls_field>.
      WRITE: / <ls_field>-name, <ls_field>-value.
    ENDLOOP.
    WRITE: /.

  ENDMETHOD.                    "output_header

  METHOD output_request_headers.

    DATA: lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    WRITE: / 'REQUEST HEADERS'.
    ii_client->request->get_header_fields( CHANGING fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING <ls_field>.
      WRITE: / <ls_field>-name, <ls_field>-value.
    ENDLOOP.
    WRITE: /.

  ENDMETHOD.                    "output_request_headers

  METHOD output_string.

    WHILE strlen( iv_str ) > 100.
      WRITE: / iv_str(100).
      iv_str = iv_str+100.
    ENDWHILE.
    WRITE: / iv_str.

  ENDMETHOD.                    "output_string

  METHOD parse.

    DATA: lv_sub1   TYPE string,
          lv_run    TYPE abap_bool,
          lv_name   TYPE string,
          lv_value  TYPE string,
          lv_regex  TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    FIND FIRST OCCURRENCE OF REGEX '<form ([^>]*)>'
      IN iv_html IGNORING CASE
      SUBMATCHES lv_sub1.
    WRITE: / 'form', lv_sub1.
    WRITE: /.

    lv_regex = '<input ([^>]*)>'.
    lv_run = abap_true.

    WHILE lv_run = abap_true.
      FIND FIRST OCCURRENCE OF REGEX lv_regex
        IN iv_html IGNORING CASE
        SUBMATCHES lv_sub1.
      IF sy-subrc <> 0.
        lv_run = abap_false.
      ELSE.
        REPLACE FIRST OCCURRENCE OF REGEX lv_regex IN iv_html WITH space.

        FIND FIRST OCCURRENCE OF REGEX 'name="([^"]*)"'
          IN lv_sub1 IGNORING CASE
          SUBMATCHES lv_name.
        FIND FIRST OCCURRENCE OF REGEX 'value="([^"]*)"'
          IN lv_sub1 IGNORING CASE
          SUBMATCHES lv_value.
        WRITE: / 'input', lv_name, lv_value.

        CASE lv_name.
*          WHEN 'SAMLRequest'.
*            lv_xml = lcl_saml=>redirect_decode( iv_encoded = lv_value
*                                                iv_url = abap_false ).
          WHEN 'j_username'.
            lv_value = p_user.
          WHEN 'j_password'.
            lv_value = p_passw.
        ENDCASE.
        APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
        <ls_field>-name = lv_name.
        <ls_field>-value = lv_value.

      ENDIF.
    ENDWHILE.
    WRITE: /.

    ii_client->request->set_form_fields( lt_fields ).

  ENDMETHOD.                    "parse

  METHOD download.

    DATA: lv_dir  TYPE string,
          lt_data TYPE TABLE OF char200.

    FIELD-SYMBOLS: <lv_data> LIKE LINE OF lt_data.


    cl_gui_frontend_services=>get_sapgui_workdir(
      CHANGING
        sapworkdir            = lv_dir
      EXCEPTIONS
        get_sapworkdir_failed = 1
        cntl_error            = 2
        error_no_gui          = 3
        not_supported_by_gui  = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
      RETURN.
    ENDIF.
    cl_gui_cfw=>flush( ).

    CONCATENATE lv_dir '\foobar.html' INTO lv_dir.

    WHILE strlen( iv_data ) > 200.
      APPEND INITIAL LINE TO lt_data ASSIGNING <lv_data>.
      <lv_data> = iv_data(200).
      iv_data = iv_data+200.
    ENDWHILE.
    APPEND INITIAL LINE TO lt_data ASSIGNING <lv_data>.
    <lv_data> = iv_data.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = lv_dir
      CHANGING
        data_tab                  = lt_data
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).
    IF sy-subrc <> 0.
      BREAK-POINT.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "download

ENDCLASS.                    "lcl_util IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_saml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_saml IMPLEMENTATION.

  METHOD post_encode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = string_to_xstring_utf8( iv_xml ).
    rv_encoded = base64_encode( lv_xstr ).

  ENDMETHOD.                    "post_encode

  METHOD response.

    DATA: lv_xml TYPE string,
          lv_response_to TYPE string.


    lv_response_to = 'asdf'.

    lv_xml = '<?xml version="1.0"?>' &&
      '<response xmlns="urn:oasis:names:tc:SAML:2.0:protocol" ' &&
      'xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion" ' &&
      'xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" ' &&
      'xmlns:ns4="http://www.w3.org/2001/04/xmlenc#" ' &&
      'Destination="https://account.hanatrial.ondemand.com/cockpit" ' &&
      'ID="RES-SSO-015f75e1-fb63-45c2-814d-350d29d9da89" ' &&
      'InResponseTo="' && lv_response_to && '" ' &&
      'IssueInstant="2015-03-15T08:18:24.279Z" Version="2.0">' &&
      '<ns2:issuer>accounts.sap.com</ns2:issuer>' &&
      '<status>' &&
      '<statuscode value="urn:oasis:names:tc:SAML:2.0:status:Success"/>' &&
      '</Status>' &&
      '<Assertion ' &&
      'xmlns="urn:oasis:names:tc:SAML:2.0:assertion" ' &&
      'xmlns:ns2="http://www.w3.org/2000/09/xmldsig#" ' &&
      'xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" ' &&
      'ID="A-387b38f9-2ef5-43fb-a9f8-9c20cf85ce72" ' &&
      'IssueInstant="2015-03-15T08:18:24.279Z" Version="2.0">' &&
      '<Issuer>accounts.sap.com</Issuer>' &&
      '<ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#">asdf</ds:Signature>' &&
      '<Subject>' &&
      '<NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified">P13939179</NameID>' &&
      '<SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer">' &&
      '<SubjectConfirmationData InResponseTo="' && lv_response_to && '" ' &&
      'NotOnOrAfter="2015-03-15T08:28:24.279Z" ' &&
      'Recipient="https://account.hanatrial.ondemand.com/cockpit"/>' &&
      '</SubjectConfirmation>' &&
      '</Subject>' &&
      '<Conditions NotBefore="2015-03-15T08:13:24.279Z" ' &&
      'NotOnOrAfter="2015-03-15T08:28:24.279Z">' &&
      '<AudienceRestriction>' &&
      '<Audience>https://nwtrial.ondemand.com/services</Audience>' &&
      '</AudienceRestriction>' &&
      '</Conditions>' &&
      '<AuthnStatement AuthnInstant="2015-03-15T08:18:24.279Z" ' &&
      'SessionIndex="S-SP-0e3df538-a4e0-4e43-9ec4-4813ea6f6562" ' &&
      'SessionNotOnOrAfter="2015-03-15T20:18:24.279Z">' &&
      '<AuthnContext>' &&
      '<AuthnContextClassRef>' &&
      'urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef>' &&
      '</AuthnContext>' &&
      '</AuthnStatement>' &&
      '</Assertion>' &&
      '</Response>'.

  ENDMETHOD.                    "response

  METHOD post_decode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = base64_decode( iv_encoded ).
    rv_xml = xstring_to_string_utf8( lv_xstr ).

  ENDMETHOD.                    "post_decode

  METHOD redirect_encode.

    DATA: lv_str  TYPE string,
          lv_xstr TYPE xstring.


    lv_xstr = deflate( iv_xml ).
    lv_str = base64_encode( lv_xstr ).
    rv_encoded = url_encode( lv_str ).

  ENDMETHOD.                    "redirect_encode

  METHOD redirect_decode.

    DATA: lv_str  TYPE string,
          lv_xstr TYPE xstring.


    IF iv_url = abap_true.
      lv_str = url_decode( iv_encoded ).
    ELSE.
      lv_str = iv_encoded.
    ENDIF.
    lv_xstr = base64_decode( lv_str ).
    rv_xml = enflate( lv_xstr ).

  ENDMETHOD.                    "redirect_decode

  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "xstring_to_string_utf8

  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "string_to_xstring_utf8

  METHOD base64_encode.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata                  = iv_xstring
      IMPORTING
        b64data                  = rv_string
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
      ASSERT 1 = 1 + 1.
    ENDIF.

  ENDMETHOD.                    "base64_encode

  METHOD base64_decode.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data                  = iv_string
      IMPORTING
        bindata                  = rv_xstring
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
      ASSERT 1 = 1 + 1.
    ENDIF.

  ENDMETHOD.                    "base64_decode

  METHOD deflate.

    DATA: lv_raw TYPE xstring.


    lv_raw = string_to_xstring_utf8( iv_data ).

    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in   = lv_raw
      IMPORTING
        gzip_out = rv_deflated ).

  ENDMETHOD.                    "deflate

  METHOD enflate.

    DATA: lv_raw TYPE xstring.


    cl_abap_gzip=>decompress_binary(
      EXPORTING
        gzip_in = iv_data
      IMPORTING
        raw_out = lv_raw ).

    rv_enflated = xstring_to_string_utf8( lv_raw ).

  ENDMETHOD.                    "enflate

  METHOD url_encode.

    rv_encoded = cl_http_utility=>if_http_utility~escape_url( iv_data ).

  ENDMETHOD.                    "url_encode

  METHOD url_decode.

    rv_decoded = cl_http_utility=>if_http_utility~unescape_url( iv_data ).

  ENDMETHOD.                    "url_decode

ENDCLASS.                    "lcl_saml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_saml_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_saml_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mv_xml     TYPE string,
          mv_encoded TYPE string.

    METHODS setup.
    METHODS check_date IMPORTING iv_xml TYPE string.
    METHODS redirect_encode FOR TESTING.
    METHODS redirect_decode FOR TESTING.
    METHODS post FOR TESTING.

ENDCLASS.                    "lcl_saml_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_saml_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_saml_test IMPLEMENTATION.

  METHOD post.

    DATA: lv_encoded TYPE string,
          lv_result  TYPE string.


    lv_encoded = 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVR' &&
      'GLTgiPz4NCjxzYW1scDpBdXRoblJlcXVlc3QgeG1sbnM6c2FtbHA9InV' &&
      'ybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpwcm90b2NvbCIgSUQ9ImF' &&
      'nZG9iamNmaWtuZW9tbWZqYW1kY2xlbmpjcGNqbWdkZ2JtcGdqbW8iIFZl' &&
      'cnNpb249IjIuMCIgSXNzdWVJbnN0YW50PSIyMDA3LTA0LTI2VDEzOjUxO' &&
      'jU2WiIgUHJvdG9jb2xCaW5kaW5nPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0' &&
      'FNTDoyLjA6YmluZGluZ3M6SFRUUC1QT1NUIiBQcm92aWRlck5hbWU9Imdv' &&
      'b2dsZS5jb20iIEFzc2VydGlvbkNvbnN1bWVyU2VydmljZVVSTD0iaHR0cH' &&
      'M6Ly93d3cuZ29vZ2xlLmNvbS9hL3NvbHdlYi5uby9hY3MiIElzUGFzc2l2' &&
      'ZT0idHJ1ZSI+PHNhbWw6SXNzdWVyIHhtbG5zOnNhbWw9InVybjpvYXNpcz' &&
      'puYW1lczp0YzpTQU1MOjIuMDphc3NlcnRpb24iPmdvb2dsZS5jb208L3Nh' &&
      'bWw6SXNzdWVyPjxzYW1scDpOYW1lSURQb2xpY3kgQWxsb3dDcmVhdGU9InRy' &&
      'dWUiIEZvcm1hdD0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOm5hbWVp' &&
      'ZC1mb3JtYXQ6dW5zcGVjaWZpZWQiIC8+PC9zYW1scDpBdXRoblJlcXVlc3Q+DQo='.

    lv_result = lcl_saml=>post_encode( lcl_saml=>post_decode( lv_encoded ) ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_result
        exp = lv_encoded ).

  ENDMETHOD.                    "post

  METHOD setup.

    mv_xml = '<?xml version="1.0" encoding="UTF-8"?>' &&
      '<samlp:AuthnRequest xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol" ' &&
      'xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion" ' &&
      'ID="aaf23196-1773-2113-474a-fe114412ab72" ' &&
      'Version="2.0" IssueInstant="2004-12-05T09:21:59Z" ' &&
      'AssertionConsumerServiceIndex="0" ' &&
      'AttributeConsumingServiceIndex="0">' &&
      '<saml:Issuer>https://sp.example.com/SAML2</saml:Issuer>' &&
      '<samlp:NameIDPolicy AllowCreate="true" ' &&
      'Format="urn:oasis:names:tc:SAML:2.0:nameid-format:transient"/>' &&
      '</samlp:AuthnRequest>'.

    mv_encoded = 'fZFfa8IwFMXfBb9DyXvaJtZ1BqsURRC2' &&
      'Mabbw95ivc5Am3TJrXPffmmLY3%2FA15Pzuyf33On8X' &&
      'JXBCaxTRmeEhTEJQBdmr%2FRbRp63K3pL5rPhYOpkVdY' &&
      'ib%2FCon%2BC9AYfDQRB4WDvRvWWksVoY6ZQTWlbgBB' &&
      'Zik9%2FfCR7GorYGTWFK8pu6DknnwKL%2FWEetlxmR8s' &&
      'BHbHJDWZqOKGdsRJM0kfQAjCUJ43KX8s78ctnIz%2Bl' &&
      'p5xpYa4dSo1fjOKGM03i8jSeCMzGevHa2%2FBK5MNo1F' &&
      'dgN2JMqPLmHc0b6WTmiVbsGoTf5qv66Zq2t60x0wXZ2' &&
      'RKydiCJXh3CWVV1CWJgqanfl0%2Bin8xutxYOvZL18NK' &&
      'UqPlvZR5el%2BVhYkAgZQdsA6fWVsZXE63W2itrTQ2cVaKV2CjSSqL1v9P%2FAXv4C'.

  ENDMETHOD.                    "setup

  METHOD check_date.

    DATA: lo_xslt  TYPE REF TO cl_xslt_processor,
          lv_value TYPE string.


    CREATE OBJECT lo_xslt TYPE cl_xslt_processor.
    lo_xslt->set_source_string( iv_xml ).

    lo_xslt->set_expression(
      expression = 'samlp:AuthnRequest/@IssueInstant'
      nsdeclarations = 'samlp urn:oasis:names:tc:SAML:2.0:protocol').

    lo_xslt->run( '' ).

    lv_value = lo_xslt->get_nodes( )->get_item( 0 )->get_value( ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_value
        exp = '2004-12-05T09:21:59Z' ).

  ENDMETHOD.                    "check_date

  METHOD redirect_decode.

    DATA: lv_decoded TYPE string.


    lv_decoded = lcl_saml=>redirect_decode( mv_encoded ).

    check_date( lv_decoded ).

  ENDMETHOD.                    "redirect_decode

  METHOD redirect_encode.

    DATA: lv_xml TYPE string.


    lv_xml = lcl_saml=>redirect_decode( lcl_saml=>redirect_encode( mv_xml ) ).

    check_date( lv_xml ).

  ENDMETHOD.                    "redirect_encode

ENDCLASS.                    "lcl_saml_test IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lv_response TYPE string,
        lv_url      TYPE string,
        li_client   TYPE REF TO if_http_client.


  lv_url = p_url. " convert type
  cl_http_client=>create_by_url(
    EXPORTING
      url    = lv_url
      ssl_id = 'ANONYM'
    IMPORTING
      client = li_client ).
  li_client->propertytype_accept_cookie = li_client->co_enabled.

  lcl_util=>send_and_receive( li_client ).

  lcl_util=>output_response_headers( li_client ).

  lv_response = li_client->response->get_cdata( ).
  lcl_util=>parse( iv_html = lv_response
                   ii_client = li_client ).

  li_client->request->set_header_field(
    name  = '~request_method'
    value = 'POST' ).
  li_client->request->set_header_field(
      name  = '~request_uri'
      value = '/saml2/idp/sso/accounts.sap.com' ).

  lcl_util=>output_request_headers( li_client ).
  lcl_util=>send_and_receive( li_client ).

  lcl_util=>output_response_headers( li_client ).
  lv_response = li_client->response->get_cdata( ).

  IF lv_response CP '*Sorry*'.
    WRITE: / 'Sorry, we could not authenticate you. Try again.'.
  ELSE.
    WRITE: / 'SOMETHING WORKED?!'.
  ENDIF.

  li_client->close( ).

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization.

  IF p_url IS INITIAL.
    p_url = 'https://s7hanaxs.hanatrial.ondemand.com/p13939179trial/foobar/0_NewPackage/index.html'.
  ENDIF.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-cprog
      variant              = 'DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02
      ##fm_subrc_ok.

ENDFORM.                    "initialization