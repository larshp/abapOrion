REPORT zorion.

*CONSTANTS:
*  c_url1 TYPE string
*    VALUE 'https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/info',
*  c_url2 TYPE string
*    VALUE 'https://accounts.sap.com/saml2/idp/sso/accounts.sap.com',
*  c_url3 TYPE string
*    VALUE 'https://account.hanatrial.ondemand.com/',
*  c_url4 TYPE string
*    VALUE 'https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/file/p13939179trial/foobar',
*  c_url5 TYPE string
*    VALUE 'https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/file/p13939179trial/foobar/0_NewPackage/0_NewFile.js'.

*https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/file/p13939179trial/foobar
CONSTANTS: c_url TYPE string VALUE 'https://s7hanaxs.hanatrial.ondemand.com/p13939179trial/foobar/0_NewPackage/index.html'.

* TODO:
* JTENANTSESSIONID  ??
* SAML stuff
* package = SAML2_CORE

* http://www.ssocircle.com/en/1203/saml-request-online-decoder-encoder/

START-OF-SELECTION.
  PERFORM run.

CLASS lcl_util DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS download
      IMPORTING VALUE(iv_data) TYPE string.
    CLASS-METHODS parse
      IMPORTING VALUE(iv_xml) TYPE string.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD parse.

    DATA: lt_results TYPE match_result_tab.


    FIND REGEX '<input type="(\C*)" name="(\C*)" value="(\C*)">'
      IN iv_xml IGNORING CASE
      RESULTS lt_results.

    BREAK-POINT.

*'<input id="(\C*)" type="(\C*)" name="(\C*)" value="(\C*)">'

  ENDMETHOD.

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

  ENDMETHOD.

ENDCLASS.

CLASS lcl_saml DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS redirect_encode
      IMPORTING iv_xml            TYPE string
      RETURNING VALUE(rv_encoded) TYPE string.

    CLASS-METHODS redirect_decode
      IMPORTING iv_encoded    TYPE string
      RETURNING VALUE(rv_xml) TYPE string.

    CLASS-METHODS post_encode
      IMPORTING iv_xml            TYPE string
      RETURNING VALUE(rv_encoded) TYPE string.

    CLASS-METHODS post_decode
      IMPORTING iv_encoded    TYPE string
      RETURNING VALUE(rv_xml) TYPE string.

  PRIVATE SECTION.

    CLASS-METHODS:
      base64_decode
        IMPORTING iv_string         TYPE string
        RETURNING VALUE(rv_xstring) TYPE xstring,
      base64_encode
        IMPORTING iv_xstring       TYPE xstring
        RETURNING VALUE(rv_string) TYPE string,
      deflate
        IMPORTING iv_data            TYPE string
        RETURNING VALUE(rv_deflated) TYPE xstring,
      enflate
        IMPORTING iv_data            TYPE xstring
        RETURNING VALUE(rv_enflated) TYPE string,
      url_encode
        IMPORTING iv_data           TYPE string
        RETURNING VALUE(rv_encoded) TYPE string,
      url_decode
        IMPORTING iv_data           TYPE string
        RETURNING VALUE(rv_decoded) TYPE string,
      string_to_xstring_utf8
        IMPORTING iv_string         TYPE string
        RETURNING VALUE(rv_xstring) TYPE xstring,
      xstring_to_string_utf8
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.

CLASS lcl_saml IMPLEMENTATION.

  METHOD post_encode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = string_to_xstring_utf8( iv_xml ).
    rv_encoded = base64_encode( lv_xstr ).

  ENDMETHOD.

  METHOD post_decode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = base64_decode( iv_encoded ).
    rv_xml = xstring_to_string_utf8( lv_xstr ).

  ENDMETHOD.

  METHOD redirect_encode.

    DATA: lv_str  TYPE string,
          lv_xstr TYPE xstring.


    lv_xstr = deflate( iv_xml ).
    lv_str = base64_encode( lv_xstr ).
    rv_encoded = url_encode( lv_str ).

  ENDMETHOD.

  METHOD redirect_decode.

    DATA: lv_str  TYPE string,
          lv_xstr TYPE xstring.


    lv_str = url_decode( iv_encoded ).
    lv_xstr = base64_decode( lv_str ).
    rv_xml = enflate( lv_xstr ).

  ENDMETHOD.

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

  ENDMETHOD.

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

  ENDMETHOD.

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

  ENDMETHOD.

  METHOD deflate.

    DATA: lv_raw TYPE xstring.


    lv_raw = string_to_xstring_utf8( iv_data ).

    cl_abap_gzip=>compress_binary(
      EXPORTING
        raw_in   = lv_raw
      IMPORTING
        gzip_out = rv_deflated ).

  ENDMETHOD.

  METHOD enflate.

    DATA: lv_raw TYPE xstring.


    cl_abap_gzip=>decompress_binary(
      EXPORTING
        gzip_in = iv_data
      IMPORTING
        raw_out = lv_raw ).

    rv_enflated = xstring_to_string_utf8( lv_raw ).

  ENDMETHOD.

  METHOD url_encode.

    rv_encoded = cl_http_utility=>if_http_utility~escape_url( iv_data ).

  ENDMETHOD.

  METHOD url_decode.

    rv_decoded = cl_http_utility=>if_http_utility~unescape_url( iv_data ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_saml_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mv_xml     TYPE string,
          mv_encoded TYPE string.

    METHODS setup.
    METHODS check_date IMPORTING iv_xml TYPE string.
    METHODS redirect_encode FOR TESTING.
    METHODS redirect_decode FOR TESTING.
    METHODS post FOR TESTING.

ENDCLASS.

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

  ENDMETHOD.

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

  ENDMETHOD.

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

  ENDMETHOD.

  METHOD redirect_decode.

    DATA: lv_decoded TYPE string.


    lv_decoded = lcl_saml=>redirect_decode( mv_encoded ).

    check_date( lv_decoded ).

  ENDMETHOD.

  METHOD redirect_encode.

    DATA: lv_xml TYPE string.


    lv_xml = lcl_saml=>redirect_decode( lcl_saml=>redirect_encode( mv_xml ) ).

    check_date( lv_xml ).

  ENDMETHOD.

ENDCLASS.


FORM run.

  DATA: lv_encoded  TYPE string,
        lv_response TYPE string,
        li_client   TYPE REF TO if_http_client,
        lt_fields   TYPE tihttpnvp,
        lv_code     TYPE i.

  FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

  cl_http_client=>create_by_url(
    EXPORTING
      url    = c_url
      ssl_id = 'ANONYM'
    IMPORTING
      client = li_client ).

*  li_client->request->set_header_field( name  = '~request_method'
*                                        value = 'POST' ).
*
*  APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
*  <ls_field>-name = 'SAMLRequest'.
*  <ls_field>-value = lv_encoded.
*
*  li_client->request->set_form_fields( lt_fields ).

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
* make sure SSL certificates is installed in STRUST
    ASSERT 1 = 1 + 1.
  ENDIF.

  li_client->response->get_status(
    IMPORTING
      code   = lv_code ).

  WRITE: / lv_code.

  lv_response = li_client->response->get_cdata( ).

  lcl_util=>parse( lv_response ).

  WHILE strlen( lv_response ) > 100.
    WRITE: / lv_response(100).
    lv_response = lv_response+100.
  ENDWHILE.
  WRITE: / lv_response.

ENDFORM.