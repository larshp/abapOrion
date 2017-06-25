REPORT zorion_hcp_test.

* https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/file/p13939179trial/
* https://s7hanaxs.hanatrial.ondemand.com/p13939179trial/foobar/0_NewPackage/index.html

PARAMETERS: p_url   TYPE text200 OBLIGATORY,
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_output.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS lcl_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS download
      IMPORTING VALUE(iv_data) TYPE string.

    CLASS-METHODS escape_data
      IMPORTING iv_data        TYPE string
      RETURNING VALUE(rv_data) TYPE string.

    CLASS-METHODS parse
      IMPORTING VALUE(iv_html) TYPE string
      RETURNING VALUE(rv_data) TYPE string.

    CLASS-METHODS output_string
      IMPORTING iv_str TYPE string.

    CLASS-METHODS output_response_headers
      IMPORTING ii_client TYPE REF TO if_http_client.

    CLASS-METHODS output_request_headers
      IMPORTING ii_client TYPE REF TO if_http_client.

    CLASS-METHODS output_request_cookies
      IMPORTING ii_client TYPE REF TO if_http_client.

    CLASS-METHODS output_response_cookies
      IMPORTING ii_client TYPE REF TO if_http_client.

    CLASS-METHODS send_and_receive
      IMPORTING ii_client TYPE REF TO if_http_client.

    CLASS-METHODS transfer_cookies
      IMPORTING ii_client TYPE REF TO if_http_client.

ENDCLASS.                    "lcl_util DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.

  METHOD transfer_cookies.

    DATA: lt_cookies TYPE tihttpcki.

    FIELD-SYMBOLS: <ls_cookie> LIKE LINE OF lt_cookies.


    ii_client->response->get_cookies( CHANGING cookies = lt_cookies ).

    LOOP AT lt_cookies ASSIGNING <ls_cookie>.
      ii_client->request->set_cookie(
        name    = <ls_cookie>-name
        path    = <ls_cookie>-path
        value   = <ls_cookie>-value
        domain  = <ls_cookie>-xdomain
        expires = <ls_cookie>-expires
        secure  = <ls_cookie>-secure ).
    ENDLOOP.

  ENDMETHOD.                    "transfer_cookies

  METHOD output_request_cookies.

    DATA: lt_cookies TYPE tihttpcki.

    FIELD-SYMBOLS: <ls_cookie> LIKE LINE OF lt_cookies.


    WRITE: / 'REQUEST COOKIES'.
    ii_client->request->get_cookies( CHANGING cookies = lt_cookies ).
    LOOP AT lt_cookies ASSIGNING <ls_cookie>.
      WRITE: / <ls_cookie>-name, <ls_cookie>-value.
    ENDLOOP.
    WRITE: /.

  ENDMETHOD.                    "output_request_cookies

  METHOD output_response_cookies.

    DATA: lt_cookies TYPE tihttpcki.

    FIELD-SYMBOLS: <ls_cookie> LIKE LINE OF lt_cookies.


    WRITE: / 'RESPONSE COOKIES'.
    ii_client->response->get_cookies( CHANGING cookies = lt_cookies ).
    LOOP AT lt_cookies ASSIGNING <ls_cookie>.
      WRITE: / <ls_cookie>-name, <ls_cookie>-value.
    ENDLOOP.
    WRITE: /.

  ENDMETHOD.                    "output_response_cookies

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
      ASSERT 0 = 1.
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

    DATA(lv_str) = iv_str.

    WHILE strlen( lv_str ) > 150.
      WRITE: / lv_str(150).
      lv_str = lv_str+150.
    ENDWHILE.
    WRITE: / lv_str.

  ENDMETHOD.                    "output_string

  METHOD parse.

    DATA: lv_sub1    TYPE string,
          lv_run     TYPE abap_bool,
          lv_name    TYPE string,
          lv_value   TYPE string,
          lv_regex   TYPE string,
          lt_fields  TYPE tihttpnvp,
          lv_string  TYPE string,
          lt_strings TYPE TABLE OF string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    FIND FIRST OCCURRENCE OF REGEX '<form ([^>]*)>'
      IN iv_html IGNORING CASE
      SUBMATCHES lv_sub1.
*    WRITE: / 'form', lv_sub1.
*    WRITE: /.

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
          SUBMATCHES lv_name ##NO_TEXT.
        FIND FIRST OCCURRENCE OF REGEX 'value="([^"]*)"'
          IN lv_sub1 IGNORING CASE
          SUBMATCHES lv_value ##NO_TEXT.
*        WRITE: / 'input', lv_name, lv_value.

        CASE lv_name.
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

* todo, this also seems to work,
*    ii_client->request->set_form_fields( lt_fields ).

    LOOP AT lt_fields ASSIGNING <ls_field>.
*      DATA: lv_escaped TYPE string.
*      lv_escaped = escape( <ls_field>-value ).
*      <ls_field>-value = escape( <ls_field>-value ).
      <ls_field>-value =
        cl_http_utility=>if_http_utility~escape_url( unescaped = <ls_field>-value ).
*      IF strlen( lv_escaped ) <> strlen( <ls_field>-value ).
*        BREAK-POINT.
*      ENDIF.
      CONCATENATE <ls_field>-name '=' <ls_field>-value INTO lv_string.
      APPEND lv_string TO lt_strings.
    ENDLOOP.
    CONCATENATE LINES OF lt_strings INTO rv_data SEPARATED BY '&'.

  ENDMETHOD.                    "parse

  METHOD escape_data.

    rv_data = iv_data.

    REPLACE ALL OCCURRENCES OF ':' IN rv_data WITH '%3A'.
    REPLACE ALL OCCURRENCES OF '/' IN rv_data WITH '%2F'.
    REPLACE ALL OCCURRENCES OF '+' IN rv_data WITH '%2B'.
    REPLACE ALL OCCURRENCES OF '=' IN rv_data WITH '%3D'.
    REPLACE ALL OCCURRENCES OF '@' IN rv_data WITH '%40'.

  ENDMETHOD.                    "escape

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
      ASSERT 0 = 1.
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
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      ASSERT 0 = 1.
    ENDIF.

  ENDMETHOD.                    "download

ENDCLASS.                    "lcl_util IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lv_response TYPE string,
        lv_url      TYPE string,
        lv_str      TYPE string,
        lv_data     TYPE string,
        li_client   TYPE REF TO if_http_client.


  lv_url = p_url. " convert type
  cl_http_client=>create_by_url(
    EXPORTING
      url    = lv_url
      ssl_id = 'ANONYM'
    IMPORTING
      client = li_client ).
  li_client->propertytype_accept_cookie = li_client->co_enabled.
*  li_client->propertytype_redirect = li_client->co_disabled.
  lcl_util=>transfer_cookies( li_client ).
  lcl_util=>send_and_receive( li_client ).



  lcl_util=>output_response_headers( li_client ).

  lv_response = li_client->response->get_cdata( ).
  lv_data = lcl_util=>parse( lv_response ).

  li_client->request->set_method( if_http_request=>co_request_method_post ).
*  li_client->request->set_header_field(
*    name  = '~request_method'
*    value = 'POST' ).
*  li_client->request->set_header_field(
*      name  = '~request_uri'
*      value = '/saml2/idp/sso/accounts.sap.com' ).

  li_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/x-www-form-urlencoded' ) ##NO_TEXT.
  lv_str = strlen( lv_data ).
  li_client->request->set_header_field(
      name  = 'content-length'
      value = lv_str ) ##NO_TEXT.
  li_client->request->set_cdata( lv_data ).

  lcl_util=>output_string( lv_data ).
  WRITE: /.
  lcl_util=>transfer_cookies( li_client ).

  lcl_util=>output_request_headers( li_client ).
  lcl_util=>output_request_cookies( li_client ).
  lcl_util=>send_and_receive( li_client ).

  lcl_util=>output_response_headers( li_client ).
  lcl_util=>output_response_cookies( li_client ).
  lv_response = li_client->response->get_cdata( ).

  IF lv_response CP '*Sorry*'.
    WRITE: / 'Sorry, we could not authenticate you. Try again.'.
    RETURN.
  ELSE.
* expected result: the xhtml form
    WRITE: / 'SOMETHING WORKED?!'.
    lcl_util=>output_string( lv_response ).
  ENDIF.

*  li_client->request->set_header_field(
*    name  = '~request_method'
*    value = 'POST' ).
*lv_url = p_url.
*  li_client->request->set_header_field(
*      name  = '~request_uri'
*      value = lv_url ).
*
*  lcl_util=>send_and_receive( li_client ).
*  lcl_util=>output_string( lv_response ).

  li_client->close( ).

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization.

  IF p_url IS INITIAL.
    p_url = 'https://s7hanaxs.hanatrial.ondemand.com/' &&
              'p13939179trial/foobar/0_NewPackage/index.html' ##NO_TEXT.
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

*&---------------------------------------------------------------------*
*&      Form  selection_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection_output.

  LOOP AT SCREEN.
    IF screen-name = 'P_PASSW'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "selection_output
