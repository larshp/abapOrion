REPORT zorion.

CONSTANTS: c_url TYPE string VALUE 'https://s7hanaxs.hanatrial.ondemand.com/sap/hana/xs/dt/base/info'.

* JTENANTSESSIONID  ??

* SAML stuff

* POST
* j_username
* j_password
* to
* https://account.hanatrial.ondemand.com/cockpit

START-OF-SELECTION.
  PERFORM run.

CLASS lcl_saml DEFINITION.

  PUBLIC SECTION.
    METHODS encode.

ENDCLASS.

CLASS lcl_saml IMPLEMENTATION.

  METHOD encode.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_saml_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS encode FOR TESTING.

ENDCLASS.

CLASS lcl_saml_test IMPLEMENTATION.

  METHOD encode.

  ENDMETHOD.

ENDCLASS.


FORM run.

  DATA: li_client TYPE REF TO if_http_client,
        lv_code   TYPE i.


  cl_http_client=>create_by_url(
    EXPORTING
      url    = c_url
      ssl_id = 'ANONYM'
    IMPORTING
      client = li_client ).

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
* make sure SSL certificates is installed in STRUST
    BREAK-POINT.
  ENDIF.

  li_client->response->get_status(
    IMPORTING
      code   = lv_code ).

  WRITE: / lv_code.

ENDFORM.