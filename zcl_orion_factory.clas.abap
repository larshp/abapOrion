class ZCL_ORION_FACTORY definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_URL type STRING
      !IV_USER type STRING
      !IV_PASSWORD type STRING
      !IV_TEST type ABAP_BOOL default ABAP_FALSE .
  methods FILE
    returning
      value(RO_API) type ref to ZCL_ORION_FILE .
  methods WORKSPACE .
  methods TRANSFER .
  methods METADATA .
  methods INFO .
  methods CHANGE_TRACKING .
protected section.
private section.

  data MV_URL type STRING .
  data MV_USER type STRING .
  data MV_PASSWORD type STRING .
  data MI_CLIENT type ref to IF_HTTP_CLIENT .

  methods OPEN_CONNECTION .
ENDCLASS.



CLASS ZCL_ORION_FACTORY IMPLEMENTATION.


METHOD change_tracking.

* todo

ENDMETHOD.


METHOD constructor.

  mv_url      = iv_url.
  mv_user     = iv_user.
  mv_password = iv_password.

  open_connection( ).

  IF iv_test = abap_true.
    DATA(lo_file) = file( ).
    lo_file->dir_list( ).
  ENDIF.

ENDMETHOD.


METHOD file.

  CREATE OBJECT ro_api
    EXPORTING
      ii_client = mi_client.

ENDMETHOD.


METHOD info.

* todo

ENDMETHOD.


METHOD metadata.

* todo

ENDMETHOD.


METHOD open_connection.

  cl_http_client=>create_by_url(
    EXPORTING
     url    =  mv_url
     ssl_id = 'ANONYM'
   IMPORTING
     client = mi_client ).

  mi_client->request->set_authorization(
    username = mv_user
    password = mv_password ).

ENDMETHOD.


METHOD transfer.

* todo

ENDMETHOD.


METHOD workspace.

* todo

ENDMETHOD.
ENDCLASS.