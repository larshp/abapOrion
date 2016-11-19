CLASS zcl_orion_factory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_url      TYPE clike
        !iv_user     TYPE clike
        !iv_password TYPE clike
        !iv_test     TYPE abap_bool DEFAULT abap_false .
    METHODS file
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_file .
    METHODS workspace
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_workspace .
    METHODS transfer
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_workspace .
    METHODS metadata
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_workspace .
    METHODS info
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_info .
    METHODS change_tracking
      RETURNING
        VALUE(ro_api) TYPE REF TO zcl_orion_workspace .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_url TYPE string .
    DATA mv_user TYPE string .
    DATA mv_password TYPE string .
    DATA mi_client TYPE REF TO if_http_client .

    METHODS open_connection .
ENDCLASS.



CLASS ZCL_ORION_FACTORY IMPLEMENTATION.


  METHOD change_tracking.

* todo

  ENDMETHOD.


  METHOD constructor.

* See https://github.com/larshp/abapOrion

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2015 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

    mv_url      = iv_url.
    mv_user     = iv_user.
    mv_password = iv_password.

    open_connection( ).

    IF iv_test = abap_true.
      DATA(lo_file) = file( ).
      lo_file->dir_get( ).
    ENDIF.

  ENDMETHOD.


  METHOD file.

    CREATE OBJECT ro_api
      EXPORTING
        ii_client = mi_client.

  ENDMETHOD.


  METHOD info.

    CREATE OBJECT ro_api
      EXPORTING
        ii_client = mi_client.

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

    mi_client->propertytype_accept_cookie = mi_client->co_enabled.

  ENDMETHOD.


  METHOD transfer.

* todo

  ENDMETHOD.


  METHOD workspace.

    CREATE OBJECT ro_api
      EXPORTING
        ii_client = mi_client.

  ENDMETHOD.
ENDCLASS.