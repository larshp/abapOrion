CLASS zcl_orion_workspace DEFINITION
  PUBLIC
  INHERITING FROM zcl_orion_api
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_orion_factory .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_list,
        name     TYPE string,
        location TYPE string,
      END OF ty_list .
    TYPES:
      ty_list_tt TYPE STANDARD TABLE OF ty_list WITH DEFAULT KEY .

    METHODS create
      IMPORTING
        !iv_name TYPE string .
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE ty_list_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ORION_WORKSPACE IMPLEMENTATION.


  METHOD create.

    DATA(lv_token) = token( ).

    set_uri( 'workspace' ) ##NO_TEXT.

    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    mi_client->request->set_header_field(
        name  = 'X-CSRF-Token'
        value = lv_token ) ##NO_TEXT.

    mi_client->request->set_header_field(
        name  = 'Slug'
        value = iv_name ) ##NO_TEXT.

    send_and_receive( ).

  ENDMETHOD.


  METHOD list.

    DATA: lv_xpath TYPE string,
          ls_list  LIKE LINE OF rt_list.


    set_uri( 'workspace' ) ##NO_TEXT.

    DATA(lv_json) = send_and_receive( ).
    DATA(lv_xml) = json_to_xml( lv_json ).

    lv_xpath = '/object/array[@name="Workspaces"]/object/str[@name="Name"]/text()' &&
      '|/object/array[@name="Workspaces"]/object/str[@name="Location"]/text()'.
    DATA(li_nodes) = xpath( iv_xml   = lv_xml
                            iv_xpath = lv_xpath ).

    DATA(li_iterator) = li_nodes->create_iterator( ).

    DATA(li_node) = li_iterator->get_next( ).
    WHILE li_node IS NOT INITIAL.

      DATA(lv_value) = li_node->get_value( ).

      IF ls_list-name IS INITIAL.
        ls_list-name = lv_value.
      ELSE.
        ls_list-location = lv_value.
        APPEND ls_list TO rt_list.
        CLEAR ls_list.
      ENDIF.

      li_node ?= li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.