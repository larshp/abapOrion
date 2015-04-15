class ZCL_ORION_INFO definition
  public
  inheriting from ZCL_ORION_API
  create private

  global friends ZCL_ORION_FACTORY .

public section.

  types:
    BEGIN OF ty_command,
      name TYPE string,
    END OF ty_command .
  types:
    ty_command_tt TYPE STANDARD TABLE OF ty_command WITH DEFAULT KEY .
  types:
    BEGIN OF ty_info,
      commands TYPE ty_command_tt,
    END OF ty_info .

  methods GET
    returning
      value(Rs_info) type TY_info .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORION_INFO IMPLEMENTATION.


METHOD get.

  DATA: lv_xpath   TYPE string,
        ls_command LIKE LINE OF rs_info-commands.


  set_uri( 'info' ) ##NO_TEXT.

  DATA(lv_json) = send_and_receive( ).
  DATA(lv_xml) = json_to_xml( lv_json ).

  lv_xpath = '/object/array[@name="Commands"]/str/text()'.
  DATA(li_nodes) = xpath( iv_xml   = lv_xml
                          iv_xpath = lv_xpath ).

  DATA(li_iterator) = li_nodes->create_iterator( ).

  DATA(li_node) = li_iterator->get_next( ).
  WHILE li_node IS NOT INITIAL.

    DATA(lv_value) = li_node->get_value( ).

    ls_command-name = lv_value.
    APPEND ls_command TO rs_info-commands.
    CLEAR ls_command.

    li_node ?= li_iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.
ENDCLASS.