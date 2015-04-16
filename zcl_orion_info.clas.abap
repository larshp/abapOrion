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
    BEGIN OF ty_value,
      name TYPE string,
      value type string,
    END OF ty_value.
  types:
    ty_values_tt TYPE STANDARD TABLE OF ty_value WITH DEFAULT KEY .

  types:
    BEGIN OF ty_info,
      info type ty_values_tt,
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
        ls_command LIKE LINE OF rs_info-commands,
        ls_value   LIKE LINE OF rs_info-info.


  set_uri( 'info' ) ##NO_TEXT.

  DATA(lv_json) = send_and_receive( ).
  DATA(lv_xml) = json_to_xml( lv_json ).

  lv_xpath = '/object/array[@name="Commands"]/str/text()'.
  DATA(li_iterator) = xpath( iv_xml   = lv_xml
                             iv_xpath = lv_xpath )->create_iterator( ).
  DATA(li_node) = li_iterator->get_next( ).
  WHILE li_node IS NOT INITIAL.
    ls_command-name = li_node->get_value( ).
    APPEND ls_command TO rs_info-commands.
    li_node ?= li_iterator->get_next( ).
  ENDWHILE.


  lv_xpath = '/object/object[@name="DeliveryUnit"]/*'.
  li_iterator = xpath( iv_xml   = lv_xml
                       iv_xpath = lv_xpath )->create_iterator( ).
  li_node = li_iterator->get_next( ).
  WHILE li_node IS NOT INITIAL.
    ls_value-name = li_node->get_attributes( )->get_named_item( 'name' )->get_value( ).
    ls_value-value = li_node->get_value( ).
    APPEND ls_value TO rs_info-info.

    li_node ?= li_iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.
ENDCLASS.