class ZCL_ORION_FILE definition
  public
  inheriting from ZCL_ORION_API
  create private

  global friends ZCL_ORION_FACTORY .

public section.

  types:
    BEGIN OF ty_dir_list,
      name TYPE string,
      dir  TYPE abap_bool,
    END OF ty_dir_list .
  types:
    ty_dir_list_tt TYPE STANDARD TABLE OF ty_dir_list WITH DEFAULT KEY .

  methods DIR_LIST
    importing
      !IV_PATH type STRING default ''
    returning
      value(RT_LIST) type TY_DIR_LIST_TT .
  methods DIR_METADATA .
  methods FILE_CONTENTS
    importing
      !IV_PATH type STRING
    returning
      value(RV_DATA) type STRING .
  methods FILE_METADATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORION_FILE IMPLEMENTATION.


METHOD dir_list.

  DATA: lv_xpath TYPE string,
        ls_list  LIKE LINE OF rt_list.


  set_uri( 'file/' && iv_path && '?depth=1' ).

  DATA(lv_json) = send_and_receive( ).
  DATA(lv_xml) = json_to_xml( lv_json ).

  lv_xpath = '/object/array[@name="Children"]/object/str[@name="Name"]/text()' &&
    '|/object/array[@name="Children"]/object/bool[@name="Directory"]/text()'.
  DATA(li_nodes) = xpath( iv_xml   = lv_xml
                          iv_xpath = lv_xpath ).

  DATA(li_iterator) = li_nodes->create_iterator( ).

  DATA(li_node) = li_iterator->get_next( ).
  WHILE li_node IS NOT INITIAL.

    DATA(lv_value) = li_node->get_value( ).

    IF ls_list-name IS INITIAL.
      ls_list-name = lv_value.
    ELSE.
      IF lv_value = 'true'.
        ls_list-dir = abap_true.
      ENDIF.
      APPEND ls_list TO rt_list.
      CLEAR ls_list.
    ENDIF.

    li_node ?= li_iterator->get_next( ).
  ENDWHILE.

  SORT rt_list BY name ASCENDING AS TEXT.

ENDMETHOD.


METHOD dir_metadata.


ENDMETHOD.


METHOD file_contents.

  set_uri( 'file/' && iv_path ).

  DATA(lv_data) = send_and_receive( ).

* todo, change returning parametr to xstring?

ENDMETHOD.


METHOD file_metadata.


ENDMETHOD.
ENDCLASS.