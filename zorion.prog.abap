REPORT zorion.

PARAMETERS: p_url   TYPE text200 OBLIGATORY,
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

DATA: go_splitter TYPE REF TO cl_gui_splitter_container,
      go_custom   TYPE REF TO cl_gui_custom_container,
      go_tree     TYPE REF TO cl_gui_simple_tree,
      go_editor   TYPE REF TO cl_gui_textedit,
      gv_tree_key TYPE tv_nodekey,
      go_factory  TYPE REF TO zcl_orion_factory,
      go_file     TYPE REF TO zcl_orion_file,
      gv_ok_code  LIKE sy-ucomm,
      gt_nodes    TYPE TABLE OF zorion_tree_node.

CLASS lcl_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      node_double_click
      FOR EVENT node_double_click
                    OF cl_gui_simple_tree
        IMPORTING node_key,
      expand_no_children
      FOR EVENT expand_no_children
                    OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD node_double_click.

    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    IF ls_node-isfolder = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_data) = go_file->file_contents( ls_node-path && ls_node-text ).

* todo, do stuff with go_editor
    BREAK-POINT.

  ENDMETHOD.

  METHOD expand_no_children.

    DATA: lt_nodes TYPE TABLE OF zorion_tree_node.


    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    DATA(lt_list) = go_file->dir_list( ls_node-path && ls_node-text && '/' ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      APPEND INITIAL LINE TO lt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>).

      <ls_node>-node_key = gv_tree_key.
      gv_tree_key        = gv_tree_key + 1.
      <ls_node>-relatkey = node_key.
      <ls_node>-relatship = cl_gui_simple_tree=>relat_last_child.
      <ls_node>-isfolder = <ls_list>-dir.
      <ls_node>-text     = <ls_list>-name.
      <ls_node>-expander = <ls_list>-dir.
      <ls_node>-path     = ls_node-path && ls_node-text && '/'.
    ENDLOOP.

    APPEND LINES OF lt_nodes TO gt_nodes.
    go_tree->add_nodes( table_structure_name = 'ZORION_TREE_NODE'
                        node_table           = lt_nodes ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS selection_output.
    CLASS-METHODS initialization.
    CLASS-METHODS run.

  PRIVATE SECTION.
    CLASS-METHODS setup_gui.
    CLASS-METHODS setup_tree.
    CLASS-METHODS refresh_tree.
    CLASS-METHODS setup_api.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD refresh_tree.

    DATA: lt_nodes TYPE TABLE OF zorion_tree_node.


    DATA(lt_list) = go_file->dir_list( ).
    gv_tree_key = 1.

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      APPEND INITIAL LINE TO lt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>).

      <ls_node>-node_key = gv_tree_key.
      gv_tree_key        = gv_tree_key + 1.
      <ls_node>-isfolder = <ls_list>-dir.
      <ls_node>-text     = <ls_list>-name.
      <ls_node>-expander = <ls_list>-dir.
    ENDLOOP.

    APPEND LINES OF lt_nodes TO gt_nodes.
    go_tree->add_nodes( table_structure_name = 'ZORION_TREE_NODE'
                        node_table           = lt_nodes ).

  ENDMETHOD.

  METHOD selection_output.

    LOOP AT SCREEN.
      IF screen-name = 'P_PASSW'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD initialization.

    IF p_url IS INITIAL.
      p_url = 'http://hanadb:8002'.
    ENDIF.
    IF p_user IS INITIAL.
      p_user = 'SYSTEM'.
    ENDIF.

    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-cprog
        variant              = 'DEFAULT'
      EXCEPTIONS
        variant_not_existent = 01
        variant_obsolete     = 02
        ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.

  METHOD setup_api.

    CREATE OBJECT go_factory
      EXPORTING
        iv_url      = CONV #( p_url )
        iv_user     = CONV #( p_user )
        iv_password = CONV #( p_passw ).

    go_file = go_factory->file( ).

  ENDMETHOD.

  METHOD run.

    setup_api( ).
    setup_gui( ).

  ENDMETHOD.

  METHOD setup_gui.

    CREATE OBJECT go_custom
      EXPORTING
        container_name = 'CUSTOM_2000'.

    CREATE OBJECT go_splitter
      EXPORTING
        parent  = go_custom
        rows    = 1
        columns = 2.
    go_splitter->set_column_mode(
      EXPORTING
        mode = go_splitter->mode_absolute ).
    go_splitter->set_column_width(
      EXPORTING
          id    = 1
          width = 400 ).

    setup_tree( ).
    refresh_tree( ).

    CALL SCREEN 2000.

  ENDMETHOD.

  METHOD setup_tree.

    DATA: lt_events TYPE cntl_simple_events.

    DATA(lo_cc) = go_splitter->get_container( row       = 1
                                              column    = 1 ).

    CREATE OBJECT go_tree
      EXPORTING
        parent              = lo_cc
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.

    APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<ls_event>).
    <ls_event>-eventid = cl_gui_simple_tree=>eventid_node_double_click.
    <ls_event>-appl_event = abap_true.

    APPEND INITIAL LINE TO lt_events ASSIGNING <ls_event>.
    <ls_event>-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
    <ls_event>-appl_event = abap_true.

    go_tree->set_registered_events( lt_events ).

    SET HANDLER lcl_handler=>node_double_click FOR go_tree.
    SET HANDLER lcl_handler=>expand_no_children FOR go_tree.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN OUTPUT.
  lcl_app=>selection_output( ).

INITIALIZATION.
  lcl_app=>initialization( ).

START-OF-SELECTION.
  lcl_app=>run( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STATUS_2000'.
  SET TITLEBAR 'TITLE_2000' WITH 'abapOrion'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  CASE gv_ok_code.
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.