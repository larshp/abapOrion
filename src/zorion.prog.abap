REPORT zorion.

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
      gv_edit     TYPE string,
      gt_nodes    TYPE TABLE OF zorion_tree_node.

CLASS lcl_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS
      node_double_click
      FOR EVENT node_double_click
                    OF cl_gui_simple_tree
        IMPORTING node_key.

    CLASS-METHODS expand_no_children
      FOR EVENT expand_no_children
                  OF cl_gui_simple_tree
      IMPORTING node_key.

    CLASS-METHODS node_context_menu_request
      FOR EVENT node_context_menu_request
                  OF cl_gui_simple_tree
      IMPORTING node_key menu.

    CLASS-METHODS node_context_menu_select
      FOR EVENT node_context_menu_select
                  OF cl_gui_simple_tree
      IMPORTING node_key fcode.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD node_context_menu_select.

    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    CASE fcode.
      WHEN 'META'.
        CLEAR gv_edit.
        DATA(lv_data) = go_file->file_metadata( ls_node-path && ls_node-text ).
        go_editor->set_visible( abap_true ).
        go_editor->set_textstream( lv_data ).
    ENDCASE.

  ENDMETHOD.

  METHOD node_context_menu_request.

    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    IF ls_node-isfolder = abap_false.
      menu->add_function(
        fcode = 'META'
        text  = 'Metadata'(001) ).
    ENDIF.

  ENDMETHOD.

  METHOD node_double_click.

    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    IF ls_node-isfolder = abap_true.
      go_editor->set_visible( abap_false ).
      CLEAR gv_edit.
      RETURN.
    ENDIF.

    gv_edit = ls_node-path && ls_node-text.
    DATA(lv_data) = go_file->file_get( gv_edit ).
    go_editor->set_visible( abap_true ).
    go_editor->set_textstream( lv_data ).

  ENDMETHOD.

  METHOD expand_no_children.

    DATA: lt_nodes TYPE TABLE OF zorion_tree_node.


    READ TABLE gt_nodes INTO DATA(ls_node) WITH KEY node_key = node_key.
    ASSERT sy-subrc = 0.

    DATA(lt_list) = go_file->dir_get( ls_node-path && ls_node-text && '/' ).

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      APPEND INITIAL LINE TO lt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>).

      <ls_node>-node_key  = gv_tree_key.
      gv_tree_key         = gv_tree_key + 1.
      <ls_node>-relatkey  = node_key.
      <ls_node>-relatship = cl_gui_simple_tree=>relat_last_child.
      <ls_node>-isfolder  = <ls_list>-dir.
      <ls_node>-text      = <ls_list>-name.
      <ls_node>-expander  = <ls_list>-dir.
      <ls_node>-path      = ls_node-path && ls_node-text && '/'.
    ENDLOOP.

    APPEND LINES OF lt_nodes TO gt_nodes.
    go_tree->add_nodes( table_structure_name = 'ZORION_TREE_NODE'
                        node_table           = lt_nodes ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS selection_output.
    CLASS-METHODS initialization.
    CLASS-METHODS run.
    CLASS-METHODS status_2000.
    CLASS-METHODS save.

  PRIVATE SECTION.
    CLASS-METHODS setup_gui.
    CLASS-METHODS setup_tree.
    CLASS-METHODS refresh_tree.
    CLASS-METHODS setup_api.
    CLASS-METHODS setup_editor.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD save.

    DATA: lv_text   TYPE string,
          lv_status TYPE i.


    go_editor->get_textmodified_status( IMPORTING status = lv_status ).
    IF lv_status = 0.
      MESSAGE s001(zorion).
      RETURN.
    ENDIF.

    go_editor->get_textstream( IMPORTING text = lv_text ).
    cl_gui_cfw=>flush( ).

    go_file->file_update(
        iv_path = gv_edit
        iv_data = lv_text ).

    MESSAGE s002(zorion).

  ENDMETHOD.

  METHOD status_2000.

    IF NOT gv_edit IS INITIAL.
      SET PF-STATUS 'STATUS_2000'.
    ELSE.
      SET PF-STATUS 'STATUS_2000' EXCLUDING 'SAVE'.
    ENDIF.
    SET TITLEBAR 'TITLE_2000' WITH 'abapOrion'.

  ENDMETHOD.

  METHOD setup_editor.

    DATA(lo_cc) = go_splitter->get_container( row    = 1
                                              column = 2 ).

    CREATE OBJECT go_editor EXPORTING parent = lo_cc.

    go_editor->set_visible( abap_false ).

  ENDMETHOD.

  METHOD refresh_tree.

    DATA: lt_nodes TYPE TABLE OF zorion_tree_node.


    DATA(lt_list) = go_file->dir_get( ).
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
      p_url = 'http://hanadb:8002' ##NO_TEXT.
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
        iv_url      = p_url
        iv_user     = p_user
        iv_password = p_passw.

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
      mode = go_splitter->mode_absolute ).
    go_splitter->set_column_width(
      id    = 1
      width = 400 ).

    setup_tree( ).
    setup_editor( ).
    refresh_tree( ).

    CALL SCREEN 2000.

  ENDMETHOD.

  METHOD setup_tree.

    DATA: lt_events TYPE cntl_simple_events.

    DATA(lo_cc) = go_splitter->get_container( row    = 1
                                              column = 1 ).

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

    APPEND INITIAL LINE TO lt_events ASSIGNING <ls_event>.
    <ls_event>-eventid = cl_gui_simple_tree=>eventid_node_context_menu_req.

    go_tree->set_registered_events( lt_events ).

    SET HANDLER lcl_handler=>node_double_click FOR go_tree.
    SET HANDLER lcl_handler=>expand_no_children FOR go_tree.
    SET HANDLER lcl_handler=>node_context_menu_request FOR go_tree.
    SET HANDLER lcl_handler=>node_context_menu_select FOR go_tree.

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
  lcl_app=>status_2000( ).
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
    WHEN 'SAVE'.
      CLEAR gv_ok_code.
      lcl_app=>save( ).
  ENDCASE.
ENDMODULE.