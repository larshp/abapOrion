REPORT zorion_examples.

PARAMETERS: p_url   TYPE text200 OBLIGATORY DEFAULT 'http://hanadb:8002',
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

PARAMETERS: p_create TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_listw  TYPE c RADIOBUTTON GROUP g1,
            p_cwork  TYPE c RADIOBUTTON GROUP g1,
            p_info   TYPE c RADIOBUTTON GROUP g1.

DATA: go_file      TYPE REF TO zcl_orion_file,
      go_workspace TYPE REF TO zcl_orion_workspace,
      go_info      TYPE REF TO zcl_orion_info.

START-OF-SELECTION.
  PERFORM run.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_PASSW'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

FORM run.

  PERFORM setup.

  IF p_create = abap_true.
    PERFORM create_file.
  ELSEIF p_listw = abap_true.
    PERFORM list_workspaces.
  ELSEIF p_cwork = abap_true.
    PERFORM create_workspace.
  ELSEIF p_info = abap_true.
    PERFORM info.
  ELSE.
    ASSERT 1 = 1 + 1.
  ENDIF.

  WRITE: / 'Done'(001).

ENDFORM.

FORM setup.

  DATA(lo_factory) = NEW zcl_orion_factory( iv_url      = p_url
                                            iv_user     = p_user
                                            iv_password = p_passw ).
  go_file = lo_factory->file( ).
  go_workspace = lo_factory->workspace( ).
  go_info = lo_factory->info( ).

ENDFORM.

FORM create_file.

  go_file->file_create(
      iv_path = 'foobar/'
      iv_name = 'asdf.txt' ) ##NO_TEXT.

  go_file->file_update( iv_path = 'foobar/asdf.txt'
                        iv_data = 'Hello' ) ##NO_TEXT.

ENDFORM.

FORM list_workspaces.

  DATA(lt_list) = go_workspace->list( ).

  LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
    WRITE: / <ls_list>-name.
    WRITE: / <ls_list>-location.
  ENDLOOP.

ENDFORM.

FORM create_workspace.

  go_workspace->create( 'foobar' ).

ENDFORM.

FORM info.

  DATA(ls_info) = go_info->get( ).

  LOOP AT ls_info-commands ASSIGNING FIELD-SYMBOL(<ls_command>).
    WRITE: / <ls_command>-name.
  ENDLOOP.

ENDFORM.
