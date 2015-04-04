REPORT zorion_example_001.

PARAMETERS: p_url   TYPE text200 OBLIGATORY,
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

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

  DATA(lo_factory) = NEW zcl_orion_factory( iv_url      = p_url
                                            iv_user     = p_user
                                            iv_password = p_passw ).

  DATA(lo_file) = lo_factory->file( ).

  lo_file->file_create(
      iv_path = 'foobar/'
      iv_name = 'asdf.txt' ) ##NO_TEXT.

  lo_file->file_update( iv_path = 'foobar/asdf.txt'
                        iv_data = 'Hello' ) ##NO_TEXT.

  WRITE: / 'Done'(001).

ENDFORM.