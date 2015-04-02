REPORT zorion.

PARAMETERS: p_url   TYPE text200 OBLIGATORY,
            p_user  TYPE text20 OBLIGATORY,
            p_passw TYPE text20 OBLIGATORY.

AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_output.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM run.

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lo_factory TYPE REF TO zcl_orion_factory.


  CREATE OBJECT lo_factory
    EXPORTING
      iv_url      = CONV #( p_url )
      iv_user     = CONV #( p_user )
      iv_password = CONV #( p_passw ).

  DATA(lo_file) = lo_factory->file( ).

  DATA(lt_list) = lo_file->dir_list( ).
  BREAK-POINT.

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization.

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
      ##fm_subrc_ok.

ENDFORM.                    "initialization

*&---------------------------------------------------------------------*
*&      Form  selection_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection_output.

  LOOP AT SCREEN.
    IF screen-name = 'P_PASSW'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "selection_output