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

* todo

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