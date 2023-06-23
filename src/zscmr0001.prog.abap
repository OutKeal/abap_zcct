*&---------------------------------------------------------------------*
*& Report ZSCMR0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscmr0001.



INCLUDE zscmr0001_top.

INCLUDE zscmr0001_f01.

INCLUDE zmdaf01.
*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zscmr0001_scr_top .
*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zscmr0001_scr_pbo .
INCLUDE zscmr0001_scr_pai .



*&---------------------------------------------------------------------*
* INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_get_mod.

  IMPORT  a = zscmt0001-partner FROM MEMORY ID 'ZSCMR0010_PAR'.
  IF sy-subrc = 0.

    IF gv_mod <> 'A'.
       PERFORM f_get_data.
    ENDIF.

    FREE MEMORY ID 'ZSCMR0010_PAR'.
  ENDIF.



*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*&---------------------------------------------------------------------*
* START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

   CALL SCREEN 9000.
