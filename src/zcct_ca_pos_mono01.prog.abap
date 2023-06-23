*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONO01 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  INIT_112  OUTPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE INIT_112 OUTPUT.

  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  CASE IT_SEL-ITEM.
    WHEN 'I_Z4'.
* Download protocol processing
      PERFORM WDLSP_READ_PROC_NEW.
    WHEN 'I_Z5'.
* Download Communications Protocol
      PERFORM WDLSP_READ_KOMM_NEW.
    WHEN 'I_Z6'.
* Display long text
      PERFORM AT_LINE_SELECTION_108.
  ENDCASE.


  LEAVE SCREEN.


ENDMODULE.                 " INIT_112  OUTPUT
