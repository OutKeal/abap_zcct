*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONO02 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  INIT_200  OUTPUT
*&---------------------------------------------------------------------*
*       Fill screen 200
*----------------------------------------------------------------------*
MODULE INIT_200 OUTPUT.

  DATA MESSAGE LIKE MESSAGE.


* suppress dialog.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.

  CASE G_HIDE-AREA.
    when c_area_inbound.
      PERFORM IB_TECHNICAL_DETAIL.
  ENDCASE.

  SET TITLEBAR '200'.
  IF G_DOCNUM IS INITIAL.
    SET PF-STATUS '200' EXCLUDING 'IDOC'.
  ELSE.
    SET PF-STATUS '200'.
  ENDIF.

  G_LINNO = SY-LINNO.
  LEAVE SCREEN.

ENDMODULE.                             " INIT_200  OUTPUT
