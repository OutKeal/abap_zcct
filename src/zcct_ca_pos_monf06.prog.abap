*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONF06 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_MESSAGE
USING WPLST LIKE WPLST
      MESSG LIKE MESSAGE.

  DATA: MSGNO LIKE SY-MSGNO.

  MSGNO = WPLST-MSGNR.
  call function 'WRITE_MESSAGE_NEW'
       EXPORTING
            MSGID = WPLST-MSGID
            MSGNO = MSGNO
            MSGTY = WPLST-FEHLERTYP
            MSGV1 = WPLST-PARAMETER1
            MSGV2 = WPLST-PARAMETER2
            MSGV3 = WPLST-PARAMETER3
            MSGV4 = WPLST-PARAMETER4
            MSGV5 = ' '
       IMPORTING
            MESSG = MESSG.

ENDFORM.                               " WRITE_MESSAGE


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_200
*&---------------------------------------------------------------------*
*       Execute user commands from screen 200 (error details)
*----------------------------------------------------------------------*
FORM USER_COMMAND_200.
  DATA: OBJECT    TYPE SWC_OBJECT,
        CONTAINER LIKE SWCONT OCCURS 0.

  CASE SY-UCOMM.
    WHEN 'IDOC'.
*     Display IDOC
      SWC_CREATE_OBJECT OBJECT C_OBJTYPE_IDOC G_DOCNUM.
      SWC_CALL_METHOD   OBJECT 'DISPLAY' CONTAINER.
      SWC_FREE_OBJECT   OBJECT.

    WHEN 'OKAY'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.                               " USER_COMMAND_200


*&---------------------------------------------------------------------*
*&      Form  IB_TECHNICAL_DETAIL
*&---------------------------------------------------------------------*
*       Display technical details for inbound processing nodes
*----------------------------------------------------------------------*
FORM IB_TECHNICAL_DETAIL.

  DATA: MESSAGE LIKE MESSAGE,
        HIDE    TYPE TYPE_IB_HIDE,
        T_WPTST LIKE WPTST OCCURS 0 WITH HEADER LINE,
        T_WPLST LIKE WPLST OCCURS 0 WITH HEADER LINE.

  HIDE = G_HIDE.

  IF NOT ( HIDE-DOCNUM IS INITIAL ).
    G_DOCNUM = HIDE-DOCNUM.
    WRITE: / TEXT-046, 25 HIDE-DOCNUM   COLOR COL_NORMAL.
    IF NOT ( HIDE-SEGNUM IS INITIAL ).
      WRITE: / TEXT-084, 25 HIDE-SEGNUM     COLOR COL_NORMAL,
               TEXT-085,    HIDE-SEGNUM_END COLOR COL_NORMAL.
    ENDIF.
    SKIP.
  ENDIF.

  IF G_NODE-TYPE = C_NODE_ERROR.
    READ TABLE G_T_WPLST INDEX HIDE-KEY.
    IF SY-SUBRC = 0.

      EDIDOT-DESCRP = ' '.
      SELECT SINGLE * FROM  EDIDOT
             WHERE  DOCTYP      = G_T_WPLST-DOCTYP
             AND    LANGUA      = SY-LANGU.

      PERFORM WRITE_MESSAGE USING G_T_WPLST MESSAGE.

      KNA1-KUNNR = HIDE-SNDPRN.
      CALL FUNCTION 'KNA1_SINGLE_READ'
           EXPORTING
                KNA1_KUNNR = KNA1-KUNNR
           IMPORTING
                WKNA1      = KNA1
           EXCEPTIONS
                NOT_FOUND  = 1
                OTHERS     = 2.
      IF SY-SUBRC <> 0.
        CLEAR IT_T001W_ALL.
      ENDIF.

      WRITE: / TEXT-034, 25 G_T_WPLST-FILIALE COLOR COL_NORMAL,
             / TEXT-013, 25 KNA1-NAME1         COLOR COL_NORMAL,
             / TEXT-014, 25 KNA1-ORT01         COLOR COL_NORMAL,
             / TEXT-059, 25(80) MESSAGE        COLOR COL_NORMAL,
             / TEXT-032, 25 G_T_WPLST-ERZDT    COLOR COL_NORMAL,
             / TEXT-047, 25 G_T_WPLST-DOCTYP   COLOR COL_NORMAL,
             / TEXT-081, 25 EDIDOT-DESCRP      COLOR COL_NORMAL,
             / TEXT-062, 25 G_T_WPLST-FUNCTION COLOR COL_NORMAL,
             / TEXT-060, 25 G_T_WPLST-SEGNAM   COLOR COL_NORMAL,
             / TEXT-061, 25 G_T_WPLST-SEGNUM   COLOR COL_NORMAL,
             / TEXT-063, 25 G_T_WPLST-PROGLOKALI COLOR COL_NORMAL,
             / TEXT-097, 25 G_T_WPLST-REFNR    COLOR COL_NORMAL.
      SKIP.
    ENDIF.
  ENDIF.

  IF NOT ( HIDE-DOCNUM IS INITIAL ).
*   Upload-Verbuchungsprotokoll (Tabelle IT_WPTST) anzeigen
    SELECT * FROM WPTST INTO TABLE T_WPTST
          WHERE DOCNUM   = HIDE-DOCNUM.

    IF SY-SUBRC = 0.
      SORT T_WPTST BY RNGBEGIN RNGEND VERBUCHUNG.
      WRITE: /(10) TEXT-067,
              (10) TEXT-068,
              (10) TEXT-064,
              (10) TEXT-070,
              (10) TEXT-069,
              (10) TEXT-062,
              (10) TEXT-065,
              (10) TEXT-066,
             / SY-ULINE.

      LOOP AT T_WPTST.
        WRITE: /(10) T_WPTST-RNGBEGIN   COLOR COL_NORMAL,
                (10) T_WPTST-RNGEND     COLOR COL_NORMAL,
                (10) T_WPTST-VERBUCHUNG COLOR COL_NORMAL,
                (10) T_WPTST-LFDNR      COLOR COL_NORMAL,
                (10) T_WPTST-VERARBEND  COLOR COL_NORMAL,
                (10) T_WPTST-FUNCTION   COLOR COL_NORMAL,
                (10) T_WPTST-SUBFUNC1   COLOR COL_NORMAL,
                (10) T_WPTST-SUBFUNC2   COLOR COL_NORMAL.
      ENDLOOP.
      SKIP.

    ENDIF.

*   Fehlermeldungen aus WPLST anzeigen
    SELECT * FROM WPLST INTO TABLE T_WPLST
          WHERE DOCNUM   = HIDE-DOCNUM.

    IF SY-SUBRC = 0.
      SORT T_WPLST BY SEGNUM MSGID MSGNR.
      WRITE: /(10) TEXT-059,
             / SY-ULINE.

      LOOP AT T_WPLST.
        PERFORM WRITE_MESSAGE USING T_WPLST CHANGING MESSAGE.
        WRITE: / T_WPLST-SEGNUM     COLOR COL_NORMAL,
                 (80) MESSAGE       COLOR COL_NORMAL,
                 T_WPLST-ERZDT      COLOR COL_NORMAL,
                 T_WPLST-FUNCTION   COLOR COL_NORMAL,
                 T_WPLST-SEGNAM     COLOR COL_NORMAL,
                 T_WPLST-REFNR      COLOR COL_NORMAL,
                 T_WPLST-PROGLOKALI COLOR COL_NORMAL.
      ENDLOOP.
      SKIP.

    ENDIF.
  ENDIF.

ENDFORM.                               " IB_TECHNICAL_DETAIL
