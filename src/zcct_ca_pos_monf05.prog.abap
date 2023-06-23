*----------------------------------------------------------------------*
*   INCLUDE /ATU/CA_POS_MONF05                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  OUTBOUND_BUILD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTBOUND_BUILD.

  DATA HIDE TYPE TYPE_HIDE.

  CLEAR: G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_ROOT.
  G_T_NODES-TLEVEL     = 1.
  G_T_NODES-TEXT       = TEXT-207.
  G_T_NODES-TLENGTH    = 37.
  G_T_NODES-TCOLOR     = COL_HEADING.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  APPEND G_T_NODES.

  CLEAR: G_T_NODES, HIDE-SECTION.
  G_T_NODES-TYPE       = C_NODE_AREA.
  HIDE-AREA            = C_AREA_OUTBOUND.            " B
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 2.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-208.
  G_T_NODES-TLENGTH    = 37.
  G_T_NODES-TCOLOR     = COL_HEADING.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  APPEND G_T_NODES.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_SECTION.
  HIDE-SECTION         = C_SECTION_NOK.              " N
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 3.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-218.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_NEGATIVE.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  G_T_NODES-FORCE_PLUS = 'X'.
  APPEND G_T_NODES.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_SECTION.
  HIDE-SECTION         = C_SECTION_OK.               " O
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 3.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-219.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_POSITIVE.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  G_T_NODES-FORCE_PLUS = 'X'.
  APPEND G_T_NODES.


ENDFORM.                    " OUTBOUND_BUILD
*&---------------------------------------------------------------------*
*&      Form  OUTBOUND_COMM_BUILD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTBOUND_COMM_BUILD.

  DATA HIDE TYPE TYPE_HIDE.

  CLEAR: G_T_NODES, HIDE-SECTION.
  G_T_NODES-TYPE       = C_NODE_AREA.
  HIDE-AREA            = C_AREA_OUTBOUND_COMM.        " C
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 2.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-209.
  G_T_NODES-TLENGTH    = 37.
  G_T_NODES-TCOLOR     = COL_HEADING.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  APPEND G_T_NODES.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_SECTION.
  HIDE-SECTION         = C_SECTION_NOK.               " N
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 3.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-210.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_NEGATIVE.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  G_T_NODES-FORCE_PLUS = 'X'.
  APPEND G_T_NODES.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_SECTION.
  HIDE-SECTION         = C_SECTION_OK.                " O
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 3.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-211.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_POSITIVE.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  G_T_NODES-FORCE_PLUS = 'X'.
  APPEND G_T_NODES.


ENDFORM.                    " OUTBOUND_COMM_BUILD
*&---------------------------------------------------------------------*
*&      Form  MESSAGE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGE_BUILD.

  DATA HIDE TYPE TYPE_HIDE.

  CLEAR: G_T_NODES, HIDE-SECTION.
  G_T_NODES-TYPE       = C_NODE_AREA.
  HIDE-AREA            = C_AREA_MESSAGE.              " D
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 2.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-TEXT       = TEXT-212.
  G_T_NODES-TLENGTH    = 37.
  G_T_NODES-TCOLOR     = COL_HEADING.
  G_T_NODES-TINTENSIV  = C_INTENSIV.
  G_T_NODES-FORCE_PLUS = 'X'.
  APPEND G_T_NODES.


ENDFORM.                    " MESSAGE

*&---------------------------------------------------------------------*
*&      Module  INIT_111  OUTPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE INIT_111 OUTPUT.

  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET TITLEBAR '101'.
  SET PF-STATUS '111'.
  PERFORM SET_DYNPRO_111.
  LEAVE SCREEN.


ENDMODULE.                 " INIT_111  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  AT_LINE_SELECTION_111
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AT_LINE_SELECTION_111.

  CLEAR: IT_SEL.
  IT_SEL-LINE = SY-LILLI.
  READ TABLE IT_SEL WITH KEY IT_SEL-LINE.
  IF SY-SUBRC > 0.
    MESSAGE E009.
*   Bitte positionieren Sie den Curser auf einer gültigen Zeile
  ELSE.
    CASE IT_SEL-ITEM.
      WHEN 'I_Z1'.
        PERFORM TOT_FILIALE.
      WHEN 'I_Z2'.
        PERFORM OPEN_FILIALE.
      WHEN 'I_Z3'.
        PERFORM KOMM_FILIALE.
      WHEN 'I_Z9'.
        PERFORM NOT_KOMM_FILIALE.
      WHEN 'I_Z12'.
        PERFORM CLOSED_FILIALE.
      WHEN OTHERS.
        MESSAGE E002.
* Not a valid selection row
    ENDCASE.
  ENDIF.



ENDFORM.                    " AT_LINE_SELECTION_111
*&---------------------------------------------------------------------*
*&      Form  OB_EXPAND_NODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_EXPAND_NODE
USING COMMAND
      NODE STRUCTURE SNODETEXT.

* append children to all selected leafs of type section or mess. type
  CASE NODE-TYPE.
    WHEN C_NODE_SECTION.
      PERFORM OB_EXPAND_SECTION USING COMMAND NODE.

*   when c_node_mestyp.
*     perform ob_show_mestyp using command node.
  ENDCASE.



ENDFORM.                    " OB_EXPAND_NODE
*&---------------------------------------------------------------------*
*&      Form  OB_EXPAND_SECTION
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_EXPAND_SECTION
USING COMMAND
      NODE STRUCTURE SNODETEXT.

  data: l_idoc       type type_idoc,
        HIDE         TYPE TYPE_OB_HIDE,
        L_WDLS-EMPFN LIKE WDLS-EMPFN.

  HIDE = NODE-HIDE.

  PERFORM READ_ERROR_WDLS.

* process all selected status-items
  CLEAR L_WDLS-EMPFN.
  IF I_Z4 > 0 OR I_Z4A > 0.                       " if items selected
    LOOP AT IT_WDLS.
*
      HIDE = NODE-HIDE.
      IF HIDE-SECTION = C_SECTION_NOK AND         " not processed
         IT_WDLS-GESST <> C_STATUS_OK AND
         IT_WDLS-GESST <> C_STATUS_USER_HINT.
*       append store line item and header for message types if the store
*       is different from the previous IDOC
        HIDE-EMPFN       = IT_WDLS-EMPFN.
        IF L_WDLS-EMPFN <> IT_WDLS-EMPFN.
          PERFORM OB_APPEND_STORE USING HIDE.
          PERFORM OB_APPEND_MESTYP_HEADER USING HIDE.
          L_WDLS-EMPFN = IT_WDLS-EMPFN.
          PERFORM OB_APPEND_MESTYP USING HIDE.
        ELSE.
          PERFORM OB_APPEND_MESTYP USING HIDE.
        ENDIF.
      ELSEIF HIDE-SECTION = C_SECTION_OK AND
         IT_WDLS-GESST    = C_STATUS_OK OR
         HIDE-SECTION     = C_SECTION_OK AND
         IT_WDLS-GESST    = C_STATUS_USER_HINT.
*       append store line item and header for message types if the store
*       is different from the previous IDOC
        HIDE-EMPFN       = IT_WDLS-EMPFN.
        IF L_WDLS-EMPFN <> IT_WDLS-EMPFN.
          PERFORM OB_APPEND_STORE USING HIDE.
          PERFORM OB_APPEND_MESTYP_HEADER USING HIDE.
          L_WDLS-EMPFN = IT_WDLS-EMPFN.
          PERFORM OB_APPEND_MESTYP USING HIDE.
        ELSE.
          PERFORM OB_APPEND_MESTYP USING HIDE.
        ENDIF.
      ENDIF.
*
    ENDLOOP.                             " at g_t_idocs.
  ENDIF.                                 " if items selected


ENDFORM.                    " OB_EXPAND_SECTION
*&---------------------------------------------------------------------*
*&      Form  OB_APPEND_STORE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_APPEND_STORE
     USING  HIDE  TYPE TYPE_OB_HIDE.

*  hide-sndprn = g_t_idocs-sndprn.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_STORE.
  G_T_NODES-NAME       = IT_WDLS-EMPFN.
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 4.
  G_T_NODES-NLENGTH    = 10.
  G_T_NODES-COLOR      = COL_KEY.

  G_T_NODES-TEXT       = IT_WDLS-NAME1.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_NORMAL.
  APPEND G_T_NODES.


ENDFORM.                    " OB_APPEND_STORE
*&---------------------------------------------------------------------*
*&      Form  OB_APPEND_MESTYP_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_APPEND_MESTYP_HEADER
     USING  HIDE  TYPE TYPE_OB_HIDE.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE      = C_NODE_HEADER.
  G_T_NODES-HIDE      = HIDE.
  G_T_NODES-TLEVEL    = 5.
  G_T_NODES-TEXT      = TEXT-035.
  G_T_NODES-TLENGTH   = 67.
  G_T_NODES-TCOLOR    = COL_HEADING.
  G_T_NODES-TINTENSIV = C_INTENSIV.
  APPEND G_T_NODES.


ENDFORM.                    " OB_APPEND_MESTYP_HEADER
*&---------------------------------------------------------------------*
*&      Form  OB_APPEND_MESTYP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_APPEND_MESTYP
     USING  HIDE  TYPE TYPE_OB_HIDE.


  HIDE-DLDNR = IT_WDLS-DLDNR.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_MESTYP.
  G_T_NODES-NAME       = IT_WDLS-SYSTP.
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 5.
  G_T_NODES-NLENGTH    = 6.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-FORCE_PLUS = ' '.
  WRITE: IT_WDLS-ERZDT TO G_T_NODES-TEXT.
  G_T_NODES-TEXT+10    = ' '.
  WRITE: IT_WDLS-ERZZT TO G_T_NODES-TEXT+11.
  G_T_NODES-TEXT+19    = ' '.
  G_T_NODES-TLENGTH    = 60.
  CASE IT_WDLS-GESST.
    WHEN C_STATUS_INIT.
      G_T_NODES-TEXT+20  = TEXT-024.
      G_T_NODES-TCOLOR   = COL_NEGATIVE.
    WHEN C_STATUS_USER_HINT.
      G_T_NODES-TEXT+20  = TEXT-025.
      G_T_NODES-TCOLOR   = COL_KEY.
    WHEN C_STATUS_DATA_MISSING.
      G_T_NODES-TEXT+20  = TEXT-026.
      G_T_NODES-TCOLOR   = COL_NEGATIVE.
    WHEN C_STATUS_IDOC_MISSING.
      G_T_NODES-TEXT+20  = TEXT-027.
      G_T_NODES-TCOLOR   = COL_NEGATIVE.
    WHEN C_STATUS_OK.
      G_T_NODES-TEXT+20  = TEXT-058.
      G_T_NODES-TCOLOR   = COL_POSITIVE.
  ENDCASE.

  APPEND G_T_NODES.



ENDFORM.                    " OB_APPEND_MESTYP
*&---------------------------------------------------------------------*
*&      Form  OB_show_MESTYP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_SHOW_MESTYP
  USING COMMAND
        NODE STRUCTURE SNODETEXT.

  DATA: HIDE     TYPE TYPE_OB_HIDE,
        DOC_NODE LIKE SNODETEXT.

  HIDE = NODE-HIDE.
  CASE NODE-TYPE.
    WHEN C_NODE_MESTYP.
      IT_WDLS-DLDNR = HIDE-DLDNR.
      IT_SEL-ITEM = 'I_Z4'.
      CALL SCREEN '112'.
  ENDCASE.

ENDFORM.                    " OB_show_MESTYP
*&---------------------------------------------------------------------*
*&      Form  OBC_EXPAND_NODE
*&---------------------------------------------------------------------*
*       Expand leaves in case of outbound-communication                *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBC_EXPAND_NODE
     USING COMMAND
           NODE STRUCTURE SNODETEXT.

* append children to all selected leafs of type section or mess. type
  CASE NODE-TYPE.
    WHEN C_NODE_SECTION.
      PERFORM OBC_EXPAND_SECTION USING COMMAND NODE.
  ENDCASE.




ENDFORM.                    " OBC_EXPAND_NODE
*&---------------------------------------------------------------------*
*&      Form  OBC_EXPAND_SECTION
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBC_EXPAND_SECTION
   USING COMMAND
         NODE STRUCTURE SNODETEXT.

  data: l_idoc       type type_idoc,
        HIDE         TYPE TYPE_OB_HIDE,
        L_WDLS-EMPFN LIKE WDLS-EMPFN.

  HIDE = NODE-HIDE.

  PERFORM READ_ERROR_WDLS_COMMUNICATION.

* process all selected status-items
  CLEAR L_WDLS-EMPFN.
  IF I_Z5 > 0 OR I_Z5A > 0.                       " if items selected
    LOOP AT IT_WDLS.
*
      HIDE = NODE-HIDE.
      IF HIDE-SECTION = C_SECTION_NOK AND         " not processed
         IT_WDLS-OKNO = C_OBC_NOK.
*       append store line item and header for message types if the store
*       is different from the previous IDOC
        HIDE-EMPFN       = IT_WDLS-EMPFN.
        IF L_WDLS-EMPFN <> IT_WDLS-EMPFN.
          PERFORM OB_APPEND_STORE USING HIDE.
          PERFORM OB_APPEND_MESTYP_HEADER USING HIDE.
          L_WDLS-EMPFN = IT_WDLS-EMPFN.
          PERFORM OBC_APPEND_MESTYP USING HIDE.
        ELSE.
          PERFORM OBC_APPEND_MESTYP USING HIDE.
        ENDIF.
      ELSEIF HIDE-SECTION = C_SECTION_OK AND
         IT_WDLS-OKNO     IS INITIAL.
*       append store line item and header for message types if the store
*       is different from the previous IDOC
        HIDE-EMPFN       = IT_WDLS-EMPFN.
        IF L_WDLS-EMPFN <> IT_WDLS-EMPFN.
          PERFORM OB_APPEND_STORE USING HIDE.
          PERFORM OB_APPEND_MESTYP_HEADER USING HIDE.
          L_WDLS-EMPFN = IT_WDLS-EMPFN.
          PERFORM OBC_APPEND_MESTYP USING HIDE.
        ELSE.
          PERFORM OBC_APPEND_MESTYP USING HIDE.
        ENDIF.
      ENDIF.
*
    ENDLOOP.                             " at g_t_idocs.
  ENDIF.                                 " if items selected



ENDFORM.                    " OBC_EXPAND_SECTION

*&---------------------------------------------------------------------*
*&      Form  READ_ERROR_WDLS_COMMUNICATION
*&---------------------------------------------------------------------*
*       count outbound-communication
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ERROR_WDLS_COMMUNICATION.

* Local fields
  DATA:   I_COUNTER TYPE I.
  DATA:   I_TWPFI LIKE TWPFI.

* Initialize counter
  I_Z5  = 0.
  I_Z5A = 0.


* IT wdls if not yet filled.
  DESCRIBE TABLE IT_WDLS LINES I_COUNTER.
  IF I_COUNTER < 1.
    PERFORM READ_ERROR_WDLS.
  ENDIF.

* OK and NOK-count status (counter I_Z5 + I_Z5A)
  LOOP AT IT_WDLS.

    CALL FUNCTION 'POS_CUST_COMM_PROFILE_READ'
          EXPORTING
             I_LOCNR               = IT_WDLS-EMPFN
          IMPORTING
             O_TWPFI               = I_TWPFI
          EXCEPTIONS
             FILIALE_UNBEKANNT     = 1
             KOMM_PROFIL_UNBEKANNT = 2
             OTHERS                = 3.

    IF IT_WDLS-VSEST = I_TWPFI-OKSTA.
      I_Z5A = I_Z5A + 1.
    ELSE.
      IT_WDLS-OKNO = C_OBC_NOK.
      MODIFY IT_WDLS.
      I_Z5 = I_Z5 + 1.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " READ_ERROR_WDLS_COMMUNICATION
*&---------------------------------------------------------------------*
*&      Form  OBC_SHOW_MESTYP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBC_SHOW_MESTYP
  USING COMMAND
        NODE STRUCTURE SNODETEXT.

  DATA: HIDE     TYPE TYPE_OB_HIDE,
        DOC_NODE LIKE SNODETEXT.

  HIDE = NODE-HIDE.
  CASE NODE-TYPE.
    WHEN C_NODE_MESTYP.
      IT_WDLS-DLDNR = HIDE-DLDNR.
      IT_SEL-ITEM = 'I_Z5'.
      CALL SCREEN '112'.
  ENDCASE.

ENDFORM.                    " OBC_SHOW_MESTYP
*&---------------------------------------------------------------------*
*&      Form  OBC_APPEND_MESTYP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBC_APPEND_MESTYP
     USING  HIDE  TYPE TYPE_OB_HIDE.


  HIDE-DLDNR = IT_WDLS-DLDNR.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_MESTYP.
  G_T_NODES-NAME       = IT_WDLS-SYSTP.
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 5.
  G_T_NODES-NLENGTH    = 6.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-FORCE_PLUS = ' '.
  WRITE: IT_WDLS-ERZDT TO G_T_NODES-TEXT.
  G_T_NODES-TEXT+10    = ' '.
  WRITE: IT_WDLS-ERZZT TO G_T_NODES-TEXT+11.
  G_T_NODES-TEXT+19    = ' '.
  G_T_NODES-TLENGTH    = 60.

* check if message is positive.
  IF IT_WDLS-OKNO = C_OBC_NOK.                     " = Not OK
    CASE IT_WDLS-VSEST.
      WHEN '  '.                      "No communication feedback
        G_T_NODES-TEXT+20  = TEXT-090.
        G_T_NODES-TCOLOR   = COL_NEGATIVE.
      WHEN OTHERS.
        SELECT SINGLE * FROM TEDS2
               WHERE  STATUS    = IT_WDLS-VSEST
               AND    LANGUA    = SY-LANGU.
        G_T_NODES-TEXT+20  = TEDS2-DESCRP.
        G_T_NODES-TCOLOR   = COL_NEGATIVE.
    ENDCASE.
  ELSE.                                            " = OK
    SELECT SINGLE * FROM TEDS2
           WHERE  STATUS    = IT_WDLS-VSEST
           AND    LANGUA    = SY-LANGU.
    G_T_NODES-TEXT+20  = TEDS2-DESCRP.
    G_T_NODES-TCOLOR   = COL_POSITIVE.
  ENDIF.

  APPEND G_T_NODES.


ENDFORM.                    " OBC_APPEND_MESTYP
*&---------------------------------------------------------------------*
*&      Form  ME_EXPAND_NODE
*&---------------------------------------------------------------------*
*       Expand message-leafs                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ME_EXPAND_NODE
     USING COMMAND
           NODE STRUCTURE SNODETEXT.

  data: l_idoc        type type_idoc,
        HIDE          TYPE TYPE_OB_HIDE,
        L_WPXST-EMPFN LIKE WPXST-FILIALE.


  HIDE = NODE-HIDE.

  PERFORM READ_ERROR_WPXST.

* process all selected status-items
  CLEAR L_WPXST-EMPFN.
  IF I_Z6 > 0.                                    " if items selected
    LOOP AT IT_WPXST.
*
*     append store line item and header for message types if the store
*     is different from the previous IDOC
      HIDE-EMPFN       = IT_WPXST-FILIALE.
      IF L_WPXST-EMPFN <> IT_WPXST-FILIALE.
        PERFORM ME_APPEND_STORE USING HIDE.
        PERFORM ME_APPEND_MESTYP_HEADER USING HIDE.
        L_WPXST-EMPFN = IT_WPXST-FILIALE.
        PERFORM ME_APPEND_MESTYP USING HIDE.
      ELSE.
        PERFORM ME_APPEND_MESTYP USING HIDE.
      ENDIF.
*
    ENDLOOP.                             " at g_t_idocs.
  ENDIF.                                 " if items selected



ENDFORM.                    " ME_EXPAND_NODE

*&---------------------------------------------------------------------*
*&      Form  ME_APPEND_MESTYP
*&---------------------------------------------------------------------*
*       Message Append                                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ME_APPEND_MESTYP
     USING  HIDE  TYPE TYPE_OB_HIDE.

  CLEAR: I_MESSAGE.
* Data conversion for function module
  I_WPXST-MSGNR  = IT_WPXST-FEHLERNR.

* Read the message
  call function 'WRITE_MESSAGE_NEW'
         EXPORTING
              MSGID = IT_WPXST-MSGID
              MSGNO = I_WPXST-MSGNR
              MSGTY = IK_MESSAGE_TYP
              MSGV1 = IT_WPXST-PARA1
              MSGV2 = IT_WPXST-PARA2
              MSGV3 = IT_WPXST-PARA3
              MSGV4 = IT_WPXST-PARA4
              MSGV5 = ' '
         IMPORTING
              ERROR = I_ERROR
              MESSG = I_MESSAGE
              MSGLN = I_LENGTH.

  HIDE-COUNTER         = IT_WPXST-COUNTER.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_MESTYP.
  G_T_NODES-NAME       = I_MESSAGE-MSGID.
  G_T_NODES-NAME+2     = ' '.
  G_T_NODES-NAME+3     = I_MESSAGE-MSGNO.
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 5.
  G_T_NODES-NLENGTH    = 7.
  G_T_NODES-COLOR      = COL_KEY.
  G_T_NODES-FORCE_PLUS = ' '.
  WRITE: IT_WPXST-DATUM TO G_T_NODES-TEXT.
  G_T_NODES-TEXT+10    = ' '.
  G_T_NODES-TEXT+11    = I_MESSAGE-MSGTX.

  IF I_MESSAGE-MSGTY   = 'E'.
    G_T_NODES-TCOLOR   = COL_NEGATIVE.
  ELSE.
    G_T_NODES-TCOLOR   = COL_WARNING.
  ENDIF.

  G_T_NODES-TLENGTH    = 80.

  APPEND G_T_NODES.



ENDFORM.                    " ME_APPEND_MESTYP
*&---------------------------------------------------------------------*
*&      Form  ME_APPEND_STORE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ME_APPEND_STORE
     USING  HIDE  TYPE TYPE_OB_HIDE.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE       = C_NODE_STORE.
  G_T_NODES-NAME       = IT_WPXST-FILIALE.
  G_T_NODES-HIDE       = HIDE.
  G_T_NODES-TLEVEL     = 4.
  G_T_NODES-NLENGTH    = 10.
  G_T_NODES-COLOR      = COL_KEY.

  G_T_NODES-TEXT       = IT_WPXST-NAME1.
  G_T_NODES-TLENGTH    = 30.
  G_T_NODES-TCOLOR     = COL_NORMAL.
  APPEND G_T_NODES.



ENDFORM.                    " ME_APPEND_STORE
*&---------------------------------------------------------------------*
*&      Form  ME_APPEND_MESTYP_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ME_APPEND_MESTYP_HEADER
     USING  HIDE  TYPE TYPE_OB_HIDE.

  CLEAR G_T_NODES.
  G_T_NODES-TYPE      = C_NODE_HEADER.
  G_T_NODES-HIDE      = HIDE.
  G_T_NODES-TLEVEL    = 5.
  G_T_NODES-TEXT      = TEXT-028.
  G_T_NODES-TLENGTH   = 88.
  G_T_NODES-TCOLOR    = COL_HEADING.
  G_T_NODES-TINTENSIV = C_INTENSIV.
  APPEND G_T_NODES.



ENDFORM.                    " ME_APPEND_MESTYP_HEADER
*&---------------------------------------------------------------------*
*&      Form  ME_SHOW_MESTYP
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ME_SHOW_MESTYP
  USING COMMAND
        NODE STRUCTURE SNODETEXT.

  DATA: HIDE     TYPE TYPE_OB_HIDE,
        DOC_NODE LIKE SNODETEXT.

  CLEAR IT_WPXST.

  HIDE = NODE-HIDE.
  CASE NODE-TYPE.
    WHEN C_NODE_MESTYP.
      IT_WPXST-COUNTER  = HIDE-COUNTER.
      IT_SEL-ITEM = 'I_Z6'.
      CALL SCREEN '112'.
  ENDCASE.


ENDFORM.                    " ME_SHOW_MESTYP


*eject.
*&---------------------------------------------------------------------*
*&      Form  OB_INIT
*&---------------------------------------------------------------------*
*       reset outbound buffers                                         *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OB_INIT.

  REFRESH: IT_WDLS, G_T_WDLSP.
  CLEAR:   IT_WDLS, G_T_WDLSP.

ENDFORM.                    " OB_INIT
