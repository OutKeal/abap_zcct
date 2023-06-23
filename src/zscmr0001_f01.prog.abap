*&---------------------------------------------------------------------*
*& 包含               ZSCMR0001_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_GET_MOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_mod .

 CLEAR:gt_1001,gt_1002,gt_1003.

 CASE sy-tcode.
    WHEN 'ZSCM0001A'.
      gv_mod = 'A'.
    WHEN 'ZSCM0001B'.
      gv_mod = 'B'.
    WHEN 'ZSCM0001C'.
      gv_mod = 'C'.
    WHEN OTHERS.
      gv_mod = 'A'.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  F_HLP_BU_GROUP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_hlp_bu_group INPUT.

  CLEAR:gt_ret_hlp,gt_ret_hlp[].

  IF gt_bugp[] IS INITIAL.
     SELECT DISTINCT a~bu_group,b~txt15
       FROM zscmt1001 AS a INNER JOIN tb002 AS b
            ON a~bu_group = b~bu_group
      WHERE b~spras = 1
       INTO TABLE @gt_bugp.
      SORT gt_bugp BY bu_group.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*      ddic_structure  = ''
      retfield        = 'BU_GROUP'
      dynpprog        = sy-repid
      dynpnr          = '9000'
      dynprofield     = 'ZSCMT0001-BU_GROUP'
      value_org       = 'S'
    TABLES
      value_tab       = gt_bugp
      return_tab      = gt_ret_hlp.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  F_HLP_ZVTWEG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_hlp_zvtweg INPUT.
  CLEAR:gt_ret_hlp,gt_ret_hlp[].

  IF gt_zvtweg[] IS INITIAL.
     SELECT vtweg AS zvtweg,vtext
       FROM tvtwt
      WHERE spras = 1
       INTO TABLE @gt_zvtweg.
      SORT gt_zvtweg BY zvtweg.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*      ddic_structure  = ''
      retfield        = 'ZVTWEG'
      dynpprog        = sy-repid
      dynpnr          = '9010'
      dynprofield     = 'ZSCMT0001-ZVTWEG'
      value_org       = 'S'
    TABLES
      value_tab       = gt_zvtweg
      return_tab      = gt_ret_hlp.

ENDMODULE.



FORM check_tab_value.

  DATA rule TYPE TABLE OF zcheck_rule WITH HEADER LINE.
  DATA:lt_tab  TYPE  STANDARD TABLE OF zscmt0001.
  DATA:lt_return LIKE STANDARD TABLE OF bapiret2.

  CLEAR rule[].
  CLEAR lt_tab[].


  LOOP AT gt_1005 INTO gs_1005.
    CLEAR rule.
    rule-fieldname = gs_1005-fieldname.
    rule-rollname  = gs_1005-rollname.
    rule-notnull   = gs_1005-notnull.
    rule-ddtext    = gs_1005-ddtext.
    APPEND rule.
  ENDLOOP.

  APPEND zscmt0001 TO lt_tab.

  CLEAR:lt_return.
  CALL FUNCTION 'ZCHECK_TAB_VALUE'
    TABLES
      tab  = lt_tab
      rule = rule
      ret  = lt_return.
  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO gt_return.
  ENDIF.

  CLEAR:lt_return.
  CALL FUNCTION 'ZSCM_BP_CHECK'
    EXPORTING
      i_scm0001       = zscmt0001
    TABLES
      et_return       = lt_return
            .
  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO gt_return.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POP_MSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM pop_msg .
  DATA:lt_msg TYPE esp1_message_tab_type.
  DATA:ls_msg TYPE esp1_message_wa_type.

  CLEAR:lt_msg[].

  LOOP AT gt_return INTO DATA(ls_return) WHERE type = 'E' OR
                                               type = 'A'.

     gv_status = 'E'.

    ls_msg-msgid   = ls_return-id.
    ls_msg-msgty   = ls_return-type.
    ls_msg-msgno   = ls_return-number.
    ls_msg-msgv1   = ls_return-message_v1.
    ls_msg-msgv2   = ls_return-message_v2.
    ls_msg-msgv3   = ls_return-message_v3.
    ls_msg-msgv4   = ls_return-message_v4.

    APPEND ls_msg TO lt_msg.
  ENDLOOP.

  IF gv_status = 'E'.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = lt_msg.
  ENDIF.

ENDFORM.
