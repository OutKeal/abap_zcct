*&---------------------------------------------------------------------*
*& Report ZCCT_QUERY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcct_query.

INCLUDE: zcct_query_dat.

INCLUDE: zcct_query_scr.

INCLUDE: zcct_query_alv.

INCLUDE: zcct_query_f01.


INITIALIZATION.

*  IF s_status[] IS INITIAL.
*    s_status-sign = 'I'.
*    s_status-option = 'EQ'.
*    s_status-low = '51'.
*    APPEND s_status.
*    s_status-low = '53'.
*    APPEND s_status.
*  ENDIF.

  IF s_credat[] IS INITIAL.
    s_credat-sign = 'I'.
    s_credat-option = 'BT'.
    s_credat-low = sy-datum - 30.
    s_credat-high = sy-datum.
    APPEND s_credat.
  ENDIF.

START-OF-SELECTION.

*  IF p_get64 = 'X'.
*
*    IF  s_credat[] IS INITIAL.
*      MESSAGE 'IDOC创建日期不应为空' TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*    PERFORM frm_get64_idoc.
*
*  ENDIF.



  IF s_budat[] IS INITIAL AND s_credat[] IS INITIAL.
    MESSAGE '请输入过账日期或者IDOC创建日期' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


  PERFORM frm_get_data.

  IF gt_head_dis[] IS INITIAL.
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_deal_data.
  gv_m = 'A'.

  CALL SCREEN 100.
