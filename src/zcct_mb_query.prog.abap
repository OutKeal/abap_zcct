*&---------------------------------------------------------------------*
*& Report ZCCT_QUERY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcct_mb_query.

INCLUDE ZCCT_MB_QUERY_DAT.
*INCLUDE: zcct_query_dat.

INCLUDE ZCCT_MB_QUERY_SCR.
*INCLUDE: zcct_query_scr.

INCLUDE ZCCT_MB_QUERY_ALV.
*INCLUDE: zcct_query_alv.

INCLUDE ZCCT_MB_QUERY_F01.
*INCLUDE: zcct_query_f01.


INITIALIZATION.



  IF s_credat[] IS INITIAL.
    s_credat-sign = 'I'.
    s_credat-option = 'BT'.
    s_credat-low = sy-datum - 30.
    s_credat-high = sy-datum.
    APPEND s_credat.
  ENDIF.

START-OF-SELECTION.




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
