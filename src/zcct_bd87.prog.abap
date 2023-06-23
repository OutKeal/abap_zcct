*&---------------------------------------------------------------------*
*& Report ZJK_POST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjk_post.

TABLES:zcct_db_h.
TABLES:zcct_db_i.
TABLES:edidc.
DATA gt_message TYPE TABLE OF esp1_message_wa_type WITH HEADER LINE.
DATA:gt_msg TYPE TABLE OF zcct_msg_dis WITH HEADER LINE.
DATA:et_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
DATA:lv_msg TYPE char40.
DATA:lv_error TYPE char1.


DATA:gt_db_h TYPE TABLE OF zcct_db_h WITH HEADER LINE.
DATA:gt_db_i TYPE TABLE OF zcct_db_i WITH HEADER LINE.

DATA:BEGIN OF gt_data OCCURS 0 ,
       status          TYPE edidc-status,
       mescod          TYPE edidc-mescod,
       docnum          TYPE zcct_db_h-docnum,
       zcct_type       TYPE zcct_db_h-zcct_type,
       zcct_type_name  TYPE zcct_type_name,
       long_vgbel      TYPE zcct_db_h-long_vgbel,
       channel1        TYPE zcct_db_h-channel1,
       channel2        TYPE zcct_db_h-channel2,
       channel3        TYPE zcct_db_h-channel3,
       zcct_post_index TYPE zcct_post_index,
       budat           TYPE zcct_db_h-budat,
       menge           TYPE zcct_db_i-menge,
       netwr           TYPE zcct_db_i-netwr,

     END OF gt_data .

DATA:lock_data LIKE TABLE OF gt_data WITH HEADER LINE  .

PARAMETERS:p_group TYPE zcct_post_group OBLIGATORY.


SELECT-OPTIONS: s_docnum FOR zcct_db_h-docnum.
SELECT-OPTIONS: s_type FOR zcct_db_h-zcct_type.
SELECT-OPTIONS: s_vgbel FOR zcct_db_h-long_vgbel.
SELECT-OPTIONS: s_chan1 FOR zcct_db_h-channel1.
SELECT-OPTIONS: s_chan2 FOR zcct_db_h-channel2.
SELECT-OPTIONS: s_chan3 FOR zcct_db_h-channel3.
SELECT-OPTIONS: s_budat FOR zcct_db_h-budat.
SELECT-OPTIONS: s_stuta FOR edidc-status.

INITIALIZATION.

  IF s_stuta[] IS INITIAL.
    s_stuta-sign = 'I'.
    s_stuta-option = 'EQ'.
    s_stuta-low = '64'.
    APPEND s_stuta.
    s_stuta-low = '51'.
    APPEND s_stuta.
    CLEAR s_stuta.
  ENDIF.


START-OF-SELECTION.

  IF p_group IS INITIAL.
    MESSAGE '请输入业务分组' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF s_docnum[] IS INITIAL
     AND p_group IS INITIAL
     AND s_type[] IS INITIAL
     AND s_vgbel[] IS INITIAL
     AND s_chan1[] IS INITIAL
     AND s_chan2[] IS INITIAL
     AND s_chan3[] IS INITIAL
     AND s_budat[] IS INITIAL
     AND s_stuta[] IS INITIAL.
    MESSAGE '请输入必要条件' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


  SELECT * FROM zcct_db_h
     INNER JOIN edidc
     ON zcct_db_h~docnum = edidc~docnum
     INNER JOIN zcct_mt ON zcct_db_h~zcct_type = zcct_mt~zcct_type
     INTO CORRESPONDING FIELDS OF TABLE @gt_data
     WHERE zcct_db_h~docnum  IN @s_docnum
     AND zcct_db_h~zcct_type IN @s_type
     AND long_vgbel   IN @s_vgbel
     AND channel1   IN @s_chan1
     AND channel2   IN @s_chan2
     AND channel3   IN @s_chan3
     AND budat      IN @s_budat
     AND status     IN @s_stuta
     AND zcct_post_group = @p_group
     .




  IF sy-subrc NE 0.
    MESSAGE '无数据' TYPE 'S' .
    STOP.
  ENDIF.

  lock_data[] = gt_data[].

  SORT lock_data BY zcct_type.
  DELETE ADJACENT DUPLICATES FROM lock_data COMPARING zcct_type.

  CLEAR lv_error.
  LOOP AT lock_data.
    CALL FUNCTION 'ENQUEUE_EZ_ZCCT_MT'
      EXPORTING
        mode_zcct_mt = 'E'
        mandt        = sy-mandt
        zcct_type    = lock_data-zcct_type
*       X_ZCCT_TYPE  = ' '
*       _SCOPE       = '2'
*       _WAIT        = ' '
*       _COLLECT     = ' '
*       EXCEPTIONS
*       FOREIGN_LOCK = 1
*       SYSTEM_FAILURE       = 2
*       OTHERS       = 3
      .
    IF sy-subrc <> 0.
      lv_msg = '业务类型' && lock_data-zcct_type  && '正在后台运行'.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E' .
      lv_error = 'X'.
    ENDIF.
  ENDLOOP.

  IF lv_error = 'X'.
    STOP.
  ENDIF.

  CLEAR gt_message[].
  SORT gt_data BY zcct_post_index budat long_vgbel.
  LOOP AT gt_data INTO DATA(gs_data).
    CALL FUNCTION 'ZCCT_IDOC_INBOUND_PROCESS'
      EXPORTING
        i_docnum  = gs_data-docnum
      IMPORTING
        e_status  = gs_data-status
      TABLES
        et_return = et_return.

    LOOP AT et_return.
      PERFORM frm_add_msg USING et_return-id
                                et_return-type
                                et_return-number
                                et_return-message_v1
                                et_return-message_v2
                                et_return-message_v3
                                et_return-message_v4.

    ENDLOOP.
    CLEAR et_return[].

    IF sy-batch = 'X'.
      CASE gs_data-status.
        WHEN '51'.
          lv_msg =  gs_data-docnum && '处理失败'.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          WRITE:/ lv_msg .
        WHEN '53'.
          lv_msg =  gs_data-docnum && '处理成功'.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          WRITE:/ lv_msg .
          WHEN OTHERS.

          lv_msg =  gs_data-docnum && '处理异常'.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          WRITE:/ lv_msg .

      ENDCASE.
    ENDIF.

  ENDLOOP.

  IF gt_message[] IS NOT INITIAL.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_message[].
    CLEAR gt_message[].

  ENDIF.

  LOOP AT lock_data.
    CALL FUNCTION 'DEQUEUE_EZ_ZCCT_MT'
      EXPORTING
        mode_zcct_mt = 'E'
        mandt        = sy-mandt
        zcct_type    = lock_data-zcct_type
*       X_ZCCT_TYPE  = ' '
*       _SCOPE       = '3'
*       _SYNCHRON    = ' '
*       _COLLECT     = ' '
      .
  ENDLOOP.



FORM frm_add_msg USING msgid
                        msgty
                        msgno
                        msgv1
                        msgv2
                        msgv3
                        msgv4.

  CLEAR gt_msg.
  gt_message-msgid = msgid .
  gt_message-msgty = msgty .
  gt_message-msgno = msgno .
  gt_message-msgv1 = msgv1 .
  gt_message-msgv2 = msgv2 .
  gt_message-msgv3 = msgv3 .
  gt_message-msgv4 = msgv4 .
  APPEND gt_message.


ENDFORM.
