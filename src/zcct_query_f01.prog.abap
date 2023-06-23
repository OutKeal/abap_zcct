*&---------------------------------------------------------------------*
*& 包含               ZCCT_QUERY_F01
*&---------------------------------------------------------------------*

FORM frm_get_data.

  SELECT *

    INTO CORRESPONDING FIELDS OF TABLE @gt_head_dis
    FROM zcct_db_h AS a
    INNER JOIN edidc AS b ON a~docnum = b~docnum
    WHERE a~zcct_type     IN @s_type
      AND a~channel1      IN @s_chan1
      AND a~channel2      IN @s_chan2
      AND a~channel3      IN @s_chan3
      AND a~canceled      IN @s_can
      AND a~budat         IN @s_budat
      AND a~long_vgbel    IN @s_lvgbel
      AND b~docnum          IN @s_docnum
      AND b~mescod          IN @s_mescod
      AND b~credat          IN @s_credat
      AND b~cretim          IN @s_cretim
      AND b~status          IN @s_status.


  IF sy-subrc EQ 0.
    CLEAR gt_mt[].
    CLEAR gt_scmt0010[].

    LOOP AT gt_head_dis ASSIGNING <gs_head_dis>.

      IF <gs_head_dis>-zcct_type IS NOT INITIAL.
        gt_mt-zcct_type = <gs_head_dis>-zcct_type.
        APPEND gt_mt.
      ENDIF.

      IF <gs_head_dis>-channel1 IS NOT INITIAL.
        gt_scmt0010-partner = <gs_head_dis>-channel1.
        APPEND gt_scmt0010.
      ENDIF.

      IF <gs_head_dis>-channel2 IS NOT INITIAL.
        gt_scmt0010-partner = <gs_head_dis>-channel2.
        APPEND gt_scmt0010.
      ENDIF.

      IF <gs_head_dis>-channel3 IS NOT INITIAL.
        gt_scmt0010-partner = <gs_head_dis>-channel3.
        APPEND gt_scmt0010.
      ENDIF.

    ENDLOOP.

    IF gt_scmt0010[] IS NOT INITIAL.

      SORT gt_scmt0010 BY partner.

      DELETE ADJACENT DUPLICATES FROM gt_scmt0010 COMPARING partner.

      SELECT partner, name1 FROM zscmt0010
        FOR ALL ENTRIES IN @gt_scmt0010
        WHERE partner = @gt_scmt0010-partner
        INTO CORRESPONDING FIELDS OF TABLE @gt_scmt0010.

      SORT gt_scmt0010 BY partner.

    ENDIF.

    IF gt_mt[] IS NOT INITIAL.

      SORT gt_mt BY zcct_type.

      DELETE ADJACENT DUPLICATES FROM gt_mt COMPARING zcct_type.

      SELECT * FROM zcct_mt
        FOR ALL ENTRIES IN @gt_mt
        WHERE zcct_type = @gt_mt-zcct_type
        INTO TABLE @gt_mt.

      SORT gt_mt BY zcct_type.

    ENDIF.

    SELECT * FROM  zcct_db_i
      FOR ALL ENTRIES IN @gt_head_dis
      WHERE docnum = @gt_head_dis-docnum
     INTO CORRESPONDING FIELDS OF TABLE @gt_item.

    SELECT * FROM  zcct_vgbel_log
      FOR ALL ENTRIES IN @gt_head_dis
      WHERE docnum = @gt_head_dis-docnum
      AND swo_objtyp <> 'IDOC'
     INTO CORRESPONDING FIELDS OF TABLE @gt_vgbel
      .

    SELECT * FROM  zcct_msg_log
      FOR ALL ENTRIES IN @gt_head_dis
      WHERE docnum = @gt_head_dis-docnum
      INTO CORRESPONDING FIELDS OF TABLE @gt_msg.



  ENDIF.



ENDFORM.

FORM frm_deal_data.

  LOOP AT gt_head_dis ASSIGNING <gs_head_dis>.
    CASE <gs_head_dis>-status.
      WHEN '56'.
        <gs_head_dis>-icon = icon_cancel.
      WHEN '51'.
        <gs_head_dis>-icon = icon_red_light.
      WHEN '53'.
        <gs_head_dis>-icon = icon_green_light.
      WHEN '68'.
        <gs_head_dis>-icon = icon_dummy.
      WHEN '64'.
        <gs_head_dis>-icon = icon_yellow_light.
    ENDCASE.

    READ TABLE gt_mt WITH KEY zcct_type = <gs_head_dis>-zcct_type BINARY SEARCH.
    IF sy-subrc EQ 0.
      <gs_head_dis>-zcct_type_name = gt_mt-zcct_type_name.
    ENDIF.

    IF <gs_head_dis>-channel1  IS NOT INITIAL.
      READ TABLE gt_scmt0010 WITH KEY partner = <gs_head_dis>-channel1 BINARY SEARCH.
      IF sy-subrc EQ 0.
        <gs_head_dis>-name1 = gt_scmt0010-name1.
      ENDIF.
    ENDIF.

    IF <gs_head_dis>-channel2  IS NOT INITIAL.
      READ TABLE gt_scmt0010 WITH KEY partner = <gs_head_dis>-channel2 BINARY SEARCH.
      IF sy-subrc EQ 0.
        <gs_head_dis>-name2 = gt_scmt0010-name1.
      ENDIF.
    ENDIF.


    IF <gs_head_dis>-channel3  IS NOT INITIAL.
      READ TABLE gt_scmt0010 WITH KEY partner = <gs_head_dis>-channel3 BINARY SEARCH.
      IF sy-subrc EQ 0.
        <gs_head_dis>-name3 = gt_scmt0010-name1.
      ENDIF.
    ENDIF.



  ENDLOOP.

  LOOP AT gt_vgbel ASSIGNING <gs_vgbel_dis>.
    CASE <gs_vgbel_dis>-swo_objtyp.
      WHEN 'BUS2017'.
        <gs_vgbel_dis>-swo_objtyp = '物料凭证'.
      WHEN 'VBRK'.
        <gs_vgbel_dis>-swo_objtyp = '销售发票'.
      WHEN 'BKPF'.
        <gs_vgbel_dis>-swo_objtyp = '会计凭证'.
    ENDCASE.

  ENDLOOP.

  LOOP AT gt_msg ASSIGNING FIELD-SYMBOL(<gs_msg_dis>).
    CASE <gs_msg_dis>-msgty.
      WHEN 'S'.
        <gs_msg_dis>-icon = icon_led_green.
      WHEN 'E'.
        <gs_msg_dis>-icon = icon_led_red.
      WHEN 'W'.
        <gs_msg_dis>-icon = icon_led_yellow.
    ENDCASE.

  ENDLOOP.

  SORT gt_head_dis BY credat DESCENDING cretim DESCENDING.
  gt_item_dis[] = gt_item[].
  gt_vgbel_dis[] = gt_vgbel[].
  gt_msg_dis[] = gt_msg[].

ENDFORM.

FORM frm_get64_idoc.

  DATA: lt_db_h TYPE TABLE OF zcct_db_h WITH HEADER LINE.
  DATA: lt_db_i TYPE TABLE OF zcct_db_i WITH HEADER LINE.
  DATA: ls_zccth TYPE  zccth.
  DATA: ls_zccti TYPE  zccti.

  SELECT edidc~* FROM edidc LEFT JOIN zcct_db_h
      ON edidc~docnum = zcct_db_h~docnum
    WHERE edidc~docnum          IN @s_docnum
      AND mescod          IN @s_mescod
      AND credat          IN @s_credat
      AND cretim          IN @s_cretim
      AND status          IN @s_status
      AND status          EQ '64'
      AND mestyp          EQ 'ZCCT'
*      AND budat           IS INITIAL
      INTO TABLE @DATA(lt_edidc).

  CHECK sy-subrc EQ 0.


  SELECT * FROM edid4
    FOR ALL ENTRIES IN @lt_edidc
    WHERE docnum = @lt_edidc-docnum
    INTO TABLE @DATA(lt_edid4).





  LOOP AT lt_edid4 INTO DATA(ls_edid4).
    CASE ls_edid4-segnam.

      WHEN 'ZCCTH'.
        CLEAR ls_zccth.
        lt_db_h-docnum = ls_edid4-docnum.
        MOVE ls_edid4-sdata TO ls_zccth.
        MOVE-CORRESPONDING ls_zccth TO lt_db_h.
        APPEND lt_db_h.
      WHEN 'ZCCTI'.
        CLEAR lt_db_i.
        lt_db_i-docnum = ls_edid4-docnum.
        MOVE ls_edid4-sdata TO ls_zccti.
        MOVE-CORRESPONDING ls_zccti TO lt_db_i.
        APPEND lt_db_i.
    ENDCASE.

  ENDLOOP.

  IF lt_db_h[] IS NOT INITIAL.
    MODIFY zcct_db_h FROM TABLE lt_db_h.
    MODIFY zcct_db_i FROM TABLE lt_db_i.
    COMMIT WORK AND WAIT.
  ENDIF.



ENDFORM.

FORM frm_hotspot_click USING e_column_id
                                    e_row_id STRUCTURE  lvc_s_row.
  CASE e_column_id.
    WHEN 'DOCNUM'.
      READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX e_row_id-index.
      IF sy-subrc EQ 0.
        CLEAR r_docnum[].
        r_docnum-low = <gs_head_dis>-docnum.
        APPEND r_docnum.
        SUBMIT zcct_ca_pos_monitor
          WITH s_docnum IN r_docnum
        AND RETURN.
      ENDIF.

    WHEN 'DOCNR'.
      READ TABLE gt_vgbel_dis ASSIGNING <gs_vgbel_dis> INDEX e_row_id-index.
      IF sy-subrc EQ 0.

        CHECK <gs_vgbel_dis>-docnr IS NOT INITIAL.

        CASE <gs_vgbel_dis>-swo_objtyp.
          WHEN '物料凭证'.
            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_mblnr = <gs_vgbel_dis>-docnr
                i_mjahr = <gs_vgbel_dis>-cjahr.
          WHEN '销售发票'.
            SET PARAMETER ID 'VF' FIELD <gs_vgbel_dis>-docnr.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          WHEN '会计凭证'.
            SET PARAMETER ID 'BLN' FIELD <gs_vgbel_dis>-docnr.
            SET PARAMETER ID 'BUK' FIELD <gs_vgbel_dis>-bukrs.
            SET PARAMETER ID 'GJR' FIELD <gs_vgbel_dis>-cjahr .
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDCASE.
      ENDIF.

    WHEN 'C_DOCNR'.
      READ TABLE gt_vgbel_dis ASSIGNING <gs_vgbel_dis> INDEX e_row_id-index.
      IF sy-subrc EQ 0.

        CHECK <gs_vgbel_dis>-c_docnr IS NOT INITIAL.

        CASE <gs_vgbel_dis>-swo_objtyp.
          WHEN '物料凭证'.
            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_mblnr = <gs_vgbel_dis>-c_docnr
                i_mjahr = <gs_vgbel_dis>-c_cjahr.
          WHEN '销售发票'.
            SET PARAMETER ID 'VF' FIELD <gs_vgbel_dis>-c_docnr.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          WHEN '会计凭证'.
            SET PARAMETER ID 'BLN' FIELD <gs_vgbel_dis>-c_docnr.
            SET PARAMETER ID 'BUK' FIELD <gs_vgbel_dis>-c_bukrs .
            SET PARAMETER ID 'GJR' FIELD <gs_vgbel_dis>-c_cjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDCASE.
      ENDIF.

  ENDCASE.

ENDFORM.


FORM f_display_detail.

  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CALL METHOD g_grid_up->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.
  IF lt_index_rows[] IS  INITIAL.
    RETURN.
  ENDIF.
  CLEAR: gt_item_dis[].
  CLEAR: gt_msg_dis[].
  CLEAR: gt_vgbel_dis[].
  LOOP AT lt_index_rows INTO ls_index_rows.
    READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX ls_index_rows-index .
    IF sy-subrc EQ  0.

      LOOP AT gt_item WHERE docnum = <gs_head_dis>-docnum.
        APPEND gt_item TO gt_item_dis.
      ENDLOOP.


      LOOP AT gt_msg WHERE docnum = <gs_head_dis>-docnum.
        APPEND gt_msg TO gt_msg_dis.
      ENDLOOP.

      LOOP AT gt_vgbel WHERE docnum = <gs_head_dis>-docnum.
        APPEND gt_vgbel TO gt_vgbel_dis.
      ENDLOOP.
    ENDIF.
  ENDLOOP.




ENDFORM.


FORM frm_call_posmon.

  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CLEAR:gt_item_dis[].
  CALL METHOD g_grid_up->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.

  IF lt_index_rows[] IS INITIAL.
    MESSAGE '请选择抬头行' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  CLEAR r_docnum[].

  LOOP AT lt_index_rows INTO ls_index_rows.
    READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX ls_index_rows-index.
    IF sy-subrc EQ 0.
      r_docnum-sign = 'I'.
      r_docnum-option = 'EQ'.
      r_docnum-low = <gs_head_dis>-docnum.
      APPEND r_docnum.
    ENDIF.
  ENDLOOP.
  IF r_docnum[] IS NOT INITIAL.
    SUBMIT zcct_ca_pos_monitor
        WITH s_docnum IN r_docnum
         AND RETURN.
  ENDIF.

ENDFORM.



FORM frm_idoc_post.
  DATA:et_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CALL METHOD g_grid_up->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.

  IF lt_index_rows[] IS INITIAL.
    MESSAGE '请选择抬头行' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  CLEAR gt_message[].
  CLEAR r_docnum[].

  LOOP AT lt_index_rows INTO ls_index_rows.
    READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX ls_index_rows-index.
    IF sy-subrc EQ 0.
      IF <gs_head_dis>-status <> '64' AND <gs_head_dis>-status <> '51'.
        PERFORM frm_add_msg USING 'ZCCT' 'W' '000' 'IDOC编号' <gs_head_dis>-docnum '状态无法处理' ''.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'ZCCT_IDOC_INBOUND_PROCESS'
        EXPORTING
          i_docnum  = <gs_head_dis>-docnum
        IMPORTING
          e_status  = <gs_head_dis>-status
        TABLES
          et_return = et_return.
      CASE <gs_head_dis>-status.
        WHEN '51'.
          <gs_head_dis>-icon = icon_red_light.
        WHEN '53'.
          <gs_head_dis>-icon = icon_green_light.
        WHEN '68'.
          <gs_head_dis>-icon = icon_dummy.
        WHEN '64'.
          <gs_head_dis>-icon = icon_yellow_light.
      ENDCASE.

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

    ENDIF.
  ENDLOOP.

  IF gt_message[] IS NOT INITIAL.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_message[].
    CLEAR gt_message[].

  ENDIF.
ENDFORM.


FORM frm_idoc_cancel.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  DATA:ls_txt TYPE char40.
  DATA:ls_budat TYPE budat.
  DATA:ls_yrq TYPE char1.
  DATA:ls_ret TYPE char1.

  DATA: et_return	TYPE TABLE OF	bapiret2 WITH HEADER LINE.

  DATA: lt_status TYPE TABLE OF bdidocstat WITH HEADER LINE.

  CALL METHOD g_grid_up->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.

  IF lt_index_rows[] IS INITIAL.
    MESSAGE '请选择抬头行' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM set_pop USING '确认冲销么?' CHANGING ls_budat ls_yrq ls_ret.

  CHECK ls_ret <> 'X'.
  IF ls_yrq = 'X'.
    CLEAR ls_budat.
  ENDIF.


  CLEAR gt_message[].
  CLEAR r_docnum[].

  LOOP AT lt_index_rows INTO ls_index_rows.
    READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX ls_index_rows-index.
    IF sy-subrc EQ 0.
      IF <gs_head_dis>-zcct_type+0(1) = 'S'.
        PERFORM frm_add_msg USING 'ZCCT' 'E' '000' 'IDOC' <gs_head_dis>-docnum '为结算,无法在此处冲销' ''.
        CONTINUE.
      ENDIF.
      IF <gs_head_dis>-status = '64' OR <gs_head_dis>-status = '51'.

        CLEAR lt_status[].
        lt_status-docnum = <gs_head_dis>-docnum.
        lt_status-status = '68'.
        APPEND lt_status.

        CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
          EXPORTING
            idoc_number               = <gs_head_dis>-docnum
          TABLES
            idoc_status               = lt_status
          EXCEPTIONS
            idoc_foreign_lock         = 1
            idoc_not_found            = 2
            idoc_status_records_empty = 3
            idoc_status_invalid       = 4
            db_error                  = 5
            OTHERS                    = 6.
        IF sy-subrc <> 0.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'  .

          PERFORM frm_add_msg USING 'ZCCT' 'E' '000'
            'IDOC'
            <gs_head_dis>-docnum
            '作废失败'
            ''.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          <gs_head_dis>-status = '68'.
          <gs_head_dis>-icon = icon_dummy.

          PERFORM frm_add_msg USING 'ZCCT' 'S' '000' 'IDOC编号' <gs_head_dis>-docnum '已作废' ''.
        ENDIF.

        CONTINUE.

      ELSEIF <gs_head_dis>-status = '53'.
        CALL FUNCTION 'ZCCT_CANCEL'
          EXPORTING
            im_long_vgbel = <gs_head_dis>-long_vgbel
*           IM_VGBEL      =
            im_cct_type   = <gs_head_dis>-zcct_type
            im_post_date  = ls_budat
*           IM_COMMIT     =
*           IM_IDOCNUM    =
*           IM_MESCOD     =
          TABLES
            et_return     = et_return
          EXCEPTIONS
            error         = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          PERFORM frm_add_msg USING 'ZCCT' 'E' '000' 'IDOC' <gs_head_dis>-docnum '冲销错误' ''.
          LOOP AT et_return WHERE type = 'E'.
            PERFORM frm_add_msg USING et_return-id
                                      et_return-type
                                      et_return-number
                                      et_return-message_v1
                                      et_return-message_v2
                                      et_return-message_v3
                                      et_return-message_v4.
          ENDLOOP.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'  .

        ELSE.

          PERFORM frm_add_msg USING 'ZCCT' 'S' '000' 'IDOC' <gs_head_dis>-docnum '冲销成功' ''.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

        ENDIF.

      ELSE.

        PERFORM frm_add_msg USING 'ZCCT' 'E' '000' 'IDOC' <gs_head_dis>-docnum '状态无法冲销' ''.
      ENDIF.


    ENDIF.
  ENDLOOP.

  IF gt_message[] IS NOT INITIAL.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_message[].
    CLEAR gt_message[].

  ENDIF.


ENDFORM.


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



FORM set_pop USING txt CHANGING ls_budat ls_yrq ls_ret.

  DATA:ls_col TYPE lvc_s_col.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA:p_gv_ret_code TYPE char1.
  CLEAR lt_flds.
  ls_flds-tabname = 'MKPF'.
  ls_flds-fieldname = 'BUDAT'.
  ls_flds-value = sy-datum.
  ls_flds-fieldtext = '冲销日期'.
  APPEND ls_flds TO lt_flds.

  CLEAR ls_flds.
  ls_flds-tabname = 'MKPF'.
  ls_flds-fieldname = 'FLS_RSTO'.
  ls_flds-value = 'X'.
  ls_flds-field_attr = '01'.
  ls_flds-fieldtext = '保持原有日期'.
  APPEND ls_flds TO lt_flds.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = txt
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF p_gv_ret_code = 'A'.
    MESSAGE '操作已取消' TYPE 'S'.
    ls_ret = 'X'.
  ENDIF.

  LOOP AT lt_flds INTO ls_flds.
    CASE ls_flds-fieldname.
      WHEN 'BUDAT'.
        ls_budat = ls_flds-value.

      WHEN 'FLS_RSTO'.
        ls_yrq = ls_flds-value.
*      WHEN 'BKTXT'.
*        gv_bktxt = ls_flds-value.
    ENDCASE.

  ENDLOOP.

ENDFORM.
