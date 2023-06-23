FUNCTION zcct_idoc_input.
*"----------------------------------------------------------------------
*"*"全局接口：
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWFAP_PAR-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWFAP_PAR-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"      IDOC_SEGINFO STRUCTURE  EDISEGINFO OPTIONAL
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------

  DATA: ls_zccth TYPE  zccth.
  DATA: ls_zccti TYPE  zccti.
  DATA: ls_zccte TYPE  zccte.

  DATA: ls_db_h TYPE zcct_db_h.
  DATA: lt_db_i TYPE TABLE OF zcct_db_i WITH HEADER LINE.



  DATA: lt_dayend_goods_check TYPE TABLE OF zcct_dayend_goods_check WITH HEADER LINE.
  DATA: ls_js TYPE zcct_js.
  DATA: ls_block TYPE zcct_block.
  DATA: ls_bukrs TYPE bukrs.

  DATA:ls_date TYPE sy-datum.

  DATA:et_return TYPE TABLE OF bapiret2 WITH HEADER LINE  .

  DATA: lt_data TYPE TABLE OF zcct_data WITH HEADER LINE.
  DATA: lt_line_data TYPE TABLE OF zcct_line_data WITH HEADER LINE.


  LOOP AT idoc_contrl.
    CLEAR ls_js.
    CLEAR ls_zccth.
    CLEAR ls_block.
    CLEAR ls_db_h.
    CLEAR lt_db_i.
    CLEAR lt_db_i[].
    CLEAR:gt_jms,gt_jms[].
    CLEAR gv_tax_rate.
    CLEAR gv_long_vgbel2.
    CLEAR:gv_rk_date,gv_rk_remark.

    gv_docnum = idoc_contrl-docnum.

    CLEAR lt_dayend_goods_check[].
    LOOP AT idoc_data WHERE docnum = idoc_contrl-docnum.
      CASE idoc_data-segnam.
        WHEN 'ZCCTH'.
          CLEAR ls_zccth.
          MOVE idoc_data-sdata TO ls_zccth.
          MOVE-CORRESPONDING ls_zccth TO ls_db_h.

          IF ls_db_h-zcct_type = '1001' OR ls_db_h-zcct_type = '1002'.
            gv_rk_date = ls_db_h-remark02.
            gv_rk_remark = ls_db_h-remark03.
          ENDIF.

          ls_db_h-docnum = idoc_contrl-docnum.
          gv_zccth = ls_zccth.
          SELECT SINGLE * INTO ls_js FROM zcct_js WHERE zcct_type = ls_zccth-zcct_type.

        WHEN 'ZCCTI'.
          CLEAR ls_zccti.
          CLEAR lt_db_i.
          CLEAR lt_data.
          MOVE idoc_data-sdata TO ls_zccti.

          MOVE-CORRESPONDING ls_zccti TO lt_db_i.

          lt_db_i-docnum = idoc_contrl-docnum.

          APPEND lt_db_i.
          CLEAR lt_db_i.

          gv_long_vgbel2 = ls_zccti-long_vgbel.
          MOVE-CORRESPONDING ls_zccti TO lt_data.

          IF ls_zccti-satnr IS NOT INITIAL OR ls_zccti-ean11 IS NOT INITIAL.
*            CALL FUNCTION 'ZMDA_GET_MATNR'
*              EXPORTING
*                i_satnr = ls_zccti-satnr
*                i_color = ls_zccti-color
*                i_size1 = ls_zccti-size1
*                i_size2 = ls_zccti-size2
*                i_ean11 = ls_zccti-ean11
*              IMPORTING
*                e_matnr = lt_data-matnr
*              EXCEPTIONS
*                notfind = 1
*                OTHERS  = 2.
            lt_data-matnr = ls_zccti-satnr.
*            IF sy-subrc <> 0.
*              PERFORM add_message_idoc TABLES idoc_status
*                USING idoc_contrl-docnum '51' 'ZCCT' '016' '' '' '' '' idoc_data-segnum.
*            ENDIF.
          ENDIF.

*
*          IF ls_js-zrj_flag IS NOT INITIAL.
*            CLEAR lt_dayend_goods_check.
*            lt_dayend_goods_check-satnr = ls_zccti-satnr.
*            lt_dayend_goods_check-color = ls_zccti-color.
*            lt_dayend_goods_check-size1 = ls_zccti-size1.
*            lt_dayend_goods_check-size2 = ls_zccti-size2.
*            lt_dayend_goods_check-menge = ls_zccti-menge.
*            lt_dayend_goods_check-netwr = ls_zccti-netwr.
*            IF ls_js-zfhth = 'T'.
*              lt_dayend_goods_check-menge = - lt_dayend_goods_check-menge.
*              lt_dayend_goods_check-netwr = - lt_dayend_goods_check-netwr.
*            ENDIF.
*
*
*
*            COLLECT lt_dayend_goods_check.
*          ENDIF.


          APPEND lt_data.
          CLEAR lt_data.

          "加盟商发货标识更新
*          IF gv_zccth-zcct_type = '3001'.
*            gt_jms-long_vgbel = ls_zccti-long_vgbel.
*            APPEND gt_jms.
*            CLEAR:gt_jms.
*          ENDIF.
        WHEN 'ZCCTE'.
          CLEAR ls_zccte.
          CLEAR lt_line_data.

          MOVE idoc_data-sdata TO ls_zccte.

          PERFORM fix_cg_tax_rate USING ls_zccth ls_zccte.

          MOVE-CORRESPONDING ls_zccte TO lt_line_data.
          APPEND lt_line_data.
      ENDCASE.
    ENDLOOP.

    PERFORM fix_cg_tax_code USING ls_zccth .



*    PERFORM save_zcct_db TABLES lt_db_i[] USING ls_db_h.


**********冻结检查
    SELECT SINGLE * INTO ls_block FROM zcct_block WHERE zcct_type = ls_zccth-zcct_type
      AND zcct_block_sta <= ls_zccth-budat AND zcct_block_end >= ls_zccth-budat
      AND bukrs = ''.
    IF sy-subrc EQ 0.
      IF ls_block-zcct_block_flag = 'X'.
        PERFORM add_message_idoc TABLES idoc_status
            USING idoc_contrl-docnum '51' 'ZCCT' '023'
                  ls_zccth-zcct_type
                  ls_block-zcct_block_sta
                  ls_block-zcct_block_end
                   ''
                  idoc_data-segnum.
      ELSEIF ls_block-zcct_conv_budat IS NOT INITIAL.
        ls_zccth-budat = ls_block-zcct_conv_budat.
      ENDIF.
    ENDIF.

    SELECT SINGLE bukrs INTO ls_bukrs FROM zscmt0010 WHERE partner = ls_zccth-channel1.
    IF sy-subrc EQ 0.
      SELECT SINGLE * INTO ls_block FROM zcct_block WHERE zcct_type = ls_zccth-zcct_type
        AND zcct_block_sta <= ls_zccth-budat AND zcct_block_end >= ls_zccth-budat
        AND bukrs = ls_bukrs.
      IF sy-subrc EQ 0.
        IF ls_block-zcct_block_flag = 'X'.
          PERFORM add_message_idoc TABLES idoc_status
              USING idoc_contrl-docnum '51' 'ZCCT' '023'
                    ls_zccth-zcct_type
                    ls_block-zcct_block_sta
                    ls_block-zcct_block_end
                     ''
                    idoc_data-segnum.
        ELSEIF ls_block-zcct_conv_budat IS NOT INITIAL.
          ls_zccth-budat = ls_block-zcct_conv_budat.
        ENDIF.
      ENDIF.
    ENDIF.
**********冻结检查


    READ TABLE idoc_status WITH KEY docnum = idoc_contrl-docnum  status = 51.
    IF sy-subrc EQ 0.
      PERFORM get_next_lognr CHANGING gv_lognr. "获取唯一日志流水编号
      PERFORM get_next_vgbel USING ls_zccth-long_vgbel
                           CHANGING gv_vgbel. "获取并校验唯一参考。生成内部参考
      PERFORM save_msg_log TABLES et_return.
      CONTINUE.
    ENDIF.


    ls_date = ls_zccth-budat.
*
*    IF ls_js-zzhlx = 'A'.
*
*      PERFORM fix_rj TABLES lt_data
*                            lt_dayend_goods_check
*                            idoc_status
*                     USING ls_zccth
*                           ls_js-zfhth
*                           idoc_contrl.
*      PERFORM check_dayend  TABLES lt_dayend_goods_check
*                                   idoc_contrl
*                                   idoc_status
*                            USING ls_date
*                                  ls_zccth-channel1
*                                  ls_js-zfhth.
*
*      READ TABLE idoc_status WITH KEY docnum = idoc_contrl-docnum  status = 51.
*      IF sy-subrc EQ 0.
*        PERFORM get_next_lognr CHANGING gv_lognr. "获取唯一日志流水编号
*        PERFORM get_next_vgbel USING ls_zccth-long_vgbel
*                             CHANGING gv_vgbel. "获取并校验唯一参考。生成内部参考
*        PERFORM save_msg_log TABLES et_return.
*        CONTINUE.
*      ELSE.
*        READ TABLE idoc_status WITH KEY docnum = idoc_contrl-docnum  status = 53.
*        IF sy-subrc EQ 0.
*          PERFORM get_next_lognr CHANGING gv_lognr. "获取唯一日志流水编号
*          PERFORM get_next_vgbel USING ls_zccth-long_vgbel
*                               CHANGING gv_vgbel. "获取并校验唯一参考。生成内部参考
*          PERFORM save_msg_log TABLES et_return.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*    ENDIF.


    IF ls_zccth-canceled IS INITIAL.

      CALL FUNCTION 'ZCCT_POST'
        EXPORTING
          im_channel1   = ls_zccth-channel1
          im_channel2   = ls_zccth-channel2
          im_channel3   = ls_zccth-channel3
          im_cct_type   = ls_zccth-zcct_type
          im_post_date  = ls_date
          im_bktxt      = ls_zccth-remark01
          im_long_vgbel = ls_zccth-long_vgbel
          im_vkorg      = ls_zccth-vkorg
          im_werks      = ls_zccth-werks
          im_idocnum    = idoc_contrl-docnum
          im_mescod     = idoc_contrl-mescod
*         IM_COMMIT     =
*     IMPORTING
*         EX_LOGNR      =
*         EX_VGBEL      =
        TABLES
          im_data       = lt_data
*         im_line_data  = lt_line_data
          et_return     = et_return
        EXCEPTIONS
          error         = 1
          OTHERS        = 2.


    ELSE.

      CALL FUNCTION 'ZCCT_CANCEL'
        EXPORTING
          im_long_vgbel = ls_zccth-long_vgbel
*         IM_VGBEL      =
          im_cct_type   = ls_zccth-zcct_type
          im_post_date  = ls_date
          im_idocnum    = idoc_contrl-docnum
          im_mescod     = idoc_contrl-mescod
*         IM_COMMIT     =
        TABLES
          et_return     = et_return
        EXCEPTIONS
          error         = 1
          OTHERS        = 2.

    ENDIF.



    LOOP AT et_return WHERE type = 'E'.
      PERFORM add_message_idoc TABLES idoc_status
         USING idoc_contrl-docnum
               '51'
              et_return-id
              et_return-number
              et_return-message_v1
              et_return-message_v2
              et_return-message_v3
              et_return-message_v4
              ''.
    ENDLOOP.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CONTINUE.

    ELSE.

      IF ls_zccth-canceled IS INITIAL.
        PERFORM update_jms_fh USING 'X'
                                    ls_date.
      ELSE.
        PERFORM update_jms_fh USING ''
                                    ls_date.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      PERFORM add_message_idoc TABLES idoc_status
       USING idoc_contrl-docnum
             '53'
             'ZCCT'
             '000'
             '过账成功'
             ''
             ''
             ''
             ''.

    ENDIF.
  ENDLOOP.

ENDFUNCTION.


FORM add_message_idoc TABLES idoc_status STRUCTURE bdidocstat
                      USING docnum status msgid msgno msgv1 msgv2 msgv3 msgv4 row.
  DATA: ls_status TYPE bdidocstat.
  ls_status-docnum = docnum.
  ls_status-status = status.
  ls_status-msgid = msgid.
  ls_status-msgno = msgno.
  ls_status-msgv1 = msgv1.
  ls_status-msgv2 = msgv2.
  ls_status-msgv3 = msgv3.
  ls_status-msgv4 = msgv4.
  APPEND ls_status TO idoc_status.
  CLEAR ls_status.

  IF status = '51'.
    PERFORM add_message USING 'E' msgid msgno msgv1 msgv2 msgv3 msgv4.
  ELSEIF status = '53'.
    PERFORM add_message USING 'S' msgid msgno msgv1 msgv2 msgv3 msgv4.
  ENDIF.



ENDFORM.

FORM save_zcct_db TABLES lt_db_i STRUCTURE zcct_db_i
                                USING ls_db_h STRUCTURE zcct_db_h.

  IF ls_db_h IS NOT INITIAL.
    MODIFY zcct_db_h FROM ls_db_h.
  ENDIF.


  IF lt_db_i[] IS NOT INITIAL.
    MODIFY zcct_db_i FROM TABLE lt_db_i.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.

FORM update_jms_fh USING pv_fhbj
                         pv_date TYPE datum.
*  DATA:lv_pick_order LIKE zscm_xdjc_0001-pick_order.
*  DATA:lv_zfhdat LIKE zscm_xdjc_0001-zfhdat.
**  DATA:lv_zfhzet LIKE zscm_xdjc_0001-zfhzet.
*
*  SORT gt_jms BY long_vgbel.
*  DELETE ADJACENT DUPLICATES FROM gt_jms COMPARING long_vgbel.
*
*  CLEAR:lv_zfhdat.
*  IF pv_fhbj = 'X'.
*    lv_zfhdat = pv_date.
*  ENDIF.
*
*  LOOP AT gt_jms.
*
*    lv_pick_order = gt_jms-long_vgbel.
*
*    UPDATE zscm_xdjc_0001
*       SET zfhbj = pv_fhbj
*           zfhdat = lv_zfhdat
*     WHERE pick_order = lv_pick_order.
*
*  ENDLOOP.

ENDFORM.
