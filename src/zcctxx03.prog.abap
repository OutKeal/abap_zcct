*&---------------------------------------------------------------------*
*& 包含               ZCCTXX03
*&---------------------------------------------------------------------*

FORM js_doc_create.

  CASE gs_js-zzhlx.
    WHEN 'A'."日结小票
      PERFORM write_jsxp_data.
    WHEN 'B'."加盟商发货
      PERFORM write_jms_data.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM check_dayend  TABLES lt_dayend_goods_check  STRUCTURE zcct_dayend_goods_check
                           idoc_contrl STRUCTURE edidc
                           idoc_status STRUCTURE bdidocstat
                      USING ls_date
                            ls_channel1
                            ls_zfhth.
  DATA: it_dayend_goods_check TYPE TABLE OF zcct_dayend_goods_check WITH HEADER LINE.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = ls_channel1
*    IMPORTING
*      output = ls_channel1.
*  CLEAR it_dayend_goods_check[].
*
*  SELECT satnr,
*         color,
*         size1,
*         size2,
*         SUM( menge ) AS menge,
*         SUM( netwr ) AS netwr
*    INTO TABLE @it_dayend_goods_check
*    FROM zdayend_head AS h
*    INNER JOIN zdayend_goods AS i ON h~long_vgbel =  i~long_vgbel
*    WHERE sale_date = @ls_date
*    AND ship_channel = @ls_channel1
*    AND zfhth = @ls_zfhth
*    AND zrjzt IN ( 'A' , 'B' )
*    GROUP BY satnr ,color ,size1 ,size2.
*
*  DELETE it_dayend_goods_check WHERE menge = 0.
*  DELETE lt_dayend_goods_check WHERE menge = 0.
*  SORT it_dayend_goods_check BY satnr color size1 size2.
*  SORT lt_dayend_goods_check BY satnr color size1 size2.
*
*
*  IF it_dayend_goods_check[] <> lt_dayend_goods_check[].
*    PERFORM add_message_idoc TABLES idoc_status
*      USING idoc_contrl-docnum '51' 'ZCCT' '019' '' '' '' '' ''.
*  ENDIF.


ENDFORM.

FORM write_jsxp_data .
*  DATA: lv_tax TYPE zcct_tax_rate.
*
*  DATA:lv_error,
*       lv_msg TYPE bapi_msg.
*  DATA:lt_channel LIKE STANDARD TABLE OF zsdayend_channel WITH HEADER LINE.
*  DATA:lt_dayend_conf LIKE STANDARD TABLE OF zsdayend_conf WITH HEADER LINE.
*  DATA:lt_dayend_conf_tmp LIKE STANDARD TABLE OF zsdayend_conf WITH HEADER LINE.
*
*  lt_channel-channel = gv_channel1.
*  APPEND lt_channel.
*  CLEAR:lt_channel.
*
*  CALL FUNCTION 'ZDAYEND_GET_INFO'
*    EXPORTING
*      iv_sdate_b     = gv_post_date
**     IV_SDATE_E     =
**     IV_REDO        =
*    IMPORTING
*      ev_type        = lv_error
*      ev_msg         = lv_msg
*    TABLES
*      it_channel     = lt_channel[]
**     IT_XP          =
*      et_dayend_conf = lt_dayend_conf[].
*
*
*  IF lv_error = 'E'.
*    PERFORM add_message USING 'E' 'ZCCT' '000' lv_msg '' '' ''.
*    RETURN.
*  ENDIF.
*
*  READ TABLE lt_dayend_conf WITH KEY type = 'E'.
*  IF sy-subrc = 0.
*    PERFORM add_message USING 'E' 'ZCCT' '000' lt_dayend_conf-msg '' '' ''.
*    RETURN.
*  ENDIF.
*
*  IF NOT lt_dayend_conf[] IS INITIAL.
*
*    CALL FUNCTION 'ZDAYEND_CALCULATE_JE'
*      TABLES
*        ct_dayend_conf = lt_dayend_conf[].
*  ENDIF.
*
**  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_dayend_conf FROM zdayend_conf
**      WHERE sale_date = gv_post_date
**      AND channel = gv_channel1
**      AND zrjzt = 'B'.
*
**---成本分摊
*  DATA:lt_rule LIKE STANDARD TABLE OF zcct_sa_rule WITH HEADER LINE .
*  DATA:lt_amount LIKE STANDARD TABLE OF zcct_sa_amount WITH HEADER LINE.
*
*  CALL FUNCTION 'ZCCT_GET_TAX'
*    EXPORTING
**     I_TAX_CODE =
*      i_tax_type = 'J'
**     I_TXT_RATE =
*    IMPORTING
**     E_TAX_CODE =
*      e_tax_rate = lv_tax
**   EXCEPTIONS
**     ERROR      = 1
**     OTHERS     = 2
*    .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*
*  IF gt_mb_cost[] IS NOT INITIAL.
*
*    lt_rule-to_fname = 'ZCBJE'.
*    lt_rule-key_fname = 'MATNR'.
*    lt_rule-fr_fname = 'MENGE'.
*    APPEND lt_rule.
*
*    lt_rule-to_fname = 'ZHSCBJE'.
*    lt_rule-key_fname = 'MATNR'.
*    lt_rule-fr_fname = 'MENGE'.
*    APPEND lt_rule.
*
*    LOOP AT gt_mb_cost.
*      lt_amount-to_fname = 'ZCBJE'.
*      lt_amount-key_value = gt_mb_cost-matnr.
*      lt_amount-amount = gt_mb_cost-netwr.
*      IF gs_js-zfhth = 'T'.
*        lt_amount-amount = 0 - lt_amount-amount.
*      ENDIF.
*      COLLECT lt_amount.
*      CLEAR:lt_amount.
*
*      lt_amount-to_fname = 'ZHSCBJE'.
*      lt_amount-key_value = gt_mb_cost-matnr.
*
*      lt_amount-amount = gt_mb_cost-netwr * ( 100 + lv_tax ) / 100.
*      IF gs_js-zfhth = 'T'.
*        lt_amount-amount = 0 - lt_amount-amount.
*      ENDIF.
*      COLLECT lt_amount.
*      CLEAR:lt_amount.
*
*
*    ENDLOOP.
*
*    CLEAR:lt_dayend_conf_tmp[].
**----成本只按照单方向zfhth去做的，所以只能做一半；后续第二次做另一半，所以需要剔除
*    LOOP AT lt_dayend_conf.
*      IF lt_dayend_conf-zfhth NE gs_js-zfhth OR
*         lt_dayend_conf-channel NE gv_channel1.
*
*        lt_dayend_conf_tmp = lt_dayend_conf.
*        APPEND lt_dayend_conf_tmp.
*
*        DELETE lt_dayend_conf.
*      ENDIF.
*    ENDLOOP.
*
*    IF lt_dayend_conf[] IS NOT INITIAL.
*      CALL FUNCTION 'ZCCT_SPLIT_AMOUNT'
*        TABLES
*          it_sa_rule   = lt_rule
*          it_sa_amount = lt_amount
*          ct_tab       = lt_dayend_conf
*        EXCEPTIONS
*          error        = 1
*          OTHERS       = 2.
*      IF sy-subrc <> 0.
*        PERFORM add_message USING 'E' 'ZCCT' '000' '代码处理异常' '' '' ''.
*        RETURN.
*      ENDIF.
*    ENDIF.
*
*
*    IF lt_dayend_conf_tmp[] IS NOT INITIAL.
*      APPEND LINES OF lt_dayend_conf_tmp TO lt_dayend_conf.
*    ENDIF.
*
*  ENDIF.
*
**---
*  CALL FUNCTION 'ZDAYEND_CONF_UPDATE'
*    EXPORTING
**     IV_COMMIT      =
*      iv_zfhth       = gs_js-zfhth
*      iv_long_vgbel  = gv_long_vgbel
*      channel        = gv_channel1
*    TABLES
*      it_dayend_conf = lt_dayend_conf.

ENDFORM.

FORM write_jms_data .
*
*  DATA:lt_dayend_conf LIKE STANDARD TABLE OF zjs_line.
*
*  CALL FUNCTION 'ZJMS_GET_INFO'
*    EXPORTING
*      iv_kunnr        = gv_cct_bl_kunnr
*      iv_long_vgbel   = gv_long_vgbel
*      iv_sdate        = gv_post_date
*      iv_zbu_typ      = 'S05'
*      iv_zdata_source = 'R3'
*      iv_zfhth        = gs_js-zfhth
*    TABLES
*      et_dayend_conf  = lt_dayend_conf
*      it_data         = gt_data.
*
*  LOOP AT lt_dayend_conf ASSIGNING FIELD-SYMBOL(<fs_conf>).
*    READ TABLE gt_mb_cost WITH KEY line_id = <fs_conf>-line_id.
*    IF sy-subrc = 0.
*      IF gs_js-zfhth = 'T'.
*        <fs_conf>-zcbje = - gt_mb_cost-netwr.
*      ELSEIF gs_js-zfhth = 'S'.
*        <fs_conf>-zcbje = gt_mb_cost-netwr.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CALL FUNCTION 'ZJMS_CONF_UPDATE'
**     EXPORTING
**       IV_COMMIT            =
*    TABLES
*      it_dayend_conf = lt_dayend_conf.

ENDFORM.
