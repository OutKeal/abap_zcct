FUNCTION zcct_cancel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IM_LONG_VGBEL) TYPE  ZLONG_VGBEL
*"     VALUE(IM_VGBEL) TYPE  VGBEL OPTIONAL
*"     VALUE(IM_CCT_TYPE) TYPE  ZCCT_TYPE OPTIONAL
*"     VALUE(IM_POST_DATE) TYPE  BUDAT OPTIONAL
*"     VALUE(IM_COMMIT) TYPE  CHAR1 OPTIONAL
*"     VALUE(IM_IDOCNUM) TYPE  EDI_DOCNUM OPTIONAL
*"     VALUE(IM_MESCOD) TYPE  EDI_MESCOD OPTIONAL
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  PERFORM init.

  IF im_idocnum IS NOT INITIAL.  "传入信息
    PERFORM set_idoc_info USING im_idocnum im_mescod.
  ENDIF.

  gv_cancel = 'X'.
  gv_skip_mb_enhance = 'X'.
  PERFORM get_next_lognr CHANGING gv_lognr.

  gv_cct_type = im_cct_type.
  gv_vgbel = im_vgbel.
  gv_long_vgbel = im_long_vgbel.



  SELECT SINGLE * FROM zcct_vgbel_head INTO gs_vgbel_head
    WHERE long_vgbel = gv_long_vgbel
    AND zcct_type = gv_cct_type
    AND stokz <> 'X'.
  IF sy-subrc NE 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' gv_long_vgbel '不存在未冲销记录'  ' ' ' '.
    PERFORM save_msg_log TABLES et_return.
    RAISE error.
  ELSEIF gs_vgbel_head-stokz = 'X'.
    PERFORM add_message USING 'W' 'ZCCT' '000' gv_long_vgbel '已被取消'  ' ' ' '.
    PERFORM save_msg_log TABLES et_return.
    RETURN.
  ENDIF.

  IF im_post_date IS NOT INITIAL.
    gv_post_date = im_post_date.
  ELSE.

    SELECT SINGLE budat FROM zcct_db_h
      WHERE docnum = @gs_vgbel_head-docnum
      INTO @gv_post_date.

    IF sy-subrc NE 0 OR gs_vgbel_head-docnum IS INITIAL.
      gv_post_date = sy-datum.
    ENDIF.

  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vgbel_log FROM zcct_vgbel_log
      WHERE lognr = gs_vgbel_head-lognr.

  IF sy-subrc NE 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' gv_long_vgbel '不存在未冲销记录'  ' ' ' '.
    PERFORM save_msg_log TABLES et_return.
    RAISE error.
  ENDIF.


  LOOP AT gt_vgbel_log.
    CHECK gt_vgbel_log-stokz <> 'X'.
    CASE gt_vgbel_log-swo_objtyp.
      WHEN 'IDOC'.
        PERFORM mb_idoc_cancel CHANGING gt_vgbel_log.
        IF gv_error = 'X'.
          PERFORM save_msg_log TABLES et_return.
          RAISE error.
        ENDIF.
      WHEN 'BUS2017'.
        PERFORM mb_doc_cancel CHANGING gt_vgbel_log.
        IF gv_error = 'X'.
          PERFORM save_msg_log TABLES et_return.
          RAISE error.
        ENDIF.
      WHEN 'BKPF'.
        PERFORM ac_doc_cancel CHANGING gt_vgbel_log.
        IF gv_error = 'X'.
          PERFORM save_msg_log TABLES et_return.
          RAISE error.
        ENDIF.
      WHEN 'VBRK'.
        PERFORM bill_cancel CHANGING gt_vgbel_log.
        IF gv_error = 'X'.
          PERFORM save_msg_log TABLES et_return.
          RAISE error.
        ENDIF.
    ENDCASE.
    MODIFY gt_vgbel_log.

  ENDLOOP.

  SELECT SINGLE * INTO gs_js FROM zcct_js
    WHERE zcct_type = gv_cct_type.
  IF sy-subrc EQ 0.
    PERFORM js_doc_cancel .
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES et_return.
      RAISE error.
    ENDIF.
  ENDIF.


  IF gv_error IS INITIAL.
    UPDATE  zcct_vgbel_head  SET stokz = 'X'
                                 modat = sy-datum
                                 mozet = sy-uzeit
                                 monam = sy-uname
                                 WHERE  lognr = gs_vgbel_head-lognr.
    MODIFY zcct_vgbel_log FROM TABLE gt_vgbel_log.
    PERFORM add_message USING 'S' 'ZCCT' '010' gv_vgbel '' '' ''.
    PERFORM save_msg_log TABLES et_return.

    IF im_commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'
*       IMPORTING
*         RETURN        =
        .
    ENDIF.
  ENDIF.


ENDFUNCTION.

FORM js_doc_cancel.

*  DATA:lv_error,
*       lv_msg TYPE bapi_msg.
*
*
*  CASE gs_js-zzhlx.
*    WHEN 'A'."日结小票
*      SELECT * INTO TABLE @DATA(lt_conf)
*        FROM zdayend_conf WHERE zrjno = @gv_long_vgbel
*        AND zfhth = @gs_js-zfhth.
*      IF sy-subrc NE 0.
*        PERFORM add_message USING 'E' 'ZCCT' '021' gv_long_vgbel ''  '' '' .
*        RETURN.
*      ENDIF.
*
*      LOOP AT lt_conf INTO DATA(ls_conf) WHERE zqr_status = 'C'.
*      ENDLOOP.
*      IF sy-subrc EQ 0 .
*        PERFORM add_message USING 'E' 'ZCCT' '022' gv_long_vgbel ''  '' '' .
*        RETURN.
*      ENDIF.
*
*      SELECT * INTO TABLE @DATA(lt_goods)
*        FROM zdayend_goods
*        WHERE zrjno = @gv_long_vgbel
*        AND zfhth = @gs_js-zfhth.
*      IF sy-subrc NE 0.
*        PERFORM add_message USING 'E' 'ZCCT' '021' gv_long_vgbel ''  '' '' .
*        RETURN.
*      ENDIF.
*
*      LOOP AT lt_goods INTO DATA(ls_goods) WHERE zrjzt = 'A'.
*      ENDLOOP.
*      IF sy-subrc EQ 0 .
*        PERFORM add_message USING 'E' 'ZCCT' '022' gv_long_vgbel ''  '' '' .
*        RETURN.
*      ENDIF.
*
*      DELETE FROM zdayend_conf WHERE zrjno = gv_long_vgbel
*                               AND zfhth = gs_js-zfhth.
*
*      LOOP AT lt_goods INTO ls_goods WHERE zfhth = gs_js-zfhth.
*        UPDATE zdayend_goods SET zrjno = ''  zrjzt = 'A'
*         WHERE long_vgbel = ls_goods-long_vgbel
*         AND line_id = ls_goods-line_id.
*      ENDLOOP.
*    WHEN 'B'."加盟商发货
*      CALL FUNCTION 'ZJMS_CONF_CANCEL'
*        EXPORTING
*          iv_long_vgbel = gv_long_vgbel
*          iv_budat      = gv_post_date
**         IV_COMMIT     =
*        IMPORTING
*          ev_type       = lv_error
*          ev_msg        = lv_msg.
*      IF lv_error = 'E'.
*        PERFORM add_message USING 'E' 'ZCCT' '000' lv_msg '' '' ''.
*        RETURN.
*      ENDIF.
*    WHEN OTHERS.
*  ENDCASE.

ENDFORM.



FORM mb_idoc_cancel CHANGING i_vgbel_log TYPE zcct_vgbel_log.

  DATA: lt_status TYPE TABLE OF bdidocstat WITH HEADER LINE.
  DATA:lv_idocnum TYPE edi_docnum.
  lv_idocnum = i_vgbel_log-swo_typeid.

  SELECT SINGLE status INTO @DATA(lv_status)
    FROM edidc WHERE docnum = @lv_idocnum.
  IF sy-subrc NE 0.
    gv_error = 'X'.
    PERFORM add_message USING 'E' 'ZCCT' '000'
          'IDOC'
          lv_idocnum
          '不存在'
          ''.
    EXIT.
  ELSEIF lv_status <> '53'.
    PERFORM add_message USING 'E' 'ZCCT' '000'
      'IDOC'
      lv_idocnum
      '不是已过账状态，无法取消'
      ''.
    EXIT.
  ENDIF.



  lt_status-docnum = lv_idocnum.
  lt_status-status = '68'.
  APPEND lt_status.

  CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
    EXPORTING
      idoc_number               = lv_idocnum
*     IDOC_OPENED_FLAG          = ' '
*     NO_DEQUEUE_FLAG           = 'X'
* IMPORTING
*     IDOC_CONTROL              =
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
    PERFORM add_message USING 'E' 'ZCCT' '000'
      'IDOC'
      lv_idocnum
      '取消失败'
      ''.
  ELSE.
    i_vgbel_log-c_docnr = gv_docnum.
    i_vgbel_log-stokz = 'X'.
  ENDIF.



ENDFORM.

FORM mb_doc_cancel CHANGING i_vgbel_log TYPE zcct_vgbel_log.

  DATA: ls_head TYPE bapi2017_gm_head_ret.
  DATA: lt_item TYPE TABLE OF bapi2017_gm_item_04 WITH HEADER LINE.

  SELECT zeile INTO TABLE lt_item
    FROM mseg
    WHERE mblnr = i_vgbel_log-docnr
    AND mjahr = i_vgbel_log-cjahr.
  SORT lt_item DESCENDING.



  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = i_vgbel_log-docnr
      matdocumentyear     = i_vgbel_log-cjahr
      goodsmvt_pstng_date = gv_post_date
      goodsmvt_pr_uname   = sy-uname
*     DOCUMENTHEADER_TEXT =
    IMPORTING
      goodsmvt_headret    = ls_head
    TABLES
      return              = ot_return
      goodsmvt_matdocitem = lt_item[].
  READ TABLE ot_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    gv_error = 'X'.
  ELSE.
    i_vgbel_log-c_docnr = ls_head-mat_doc.
    i_vgbel_log-c_cjahr = ls_head-doc_year.
    i_vgbel_log-stokz = 'X'.
  ENDIF.




ENDFORM.


FORM bill_cancel CHANGING i_vgbel_log TYPE zcct_vgbel_log.
  DATA: lt_success TYPE TABLE OF bapivbrksuccess WITH HEADER LINE.
  CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
    EXPORTING
      billingdocument = i_vgbel_log-docnr
*     TESTRUN         =
      no_commit       = 'X'
      billingdate     = gv_post_date
    TABLES
      return          = ot_return
      success         = lt_success.

  READ TABLE ot_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    gv_error = 'X'.
  ELSE.
    READ TABLE lt_success INDEX 1.
    i_vgbel_log-c_docnr = lt_success-bill_doc.
    i_vgbel_log-stokz = 'X'.
  ENDIF.


ENDFORM.

FORM ac_doc_cancel CHANGING i_vgbel_log TYPE zcct_vgbel_log.

  DATA:ls_bkpf TYPE bkpf.
  DATA:ls_reversal  LIKE  bapiacrev.
  DATA:ls_obj_type TYPE bapiacrev-obj_type.
  DATA:ls_obj_key TYPE bapiacrev-obj_key.

  SELECT SINGLE * INTO ls_bkpf FROM bkpf
    WHERE bukrs = i_vgbel_log-bukrs
    AND belnr = i_vgbel_log-docnr
    AND gjahr = i_vgbel_log-cjahr.

  IF sy-subrc NE 0.
    PERFORM add_message USING 'E' 'ZCCT' '009'
          i_vgbel_log-docnr
          i_vgbel_log-bukrs
          i_vgbel_log-cjahr
          ''.
    gv_error = 'X'.
    RETURN.
  ENDIF.


  ls_reversal-obj_type  = ls_bkpf-awtyp."AWTYP CHAR  5 0 参考交易
  ls_reversal-obj_key   = ls_bkpf-awkey."AWKEY CHAR  20  0 字段参考关键
  ls_reversal-obj_sys   = ls_bkpf-awsys. "CHAR 10  0 源凭证的逻辑系统
  ls_reversal-obj_key_r = ls_bkpf-awkey.  "CHAR  20  0 取消: 对象码 (AWREF_REV and AWORG_REV)
*  ls_reversal-obj_key_r = gv_post_date.  "CHAR  20  0 取消: 对象码 (AWREF_REV and AWORG_REV)

  ls_reversal-pstng_date = gv_post_date. "BUKRS  CHAR  4 0 公司代码
  ls_reversal-comp_code = ls_bkpf-bukrs. "BUKRS  CHAR  4 0 公司代码
  ls_reversal-reason_rev = '02'.   "STGRD CHAR  2 0 冲销原因


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      reversal = ls_reversal
      bus_act  = ls_bkpf-glvor
    IMPORTING
      obj_type = ls_obj_type
      obj_key  = ls_obj_key
*     obj_sys  =
    TABLES
      return   = ot_return.

  READ TABLE ot_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    gv_error = 'X'.
  ELSE.
    i_vgbel_log-c_docnr = ls_obj_key+0(10).
    i_vgbel_log-c_cjahr = ls_obj_key+14(4).
    i_vgbel_log-c_bukrs = ls_obj_key+10(4).
    i_vgbel_log-stokz = 'X'.
  ENDIF.




ENDFORM.
