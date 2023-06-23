FUNCTION zcct_mb_post.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IMKPF) LIKE  IMKPF STRUCTURE  IMKPF
*"  EXPORTING
*"     VALUE(EMKPF) LIKE  EMKPF STRUCTURE  EMKPF
*"     VALUE(CMKPF) LIKE  IMKPF STRUCTURE  IMKPF
*"  TABLES
*"      EMSEG STRUCTURE  EMSEG
*"      IMSEG STRUCTURE  IMSEG
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------



  cmkpf = imkpf.

  CHECK gv_skip_mb_enhance <> 'X'.

  CALL FUNCTION 'ZCCT_INIT'.

  PERFORM check_zcct_business TABLES imseg[] USING imkpf.
  IF gv_error = 'X'.
    PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
    RETURN.
  ENDIF.

  CHECK gv_zcct_active = 'X'.



*  IF gv_brand IS INITIAL.
*    READ TABLE imseg INDEX 1.
*    SELECT SINGLE brand_id INTO gv_brand FROM mara WHERE matnr = imseg -matnr.
*  ENDIF.
*  BREAK wangf.

  PERFORM init_post_config using gv_cct_vkorg gv_cct_werks. "获取配置信息
  IF gv_error = 'X'.
*    PERFORM add_message_emkpf USING 'E' 'ZCCT' '00' '无跨公司交易配置,清检查错误' '' '' '' CHANGING emkpf .
    RETURN.
  ENDIF.


  PERFORM set_gt_data TABLES imseg[] USING cmkpf .
  IF gv_error = 'X'.
    PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
    RETURN.
  ENDIF.


  PERFORM get_next_vgbel USING '' CHANGING gv_vgbel. "获取并校验唯一参考。若未传入则生成流水号
  IF gv_error = 'X'.
    PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
    RETURN.
  ENDIF.
  cmkpf-xblnr = gv_vgbel.

  PERFORM get_next_lognr CHANGING gv_lognr. "获取唯一日志流水编号
  IF gv_error = 'X'.
    PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
    RETURN.
  ENDIF.

  IF gs_config-zcct_bill_act = 'X'.
    PERFORM bill_create."按照配置生成多张发票
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES ot_return."保存错误消息日志
      PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
      PERFORM move_return_emseg TABLES ot_return emseg.
      RETURN.
    ENDIF.
  ENDIF.


  IF gs_config-zcct_mbdc_act = 'X'.
    PERFORM mb_doc_modify TABLES imseg.
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES ot_return."保存错误消息日志
      PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
      PERFORM move_return_emseg TABLES ot_return emseg.
      RETURN.
    ENDIF.
  ENDIF.


  IF gs_config-zcct_acdc_act = 'X'.
    PERFORM ac_doc_create."按照配置生成多张会计凭证，按照公司/凭证类型合并。
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES ot_return."保存错误消息日志
      PERFORM move_return_emkpf TABLES ot_return CHANGING emkpf.
      PERFORM move_return_emseg TABLES ot_return emseg.
      RETURN.
    ENDIF.
  ENDIF.



ENDFUNCTION.
