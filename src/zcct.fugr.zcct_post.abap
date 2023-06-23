FUNCTION zcct_post.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IM_CHANNEL1) TYPE  ZCHANNEL1 OPTIONAL
*"     VALUE(IM_CHANNEL2) TYPE  ZCHANNEL2 OPTIONAL
*"     VALUE(IM_CHANNEL3) TYPE  ZCHANNEL3 OPTIONAL
*"     VALUE(IM_CCT_TYPE) TYPE  ZCCT_TYPE
*"     VALUE(IM_POST_DATE) TYPE  BUDAT
*"     VALUE(IM_BKTXT) TYPE  CHAR50 OPTIONAL
*"     VALUE(IM_LONG_VGBEL) TYPE  ZLONG_VGBEL OPTIONAL
*"     VALUE(IM_VKORG) TYPE  VKORG OPTIONAL
*"     VALUE(IM_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(IM_COMMIT) TYPE  CHAR1 OPTIONAL
*"     VALUE(IM_IDOCNUM) TYPE  EDI_DOCNUM OPTIONAL
*"     VALUE(IM_MESCOD) TYPE  EDI_MESCOD OPTIONAL
*"  EXPORTING
*"     VALUE(EX_LOGNR) TYPE  LOGNR
*"     VALUE(EX_VGBEL) TYPE  VGBEL
*"  TABLES
*"      IM_DATA STRUCTURE  ZCCT_DATA
*"      IM_LINE_DATA STRUCTURE  ZCCT_LINE_DATA OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      ET_BORIDENT STRUCTURE  BORIDENT OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZCCT_INIT'.


  IF im_idocnum IS NOT INITIAL.  "传入信息
    PERFORM set_idoc_info USING im_idocnum im_mescod.
  ENDIF.

  . "清空函数组全局变量


*************************************函数组全局变量赋值
  gv_channel1 = im_channel1.
  gv_channel2 = im_channel2.
  gv_channel3 = im_channel3.
  gv_cct_type = im_cct_type.
  gv_post_date = im_post_date.
  gv_bktxt = im_bktxt.
  gt_data[] = im_data[].
  gt_line_data[] = im_line_data[].
  gv_long_vgbel = im_long_vgbel.


  IF et_borident[] IS  NOT INITIAL.
    gt_borident[] = et_borident[].
  ENDIF.


  PERFORM get_next_lognr CHANGING gv_lognr. "获取唯一日志流水编号
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
  ENDIF.

  PERFORM get_next_vgbel USING im_long_vgbel
                               CHANGING gv_vgbel. "获取并校验唯一参考。生成内部参考
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.



  IF im_cct_type IS  INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '000' '业务类型不能为空' '' '' ''.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.

  IF im_data[] IS  INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '000' '业务明细不能为空' '' '' ''.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.

  PERFORM init_mt_config. "获取渠道移动类型配置
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.

  PERFORM init_bp_info.
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.

  PERFORM init_post_config USING im_vkorg im_werks. "获取过账配置信息
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.

  PERFORM eina_create.
  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误消息日志
    RAISE error.
  ENDIF.


  IF gs_config-zcct_bill_act = 'X'.
    PERFORM bill_create."按照配置生成多张发票
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES et_return."保存错误消息日志
      RAISE error.
    ENDIF.
  ENDIF.

  IF gs_config-zcct_mbdc_act = 'X'.
    gv_skip_mb_enhance = 'X'.

    PERFORM mb_doc_create. "按照配置生成多行物料凭证
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES et_return."保存错误消息日志
      RAISE error.
    ENDIF.
  ENDIF.


  IF gs_config-zcct_acdc_act = 'X'.
    PERFORM ac_doc_create."按照配置生成多张会计凭证，按照公司/凭证类型合并。
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES et_return."保存错误消息日志
      RAISE error.
    ENDIF.
  ENDIF.

  IF gs_js IS NOT INITIAL.
    PERFORM js_doc_create.
    IF gv_error = 'X'.
      PERFORM save_msg_log TABLES et_return."保存错误消息日志
      RAISE error.
    ENDIF.
  ENDIF.

  IF gv_error = 'X'.
    PERFORM save_msg_log TABLES et_return."保存错误日期
    RAISE error.
  ELSE.
    PERFORM binary_relation."标准关系系浏览器，关联所有对象到第一个单据对象
    PERFORM save_msg_log TABLES et_return. "保存正确消息日志
    PERFORM save_vgbel_log."参考日志记录
    et_borident[] = gt_borident[].
  ENDIF.
  ex_lognr = gv_lognr.
  ex_vgbel = gv_vgbel.

*  CALL FUNCTION 'ZCCT_INIT'            .


  IF im_commit = 'X' AND gv_error IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.


ENDFUNCTION.
