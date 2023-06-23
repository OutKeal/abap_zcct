FUNCTION zcct_mb_save_log.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MBLNR) TYPE  MBLNR OPTIONAL
*"     VALUE(I_MJAHR) TYPE  MJAHR OPTIONAL
*"----------------------------------------------------------------------
  CHECK gv_skip_mb_enhance <> 'X'.
  CHECK gv_vgbel IS NOT INITIAL.
  CHECK gv_error IS INITIAL.
  gt_borident-objkey = i_mblnr && i_mjahr."写入关系浏览器对象
  gt_borident-objtype = 'BUS2017'.
  INSERT  gt_borident INDEX 1.
  CLEAR ot_return.
  ot_return-type = 'S'.
  ot_return-id = 'ZCCT'.
  ot_return-number = '011'.
  ot_return-message_v1 = i_mblnr.
  INSERT ot_return INDEX 1.


  PERFORM save_msg_log TABLES ot_return. "保存正确消息日志
  PERFORM save_vgbel_log ."参考日志记录

  PERFORM binary_relation."标准关系系浏览器，关联所有对象到第一个单据对象

  CALL FUNCTION 'ZCCT_INIT'.




ENDFUNCTION.
