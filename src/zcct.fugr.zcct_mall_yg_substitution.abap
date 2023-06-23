FUNCTION zcct_mall_yg_substitution.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      ET_RETURN STRUCTURE  ZSFI_YG_SUB
*"----------------------------------------------------------------------
*  CLEAR:et_return[],et_return.
*
*  LOOP AT gt_vbrkvb INTO DATA(ls_vbrkvb) WHERE ktgrd = '50'.
*
*    APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*    <fs_ret>-bukrs = ls_vbrkvb-bukrs.
*    <fs_ret>-flag_sub = 'X'.
*
*  ENDLOOP.

ENDFUNCTION.
