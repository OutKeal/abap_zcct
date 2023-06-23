FUNCTION ZCCT_GET_BL_COST.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  EXPORTING
*"     VALUE(E_CCT_ACTIVE) TYPE  CHAR1
*"  TABLES
*"      ET_GT_COST STRUCTURE  ZCCT_COST
*"--------------------------------------------------------------------
*{   INSERT         S4DK900149                                        1
  IF gs_config IS NOT INITIAL.
    e_cct_active = 'X'.
    et_gt_cost[] = gt_cost[].
  ENDIF.




*}   INSERT
ENDFUNCTION.
