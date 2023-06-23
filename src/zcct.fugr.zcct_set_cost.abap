FUNCTION zcct_set_cost.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_GT_COST STRUCTURE  ZCCT_MB_COST
*"----------------------------------------------------------------------


  SORT it_gt_cost[] BY werks matnr.
  gt_mb_cost[] = it_gt_cost[].


ENDFUNCTION.
