FUNCTION zcct_set_bl_cost.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_COST STRUCTURE  ZCCT_COST
*"----------------------------------------------------------------------


  SORT it_cost[] BY vkorg line_id matnr.
  gt_cost[] = it_cost[].


ENDFUNCTION.
