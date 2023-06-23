FUNCTION zcct_rkwa_wrbtr_update.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_RKWA STRUCTURE  RKWA
*"----------------------------------------------------------------------
  LOOP AT it_rkwa.
    UPDATE rkwa SET wrbtr = it_rkwa-wrbtr
           WHERE mblnr = it_rkwa-mblnr
           AND mjahr = it_rkwa-mjahr
           AND zeile = it_rkwa-zeile.

  ENDLOOP.




ENDFUNCTION.
