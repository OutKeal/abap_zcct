FUNCTION zcct_get_po_info.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  CHANGING
*"     VALUE(C_BSART) TYPE  BSART OPTIONAL
*"     VALUE(C_EKGRP) TYPE  EKGRP OPTIONAL
*"     VALUE(C_TAX_CODE) TYPE  MWSKZ OPTIONAL
*"----------------------------------------------------------------------

  c_bsart = gv_bsart.
  c_ekgrp = gv_ekgrp.
  c_tax_code = gv_tax_code.



ENDFUNCTION.
