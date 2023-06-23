FUNCTION zcct_get_bill_kunnr.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(EV_PARTNER) TYPE  BU_PARTNER
*"----------------------------------------------------------------------
TABLES: tvko.
READ TABLE gt_bp_info INTO DATA(ls_bp_info) WITH KEY zcct_role = 'BL'.
IF sy-subrc EQ 0.
  ev_partner = ls_bp_info-partner.
ELSE.
  READ TABLE gt_bp_info INTO  ls_bp_info  WITH KEY zcct_role = 'IC'.
  IF sy-subrc EQ 0.
   SELECT SINGLE kunnr
     INTO tvko-kunnr
     FROM tvko
    WHERE vkorg = ls_bp_info-bukrs.
    IF sy-subrc EQ 0.
      ev_partner = tvko-kunnr.
    ENDIF.
  ENDIF.
ENDIF.
ENDFUNCTION.
