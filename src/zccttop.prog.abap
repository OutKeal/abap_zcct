*&---------------------------------------------------------------------*
*& 包含               ZCCTTOP
*&---------------------------------------------------------------------*

* INCLUDE LZCCTD...                          " Local class definition
DATA: gv_brand TYPE wrf_brand_id.
DATA: gv_cct_werks TYPE werks_d.
DATA: gv_cct_lgort TYPE lgort_d.
DATA: gv_cct_umlgo TYPE lgort_d.
DATA: gv_cct_vkorg TYPE vkorg.
DATA: gv_cct_vtweg TYPE vtweg.
DATA: gv_cct_spart TYPE spart.
DATA: gv_cct_bukrs TYPE bukrs.
DATA: gv_cct_umwrk TYPE werks_d.
DATA: gv_cct_prctr TYPE prctr.
DATA: gv_vgbel TYPE vgbel.
DATA: gv_long_vgbel TYPE zlong_vgbel.
DATA: gv_channel1 TYPE ze_channel.
DATA: gv_channel2 TYPE ze_channel.
DATA: gv_channel3 TYPE ze_channel.
DATA: gv_cct_bl_kunnr TYPE kunnr.
DATA: gv_cct_ac_lifnr TYPE lifnr.
DATA: gv_cct_ac_kunnr TYPE kunnr.
DATA: gv_lognr TYPE lognr.
DATA: gv_vgpos TYPE vgpos.
DATA: gv_cct_type TYPE zcct_type.
DATA: gv_post_date TYPE budat.
DATA: gv_price_date TYPE budat.
DATA: gv_bktxt TYPE bktxt.
DATA: gs_config TYPE zcct_config.

DATA: mb_doc TYPE bapi2017_gm_head_ret-mat_doc.
DATA: mb_year TYPE bapi2017_gm_head_ret-doc_year.

DATA: gv_fldname TYPE fieldname.
DATA: gv_strname TYPE fieldname.
DATA: gv_conname TYPE fieldname.
DATA: gv_convalue TYPE value.


DATA:gv_skip_mb_enhance  TYPE char1.

DATA: gv_zcct_active TYPE char1.


DATA: gv_error TYPE char1.




DATA: gt_mb_config TYPE TABLE OF zcct_mb_config WITH HEADER LINE.
DATA: gt_bl_config TYPE TABLE OF zcct_bl_config WITH HEADER LINE.
DATA: gt_ac_config TYPE TABLE OF zcct_ac_config WITH HEADER LINE.
DATA: gt_line_config TYPE TABLE OF zcct_line_config WITH HEADER LINE.

DATA: gt_bp_config TYPE TABLE OF zcct_bp_config WITH HEADER LINE.
DATA: gt_pr_config TYPE TABLE OF zcct_pr_config WITH HEADER LINE.

DATA: gt_price TYPE TABLE OF zcct_tran_price WITH HEADER LINE.

DATA: gt_bp_info TYPE TABLE OF zcct_bp_info WITH HEADER LINE.


DATA: gt_data TYPE TABLE OF zcct_data WITH HEADER LINE.
DATA: gt_line_data TYPE TABLE OF zcct_line_data WITH HEADER LINE.

DATA: ot_return TYPE TABLE OF bapiret2 WITH HEADER LINE.




DATA:gt_borident TYPE TABLE OF borident WITH HEADER LINE.

DATA:g_foldoc_attr TYPE  wpusa_foldoc_attr.


DATA:gs_vgbel_head TYPE  zcct_vgbel_head  .
DATA:gt_vgbel_head TYPE zcct_vgbel_head.

DATA:gt_vgbel_log TYPE TABLE OF zcct_vgbel_log  WITH HEADER LINE.


DATA:gt_cost TYPE TABLE OF zcct_cost WITH HEADER LINE.


DATA: gt_mb_cost TYPE TABLE OF zcct_mb_cost WITH HEADER LINE.

DATA: gs_js TYPE  zcct_js.


DATA: gt_zcct_bl_list TYPE TABLE OF zcct_bl_list WITH HEADER LINE.



FIELD-SYMBOLS <field_value> TYPE any.



DATA: gv_zccth TYPE zccth.

DATA: gv_bsart TYPE bsart.
DATA: gv_ekgrp TYPE ekgrp.


DATA: gv_last_netpr TYPE netpr.
DATA: gv_last_cmpre TYPE cmpre.
DATA: gv_last_amount_netpr TYPE netpr.
DATA: gv_last_amount_cmpre TYPE cmpre.
DATA: gv_last_bill_netpr TYPE netpr.

DATA: gv_docnum TYPE edi_docnum.
DATA: gv_mescod TYPE edi_mescod.
DATA: gv_cancel TYPE char1.

DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.

DATA: gt_t0010 TYPE TABLE OF zmmt0010 WITH HEADER LINE.

DATA:BEGIN OF gt_jms OCCURS 0,
       long_vgbel TYPE char50,
     END OF gt_jms.

DATA:gv_rk_remark TYPE char50.
DATA:gv_rk_date TYPE datum.
DATA:gv_long_vgbel2 TYPE char50.

*DATA: gt_zqm_werks_active TYPE TABLE OF zqm_werks_active  WITH HEADER LINE.
DATA: BEGIN OF gt_vbrkvb OCCURS 0.
        INCLUDE STRUCTURE vbrkvb.
DATA: END OF gt_vbrkvb.

DATA:gv_tax_code TYPE MWSKZ.
DATA:gv_tax_rate TYPE ZCCT_TAX_RATE.

RANGES:rs_hkont_bp FOR bseg-hkont.
