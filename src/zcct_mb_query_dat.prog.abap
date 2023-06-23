*&---------------------------------------------------------------------*
*& 包含               ZCCT_QUERY_DAT
*&---------------------------------------------------------------------*


TABLES:zcct_db_h,edidc.


DATA:gv_m TYPE char1.


DATA:gt_item TYPE TABLE OF zcct_item_dis WITH HEADER LINE.
DATA:sum_item TYPE TABLE OF zcct_item_dis WITH HEADER LINE.
DATA:gt_msg TYPE TABLE OF zcct_msg_dis WITH HEADER LINE.
DATA:gt_vgbel TYPE TABLE OF zcct_vgbel_dis WITH HEADER LINE.


DATA:gt_head_dis TYPE TABLE OF zcct_mb_head_dis WITH HEADER LINE.
DATA:gt_item_dis TYPE TABLE OF zcct_item_dis WITH HEADER LINE.
DATA:gt_msg_dis TYPE TABLE OF zcct_msg_dis WITH HEADER LINE.
DATA:gt_vgbel_dis TYPE TABLE OF zcct_vgbel_dis WITH HEADER LINE.

FIELD-SYMBOLS:<gs_head_dis>  TYPE zcct_mb_head_dis.
FIELD-SYMBOLS:<gs_vgbel_dis>  TYPE zcct_vgbel_dis.

DATA gt_message TYPE TABLE OF esp1_message_wa_type WITH HEADER LINE.

DATA: gt_scmt0010 TYPE TABLE OF zscmt0010 WITH HEADER LINE.
DATA: gt_dz TYPE TABLE OF ztmm013_item WITH HEADER LINE.
DATA: gt_mt TYPE TABLE OF zcct_mt WITH HEADER LINE.

RANGES:r_docnum FOR edidc-docnum.
