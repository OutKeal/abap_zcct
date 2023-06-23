*&---------------------------------------------------------------------*
*& 包含               ZSCMR0001_TOP
*&---------------------------------------------------------------------*
TABLES:zscmt0001.

*---配置表
DATA:gs_1001 LIKE zscmt1001."BP账户组配置表
DATA:gs_1002 LIKE zscmt1002."BP字段配置表/TCODE
DATA:gs_1003 LIKE zscmt1003."字段状态组
DATA:gs_1004 LIKE zscmt1004."字段状态组
DATA:gs_1005 LIKE zscmt1005."字段保存/提交检查
DATA:
  gt_1001 TYPE STANDARD TABLE OF zscmt1001,
  gt_1002 TYPE STANDARD TABLE OF zscmt1002,
  gt_1003 TYPE STANDARD TABLE OF zscmt1003,
  gt_1004 TYPE STANDARD TABLE OF zscmt1004,
  gt_1005 TYPE STANDARD TABLE OF zscmt1005.
*---主数据信息临时表
*DATA:gs_data TYPE zscmt0001."临时主数据表
*DATA:gt_data TYPE STANDARD TABLE OF zscmt0001."临时主数据表

TYPES: BEGIN OF ty_tag,
         tg1 TYPE c,      " '0' 隐藏 / '1' 显示.
         tg2 TYPE c,
         tg3 TYPE c,
         tg4 TYPE c,
         tg5 TYPE c,
         tg6 TYPE c,
         tg7 TYPE c,
       END OF ty_tag.

DATA:
  gs_tag  TYPE ty_tag.

DATA:
*  gv_task   TYPE c, " 当前流程所对应的模式，影响到屏幕的控制
  gv_redol  TYPE c, "强制只读
*  gs_curs   TYPE  shp_cursor,
  gv_scr_sta_txt TYPE c LENGTH 6,
  gv_scr_butxt   LIKE t001-butxt,
  gv_scr_sp_led  TYPE string, "屏幕审批状态
  gv_mod    TYPE c, " 模式  A创建  B修改  C显示
  save_ok   TYPE sy-ucomm.
DATA:ok_code LIKE sy-ucomm.

DATA:gv_status, "E:错误 ；S:成功
     gv_msg TYPE string."错误消息
DATA:gt_return LIKE STANDARD TABLE OF bapiret2.


*----search help-----*
*---
DATA:gt_ret_hlp TYPE TABLE OF ddshretval WITH HEADER LINE."标准返回help
DATA:gv_vrmid TYPE vrm_id.
DATA:gt_vtweg TYPE vrm_values.

DATA:BEGIN OF gt_bugp OCCURS 0,
     bu_group LIKE zscmt1001-bu_group,
     txt15    LIKE tb002-txt15,
     END OF gt_bugp.

DATA:BEGIN OF gt_zvtweg OCCURS 0,
     zvtweg LIKE tvtwt-vtweg,
     vtext LIKE tvtwt-vtext,
     END OF gt_zvtweg.
