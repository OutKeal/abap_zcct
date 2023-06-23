*&---------------------------------------------------------------------*
*& 包含               ZCCT_QUERY_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_type FOR zcct_db_h-zcct_type,
                  s_burks FOR zcct_db_h-bukrs,
                  s_chan1 FOR zcct_db_h-channel1,
                  s_chan2 FOR zcct_db_h-channel2,
*                s_chan3 FOR zcct_db_h-channel3,
*                s_can FOR zcct_db_h-canceled,
                  s_budat FOR zcct_db_h-budat,
                  s_lvgbel FOR zcct_db_h-long_vgbel,
                  s_lvgbe2 FOR zcct_db_h-long_vgbel,
                  s_mark1 FOR zcct_db_h-remark01,
                  s_mark2 FOR zcct_db_h-remark02,
                  s_mark3 FOR zcct_db_h-remark03,
                  s_mark4 FOR zcct_db_h-remark04.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_docnum FOR edidc-docnum,
                  s_mescod FOR edidc-mescod,
                  s_credat FOR edidc-credat,
                  s_cretim FOR edidc-cretim,
                  s_status FOR edidc-status.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
*PARAMETERS: P_GET64 TYPE CHAR1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.
