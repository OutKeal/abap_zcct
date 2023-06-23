FUNCTION zcct_eina_maintain.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IM_MODEL) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(IM_LIFNR) TYPE  LIFNR
*"     VALUE(IM_MATNR) TYPE  MATNR
*"     VALUE(IM_EKORG) TYPE  EKORG
*"     VALUE(IM_WERKS) TYPE  WERKS_D
*"     VALUE(IM_ESOKZ) TYPE  ESOKZ
*"     VALUE(IM_NETPR) TYPE  NETWR
*"     VALUE(IM_MODE) TYPE  CHAR1 DEFAULT 'N'
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  DATA: ls_code TYPE sy-tcode.
  DATA: p_mode TYPE char1.

  DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  p_mode = im_mode.
  CLEAR bdcdata[].

  CASE im_model.
    WHEN '1'.
      ls_code = 'ME11'.
      PERFORM set_bdc_me11  USING
            im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.
    WHEN '2'.
      ls_code = 'ME12'.
      PERFORM set_bdc_me12  USING
            im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.
    WHEN '3'.
      ls_code = 'ME15'.
      PERFORM set_bdc_me15  USING
             im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.
  ENDCASE.

  CALL TRANSACTION ls_code USING bdcdata
                            MODE  p_mode
                            MESSAGES INTO messtab.

  LOOP AT messtab WHERE msgtyp = 'E'.
    et_return-type = messtab-msgtyp.
    et_return-id = messtab-msgid.
    et_return-number = messtab-msgnr.
    et_return-message_v1 = messtab-msgv1.
    et_return-message_v2 = messtab-msgv2.
    et_return-message_v3 = messtab-msgv3.
    et_return-message_v4 = messtab-msgv4.
    APPEND et_return.
    clear et_return.
  ENDLOOP.

ENDFUNCTION.

FORM set_bdc_me11   USING
            im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.
  DATA:ls_netpr TYPE char11.
  PERFORM bdc_dynpro USING 'SAPMM06I' '0100'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'EINA-LIFNR' im_lifnr.
  PERFORM bdc_field  USING 'EINA-MATNR' im_matnr.
  PERFORM bdc_field  USING 'EINE-EKORG' im_ekorg.
  PERFORM bdc_field  USING 'EINE-WERKS' im_werks.

  CASE im_esokz.
    WHEN '0'.
      PERFORM bdc_field  USING 'RM06I-NORMB' 'X'.
    WHEN '3'.
      PERFORM bdc_field  USING 'RM06I-LOHNB' 'X'.
    WHEN '2'.
      PERFORM bdc_field  USING 'RM06I-KONSI' 'X'.
  ENDCASE.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0101'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0102'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BU'.
  PERFORM bdc_field  USING 'EINE-APLFZ' '1'.
  PERFORM bdc_field  USING 'EINE-EKGRP' '101'.
  PERFORM bdc_field  USING 'EINE-NORBM' '1'.
  PERFORM bdc_field  USING 'EINE-MWSKZ' 'J1'.
*  DATA:ls_netpr TYPE char15.
  ls_netpr = im_netpr.
  CONDENSE ls_netpr NO-GAPS.
  PERFORM bdc_field  USING 'EINE-NETPR' ls_netpr.

ENDFORM.

FORM set_bdc_me12   USING
            im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0100'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'EINA-LIFNR' im_lifnr.
  PERFORM bdc_field  USING 'EINA-MATNR' im_matnr.
  PERFORM bdc_field  USING 'EINE-EKORG' im_ekorg.
  PERFORM bdc_field  USING 'EINE-WERKS' im_werks.

  CASE im_esokz.
    WHEN '0'.
      PERFORM bdc_field  USING 'RM06I-NORMB' 'X'.
    WHEN '3'.
      PERFORM bdc_field  USING 'RM06I-LOHNB' 'X'.
    WHEN '2'.
      PERFORM bdc_field  USING 'RM06I-KONSI' 'X'.
  ENDCASE.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0101'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0102'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=KO'.

  PERFORM bdc_dynpro USING 'SAPLV14A' '0102'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=NEWD'.

  PERFORM bdc_dynpro USING 'SAPMV13A' '0201'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=SICH'.
  DATA:ls_netpr TYPE char15.
  ls_netpr = im_netpr.
  CONDENSE ls_netpr NO-GAPS.
  PERFORM bdc_field  USING 'KONP-KBETR(01)' ls_netpr.

ENDFORM.


FORM set_bdc_me15   USING
            im_matnr im_lifnr im_ekorg im_werks im_esokz im_netpr.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0100'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'EINA-LIFNR' im_lifnr.
  PERFORM bdc_field  USING 'EINA-MATNR' im_matnr.
  PERFORM bdc_field  USING 'EINE-EKORG' im_ekorg.
  PERFORM bdc_field  USING 'EINE-WERKS' im_werks.
  CASE im_esokz.
    WHEN '0'.
      PERFORM bdc_field  USING 'RM06I-NORMB' 'X'.
    WHEN '2'.
      PERFORM bdc_field  USING 'RM06I-LOHNB' 'X'.
    WHEN '3'.
      PERFORM bdc_field  USING 'RM06I-KONSI' 'X'.
  ENDCASE.

  PERFORM bdc_dynpro USING 'SAPMM06I' '0104'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BU'.
  PERFORM bdc_field  USING 'EINA-LOEKZ' 'X'.
  PERFORM bdc_field  USING 'EINE-LOEKZ' 'X'.


ENDFORM.



FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
