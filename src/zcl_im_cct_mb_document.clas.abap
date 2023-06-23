class ZCL_IM_CCT_MB_DOCUMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CCT_MB_DOCUMENT IMPLEMENTATION.


  METHOD if_ex_mb_document_badi~mb_document_before_update.
    DATA:ls_mseg TYPE mseg.
    DATA:lt_mb_cost TYPE TABLE OF zcct_mb_cost.
    DATA:ls_mb_cost TYPE zcct_mb_cost.

    DATA: lt_rkwa TYPE TABLE OF rkwa .
    DATA: ls_rkwa TYPE rkwa .

    LOOP AT xmseg INTO ls_mseg.
      IF ls_mseg-matnr IS INITIAL.
        CONTINUE.
      ENDIF.
      ls_mb_cost-line_id = ls_mseg-line_id .
      ls_mb_cost-matnr = ls_mseg-matnr .
      ls_mb_cost-werks = ls_mseg-werks .
      ls_mb_cost-menge = ls_mseg-menge .




      IF ls_mseg-sobkz = 'K'.
        ls_mb_cost-netwr = ls_mseg-exbwr .
      ELSE.
        ls_mb_cost-netwr = ls_mseg-dmbtr .
      ENDIF.


      APPEND ls_mb_cost TO lt_mb_cost.
      CLEAR ls_mb_cost.

      IF ls_mseg-sobkz = 'K' AND ls_mseg-bustw IS NOT INITIAL .
        ls_rkwa-mblnr = ls_mseg-mblnr.
        ls_rkwa-mjahr = ls_mseg-mjahr.
        ls_rkwa-zeile = ls_mseg-zeile.
        ls_rkwa-wrbtr = ls_mseg-exbwr.
        APPEND ls_rkwa TO lt_rkwa.
        CLEAR ls_rkwa.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'ZCCT_SET_COST'
      TABLES
        it_gt_cost = lt_mb_cost.


    IF lt_rkwa[] IS NOT INITIAL.
      CALL FUNCTION 'ZCCT_RKWA_WRBTR_UPDATE' IN BACKGROUND TASK
        TABLES
          it_rkwa = lt_rkwa.
    ENDIF.


  ENDMETHOD.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
  endmethod.
ENDCLASS.
