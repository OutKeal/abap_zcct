"Name: \PR:SAPLV60A\EX:RV_INVOICE_DOCUMENT_ADD_15\EN:SIPT_BILL_DOC_CHECK_IMPL\SE:END\EI
ENHANCEMENT 0 ZCCT_SET_COST.
DATA: zcct_active TYPE char1.
DATA: gt_cost TYPE TABLE OF zcct_cost WITH HEADER LINE.
  DATA: gs_cost TYPE  zcct_cost .
  FIELD-SYMBOLS: <fs_cost> TYPE zcct_cost.

    SELECT SINGLE vkorg INTO @DATA(lv_vkorg) FROM TVKO
      WHERE kunnr =  @vbrk-kunag.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'ZCCT_GET_BL_COST'
          IMPORTING
            e_cct_active     = zcct_active
          TABLES
            et_gt_cost       = gt_cost[]
                  .
        IF zcct_active = 'X'.



                LOOP AT xvbrp INTO DATA(ls_cct_vbrp) WHERE VBELN = VBRK-VBELN.
                    READ TABLE gt_cost ASSIGNING <fs_cost> WITH KEY vkorg = lv_vkorg
                                                                    line_id = ls_cct_vbrp-posnr
                                                                    matnr = ls_cct_vbrp-matnr
                                                                     .
                IF sy-subrc EQ 0.
                  <fs_cost>-netwr = ls_cct_vbrp-netwr.
                  <fs_cost>-kzwi1 = ls_cct_vbrp-kzwi1.
                  <fs_cost>-mwsbp = ls_cct_vbrp-mwsbp.
                  <fs_cost>-menge = ls_cct_vbrp-fkimg.
                  <fs_cost>-waers = ls_cct_vbrp-waerk.
                  <fs_cost>-mwskz = ls_cct_vbrp-mwskz.
                ELSE.
                  gs_cost-vkorg = lv_vkorg.
                  gs_cost-line_id = ls_cct_vbrp-posnr.
                  gs_cost-matnr = ls_cct_vbrp-matnr.

                  gs_cost-menge = ls_cct_vbrp-fkimg.
                  gs_cost-netwr = ls_cct_vbrp-netwr.
                  gs_cost-kzwi1 = ls_cct_vbrp-kzwi1.
                  gs_cost-mwsbp = ls_cct_vbrp-mwsbp.
                  gs_cost-waers = ls_cct_vbrp-waerk.
                  gs_cost-mwskz = ls_cct_vbrp-mwskz.
                  APPEND gs_cost TO gt_cost.
                  CLEAR gs_cost.
                ENDIF.

               ENDLOOP.

                CALL FUNCTION 'ZCCT_SET_BL_COST'
                  TABLES
                    it_cost       = gt_cost[]
                      .


        ENDIF.
      ENDIF.

ENDENHANCEMENT.
