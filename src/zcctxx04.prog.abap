

*********增加通用性，特殊业务全部FIX开头，并存在04的INCLUDE中。


FORM fix_kp_cb.""""成本凭证特殊处理

*  DATA:ls_t0010 TYPE  zscmt1010 .
*
*  DATA:ls_line TYPE zcct_line.
*
*  SELECT SINGLE * INTO ls_t0010 FROM zscmt1010
*    WHERE channel = gv_channel1.
*
*  CHECK sy-subrc EQ 0.
*
*  CHECK ls_t0010-zcb_flag EQ 'X'.
*
*  LOOP AT gt_ac_config.
*    ls_line = gt_ac_config-zcct_line.
*  ENDLOOP.
*  gv_cct_ac_lifnr = ls_t0010-lifnr.
*
*
*  SELECT SINGLE * FROM zscmt0010 INTO @DATA(ls_zscmt0010) WHERE partner = @gv_cct_ac_lifnr.
*  MOVE-CORRESPONDING ls_zscmt0010 TO gt_bp_info.
*  gt_bp_info-zcct_role = 'SV'.
*  APPEND gt_bp_info.
*  CLEAR gt_bp_info.
*
*  gs_config-zcct_acdc_act = 'X'.
*  IF gv_cct_type+3(1) = 'S' OR  gv_cct_type+3(1) = 'Y'.
*    CLEAR gt_ac_config.
*    gt_ac_config-zcct_werks = gs_config-zcct_werks.
*    gt_ac_config-zcct_vkorg = gs_config-zcct_vkorg.
*    gt_ac_config-zcct_type = gv_cct_type.
*    gt_ac_config-zcct_line = ls_line + 1.
*    gt_ac_config-blart = 'KR'.
*    gt_ac_config-bukrs = gv_cct_werks.
*    gt_ac_config-shkzg = 'H'.
*    gt_ac_config-zmitkz = 'K'.
*    gt_ac_config-mwskz = 'J1'.
*    gt_ac_config-actgroup = 'X'.
*    gt_ac_config-zprice_type = '3'.
*    gt_ac_config-zallow_zero = 'X'.
*    APPEND gt_ac_config.
*
*    CLEAR gt_ac_config.
*    gt_ac_config-zcct_werks = gs_config-zcct_werks.
*    gt_ac_config-zcct_vkorg = gs_config-zcct_vkorg.
*    gt_ac_config-zcct_type = gv_cct_type.
*    gt_ac_config-zcct_line = ls_line + 2.
*    gt_ac_config-blart = 'KR'.
*    gt_ac_config-bukrs = gv_cct_werks.
*    gt_ac_config-shkzg = 'S'.
*    IF ls_t0010-zbu_typ = 'S15'.
*      gt_ac_config-saknr = '6401010200'.
*    ELSE.
*      gt_ac_config-saknr = '6401020000'.
*    ENDIF.
*
*
*    gt_ac_config-ztax_split = '-'.
*    gt_ac_config-mwskz = 'J1'.
*    gt_ac_config-actgroup = 'X'.
*    gt_ac_config-zprice_type = '3'.
*    gt_ac_config-zallow_zero = 'X'.
*    APPEND gt_ac_config.
*    gs_config-zcct_acdc_act = 'X'.
*  ENDIF.
*
*
*  IF gv_cct_type+3(1) = 'T' OR  gv_cct_type+3(1) = 'Z'.
*    CLEAR gt_ac_config.
*    gt_ac_config-zcct_werks = gs_config-zcct_werks.
*    gt_ac_config-zcct_vkorg = gs_config-zcct_vkorg.
*    gt_ac_config-zcct_type = gv_cct_type.
*    gt_ac_config-zcct_line = ls_line + 1.
*    gt_ac_config-blart = 'KG'.
*    gt_ac_config-bukrs = gv_cct_werks.
*    gt_ac_config-shkzg = 'S'.
*    gt_ac_config-zmitkz = 'K'.
*    gt_ac_config-mwskz = 'J1'.
*    gt_ac_config-actgroup = 'X'.
*    gt_ac_config-zprice_type = '3'.
*
*    gt_ac_config-zallow_zero = 'X'.
*    APPEND gt_ac_config.
*
*    CLEAR gt_ac_config.
*    gt_ac_config-zcct_werks = gs_config-zcct_werks.
*    gt_ac_config-zcct_vkorg = gs_config-zcct_vkorg.
*    gt_ac_config-zcct_type = gv_cct_type.
*    gt_ac_config-zcct_line = ls_line + 2.
*    gt_ac_config-blart = 'KG'.
*    gt_ac_config-bukrs = gv_cct_werks.
*    gt_ac_config-shkzg = 'H'.
*    IF ls_t0010-zbu_typ = 'S15'.
*      gt_ac_config-saknr = '6401010200'.
*    ELSE.
*      gt_ac_config-saknr = '6401020000'.
*    ENDIF.
*    gt_ac_config-ztax_split = '-'.
*    gt_ac_config-mwskz = 'J1'.
*    gt_ac_config-actgroup = 'X'.
*    gt_ac_config-zprice_type = '3'.
*
*    gt_ac_config-zallow_zero = 'X'.
*    APPEND gt_ac_config.
*    gs_config-zcct_acdc_act = 'X'.
*  ENDIF.
*
*
*
*
*





ENDFORM.

FORM fix_mb_item_7006 TABLES cb_item STRUCTURE  bapi2017_gm_item_create.
  CHECK gv_cct_type = '7006'.
  DATA:lt_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.
  DATA:it_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.

  CLEAR it_item[].
  CLEAR lt_item[].


  LOOP AT cb_item.
    IF cb_item-mvt_ind = 'F'.
      APPEND cb_item TO lt_item.
    ENDIF.

    IF cb_item-mvt_ind = 'B'.
      CLEAR cb_item-material_long.
      CLEAR cb_item-material.
      CLEAR cb_item-batch.
      CLEAR cb_item-move_batch.
      CLEAR cb_item-stge_loc.
      CLEAR cb_item-line_id.
      COLLECT cb_item INTO it_item.
    ENDIF.
  ENDLOOP.

  CLEAR cb_item[].

  DATA:lv_item_id TYPE mb_line_id.

  CLEAR lv_item_id.

  LOOP AT lt_item.
    ADD 1 TO lv_item_id.
    lt_item-line_id = lv_item_id.
    APPEND lt_item TO cb_item.
    CLEAR lt_item.
  ENDLOOP.

  LOOP AT it_item.
    ADD 1 TO lv_item_id.
    it_item-line_id = lv_item_id.
    APPEND it_item TO cb_item.
    CLEAR it_item.
  ENDLOOP.


ENDFORM.


FORM fix_cg_tax_rate USING ls_zccth TYPE zccth
                           ls_zccte TYPE zccte.
*  IF ls_zccth-zcct_type = '1001'
*      OR ls_zccth-zcct_type = '1002'
*      OR ls_zccth-zcct_type = '1003'
*      OR ls_zccth-zcct_type = '1004'.
*    IF gv_tax_rate IS INITIAL.
*      IF ls_zccte-key_name = 'Z003' .
*        gv_tax_rate = ls_zccte-key_value.
*      ENDIF.
*
*
*    ENDIF.
*  ENDIF.



ENDFORM.


FORM fix_cg_tax_code USING ls_zccth TYPE zccth.
*  IF ls_zccth-zcct_type = '1001'
*      OR ls_zccth-zcct_type = '1002'
*      OR ls_zccth-zcct_type = '1003'
*      OR ls_zccth-zcct_type = '1004'.
*    IF gv_tax_rate IS INITIAL.
*      SELECT SINGLE zshuilv
*        INTO gv_tax_rate FROM zscmt0010
*        WHERE partner = ls_zccth-channel1.
*    ENDIF.
*
*    CALL FUNCTION 'ZCCT_GET_TAX'
*      EXPORTING
**       i_tax_code =
*        i_tax_type = 'J'
*        i_txt_rate = gv_tax_rate
*      IMPORTING
*        e_tax_code = gv_tax_code
*        e_tax_rate = gv_tax_rate
*      EXCEPTIONS
*        error      = 1
*        OTHERS     = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM fix_pd_prctr CHANGING mb_item TYPE bapi2017_gm_item_create.
*  DATA: ls_bu_group TYPE bu_group.
*  IF gv_cct_type = '5003' OR gv_cct_type = '5004' OR gv_cct_type = '2003' OR gv_cct_type = '2002'.
*
*
*    SELECT SINGLE bu_group,prctr,kostl FROM zscmt0010
*      WHERE partner = @gv_channel1
*      INTO ( @ls_bu_group ,@mb_item-profit_ctr , @mb_item-costcenter ).
*
*    IF ls_bu_group = 'A002'.
*      IF mb_item-plant = '1000'.
*        mb_item-costcenter = '0010000108'.
*        CLEAR mb_item-profit_ctr.
*      ELSEIF mb_item-plant = '6000'.
*        mb_item-costcenter = 'CQ0230WLBM'.
*        CLEAR mb_item-profit_ctr.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*
*  IF gv_cct_type = '1002'.
*    SELECT SINGLE prctr FROM zscmt0010
*      WHERE partner = @gv_channel2
*      INTO @mb_item-profit_ctr.
*
*    EXPORT prctr = mb_item-profit_ctr TO MEMORY ID 'ZMMR0150_PRCTR'.
*
*  ENDIF.
ENDFORM.

FORM fix_rj TABLES lt_data STRUCTURE zcct_data
                   lt_dayend_goods_check STRUCTURE zcct_dayend_goods_check
                   idoc_status STRUCTURE bdidocstat
             USING  ls_zccth TYPE zccth
                    ls_zfhth
                    idoc_contrl TYPE edidc.
*  DATA:ls_posnr TYPE posnr.
*  DATA:ls_tabix TYPE sy-tabix.
*  CHECK lt_data[] IS NOT INITIAL.
*
*  SELECT  * INTO TABLE @DATA(lt_cy) FROM zdayend_cy
*    WHERE channel = @ls_zccth-channel1
*      AND sale_date = @ls_zccth-budat
*      AND zfhth = @ls_zfhth.
*  IF sy-subrc EQ 0.
*    LOOP AT lt_data.
*      ls_posnr = lt_data-line_id.
*      READ TABLE lt_cy INTO DATA(ls_cy)
*                            WITH KEY satnr = lt_data-satnr
*                                     size1 = lt_data-size1.
*      IF sy-subrc EQ 0.
*
*        ls_tabix = sy-tabix.
*        DELETE lt_cy INDEX ls_tabix.
*        IF ls_zfhth = 'S'.
*          lt_data-menge = lt_data-menge - ls_cy-menge.
*          lt_data-netwr = lt_data-netwr - ls_cy-netwr.
*          MODIFY lt_data.
*        ELSEIF ls_zfhth = 'T'.
*          lt_data-menge = lt_data-menge + ls_cy-menge.
*          lt_data-netwr = lt_data-netwr + ls_cy-netwr.
*          MODIFY lt_data.
*        ENDIF.
*        READ TABLE lt_dayend_goods_check ASSIGNING FIELD-SYMBOL(<fs_check>) WITH KEY satnr = lt_data-satnr size1 = lt_data-size1.
*        IF sy-subrc EQ 0.
*          <fs_check>-menge = <fs_check>-menge - ls_cy-menge.
*          <fs_check>-netwr = <fs_check>-netwr - ls_cy-netwr.
*        ENDIF.
*
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT lt_cy INTO ls_cy.
*      READ TABLE lt_data WITH KEY satnr = ls_cy-satnr
*                                  size1 = ls_cy-size1.
*      IF sy-subrc NE 0.
*        ls_posnr = ls_posnr + 1.
*        lt_data-line_id = ls_posnr.
*        lt_data-satnr = ls_cy-satnr.
*        lt_data-color = 'M00'.
*        lt_data-size1 = ls_cy-size1.
*        lt_data-size2 = '0'.
*        lt_data-menge = ls_cy-menge.
*        lt_data-netwr = ls_cy-netwr.
*
*        IF lt_data-satnr IS NOT INITIAL .
*          CALL FUNCTION 'ZMDA_GET_MATNR'
*            EXPORTING
*              i_satnr = lt_data-satnr
*              i_color = lt_data-color
*              i_size1 = lt_data-size1
*              i_size2 = lt_data-size2
**             i_ean11 = lt_data-ean11
*            IMPORTING
*              e_matnr = lt_data-matnr
*            EXCEPTIONS
*              notfind = 1
*              OTHERS  = 2.
*          IF sy-subrc <> 0.
*            PERFORM add_message_idoc TABLES idoc_status
*              USING idoc_contrl-docnum '51' 'ZCCT' '016' '' '' '' '' ''.
*          ENDIF.
*        ENDIF.
*
*        APPEND lt_data.
*
*
*        CLEAR lt_dayend_goods_check.
*        lt_dayend_goods_check-satnr = lt_data-satnr.
*        lt_dayend_goods_check-color = lt_data-color.
*        lt_dayend_goods_check-size1 = lt_data-size1.
*        lt_dayend_goods_check-size2 = lt_data-size2.
*        lt_dayend_goods_check-menge = lt_data-menge.
*        lt_dayend_goods_check-netwr = lt_data-netwr.
*        IF ls_zfhth = 'T'.
*          lt_dayend_goods_check-menge = - lt_dayend_goods_check-menge.
*          lt_dayend_goods_check-netwr = - lt_dayend_goods_check-netwr.
*        ENDIF.
*
*        COLLECT lt_dayend_goods_check.
*        CLEAR lt_data.
*
*
*      ENDIF.
*    ENDLOOP.
*
*    DELETE lt_data WHERE menge = 0 AND netwr = 0.
*    DELETE lt_dayend_goods_check WHERE menge = 0 AND netwr = 0.
*    IF lt_data[] IS INITIAL.
*      PERFORM add_message_idoc TABLES idoc_status
*         USING idoc_contrl-docnum '53' 'ZCCT' '000' '日结明细已清空' '' '' '' ''.
*    ENDIF.
*  ENDIF.




ENDFORM.


FORM fix_billing TABLES ivbrkvb STRUCTURE vbrkvb
                        ivbrpvb STRUCTURE vbrpvb
                        ivbpavb STRUCTURE vbpavb.

  DATA:ls_price_date TYPE datum.
  DATA:lv_waers_jy TYPE waers.

  LOOP AT ivbrkvb.

    ls_price_date = gv_zccth-remark02.

    CHECK ls_price_date IS NOT INITIAL.

**20220223wxy加，begin
    select single zzwaerk_jy into lv_waers_jy from zsdiv_int where zzivno = GV_LONG_VGBEL and kunnr = ivbrkvb-kunag.
    if sy-subrc = 0.
      ivbrkvb-waerk = lv_waers_jy.
    endif.
**20220223wxy加，end

    SELECT SINGLE waers FROM t001 WHERE bukrs = @ivbrkvb-bukrs
      INTO @DATA(ls_waers).

    CHECK ls_waers <> ivbrkvb-waerk.

    ivbrkvb-kurrf_dat = ls_price_date.

    PERFORM frm_get_exchangerate USING ivbrkvb-waerk ls_waers ls_price_date CHANGING ivbrkvb-kurrf.

    MODIFY ivbrkvb.
  ENDLOOP.
*  LOOP AT ivbrpvb.
*    ivbrpvb-vgbel = gv_vgbel.
*    ivbrpvb-prctr = gv_cct_prctr.
*    MODIFY ivbrpvb.
*    CLEAR ivbrpvb.
*  ENDLOOP.
*
*
*  LOOP AT ivbrkvb."处理运动城自营/外招需要不同的科目设置组 以确认不同科目
*    IF ( gv_cct_type = 'S02Y' OR gv_cct_type = 'S02Z'
*      OR gv_cct_type = 'S12Y' OR gv_cct_type = 'S12Z'
*      OR gv_cct_type = 'S22Y' OR gv_cct_type = 'S22Z' )
*   AND ( ivbrkvb-fkart = 'ZS01' OR ivbrkvb-fkart = 'ZT01' ).
*
*      ivbrkvb-ktgrd = '50'.
*      MODIFY ivbrkvb.
*      CONTINUE.
*    ENDIF.
*    IF ivbrkvb-kunrg <> ivbrkvb-kunag.
*      SELECT SINGLE ktgrd FROM knvv
*        INTO ivbrkvb-ktgrd
*        WHERE kunnr = ivbrkvb-kunag
*        AND vkorg = ivbrkvb-vkorg
*        AND vtweg = ivbrkvb-vtweg
*        AND spart = ivbrkvb-spart.
*      MODIFY ivbrkvb.
*    ENDIF.
*  ENDLOOP.
*
*
*
*  LOOP AT ivbrkvb.
*    IF ivbrkvb-fkart = 'ZS01' OR ivbrkvb-fkart = 'ZT01'.
*
*      LOOP AT ivbpavb WHERE vbeln = ivbrkvb-vbeln."
*        IF ( gv_cct_type = 'S02Y' OR gv_cct_type = 'S02Z'
*          OR gv_cct_type = 'S12Y' OR gv_cct_type = 'S12Z'
*          OR gv_cct_type = 'S22Y' OR gv_cct_type = 'S22Z' ).
*
*          ivbrkvb-kunrg = gv_channel1.
*          MODIFY ivbrkvb.
*          IF ivbpavb-parvw = 'RE' OR ivbpavb-parvw = 'RG'.
*            ivbpavb-kunnr = gv_channel1.
*            MODIFY ivbpavb.
*          ENDIF.
*
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*    IF ivbrkvb-fkart = 'ZS21' OR ivbrkvb-fkart = 'ZT21'.
*      LOOP AT ivbpavb WHERE parvw = 'WE' AND vbeln = ivbrkvb-vbeln.
*        READ TABLE gt_bp_info WITH KEY zcct_role = 'BL'.
*        IF sy-subrc EQ 0.
*          ivbpavb-kunnr = gt_bp_info-partner.
*          MODIFY ivbpavb.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*  ENDLOOP.
*
*
*  CLEAR:gt_vbrkvb[].
*  IF ivbrkvb[] IS NOT INITIAL.
*    APPEND LINES OF ivbrkvb TO gt_vbrkvb.
*  ENDIF.
*
*
*  LOOP AT ivbpavb WHERE parvw = 'WE'.
*    READ TABLE gt_bp_info WITH KEY zcct_role = 'BL'.
*    IF sy-subrc EQ 0.
*      ivbpavb-kunnr = gt_bp_info-partner.
*      MODIFY ivbpavb.
*    ENDIF.
*  ENDLOOP.



ENDFORM.

FORM fix_ac_extension TABLES ct_item  STRUCTURE bapiacgl09
                         ct_extension2  STRUCTURE bapiparex.

*  DATA:ls_enh  LIKE zsfi_acc_enh.
*
*  IF rs_hkont_bp[] IS INITIAL.
*    PERFORM fix_kh_hkont.
*  ENDIF.
*
*  CHECK rs_hkont_bp[] IS NOT INITIAL.
*
*  LOOP AT ct_item INTO DATA(ls_item)
*                   WHERE gl_account IN rs_hkont_bp
*                     AND customer IS NOT INITIAL .
*    CLEAR:ls_enh.
*    MOVE-CORRESPONDING ls_item TO ls_enh.
*    "增强字段
*    ls_enh-posnr   = ls_item-itemno_acc.
*    ls_enh-zzkunnr = ls_item-customer.
*
*
*    APPEND INITIAL LINE TO ct_extension2 ASSIGNING FIELD-SYMBOL(<fs_ext>).
*    MOVE 'ZSFI_ACC_ENH' TO <fs_ext>-structure.
*
*    CALL METHOD cl_abap_container_utilities=>fill_container_c
*      EXPORTING
*        im_value     = ls_enh
*      IMPORTING
*        ex_container = <fs_ext>-valuepart1.
*
*  ENDLOOP.
  "

ENDFORM.

FORM fix_kh_hkont.
  DATA:lv_setid LIKE sethier-setid.
  DATA:lt_value LIKE STANDARD TABLE OF rgsb4.

*  CLEAR:rs_hkont_bp[].
*  CLEAR:lv_setid,lt_value.
*
*  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
*    EXPORTING
*      setclass             = '0000'
*      shortname            = 'ZFLKM'
*    IMPORTING
*      setid                = lv_setid
*    EXCEPTIONS
*      no_co_area_specified = 1
*      illegal_setclass     = 2
*      OTHERS               = 3.
*  CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
**     CLIENT        = ' '
**     FORMULA_RETRIEVAL           = ' '
**     LEVEL         = 0
*      setnr         = lv_setid
**     VARIABLES_REPLACEMENT       = ' '
**     TABLE         = ' '
**     CLASS         = ' '
**     NO_DESCRIPTIONS             = 'X'
**     NO_RW_INFO    = 'X'
**     DATE_FROM     =
**     DATE_TO       =
**     FIELDNAME     = ' '
*    TABLES
*      set_values    = lt_value
*    EXCEPTIONS
*      set_not_found = 1
*      OTHERS        = 2.
*  LOOP AT lt_value INTO DATA(ls_value).
*    CLEAR:rs_hkont_bp.
*
*    CHECK ls_value-from IS NOT INITIAL OR
*          ls_value-to   IS NOT INITIAL.
*
*    IF ls_value-from = ls_value-to.
*      rs_hkont_bp-sign = 'I'.
*      rs_hkont_bp-option = 'EQ'.
*      rs_hkont_bp-low = ls_value-from.
*      APPEND rs_hkont_bp.
*    ELSE.
*      IF ls_value-to IS INITIAL.
*        rs_hkont_bp-sign = 'I'.
*        rs_hkont_bp-option = 'EQ'.
*        rs_hkont_bp-low = ls_value-from.
*        APPEND rs_hkont_bp.
*      ELSE.
*        rs_hkont_bp-sign = 'I'.
*        rs_hkont_bp-option = 'BT'.
*        rs_hkont_bp-low = ls_value-from.
*        rs_hkont_bp-high = ls_value-to.
*        APPEND rs_hkont_bp.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.


FORM fix_tax_code USING ls_cct_type ls_bukrs  CHANGING C_mwskz.
*  IF ls_cct_type = 'S14S' OR ls_cct_type = 'S14T'.
*    IF ( ls_bukrs = '1040'
*      OR ls_bukrs = '2600'
*      OR ls_bukrs = '2700'
*
*      )  AND gt_ac_config-mwskz = 'X3'.
*      gt_ac_config-mwskz = 'X4'.
*    ELSEIF ls_bukrs = '2203' AND gt_ac_config-mwskz = 'X3'.
*      gt_ac_config-mwskz = 'X1'.
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM fix_th_bp .

*  CHECK gv_cct_type = '4004'.
*  SELECT SINGLE * FROM zcct_mb_log INTO @DATA(ls_log) WHERE long_vgbel = @gv_long_vgbel2.
*
*  IF ls_log-mblnr IS INITIAL.
*    PERFORM add_message USING 'E' 'ZCCT' '000' '找不到退货的业务IDOC'  '' '' '' .
*    RETURN.
*  ENDIF.
*
*  SELECT SINGLE * INTO @DATA(ls_mseg) FROM mseg WHERE mblnr = @ls_log-mblnr.
*
*  CHECK ls_mseg-lgort = ls_mseg-umlgo.
*
*  LOOP AT gt_bp_config WHERE zcct_role = 'A1'.
*    gt_bp_config-zcct_channel = '1'.
*    MODIFY gt_bp_config.
*
*  ENDLOOP.

ENDFORM.

FORM fix_cct_type_7002 CHANGING cs_item STRUCTURE bapiacgl09.

  CHECK gv_cct_type = '7002'.

  CHECK cs_item-gl_account = '5001010000'.

  CLEAR cs_item-costcenter.

  BREAK h-wangf.

  IF gt_data-vbeln_va IS INITIAL OR gt_data-posnr_va IS INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '001' '销售订单号不存在,无法出口发料' '' '' ''.
    RETURN.
  ENDIF.

  SELECT SINGLE zfscgdh INTO @DATA(ls_order)
    FROM ztpp0089 WHERE vbeln = @gt_data-vbeln_va AND posnr = @gt_data-posnr_va.
  IF sy-subrc NE 0.
    PERFORM add_message USING 'E' 'ZCCT' '001' '销售订单号' gt_data-vbeln_va '在大货通知单中不存在,无法发料' ''.
    RETURN.
  ENDIF.

  IF ls_order IS INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '001' '销售订单' gt_data-vbeln_va '辅生产订单为空,无法发料' ''.
    RETURN.
  ENDIF.

  cs_item-orderid = ls_order.

ENDFORM.

FORM fix_set_aufnr_cost TABLES ct_data STRUCTURE zcct_data.
  CHECK gv_cct_type = '7002' OR gv_cct_type = '8007'.

  CHECK ( gs_config-zcct_werks+0(1) EQ '5' AND gs_config-zcct_vkorg+0(1) NE '5' )
    OR ( gs_config-zcct_werks+0(1) EQ '3' AND gs_config-zcct_vkorg+0(1) NE '3' ).

  DATA:lt_data TYPE TABLE OF zcct_data WITH HEADER LINE.

  DATA:ls_netwr_jgf_x TYPE netwr.

  DATA:ltx_order_cost TYPE TABLE OF zcct_order_cost WITH HEADER LINE .
  DATA:lty_order_cost TYPE TABLE OF zcct_order_cost WITH HEADER LINE .

  DATA:ls_netwr_in_y TYPE netwr.
  DATA:ls_netwr_in_x TYPE netwr.
  DATA:ls_netwr_out_y  TYPE netwr.
  DATA:ls_netwr_out_x TYPE netwr.
  DATA:ls_netwr_out_e TYPE netwr.

  DATA:ls_menge_y TYPE menge_d.
  DATA:ls_menge_x TYPE menge_d.



  lt_data[] = ct_data[].

  SORT lt_data BY vbeln_va posnr_va.

  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING vbeln_va posnr_va.


  LOOP AT lt_data.
    IF lt_data-vbeln_va IS INITIAL OR lt_data-posnr_va IS INITIAL.
      PERFORM add_message USING 'E' 'ZCCT' '000' '销售订单号不存在,无法出口发料' '' '' ''.
      RETURN.
    ENDIF.

    SELECT SINGLE zfscgdh INTO @DATA(ls_order)
      FROM ztpp0089 WHERE vbeln = @lt_data-vbeln_va AND posnr = @lt_data-posnr_va.
    IF sy-subrc NE 0.
      PERFORM add_message USING 'E' 'ZCCT' '000' '销售订单号' lt_data-vbeln_va '在大货通知单中不存在,无法发料' ''.
      RETURN.
    ENDIF.

    IF ls_order IS INITIAL.
      PERFORM add_message USING 'E' 'ZCCT' '000' '销售订单' lt_data-vbeln_va '辅生产订单为空,无法发料' ''.
      RETURN.
    ENDIF.

    IF gv_cct_type = '7002'.
      CLEAR ls_netwr_in_x.
      LOOP AT ct_data WHERE vbeln_va = lt_data-vbeln_va AND posnr_va = lt_data-posnr_va.
        ls_netwr_in_x = ls_netwr_in_x + ct_data-amount2.
        ct_data-aufnr = ls_order.
        MODIFY ct_data.
      ENDLOOP.
      ltx_order_cost-aufnr = ls_order.
      ltx_order_cost-docnum = gv_docnum.
      ltx_order_cost-cct_type = gv_cct_type.
      ltx_order_cost-netwr_fl = ls_netwr_in_x.
      APPEND ltx_order_cost.
      CLEAR ltx_order_cost.
    ENDIF.





    IF gv_cct_type = '8007'.

      CLEAR: ls_menge_y,ls_menge_x,
             ls_netwr_jgf_x,
             ls_netwr_in_x,ls_netwr_in_y,
             ls_netwr_out_x,ls_netwr_out_y,ls_netwr_out_e.

      SELECT zcct_order_cost~* FROM zcct_order_cost
        INNER JOIN edidc ON zcct_order_cost~docnum = edidc~docnum
        INTO TABLE @lty_order_cost
        WHERE aufnr = @ls_order
        AND status = '53'.

      SELECT SINGLE psmng INTO @DATA(ls_psmng) FROM afpo
        WHERE aufnr = @ls_order.

      LOOP AT lty_order_cost.
        ls_menge_y = ls_menge_y + lty_order_cost-wemng.
        ls_netwr_in_y = ls_netwr_in_y + lty_order_cost-netwr_fl + lty_order_cost-netwr_jgf.
        ls_netwr_out_y = ls_netwr_in_y + lty_order_cost-netwr_sh .
      ENDLOOP.


      LOOP AT ct_data WHERE vbeln_va = lt_data-vbeln_va AND posnr_va = lt_data-posnr_va.
        ct_data-aufnr = ls_order.
        ls_menge_x = ls_menge_x + ct_data-menge.
        ls_netwr_jgf_x = ls_netwr_jgf_x + ct_data-amount1.
        MODIFY ct_data.
        CLEAR ct_data.
      ENDLOOP.

      ls_netwr_out_x = ( ls_netwr_in_y + ls_netwr_jgf_x ) "原投入金额+本次的加工费
                       / ls_psmng *                       "除以总计划数量
                       ( ls_menge_x + ls_menge_y )        "乘以 本次收货数量+原收货数量
                       - ls_netwr_out_y.                  "减去过去产出

      IF ls_netwr_out_x <= 0.
        ls_netwr_out_x = 0.
      ENDIF.

      ls_netwr_out_e = ls_netwr_out_x.

      LOOP AT ct_data WHERE vbeln_va = lt_data-vbeln_va AND posnr_va = lt_data-posnr_va.
        ct_data-amount3 = ls_netwr_out_x / ls_menge_x * ct_data-menge .
        ls_netwr_out_e = ls_netwr_out_e - ct_data-amount3.
        MODIFY ct_data.
        CLEAR ct_data.
      ENDLOOP.

      IF ls_netwr_out_e IS NOT INITIAL.
        READ TABLE ct_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY
                                      vbeln_va = lt_data-vbeln_va posnr_va = lt_data-posnr_va.
        IF sy-subrc EQ 0.
          <fs_data>-amount3 = <fs_data>-amount3 + ls_netwr_out_e.
        ENDIF.
      ENDIF.

      ltx_order_cost-aufnr = ls_order.
      ltx_order_cost-docnum = gv_docnum.
      ltx_order_cost-cct_type = gv_cct_type.
      ltx_order_cost-wemng = ls_menge_x.
      ltx_order_cost-netwr_jgf = ls_netwr_jgf_x.
      ltx_order_cost-netwr_sh = ls_netwr_out_x.
      APPEND ltx_order_cost.
      CLEAR ltx_order_cost.

    ENDIF.

  ENDLOOP.

  IF ltx_order_cost[] IS NOT INITIAL.
    MODIFY zcct_order_cost FROM TABLE ltx_order_cost.
  ENDIF.

ENDFORM.

FORM fix_aufnr CHANGING cs_item STRUCTURE bapiacgl09.

  CHECK gv_cct_type = '7002' OR gv_cct_type = '8007'.

  CHECK ( gs_config-zcct_werks+0(1) EQ '5' AND gs_config-zcct_vkorg+0(1) NE '5' )
    OR ( gs_config-zcct_werks+0(1) EQ '3' AND gs_config-zcct_vkorg+0(1) NE '3' ).


  IF cs_item-gl_account+0(1) = '5'.
    CLEAR cs_item-costcenter.
  ELSE.
    CLEAR cs_item-orderid.
  ENDIF.

ENDFORM.
