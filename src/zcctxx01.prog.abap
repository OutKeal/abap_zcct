

*&---------------------------------------------------------------------*
*& 包含               ZCCTXX01
*&---------------------------------------------------------------------*

FORM init.
  CLEAR:
         gv_brand      ,
         gv_cct_werks  ,
         gv_cct_lgort  ,
         gv_cct_umlgo  ,
         gv_cct_vkorg  ,
         gv_cct_vtweg  ,
         gv_cct_spart  ,
         gv_cct_bukrs  ,
         gv_cct_umwrk  ,
         gv_cct_prctr  ,
         gv_vgbel      ,
         gv_long_vgbel ,
         gv_channel1   ,
         gv_channel2   ,
         gv_channel3   ,
         gv_cct_bl_kunnr,
         gv_lognr      ,
         gv_vgpos      ,
         gv_cct_type   ,
         gv_post_date  ,
         gv_bktxt      ,
         gs_config     ,
         mb_doc,
         mb_year,
         gv_fldname,
         gv_strname,
         gv_conname,
         gv_skip_mb_enhance,
         gv_convalue,
         gv_zcct_active,
         gv_cct_ac_kunnr,
         gv_cct_ac_lifnr,



         gv_error.
  CLEAR: gt_mb_config,
         gt_mb_config[],
         gt_bl_config,
         gt_bl_config[],
         gt_ac_config,
         gt_ac_config[],
         gt_line_config,
         gt_line_config[],

         gt_bp_config,
         gt_bp_config[],

         gt_pr_config,
         gt_pr_config[],


         gt_price,
         gt_price[],

         gt_bp_info,
         gt_bp_info[],


         gt_data,
         gt_data[],
         gt_line_data,
         gt_line_data[],

         ot_return,
         ot_return[],

         gt_borident,
         gt_borident[],

         g_foldoc_attr,

         gt_vgbel_log,
         gt_vgbel_log[],
         gt_zcct_bl_list,
         gt_zcct_bl_list[],
         gt_cost,
         gt_cost[],

         gt_mb_cost,
         gt_mb_cost[],

         gt_t0010,
         gt_t0010[].


  CLEAR gs_js.

  CLEAR gv_docnum.
  CLEAR gv_mescod.
  CLEAR gv_cancel.

  CLEAR gv_bsart.
  CLEAR gv_ekgrp.

*         gt_zqm_werks_active,
*         gt_zqm_werks_active[].

  IF <field_value> IS ASSIGNED.
    FREE <field_value>.
  ENDIF.

  PERFORM clear_amount.



ENDFORM.
FORM set_idoc_info USING im_idocnum im_mescod.

  gv_docnum = im_idocnum.
  gv_mescod = im_mescod.
  gt_borident-objkey = im_idocnum."写入关系浏览器对象
  gt_borident-objtype = 'IDOC'.
  APPEND gt_borident.
  CLEAR gt_borident.

ENDFORM.

FORM get_next_vgbel USING i_long_vgbel  CHANGING e_vgbel TYPE vgbel.
  IF i_long_vgbel IS INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '000' '外部长参考不能为空' '' '' ''.
    gv_error = 'X'.
    RETURN.
  ENDIF.

  SELECT SINGLE long_vgbel INTO @DATA(ls_vgbel) FROM zcct_vgbel_head
    WHERE long_vgbel = @i_long_vgbel AND zcct_type = @gv_cct_type AND stokz <> 'X'.
  IF sy-subrc EQ 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' i_long_vgbel '已存在' '' ''.
  ENDIF.


  IF e_vgbel IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZCCT_VGBEL'
      IMPORTING
        number                  = e_vgbel
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      PERFORM add_message USING 'E' 'ZCCT' '000' '参考编号获取错误' '' '' ''.
      gv_error = 'X'.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.



ENDFORM.



FORM get_next_lognr  CHANGING e_lognr TYPE lognr.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCCT_LOGNR'
    IMPORTING
      number                  = e_lognr
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' '日志编号获取错误' '' '' ''.
    gv_error = 'X'.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.


FORM get_next_bl_vg  CHANGING e_lognr TYPE lognr.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCCT_BL_VG'
    IMPORTING
      number                  = e_lognr
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' '日志编号获取错误' '' '' ''.
    gv_error = 'X'.
  ENDIF.

ENDFORM.

FORM init_mt_config.

  SELECT SINGLE * FROM zcct_mt
    INTO @DATA(ls_zcct_mt)
    WHERE zcct_type = @gv_cct_type.

  IF sy-subrc NE 0.
    PERFORM add_message USING 'E' 'ZCCT' '000' '业务类型不存在'  '' '' '' .
    gv_error = 'X'.
    RETURN.
  ENDIF.

  SELECT * INTO TABLE gt_bp_config FROM zcct_bp_config
    WHERE zcct_type = gv_cct_type.

  SELECT * INTO TABLE gt_pr_config FROM zcct_pr_config
    WHERE zcct_type = gv_cct_type.

  SELECT SINGLE * INTO gs_js FROM zcct_js
    WHERE zcct_type = gv_cct_type.


  PERFORM fix_th_bp.
ENDFORM.

FORM init_bp_info.
  CHECK gt_bp_config[] IS NOT INITIAL.
  DATA: ls_zscmt0010   TYPE zscmt0010,
        ls_zscmt0010_2 TYPE zscmt0010.
  DATA: ls_channel  TYPE bu_partner,
        ls_channel1 TYPE bu_partner,
        ls_channel2 TYPE bu_partner,
        ls_channel3 TYPE bu_partner.

  LOOP AT gt_bp_config.
    IF gv_channel1 IS NOT INITIAL.
      PERFORM frm_add_zero CHANGING gv_channel1.
      ls_channel1 = gv_channel1.
    ENDIF.

    IF gv_channel2 IS NOT INITIAL.
      PERFORM frm_add_zero CHANGING gv_channel2.
      ls_channel2 = gv_channel2.
    ENDIF.

    IF gv_channel3 IS NOT INITIAL.
      PERFORM frm_add_zero CHANGING gv_channel3.
      ls_channel3 = gv_channel3.
    ENDIF.

    CASE  gt_bp_config-zcct_channel.
      WHEN '1'.
        ls_channel = ls_channel1.
      WHEN '2'.
        ls_channel = ls_channel2.
      WHEN '3'.
        ls_channel = ls_channel3.
    ENDCASE.

    IF ls_channel IS INITIAL.
      PERFORM add_message USING 'E' 'ZCCT' '017' gt_bp_config-zcct_channel  '' '' '' .
      gv_error = 'X'.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'ZMDA_GET_PARTNER'
      EXPORTING
        i_partner  = ls_channel
      IMPORTING
        e_scmt0010 = ls_zscmt0010
      EXCEPTIONS
        notfind    = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      PERFORM add_message USING 'E' 'ZCCT' '018' gt_bp_config-zcct_channel ls_channel  '' '' .
      gv_error = 'X'.
      CONTINUE.
    ENDIF.

    CLEAR gt_bp_info.
    MOVE-CORRESPONDING ls_zscmt0010 TO gt_bp_info.
    gt_bp_info-zcct_role = gt_bp_config-zcct_role.
    APPEND gt_bp_info .
    CLEAR gt_bp_info.

    CASE gt_bp_config-zcct_role.
      WHEN 'A1'. "物料凭证一般库位，发送方库存地点
        gv_cct_werks = ls_zscmt0010-werks.
        gv_cct_lgort = ls_zscmt0010-lgort.
        IF ls_zscmt0010-werks IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '019' ls_channel ''  '' '' .
          gv_error = 'X'.
          CONTINUE.
        ENDIF.

        IF ls_channel2 IS NOT INITIAL.
          CALL FUNCTION 'ZMDA_GET_PARTNER'
            EXPORTING
              i_partner  = ls_channel2
            IMPORTING
              e_scmt0010 = ls_zscmt0010_2
            EXCEPTIONS
              notfind    = 1
              OTHERS     = 2.
          IF sy-subrc EQ 0 .
            gv_cct_umwrk = ls_zscmt0010_2-werks.
            gv_cct_umlgo = ls_zscmt0010_2-lgort.
          ENDIF.
        ENDIF.

      WHEN 'A2'.  "物料凭证MOVE 接受库位
        gv_cct_umwrk = ls_zscmt0010-werks.
        gv_cct_umlgo = ls_zscmt0010-lgort.
        IF ls_zscmt0010-werks IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '019' ls_channel ''  '' '' .
          gv_error = 'X'.
          CONTINUE.
        ENDIF.
      WHEN 'BL'.  "发票默认付款方

        gv_cct_vkorg = ls_zscmt0010-vkorg.
        gv_cct_vtweg = ls_zscmt0010-vtweg.
        gv_cct_spart = ls_zscmt0010-spart.
        gv_cct_bukrs = ls_zscmt0010-bukrs.
        gv_cct_prctr = ls_zscmt0010-prctr.
        gv_cct_bl_kunnr = ls_channel.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gv_cct_bl_kunnr
          IMPORTING
            output = gv_cct_bl_kunnr.

        LOOP AT gt_data.
          gt_data-kunnr = gv_cct_bl_kunnr.
          MODIFY gt_data.
        ENDLOOP.

        IF gv_cct_vkorg IS INITIAL
          OR gv_cct_vtweg IS INITIAL
          OR gv_cct_bukrs IS INITIAL.
*          OR gv_cct_prctr IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '020' ls_channel ''  '' '' .
          gv_error = 'X'.
          CONTINUE.
        ENDIF.

      WHEN 'IC'.  "跨公司销售客户角色

        READ TABLE gt_bp_info INTO DATA(ls_bp_info) WITH KEY partner = ls_channel.
        IF sy-subrc EQ 0.
          gv_cct_vkorg = ls_bp_info-vkorg.
          gv_cct_vtweg = ls_bp_info-vtweg.
          gv_cct_spart = ls_bp_info-spart.
          gv_cct_bukrs = ls_bp_info-bukrs.
          gv_cct_prctr = ls_bp_info-prctr.
        ENDIF.
        gv_cct_bl_kunnr = ls_zscmt0010-bukrs.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gv_cct_bl_kunnr
          IMPORTING
            output = gv_cct_bl_kunnr.

        LOOP AT gt_data.
          gt_data-kunnr = gv_cct_bl_kunnr.
          MODIFY gt_data.
        ENDLOOP.

        IF gv_cct_vkorg IS INITIAL
          OR gv_cct_vtweg IS INITIAL
          OR gv_cct_bukrs IS INITIAL.
*          OR gv_cct_prctr IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '020' ls_channel ''  '' '' .
          gv_error = 'X'.
          CONTINUE.
        ENDIF.


      WHEN 'IV'.  "跨公司采购供应商角色

        READ TABLE gt_bp_info INTO ls_bp_info WITH KEY partner = ls_channel.
        IF sy-subrc EQ 0.
          gv_cct_vkorg = ls_bp_info-vkorg.
          gv_cct_vtweg = ls_bp_info-vtweg.
          gv_cct_spart = ls_bp_info-spart.
*          gv_cct_bukrs = ls_bp_info-bukrs.
          gv_cct_prctr = ls_bp_info-prctr.
        ENDIF.
        gv_cct_ac_lifnr = ls_zscmt0010-bukrs.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gv_cct_ac_lifnr
          IMPORTING
            output = gv_cct_ac_lifnr.

        LOOP AT gt_data.
          gt_data-lifnr = gv_cct_ac_lifnr.
          MODIFY gt_data.
        ENDLOOP.

        IF gv_cct_bukrs IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '020' ls_channel ''  '' '' .
          CONTINUE.
        ENDIF.

      WHEN 'LC' .  "物料凭证客户
        LOOP AT gt_data.
          gt_data-kunnr = ls_channel.
          MODIFY gt_data.
        ENDLOOP.
      WHEN 'LV' ."物料凭证供应商
        LOOP AT gt_data.
          gt_data-lifnr = ls_channel.
          MODIFY gt_data.
        ENDLOOP.
      WHEN 'WZ'.
        gv_cct_ac_lifnr = ls_channel.

      WHEN 'KU'.
        gv_cct_ac_kunnr = ls_channel.
*      WHEN 'SV'.
*        SELECT SINGLE lifnr INTO gv_cct_ac_lifnr
*          FROM zscmt1010 WHERE channel = ls_channel.
*        IF sy-subrc NE 0.
*          PERFORM add_message USING 'E' 'ZCCT' '020' ls_channel ''  '' '' .
*        ELSE.
*          SELECT SINGLE * FROM zscmt0010 INTO ls_zscmt0010 WHERE partner = gv_cct_ac_lifnr.
*          MOVE-CORRESPONDING ls_zscmt0010 TO gt_bp_info.
*          gt_bp_info-zcct_role = gt_bp_config-zcct_role.
*          APPEND gt_bp_info.
*          CLEAR gt_bp_info.
*        ENDIF.
*        gv_cct_ac_lifnr = ls_channel.
      WHEN OTHERS.

    ENDCASE.
  ENDLOOP.


ENDFORM.



FORM init_post_config USING i_vkorg i_werks.

  IF i_vkorg IS INITIAL.
    i_vkorg = gv_cct_vkorg.
  ENDIF.

  IF i_werks IS INITIAL.
    i_werks = gv_cct_werks.
  ENDIF.

  SELECT SINGLE * FROM zcct_config INTO gs_config
    WHERE zcct_werks = i_werks
      AND zcct_vkorg = i_vkorg
      AND zcct_type = gv_cct_type.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM zcct_config INTO gs_config
      WHERE zcct_werks = ''
        AND zcct_vkorg = ''
        AND zcct_type = gv_cct_type.
    IF sy-subrc NE 0.
      PERFORM add_message USING 'E' 'ZCCT' '001' gv_cct_werks  gv_cct_type '' '' .
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  IF gs_config-zcct_mbdc_act = 'X'.
    SELECT * INTO TABLE gt_mb_config FROM zcct_mb_config
            WHERE zcct_werks = gs_config-zcct_werks
            AND zcct_vkorg = gs_config-zcct_vkorg
            AND zcct_type = gs_config-zcct_type
            ORDER BY zcct_line.
    IF sy-subrc NE 0 .
      PERFORM add_message USING 'E' 'ZCCT' '002' gv_cct_werks  gv_cct_type '' '' .
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  IF gs_config-zcct_bill_act = 'X'.
    SELECT * INTO TABLE gt_bl_config FROM zcct_bl_config
      WHERE zcct_werks = gs_config-zcct_werks
          AND zcct_vkorg = gs_config-zcct_vkorg
      AND zcct_type = gs_config-zcct_type
      ORDER BY zcct_line.
    IF sy-subrc NE 0 .
      PERFORM add_message USING 'E' 'ZCCT' '003' gv_cct_werks  gv_cct_type '' '' .
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  IF gs_config-zcct_acdc_act = 'X'.
    SELECT * INTO TABLE gt_ac_config FROM zcct_ac_config
          WHERE zcct_werks = gs_config-zcct_werks
          AND zcct_vkorg = gs_config-zcct_vkorg
          AND zcct_type = gs_config-zcct_type
          ORDER BY zcct_line.

    IF gt_ac_config[] IS INITIAL.
      PERFORM add_message USING 'E' 'ZCCT' '004' gv_cct_werks  gv_cct_type '' '' .
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM fix_kp_cb.

ENDFORM.


FORM save_msg_log TABLES c_return STRUCTURE bapiret2.
  DATA:lt_msg_log TYPE TABLE OF zcct_msg_log WITH HEADER LINE.
  DATA:lt_wplst TYPE TABLE OF wplst WITH HEADER LINE.

  CLEAR lt_msg_log.
  CLEAR lt_msg_log[].
  CLEAR lt_wplst.
  CLEAR lt_wplst[].
  IF gv_error = 'X'.
    DELETE ot_return WHERE type = 'S'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  c_return[] = ot_return[].


  LOOP AT ot_return.
    IF gv_lognr IS NOT INITIAL AND gv_vgbel IS NOT INITIAL.
      MOVE-CORRESPONDING ot_return TO lt_msg_log.
      lt_msg_log-lognr = gv_lognr.
      lt_msg_log-vgbel = gv_vgbel.
      lt_msg_log-zcct_type = gv_cct_type.
      lt_msg_log-long_vgbel = gv_long_vgbel.
      lt_msg_log-zcct_line = sy-tabix.
      lt_msg_log-docnum = gv_docnum.
      lt_msg_log-msgty = ot_return-type.
      lt_msg_log-msgid = ot_return-id.
      lt_msg_log-msgno = ot_return-number.
      lt_msg_log-msgv1 = ot_return-message_v1.
      lt_msg_log-msgv2 = ot_return-message_v2.
      lt_msg_log-msgv3 = ot_return-message_v3.
      lt_msg_log-msgv4 = ot_return-message_v4.
      lt_msg_log-message = ot_return-message.
      lt_msg_log-ernam = sy-uname.
      lt_msg_log-erdat = sy-datum.
      lt_msg_log-erzet = sy-uzeit.
      APPEND lt_msg_log.
      CLEAR lt_msg_log.
    ENDIF.

    IF gv_docnum IS NOT INITIAL.
      lt_wplst-docnum = gv_docnum.
      lt_wplst-segnum = 1.
      lt_wplst-msgnr = ot_return-number.
      lt_wplst-fehlertyp = ot_return-type.
      lt_wplst-msgid = ot_return-id.
      lt_wplst-filiale = gv_channel1.
      lt_wplst-parameter1 = ot_return-message_v1.
      lt_wplst-parameter2 = ot_return-message_v2.
      lt_wplst-parameter3 = ot_return-message_v3.
      lt_wplst-parameter4 = ot_return-message_v4.
      lt_wplst-erzdt = sy-datum.
      lt_wplst-doctyp = 'ZCCT'.

      APPEND lt_wplst.
      CLEAR lt_wplst.

    ENDIF.
  ENDLOOP.




  PERFORM save_ift0001.

  IF lt_msg_log[] IS NOT INITIAL.
    MODIFY zcct_msg_log FROM TABLE lt_msg_log[].
  ENDIF.

  IF lt_wplst[] IS NOT INITIAL.
    DELETE FROM wplst WHERE docnum = gv_docnum.
    MODIFY wplst FROM TABLE lt_wplst[].
  ENDIF.



  IF gv_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*     IMPORTING
*       RETURN        =
      .

  ENDIF.

  EXPORT ot_return TO MEMORY ID 'ZCCT_RETURN'.

ENDFORM.

FORM save_ift0001.
*  DATA:ls_zift0001 TYPE TABLE OF zift0001 WITH HEADER LINE.
*
*  IF gv_docnum IS NOT INITIAL .
*    ls_zift0001-docnum = gv_docnum.
*    ls_zift0001-mestyp = 'ZCCT'.
*    ls_zift0001-butype = gv_mescod.
*    ls_zift0001-key1 = gv_cct_type.
*    ls_zift0001-key2 = gv_long_vgbel.
*    IF gv_cancel = 'X'.
*      ls_zift0001-key3 = 'C'.
*    ENDIF.
*    ls_zift0001-zifdir = 'I'.
*
*
*    IF gv_error = 'X'.
*      READ TABLE ot_return WITH KEY type = 'E'.
*      IF sy-subrc EQ 0.
*        ls_zift0001-ztype = ot_return-type.
*        ls_zift0001-zmsg = ot_return-message.
*      ENDIF.
*    ELSE.
*      READ TABLE ot_return INDEX 1.
*      IF sy-subrc EQ 0.
*        ls_zift0001-ztype = ot_return-type.
*        ls_zift0001-zmsg = ot_return-message.
*      ENDIF.
*    ENDIF.
*    ls_zift0001-erdat = sy-datum.
*    ls_zift0001-erzet = sy-uzeit.
*    ls_zift0001-ernam = sy-uname.
*    ls_zift0001-modat = sy-datum.
*    ls_zift0001-mozet = sy-uzeit.
*    ls_zift0001-monam = sy-uname.
*
*    MODIFY zift0001 FROM ls_zift0001.
*  ENDIF.

ENDFORM.

FORM save_vgbel_log .

  DATA: lt_vgbel_log TYPE TABLE OF zcct_vgbel_log WITH HEADER LINE.
  DATA: ls_vgbel_head TYPE  zcct_vgbel_head .
  CLEAR ls_vgbel_head.
  DATA: ls_mb_log TYPE zcct_mb_log.

  ls_vgbel_head-lognr = gv_lognr.
  ls_vgbel_head-vgbel = gv_vgbel.
  ls_vgbel_head-long_vgbel = gv_long_vgbel.
  ls_vgbel_head-docnum = gv_docnum.
  ls_vgbel_head-zcct_type = gv_cct_type.
  ls_vgbel_head-ernam = sy-uname.
  ls_vgbel_head-erdat = sy-datum.
  ls_vgbel_head-erzet = sy-uzeit.
  MODIFY zcct_vgbel_head FROM ls_vgbel_head.


  LOOP AT gt_borident.


    lt_vgbel_log-lognr = gv_lognr.
    lt_vgbel_log-docnum = gv_docnum.
    lt_vgbel_log-zcct_line = sy-tabix.
    lt_vgbel_log-swo_typeid = gt_borident-objkey.
    lt_vgbel_log-swo_objtyp = gt_borident-objtype.

    CASE gt_borident-objtype.
      WHEN 'BUS2017'.
        lt_vgbel_log-docnr = gt_borident-objkey+0(10).
        lt_vgbel_log-cjahr = gt_borident-objkey+10(4).

        ls_mb_log-docnum = gv_docnum.
        ls_mb_log-mblnr = lt_vgbel_log-docnr.
        ls_mb_log-mjahr  = lt_vgbel_log-cjahr.
        ls_mb_log-long_vgbel = gv_long_vgbel.
        ls_mb_log-long_vgbel2 = gv_long_vgbel2.
        ls_mb_log-remark01 = gv_zccth-remark01.
        ls_mb_log-remark02 = gv_zccth-remark02.
        ls_mb_log-remark03 = gv_zccth-remark03.
        ls_mb_log-remark04 = gv_zccth-remark04.
        ls_mb_log-remark05 = gv_zccth-remark05.
        ls_mb_log-remark06 = gv_zccth-remark06.
        ls_mb_log-remark07 = gv_zccth-remark07.
        ls_mb_log-remark08 = gv_zccth-remark08.
        ls_mb_log-remark09 = gv_zccth-remark09.
        ls_mb_log-remark10 = gv_zccth-remark10.


      WHEN 'BKPF'.
        lt_vgbel_log-bukrs = gt_borident-objkey+0(4).
        lt_vgbel_log-docnr = gt_borident-objkey+4(10).
        lt_vgbel_log-cjahr = gt_borident-objkey+14(4).
      WHEN 'VBRK'.
        lt_vgbel_log-docnr = gt_borident-objkey+0(10).
    ENDCASE.
    APPEND lt_vgbel_log.
    CLEAR lt_vgbel_log.

  ENDLOOP.

  MODIFY zcct_vgbel_log FROM TABLE lt_vgbel_log.

  IF ls_mb_log IS NOT INITIAL.
    MODIFY zcct_mb_log FROM ls_mb_log.
  ENDIF.


ENDFORM.


FORM mb_doc_create.
  DATA: ls_mwskz     TYPE mwskz.
  DATA: mb_head TYPE bapi2017_gm_head_01.
  DATA: mb_code TYPE bapi2017_gm_code.
  DATA: mb_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE .
  DATA: mb_line_id TYPE mb_line_id.
  DATA: tax_amount TYPE netwr.

  DATA:lv_netwr TYPE netwr.
  DATA:lv_tax TYPE netwr.
  DATA:lv_waers TYPE waers.


  LOOP AT gt_ac_config .
    ls_mwskz = gt_ac_config-mwskz.
    IF gt_ac_config-mwskz IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF ls_mwskz IS INITIAL.
    IF gv_tax_code IS NOT INITIAL.
      ls_mwskz = gv_tax_code.
    ELSE.
      CALL FUNCTION 'ZCCT_GET_TAX'
        EXPORTING
          i_tax_type = 'J'
        IMPORTING
          e_tax_code = ls_mwskz.
    ENDIF.
  ENDIF.

  mb_head-pstng_date = gv_post_date.
  mb_head-doc_date = gv_post_date.
  mb_head-pr_uname = sy-uname.
  mb_head-header_txt = gv_bktxt.
  mb_code-gm_code = '06'.

  CLEAR mb_line_id.

*-------->根据物料号获取税率高低的配置
  IF gt_data[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(lt_mlan)
      FROM mlan
      FOR ALL ENTRIES IN @gt_data[]
      WHERE matnr = @gt_data-matnr
      AND aland = 'CN'.
  ENDIF.

  LOOP AT gt_data."拼装物料凭证

    LOOP AT gt_mb_config.
      CLEAR lv_netwr.
      CLEAR lv_tax.

      CLEAR mb_item.
      mb_line_id = mb_line_id + 1.
      mb_item-line_id = mb_line_id.
*      mb_item-line_id = gt_data-line_id.

      mb_head-ref_doc_no = gv_vgbel.
      mb_item-material_long = gt_data-matnr.

      mb_item-batch = gt_data-charg.
      mb_item-move_batch = gt_data-umcha.

      IF gt_data-menge < 0.
        IF gt_mb_config-nbwart IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '019' gv_cct_type  '' '' '' ."插入单一消息
        ENDIF.
        mb_item-entry_qnt = - gt_data-menge.
        mb_item-move_type = gt_mb_config-nbwart.
      ELSEIF gt_data-menge = 0.
        PERFORM add_message USING 'E' 'ZCCT' '000' '数量不能等于0'  '' '' '' ."插入单一消息
      ELSEIF gt_data-menge > 0.
        mb_item-entry_qnt = gt_data-menge.
        mb_item-move_type = gt_mb_config-bwart.
      ENDIF.

      mb_item-mvt_ind = gt_mb_config-kzbew.
      mb_item-spec_stock = gt_mb_config-sobkz.

      mb_item-stck_type = gt_mb_config-insmk.
      mb_item-move_reas = gt_mb_config-grund.


      IF gt_mb_config-werks IS NOT INITIAL.

        IF gt_mb_config-werks = 'A1'.
          mb_item-plant = gv_cct_werks.
        ELSEIF gt_mb_config-werks = 'A2'.
          mb_item-plant = gv_cct_umwrk.
        ELSE.
          mb_item-plant = gt_mb_config-werks.
        ENDIF.

      ELSE.
        mb_item-plant = gv_cct_werks.
      ENDIF.

      IF gt_mb_config-lgort IS NOT INITIAL.

        IF gt_mb_config-lgort = 'A1'.
          mb_item-stge_loc = gv_cct_lgort.
        ELSEIF gt_mb_config-lgort = 'A2'.
          mb_item-stge_loc = gv_cct_umlgo.
        ELSE.
          mb_item-stge_loc = gt_mb_config-lgort.
        ENDIF.

      ELSE.
        mb_item-stge_loc = gv_cct_lgort.
      ENDIF.


      IF gt_mb_config-umwrk IS NOT INITIAL.
        IF gt_mb_config-umwrk = 'A1'.
          mb_item-move_plant = gv_cct_werks.
        ELSEIF gt_mb_config-umwrk = 'A2'.
          mb_item-move_plant = gv_cct_umwrk.
        ELSE.
          mb_item-move_plant = gt_mb_config-umwrk.
        ENDIF.

      ELSE.
        mb_item-move_plant = gv_cct_umwrk.
      ENDIF.

      IF gt_mb_config-umlgo IS NOT INITIAL.

        IF gt_mb_config-umlgo = 'A1'.
          mb_item-move_stloc = gv_cct_lgort.
        ELSEIF gt_mb_config-umlgo = 'A2'.
          mb_item-move_stloc = gv_cct_umlgo.
        ELSE.
          mb_item-move_stloc = gt_mb_config-umlgo.
        ENDIF.

      ELSE.
        mb_item-move_stloc = gv_cct_umlgo.
      ENDIF.



      mb_item-vendor = gt_data-lifnr.
      mb_item-customer = gt_data-kunnr.

      mb_item-sales_ord = gt_data-vbeln_va.
      mb_item-s_ord_item = gt_data-posnr_va.

      mb_item-val_sales_ord = gt_data-vbeln_va.
      mb_item-val_s_ord_item = gt_data-posnr_va.

      IF mb_item-sales_ord IS NOT INITIAL AND mb_item-spec_stock IS INITIAL..
        mb_item-spec_stock = 'E'.
      ENDIF.

      IF gt_mb_config-bwart = '261'.
        mb_item-orderid = gt_data-aufnr.
      ENDIF.
      CASE gt_mb_config-kzbew.
        WHEN 'F'.
          mb_item-orderid = gt_data-aufnr.
        WHEN 'B'.
          mb_item-po_number = gt_data-ebeln.
          mb_item-po_item = gt_data-ebelp.
      ENDCASE.

      IF gt_mb_config-prctr IS NOT INITIAL.
        mb_item-profit_ctr = gt_mb_config-prctr.
      ELSE.
        mb_item-profit_ctr = gv_cct_prctr.
      ENDIF.

      mb_item-costcenter = gt_mb_config-kostl.
      IF mb_item-costcenter = 'DATA'.
        mb_item-costcenter = gt_data-kostl.
      ENDIF.


*      LOOP AT gt_line_config WHERE zcct_werks = gv_cct_werks"动态指针传值，须函数结构转入值与本行LINE配置匹配。
*                                AND zcct_type = gv_cct_type
*                                AND zcct_config = 'MB'
*                                AND zcct_line = gt_mb_config-zcct_line.
*        gv_conname = 'MB_ITEM-' && gt_line_config-field.
*        ASSIGN (gv_conname) TO <field_value>.
*        IF <field_value> IS  ASSIGNED.
*          IF gt_line_config-field_type = 'A'.
*            <field_value> = gt_line_config-fix_value.
*          ENDIF.
*          IF gt_line_config-field_type = 'B'.
*            READ TABLE gt_line_data WITH KEY line_field = gt_line_config-line_field.
*            IF sy-subrc EQ 0.
*              <field_value> = gt_line_data-line_value.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.

*      PERFORM get_next_amount USING"计算金额，金额=基础值*折扣(默认1).基础值四种类型，传入的成本额，销售额，或者上一步滚动计算的成本额/销售额
*             gt_mb_config-zcct_amount_type gt_mb_config-zcct_rebate gt_mb_config-werks.
*
*      CASE gt_mb_config-zcct_amount_type.
*        WHEN 'A' OR 'C'.
*          mb_item-amount_lc = gv_last_amount_netpr.
*        WHEN 'B' OR 'D'.
*          mb_item-amount_lc = gv_last_amount_cmpre.
*      ENDCASE.

*      mb_line_id = mb_line_id + 1.
*      mb_item-line_id = mb_line_id.
      IF mb_item-spec_stock = 'K'.
        PERFORM frm_set_dx_amount USING ls_mwskz  CHANGING mb_item .
      ENDIF.

      CLEAR gt_pr_config.
      READ TABLE gt_pr_config WITH KEY zkschl_type = 'MB' zcct_line = gt_mb_config-zcct_line.
      IF sy-subrc NE 0.
        READ TABLE gt_pr_config WITH KEY zkschl_type = 'MB' zcct_line = ''.
        IF sy-subrc EQ 0.
        ENDIF.
      ENDIF.

      IF gt_pr_config IS NOT INITIAL.


        PERFORM get_amount USING gt_pr_config-zprice_type
            gt_data
            CHANGING lv_netwr lv_tax lv_waers.


        IF gt_pr_config-zkschl = 'MB01'.

          IF lv_tax IS INITIAL.
            IF gv_cct_bukrs IS INITIAL.
              PERFORM calculate_tax USING mb_item-plant ls_mwskz 'CNY' lv_netwr
                                    CHANGING tax_amount.
            ELSE.
              PERFORM calculate_tax USING gv_cct_bukrs ls_mwskz 'CNY' lv_netwr
                                    CHANGING tax_amount.
            ENDIF.

            mb_item-amount_lc = lv_netwr - tax_amount.

          ELSE.

            mb_item-amount_lc = lv_netwr - lv_tax.

          ENDIF.
        ELSEIF gt_pr_config-zkschl = 'MB02'.
          mb_item-amount_lc = lv_netwr.
        ENDIF.
      ENDIF.

      PERFORM fix_pd_prctr CHANGING mb_item.


      APPEND mb_item.


      LOOP AT gt_pr_config WHERE zkschl_type = 'PO'.
        CLEAR gt_price.
*        gt_price-vkorg = l_t_komfkgn-vkorg.
        gt_price-zkschl_type = gt_pr_config-zkschl_type.
        gt_price-werks = mb_item-plant.
        gt_price-lifnr = mb_item-vendor.
*        gt_price-kposn = gt_data-line_id.
        gt_price-matnr = gt_data-matnr.
        gt_price-kschl = gt_pr_config-zkschl.
        gt_price-menge = gt_data-menge.

        PERFORM get_amount USING gt_pr_config-zprice_type
            gt_data
            CHANGING gt_price-kawrt
                     lv_tax
                     lv_waers.

        APPEND gt_price.

      ENDLOOP.
      IF sy-subrc EQ 0.
        gv_bsart = 'A01'.
        gv_ekgrp = '101'.
      ENDIF.
      CLEAR mb_item.
      CLEAR gt_price.
    ENDLOOP.
  ENDLOOP.


  PERFORM fix_mb_item_7006 TABLES mb_item.


  CHECK gv_error IS INITIAL.


  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = mb_head
      goodsmvt_code    = mb_code
    IMPORTING
      materialdocument = mb_doc
      matdocumentyear  = mb_year
    TABLES
      goodsmvt_item    = mb_item
      return           = ot_return.
  LOOP AT ot_return WHERE type = 'E' OR type = 'A' OR type = 'X' .
  ENDLOOP.
  IF sy-subrc EQ 0.
    gv_error = 'X'.
  ELSE.

    IF mb_doc IS NOT INITIAL AND mb_year IS NOT INITIAL.
      PERFORM add_message USING 'S' 'ZCCT' '005' mb_doc  '' '' '' ."插入单一消息
    ENDIF.

    gt_borident-objkey = mb_doc && mb_year."写入关系浏览器对象
    gt_borident-objtype = 'BUS2017'.
    APPEND gt_borident.
    CLEAR gt_borident.
  ENDIF.



ENDFORM.

FORM frm_set_dx_amount USING ls_mwskz
                       CHANGING mb_item STRUCTURE bapi2017_gm_item_create.
  DATA: lv_netwr TYPE netwr.
  DATA: tax_amount TYPE netwr.

*
*  READ TABLE gt_t0010 WITH KEY matnr = gt_data-satnr.
*  IF sy-subrc NE 0.
*    SELECT SINGLE * FROM zmmt0010 WHERE matnr = @gt_data-satnr
*      INTO @gt_t0010.
*    IF sy-subrc EQ 0.
*      APPEND gt_t0010.
*    ELSE.
*      PERFORM add_message USING 'E' 'ZCCT' '000' '款号' gt_data-satnr '主数据不存在' ''.
*      RETURN.
*    ENDIF.
*  ENDIF.



*  IF gt_t0010-zdxms IS NOT INITIAL AND gv_cct_type <> '2007' .
*    IF gv_cct_type = '2003' OR gv_cct_type = '2002'
*      OR gv_cct_type = '5003' OR gv_cct_type = '5004'.
*      CLEAR gt_data-netwr.
*    ENDIF.
*
*
*    mb_item-spec_stock = 'K'.
*    mb_item-vendor = gt_t0010-zdxvendor.
*    IF gt_data-netwr IS NOT INITIAL AND gt_t0010-zdxms = 'B'  .
*
*      lv_netwr = gt_data-netwr * gt_t0010-zdxzk / 100.
*
*      PERFORM calculate_tax USING '1000' ls_mwskz 'CNY' lv_netwr
*                CHANGING tax_amount.
*      mb_item-amount_lc = lv_netwr - tax_amount.
*    ELSE.
*      lv_netwr = gt_t0010-zdpj * gt_t0010-zdxzk / 100 * mb_item-entry_qnt.
*
*      PERFORM calculate_tax USING '1000' ls_mwskz 'CNY' lv_netwr
*          CHANGING tax_amount.
*      mb_item-amount_lc = lv_netwr - tax_amount.
*    ENDIF.
*
*
*    mb_item-amount_sv = mb_item-amount_lc.
*  ENDIF.
ENDFORM.

FORM get_amount USING price_type
                 i_data  STRUCTURE zcct_data
                 CHANGING amount tax waers.
  CASE price_type.
    WHEN 'A'.
      amount = i_data-netwr.
      waers = i_data-waers.
    WHEN '1'.
      amount = i_data-amount1.
      waers = i_data-waers.
    WHEN '2'.
      amount = i_data-amount2.
      waers = i_data-waers.
    WHEN '3'.
      amount = i_data-amount3.
      waers = i_data-waers.
    WHEN '4'.
      amount = i_data-amount4.
      waers = i_data-waers.
    WHEN '5'.
      amount = i_data-amount5.
      waers = i_data-waers.
    WHEN '6'.
      amount = i_data-amount6.
      waers = i_data-waers.
    WHEN '7'.
      amount = i_data-amount7.
      waers = i_data-waers.
    WHEN '8'.
      amount = i_data-amount8.
      waers = i_data-waers.
    WHEN '9'.
      amount = i_data-amount9.
      waers = i_data-waers.
    WHEN 'L'.
      READ TABLE gt_cost WITH KEY vkorg   = gt_data-bukrs
                                  line_id = i_data-line_id
                                  matnr   = i_data-matnr.
      IF sy-subrc EQ 0.
        amount = gt_cost-kzwi1.
        tax = gt_cost-mwsbp.
        waers = gt_cost-waers.

        gt_ac_config-waers = waers.
        IF gt_ac_config-mwskz IS NOT INITIAL AND gt_cost-mwskz IS NOT INITIAL.
          IF gt_cost-mwskz+0(1) = 'X'.
            gt_ac_config-mwskz = 'J' && gt_cost-mwskz+1(1).
          ENDIF.
        ENDIF.

      ENDIF.
  ENDCASE.


ENDFORM.

FORM calculate_tax USING i_bukrs i_mwskz i_werks i_netwr CHANGING tax .
  DATA:lt_ftaxp TYPE  TABLE OF ftaxp WITH HEADER LINE.
  DATA: i_wrbtr TYPE bseg-wrbtr.

  CHECK i_mwskz IS NOT INITIAL.

*  CALL FUNCTION 'GET_TAX_PERCENTAGE'
*    EXPORTING
*      aland   = 'CN'
*      datab   = sy-datum
*      mwskz   = i_mwskz
*      txjcd   = ' '
**     EXPORT  = ' '
*    TABLES
*      t_ftaxp = lt_ftaxp.
*  READ TABLE lt_ftaxp INDEX 1.
*  CHECK sy-subrc EQ 0.
*  i_wrbtr = lt_ftaxp-kbetr / 10.
*
*  tax = i_netwr * i_wrbtr / ( 100 +  i_wrbtr ) .

  DATA: ls_bapidoccur TYPE bseg-wrbtr.
  DATA: lt_mwdat TYPE TABLE OF rtax1u15 WITH HEADER LINE.
  ls_bapidoccur = i_netwr.
  CLEAR lt_mwdat[].

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT' "净价拆分含税价和税，并获取税科目
    EXPORTING
      i_bukrs = i_bukrs
      i_mwskz = i_mwskz
      i_waers = i_werks
      i_wrbtr = ls_bapidoccur
    TABLES
      t_mwdat = lt_mwdat.

  CLEAR tax.
  LOOP AT lt_mwdat .
    tax = tax + lt_mwdat-wmwst ."/ 100 .
  ENDLOOP.


ENDFORM.


FORM eina_create.

  DATA:lt_data TYPE TABLE OF zcct_data WITH HEADER LINE.
  DATA:ls_esokz TYPE esokz.
  DATA:ls_ekorg TYPE ekorg.
  DATA:ls_werks TYPE werks_d.
  DATA:ls_mwskz TYPE mwskz.
  DATA:ls_bukrs TYPE bukrs.
  DATA:tax_amount TYPE netpr.
  DATA:et_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

*  DATA:lv_zk TYPE ze_dxzk.
  DATA:lv_netpr TYPE netpr.

  READ TABLE gt_pr_config WITH KEY zkschl_type = 'EI'.
  CHECK sy-subrc EQ 0.
  CLEAR lt_data[].

  DATA:lv_tax TYPE netwr.
  DATA:lv_waers TYPE waers.

  ls_esokz = gt_pr_config-zkschl+3(1).





  LOOP AT gt_data .
    IF sy-tabix = 1.
      ls_werks = gv_cct_werks.
    ENDIF.

    lt_data-satnr = gt_data-satnr.

    lt_data-lifnr = gt_data-lifnr.

    PERFORM get_amount USING gt_pr_config-zprice_type
            gt_data
            CHANGING lt_data-netwr
                     lv_tax
                     lv_waers.
    lt_data-netwr = lt_data-netwr / gt_data-menge.

    APPEND lt_data.

    CLEAR lt_data.
  ENDLOOP.

  SORT lt_data.

  DELETE ADJACENT DUPLICATES FROM lt_data.

  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING satnr.

  IF sy-subrc EQ 0 .
    PERFORM add_message USING 'E' 'ZCCT' '000' '款号' lt_data-satnr '价格不唯一' ''.
    RETURN.
  ENDIF.

  SELECT SINGLE ekorg INTO ls_ekorg FROM t001w
    WHERE werks = ls_werks.

  SELECT SINGLE bukrs INTO ls_bukrs FROM t001k
    WHERE bwkey = ls_werks.

  IF ls_werks IS INITIAL OR ls_ekorg IS INITIAL.
    PERFORM add_message USING 'E' 'ZCCT' '000' '工厂和采购组织确认失败,无法创建信息记录' '' '' ''.
    RETURN.
  ENDIF.

  SELECT matnr , netpr INTO TABLE @DATA(lt_eine)
    FROM eine JOIN eina ON eine~infnr = eina~infnr
    FOR ALL ENTRIES IN @lt_data
    WHERE matnr = @lt_data-satnr
    AND ekorg = @ls_ekorg
    AND esokz = @ls_esokz
    AND werks = @ls_werks.

*  SELECT matnr AS satnr ,
*    zdxms ,zdxvendor ,zdxwerks,zdxzk,zdpj,netpr
*    INTO TABLE @DATA(lt_t0010)
*    FROM zmmt0010 FOR ALL ENTRIES IN @lt_data
*    WHERE matnr = @lt_data-satnr.

*  LOOP AT lt_data.
*
*    READ TABLE lt_t0010 INTO DATA(ls_t0010) WITH KEY satnr = lt_data-satnr.
*    IF sy-subrc EQ 0.
*      IF ls_t0010-zdxvendor IS NOT INITIAL
*        AND ls_t0010-zdxvendor <> lt_data-lifnr.
*        PERFORM add_message USING 'E' 'ZCCT' '000' '代销供应商已存在且不为' lt_data-lifnr  '。请检查' ''.
*        RETURN.
*      ENDIF.
*
*      IF ls_t0010-zdxwerks IS NOT INITIAL AND ls_t0010-zdxwerks <> ls_werks.
*        PERFORM add_message USING 'E' 'ZCCT' '000' '代销工厂已存在且不为' ls_werks  '。请检查' ''.
*        RETURN.
*      ENDIF.
*
*      IF lt_data-netwr IS INITIAL.
*        PERFORM add_message USING 'E' 'ZCCT' '000' lt_data-satnr '代销价格不能为空'  '' ''.
*        RETURN.
*      ENDIF.
*
*      IF ls_t0010-zdxms IS INITIAL.
*        PERFORM add_message USING 'E' 'ZCCT' '000' lt_data-satnr '不允许代销'  '' ''.
*        RETURN.
*      ENDIF.
*
*      IF ls_t0010-zdpj IS INITIAL.
*        PERFORM add_message USING 'E' 'ZCCT' '000' lt_data-satnr '吊牌价不应为空'  '' ''.
*        RETURN.
*      ENDIF.
*
*      ls_mwskz = 'J1'.
*
*      PERFORM calculate_tax USING ls_bukrs ls_mwskz 'CNY' lt_data-netwr
*                      CHANGING tax_amount.
*      lv_netpr = lt_data-netwr - tax_amount.
*      lv_zk = lt_data-netwr * 100 / ls_t0010-zdpj.
*
*      IF ls_t0010-netpr <> lv_netpr OR ls_t0010-zdxzk <> lv_zk OR ls_t0010-zdxvendor <> lt_data-lifnr.
*        UPDATE zmmt0010 SET netpr = lv_netpr zdxzk = lv_zk zdxvendor = lt_data-lifnr zdxwerks = ls_werks
*            WHERE matnr = lt_data-satnr.
*      ENDIF.
*
*    ELSE.
*      PERFORM add_message USING 'E' 'ZCCT' '000' '物料号获取失败' ''  '' ''.
*      RETURN.
*    ENDIF.
*
*
*
*    READ TABLE lt_eine INTO DATA(ls_eine) WITH KEY matnr =  lt_data-satnr.
*    IF sy-subrc NE 0.
*      CALL FUNCTION 'ZCCT_EINA_MAINTAIN'
*        EXPORTING
*          im_model  = '1'
*          im_matnr  = lt_data-satnr
*          im_lifnr  = lt_data-lifnr
*          im_ekorg  = ls_ekorg
*          im_werks  = ls_werks
*          im_esokz  = ls_esokz
*          im_netpr  = lt_data-netwr
*        TABLES
*          et_return = et_return.
*
*    ELSE.
*      IF ls_eine-netpr <> lt_data-netwr.
*        CALL FUNCTION 'ZCCT_EINA_MAINTAIN'
*          EXPORTING
*            im_model  = '2'
*            im_matnr  = lt_data-satnr
*            im_lifnr  = lt_data-lifnr
*            im_ekorg  = ls_ekorg
*            im_werks  = ls_werks
*            im_esokz  = ls_esokz
*            im_netpr  = lt_data-netwr
*          TABLES
*            et_return = et_return.
*      ENDIF.
*    ENDIF.
*    LOOP AT et_return.
*      IF et_return-type = 'E'.
*        gv_error = 'X'.
*      ENDIF.
*      APPEND et_return TO ot_return.
*    ENDLOOP.
*    CLEAR et_return[].
*  ENDLOOP.



ENDFORM.


FORM bill_create.
  DATA: BEGIN OF ikomfk OCCURS 100.
          INCLUDE STRUCTURE komfk.
  DATA: END OF ikomfk.
  DATA: BEGIN OF l_t_komfkgn OCCURS 0.
          INCLUDE STRUCTURE komfkgn.
  DATA: END OF l_t_komfkgn.
  DATA: BEGIN OF l_t_komfkko OCCURS 0.
          INCLUDE STRUCTURE komv.
  DATA: END OF l_t_komfkko.
  DATA: lt_komfktx TYPE wpupl_komfktx_list.
  DATA: BEGIN OF ikomv OCCURS 500.
          INCLUDE STRUCTURE komv.
  DATA: END OF ikomv.
  DATA: BEGIN OF ithead OCCURS 100.
          INCLUDE STRUCTURE theadvb.
  DATA: END OF ithead.
  DATA: l_vbfs                  LIKE vbfs OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF ivbpavb OCCURS 100.
          INCLUDE STRUCTURE vbpavb.
  DATA: END OF ivbpavb.

  DATA: BEGIN OF ivbrkvb OCCURS 100.
          INCLUDE STRUCTURE vbrkvb.
  DATA: END OF ivbrkvb.
  DATA: BEGIN OF ivbrpvb OCCURS 100.
          INCLUDE STRUCTURE vbrpvb.
  DATA: END OF ivbrpvb.
  DATA: BEGIN OF ivbss OCCURS 100.
          INCLUDE STRUCTURE vbss.
  DATA: END OF ivbss.
  DATA: l_t_komfkzm LIKE komfkzm OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF ivbfs OCCURS 100.
          INCLUDE STRUCTURE vbfs.
  DATA: END OF ivbfs.

  DATA:lv_waers_x TYPE waers.
  DATA:lv_waers_y TYPE waers.
  DATA:lv_tax TYPE netwr.
  DATA:lv_waers_jy TYPE waers.

  TABLES:vbsk.

  LOOP AT gt_bl_config."拼装发票信息
    IF gt_bl_config-vkorg IS INITIAL.
      l_t_komfkgn-vkorg = gv_cct_vkorg.
    ELSE.
      l_t_komfkgn-vkorg = gt_bl_config-vkorg.
    ENDIF.

    l_t_komfkgn-auart = gt_bl_config-auart.

    l_t_komfkgn-prsdt = gv_zccth-remark02.


*    gv_price_date = l_t_komfkgn-prsdt.

*    IF gv_price_date IS INITIAL.
*      gv_price_date = gv_post_date.
*    ENDIF.



    SELECT SINGLE bukrs
     INTO l_t_komfkgn-bukrs
     FROM tvko
     WHERE vkorg = l_t_komfkgn-vkorg.

    IF gt_bl_config-vtweg IS NOT INITIAL.
      l_t_komfkgn-vtweg = gt_bl_config-vtweg.
    ELSE.
      l_t_komfkgn-vtweg = gv_cct_vtweg.
    ENDIF.

    IF gt_bl_config-spart IS NOT INITIAL.
      l_t_komfkgn-spart = gt_bl_config-spart.
    ELSE.
      l_t_komfkgn-spart = gv_cct_spart.
    ENDIF.

    l_t_komfkgn-kalsm = gt_bl_config-kalsm.



    l_t_komfkgn-fkara = gt_bl_config-fkart.

    l_t_komfkgn-zuonr = gv_vgbel.

    IF gt_bl_config-werks IS INITIAL.
      l_t_komfkgn-werks = gv_cct_werks.
    ELSE.
      l_t_komfkgn-werks = gt_bl_config-werks.
    ENDIF.

    l_t_komfkgn-kursk = '1'.
    l_t_komfkgn-fkdat = gv_post_date.

    IF gt_bl_config-kunnr IS INITIAL.
      l_t_komfkgn-kunag = gv_cct_bl_kunnr.
    ELSE.
      l_t_komfkgn-kunag = gt_bl_config-kunnr.
    ENDIF.

*    l_t_komfkgn-land1 = 'CN'.
    l_t_komfkgn-pstyv = gt_bl_config-pstyv.

    IF gt_bl_config-prctr IS INITIAL.
      l_t_komfkgn-prctr = gv_cct_prctr.
    ELSE.
      l_t_komfkgn-prctr = gt_bl_config-prctr.
    ENDIF.

**vbrp-sgtxt = 客户名称>>
    SELECT SINGLE name1  waers FROM zscmt0010 INTO (l_t_komfkgn-sgtxt,lv_waers_y)
                     WHERE partner = l_t_komfkgn-kunag.
**<vbrp-sgtxt = 客户名称

    CLEAR gv_vgpos.
    PERFORM clear_amount.

*add by at-yuxs 20220823 德清-山东加工合同定价过程调整 begin
*    IF l_t_komfkgn-kalsm = 'Z00006' AND
*       l_t_komfkgn-kunag = '0000001000' AND
*       l_t_komfkgn-vkorg = '1010' AND
*       gt_data[] IS NOT INITIAL.
*      READ TABLE gt_data INDEX 1.
*      SELECT SINGLE b~zhtbh INTO @DATA(lv_htbh) FROM ztpp0089 AS a
*           JOIN ztpp_jght_item AS b ON a~zppdhd = b~zppdhd
*           JOIN ztpp_jght_head AS c ON c~zhtbh = b~zhtbh AND zhtlx = '加工合同' AND zhtzt = 'C'
*           WHERE vbeln = @gt_data-vbeln_va AND posnr = @gt_data-posnr_va.
*      IF sy-subrc = 0.
*        l_t_komfkgn-kalsm = 'Z00016'.
*      ENDIF.
*    ENDIF.
*add by at-yuxs 20220823 德清-山东加工合同定价过程调整 end


    LOOP AT gt_data.
*      PERFORM get_next_amount USING gt_bl_config-zcct_amount_type gt_bl_config-zcct_rebate gt_bl_config-werks.
      "计算金额，金额=基础值*折扣(默认1).基础值四种类型，传入的成本额，销售额，或者上一步滚动计算的成本额/销售额
      gv_vgpos = gv_vgpos + 1.
      l_t_komfkgn-mandt = sy-mandt.


*      l_t_komfkgn-vgbel = gt_bl_config-zcct_line.
      PERFORM get_next_bl_vg CHANGING l_t_komfkgn-vgbel.


      l_t_komfkgn-vgpos = gt_data-line_id.
      l_t_komfkgn-vgpos_ko = gt_data-line_id.
      l_t_komfkgn-matnr = gt_data-matnr.
*      SELECT SINGLE spart INTO l_t_komfkgn-spart FROM mara
*          WHERE matnr = l_t_komfkgn-matnr.

      l_t_komfkgn-aubel = gt_data-vbeln_va.
      l_t_komfkgn-aupos = gt_data-posnr_va.
      l_t_komfkgn-kwmeng = gt_data-menge.
      l_t_komfkgn-vrkme = ''."单位
      APPEND l_t_komfkgn.



      LOOP AT gt_pr_config WHERE zkschl_type = 'BL' AND zcct_line = ' '.
        CLEAR gt_price.
        gt_price-zkschl_type = gt_pr_config-zkschl_type.
        gt_price-vkorg = l_t_komfkgn-vkorg.
        gt_price-werks = l_t_komfkgn-werks.
        gt_price-kunag = l_t_komfkgn-kunag.
        gt_price-kposn = gt_data-line_id.
        gt_price-matnr = gt_data-matnr.
        gt_price-menge = gt_data-menge.
        gt_price-kschl = gt_pr_config-zkschl.


        PERFORM get_amount USING gt_pr_config-zprice_type
            gt_data
            CHANGING gt_price-kawrt
                     lv_tax
                     lv_waers_x
                     .
*        IF lv_waers_x <> lv_waers_y.
*          PERFORM exchangerate USING lv_waers_x lv_waers_y gv_post_date CHANGING gt_price-kawrt.
*          PERFORM exchangerate USING lv_waers_x lv_waers_y gv_post_date CHANGING lv_tax.
*        ENDIF.
        APPEND gt_price.
      ENDLOOP.

      LOOP AT gt_pr_config WHERE zkschl_type = 'BL' AND zcct_line = gt_bl_config-zcct_line.
        CLEAR gt_price.
        gt_price-zkschl_type = gt_pr_config-zkschl_type.
        gt_price-vkorg = l_t_komfkgn-vkorg.
        gt_price-werks = l_t_komfkgn-werks.
        gt_price-kunag = l_t_komfkgn-kunag.
        gt_price-kposn = gt_data-line_id.
        gt_price-matnr = gt_data-matnr.
        gt_price-menge = gt_data-menge.
        gt_price-kschl = gt_pr_config-zkschl.


        PERFORM get_amount USING gt_pr_config-zprice_type
            gt_data
            CHANGING gt_price-kawrt
                     lv_tax
                     lv_waers_x
                     .
*        IF lv_waers_x <> lv_waers_y.
*          PERFORM exchangerate USING lv_waers_x lv_waers_y gv_post_date CHANGING gt_price-kawrt.
*          PERFORM exchangerate USING lv_waers_x lv_waers_y gv_post_date CHANGING lv_tax.
*        ENDIF.

        APPEND gt_price.
      ENDLOOP.

*      LOOP AT gt_pr_config WHERE zkschl_type = 'BL'.
*        l_t_komfkko-knumv =  gt_bl_config-zcct_line.
*        l_t_komfkko-kposn = gt_data-line_id.
*        l_t_komfkko-kschl = gt_pr_config-zkschl.
*        l_t_komfkko-kbetr = gt_data-netwr / gt_data-menge.
*        l_t_komfkko-waers = 'CNY'.
*        APPEND l_t_komfkko.
*        CLEAR l_t_komfkko.
*      ENDLOOP.

*      CASE gt_bl_config-zcct_amount_type.
*        WHEN 'A' OR 'C'.
*          l_t_komfkko-knumv =  gt_bl_config-zcct_line.
*          l_t_komfkko-kposn = gt_data-line_id.
*          l_t_komfkko-kschl = gt_bl_config-kschl.
*          l_t_komfkko-kbetr = gv_last_amount_netpr / gt_data-menge.
*          l_t_komfkko-waers = 'CNY'.
*          APPEND l_t_komfkko.
*          CLEAR l_t_komfkko.
*        WHEN 'B' OR 'D'.
*          l_t_komfkko-knumv =  gt_bl_config-zcct_line.
*          l_t_komfkko-kposn = gt_data-line_id.
*          l_t_komfkko-kschl = gt_bl_config-kschl.
*          l_t_komfkko-kbetr = gv_last_amount_cmpre / gt_data-menge..
*          l_t_komfkko-waers = 'CNY'.
*
*          IF l_t_komfkko-kschl IS NOT INITIAL.
*            APPEND l_t_komfkko.
*          ENDIF.
*          CLEAR l_t_komfkko.
*      ENDCASE.
    ENDLOOP.

    CLEAR l_t_komfkgn.
  ENDLOOP.
  CHECK gv_error IS INITIAL.


*  LOOP AT l_t_komfkgn.
*    MOVE-CORRESPONDING l_t_komfkgn TO gt_zcct_bl_list.
*    APPEND gt_zcct_bl_list.
*    CLEAR gt_zcct_bl_list.
*  ENDLOOP.
*  DELETE ADJACENT DUPLICATES FROM gt_zcct_bl_list.

  SORT l_t_komfkgn BY vgbel vgpos .
  CALL FUNCTION 'GN_INVOICE_CREATE' "创建虚拟销售订单，获取发票所需信息
    EXPORTING
      delivery_date = gv_post_date
      invoice_date  = gv_post_date
      pricing_date  = gv_post_date
      vbsk_i        = vbsk
      with_posting  = '-'
      caller_type   = 'P'
*BI 360227
      id_no_dialog  = 'X'
*EI 360227
    IMPORTING
      vbsk_e        = vbsk
    TABLES
      xkomfk        = ikomfk
      xkomfkgn      = l_t_komfkgn  " ikomfkgn
      xkomfkko      = l_t_komfkko  " ikomfkko
      xkomfktx      = lt_komfktx
      xkomv         = ikomv
      xthead        = ithead
      xvbfs         = l_vbfs
      xvbpa         = ivbpavb
      xvbrk         = ivbrkvb
      xvbrp         = ivbrpvb
      xvbss         = ivbss
*BD 158179
*     xkomfkzm      = g_t_komfkzm
*ED 158179
*BI 158179
      xkomfkzm      = l_t_komfkzm
*EI 158179
    EXCEPTIONS
      error_message = 10005
      OTHERS        = 310.

  IF sy-subrc EQ 10005.
    PERFORM add_message USING sy-msgty sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    gv_error = 'X'.
    RETURN.
  ELSEIF sy-subrc EQ 310.
    PERFORM add_message USING 'E' 'WP' '310' '' '' '' ''.
    gv_error = 'X'.
    RETURN.
  ENDIF.



  LOOP AT l_vbfs.
    IF  l_vbfs-msgid = 'VF'AND l_vbfs-msgno = '071'.
      l_vbfs-msgty = 'E'.
    ENDIF.

    PERFORM add_message_row USING l_vbfs-msgty l_vbfs-msgid l_vbfs-msgno l_vbfs-msgv1 l_vbfs-msgv2 l_vbfs-msgv3 l_vbfs-msgv4 l_vbfs-posnr.
    IF l_vbfs-msgty = 'E' .
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.

  REFRESH ivbfs.
  REFRESH l_vbfs.

  "获取付款方客户名称
  READ TABLE ivbrkvb INDEX 1.
  if sy-subrc = 0.
    loop at l_t_komfkgn ASSIGNING FIELD-SYMBOL(<ls_komfkgn>).
      select SINGLE name1 from zscmt0010 into <ls_komfkgn>-sgtxt where partner = ivbrkvb-kunrg.
    ENDLOOP.
    loop at ivbrpvb ASSIGNING FIELD-SYMBOL(<ls_vbrpvb>).
      select SINGLE name1 from zscmt0010 into <ls_vbrpvb>-sgtxt where partner = ivbrkvb-kunrg.
    endloop.
  endif.

  PERFORM fix_billing TABLES ivbrkvb ivbrpvb ivbpavb.

*  DATA: lv_werks TYPE werks_d.

  CALL FUNCTION 'RV_INVOICE_DOCUMENT_ADD' "创建发票
    EXPORTING
      vbsk_i          = vbsk
      with_posting    = '-'
      without_refresh = 'X'
    IMPORTING
      vbsk_e          = vbsk
    TABLES
      xkomfk          = ikomfk
      xkomv           = ikomv
      xthead          = ithead
      xvbfs           = l_vbfs
      xvbpa           = ivbpavb
      xvbrk           = ivbrkvb
      xvbrp           = ivbrpvb
      xvbss           = ivbss
    EXCEPTIONS
      error_message   = 10005
      OTHERS          = 310.
  IF sy-subrc EQ 10005.
    PERFORM add_message USING sy-msgty sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    gv_error = 'X'.
    RETURN.
  ELSEIF sy-subrc EQ 310.
    PERFORM add_message USING 'E' 'WP' '310' '' '' '' ''.
    gv_error = 'X'.
    RETURN.
  ENDIF.
  LOOP AT l_vbfs.
    IF  l_vbfs-msgid = 'VF'AND l_vbfs-msgno = '071'.
      l_vbfs-msgty = 'E'.
    ENDIF.

    PERFORM add_message_row USING l_vbfs-msgty l_vbfs-msgid l_vbfs-msgno l_vbfs-msgv1 l_vbfs-msgv2 l_vbfs-msgv3 l_vbfs-msgv4 l_vbfs-posnr.
    IF l_vbfs-msgty = 'E'.
      gv_error = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.


  IF gv_error IS INITIAL.


    LOOP AT l_vbfs."处理发票关系浏览器信息
      gt_borident-objkey = l_vbfs-vbeln.
      gt_borident-objtype = 'VBRK'.
      APPEND gt_borident.
      CLEAR gt_borident.
    ENDLOOP.


  ENDIF.


ENDFORM.



FORM ac_doc_create.
  DATA: ls_header     TYPE  bapiache09,
        lt_item       TYPE STANDARD TABLE OF  bapiacgl09,  "总账科目行项目
        ls_item       TYPE bapiacgl09,  "总账科目行项目
        lt_curritem   TYPE STANDARD TABLE OF  bapiaccr09,   "货币行项目
        ls_curritem   TYPE bapiaccr09,   "货币行项目
        lt_accountpay TYPE STANDARD TABLE OF bapiacap09,
        ls_accountpay TYPE bapiacap09,
        lt_accountrec TYPE STANDARD TABLE OF bapiacar09,
        ls_accountrec TYPE bapiacar09,

        is_header     TYPE  bapiache09,
        it_item       TYPE STANDARD TABLE OF  bapiacgl09,  "总账科目行项目
        is_item       TYPE bapiacgl09,  "总账科目行项目
        it_curritem   TYPE STANDARD TABLE OF  bapiaccr09,   "货币行项目
        is_curritem   TYPE bapiaccr09,   "货币行项目
        it_accountpay TYPE STANDARD TABLE OF bapiacap09,
        is_accountpay TYPE bapiacap09,
        it_accountrec TYPE STANDARD TABLE OF bapiacar09,
        is_accountrec TYPE bapiacar09,

        lt_extension2 TYPE STANDARD TABLE OF bapiparex ,  "项目货币数据
        ls_extension2 TYPE  bapiparex,
        lt_accounttax TYPE STANDARD TABLE OF bapiactx09,
        ls_accounttax TYPE bapiactx09,
        lt_mwdat      TYPE STANDARD TABLE OF rtax1u15,
        ls_mwdat      TYPE rtax1u15,
        obj_type      LIKE  bapiache09-obj_type,
        obj_key       LIKE  bapiache09-obj_key,
        obj_sys       LIKE  bapiache09-obj_sys,
        lv_lifnr      TYPE elifn,
        lv_gsber      TYPE gsber,
        lv_prctr      TYPE prctr,
        lv_currency   TYPE waers,
        lv_flag       TYPE char01,
        lv_werks      TYPE werks_d,
        icon_name(20) TYPE c,
        lv_item       TYPE posnr_acc,

        ls_bapidoccur TYPE bapidoccur,
        ls_kbetr      TYPE netwr,
        sum_kbetr     TYPE netwr,

        ls_headerdata TYPE bapi_incinv_create_header,
        l_docnumber   LIKE bapi_incinv_fld-inv_doc_no,
        l_year        LIKE bapi_incinv_fld-fisc_year,
        lt_itemdata   TYPE STANDARD TABLE OF bapi_incinv_create_item,
        ls_itemdata   TYPE bapi_incinv_create_item,
        lt_taxdata    TYPE STANDARD TABLE OF bapi_incinv_create_tax,
        ls_taxdata    TYPE bapi_incinv_create_tax.

  DATA: lt_return   TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE.   "返回消息

  DATA: lt_value  TYPE TABLE OF sval,
        ls_value  TYPE sval,
        lv_rtn_cd.
  DATA: lv_awkey TYPE awkey.

  DATA:BEGIN OF ls_collect,"根据利润中心汇总金额
         prctr TYPE prctr,
         wrbtr TYPE bapidoccur,
       END OF ls_collect.
  DATA:lt_collect LIKE STANDARD TABLE OF ls_collect.

  CLEAR:lt_value,ls_value,lv_rtn_cd.

  DATA:lt_ac_config TYPE TABLE OF zcct_ac_config WITH HEADER LINE.

  lt_ac_config[] = gt_ac_config[].

  DELETE ADJACENT DUPLICATES FROM lt_ac_config COMPARING blart bukrs.

  TYPES:BEGIN OF ty_lines ,
          zcct_line TYPE zcct_line,
          posnr_acc TYPE  posnr_acc,
        END OF ty_lines.

  DATA: lt_lines TYPE TABLE OF ty_lines WITH HEADER LINE.
  DATA: flag_coll TYPE char1.
  DATA: lv_tax TYPE netwr.
  DATA: lv_waers TYPE waers.

  DATA: lv_clean_pay_taxcode TYPE flag.

  FIELD-SYMBOLS : <fs_line>  TYPE ty_lines.


  LOOP AT lt_ac_config."按公司、凭证类型拆分会计凭证
    CLEAR: ls_header,lv_item.
    CLEAR: lt_item[].
    CLEAR: lt_accountrec[].
    CLEAR: lt_accountpay[].
    CLEAR: lt_curritem[].
    CLEAR: lt_accounttax[].
    CLEAR: lt_extension2[].

    IF lt_ac_config-bukrs IS NOT INITIAL.
      ls_header-comp_code = lt_ac_config-bukrs.   "公司代码
    ELSE.
      ls_header-comp_code = gv_cct_bukrs.
    ENDIF.

    ls_header-username = sy-uname. "创建用户
    ls_header-ref_doc_no = gv_vgbel. "参考凭证

    ls_header-doc_date = gv_post_date. "凭证日期
    ls_header-pstng_date = gv_post_date. "记帐日期
    ls_header-fisc_year = gv_post_date+(4). "会计年度
    ls_header-fis_period = gv_post_date+4(2). "会计期间
    ls_header-header_txt = gv_bktxt. "备注
    ls_header-doc_type = lt_ac_config-blart. "凭证类型

    CLEAR lt_lines[].

    PERFORM fix_set_aufnr_cost TABLES gt_data .


    LOOP AT gt_data.
*      PERFORM clear_amount."清空金额

      LOOP AT gt_ac_config WHERE blart = lt_ac_config-blart AND bukrs = lt_ac_config-bukrs."拼装会计凭证信息

*        PERFORM get_next_amount USING lt_ac_config-zcct_amount_type lt_ac_config-zcct_rebate gt_ac_config-werks.
        "计算金额，金额=基础值*折扣(默认1).基础值四种类型，传入的成本额，销售额，或者上一步滚动计算的成本额/销售额
        IF lv_item IS INITIAL.
          lv_item = 1.
        ENDIF.

        IF gt_ac_config-actgroup IS INITIAL.
          lv_item = lv_item + 1.
        ELSE.
          CLEAR lt_lines.
          READ TABLE lt_lines WITH KEY zcct_line = gt_ac_config-zcct_line.
          IF sy-subrc NE 0.
            lt_lines-zcct_line = gt_ac_config-zcct_line.
            lv_item = lv_item + 1.
            lt_lines-posnr_acc = lv_item.
            APPEND lt_lines.
          ENDIF.
        ENDIF.

*        ls_item-tax_code   = gt_ac_config-BSCHL.帐码
*        ls_item-tax_code   = gt_ac_config-UMSKZ.特别总账

        "4008凭证税码根据物料来确定 add by 2022.01.21
        lv_clean_pay_taxcode = ''.
        IF gt_ac_config-mwskz IS NOT INITIAL AND gt_data-matnr IS NOT INITIAL
          AND ( gv_cct_type = '4008' OR gv_cct_type = '6004').
          SELECT SINGLE taxm1 INTO @DATA(lv_taxm1)
            FROM mlan
            WHERE matnr = @gt_data-matnr
              AND aland = 'CN'.

          lv_clean_pay_taxcode  = 'X'.
          CASE lv_taxm1.
            WHEN '1'.
              gt_ac_config-mwskz = 'J1'.
            WHEN '3'.
              gt_ac_config-mwskz = 'J2'.
            WHEN OTHERS.
          ENDCASE.

          CLEAR lv_taxm1.
        ENDIF.

        IF gt_ac_config-zprice_type IS INITIAL.

          READ TABLE gt_pr_config WITH KEY zkschl_type = 'AC'.
          IF sy-subrc EQ 0.
            gt_data-bukrs = ls_header-comp_code.
            PERFORM get_amount USING gt_pr_config-zprice_type
                gt_data
                CHANGING ls_bapidoccur
                         lv_tax
                         lv_waers.
          ENDIF.
        ELSE.
          gt_data-bukrs = ls_header-comp_code.
          PERFORM get_amount USING gt_ac_config-zprice_type
              gt_data
              CHANGING ls_bapidoccur
                       lv_tax
                       lv_waers.
        ENDIF.

        CASE gt_ac_config-zmitkz.
          WHEN 'D'. "客户统御
            IF gt_ac_config-saknr IS NOT INITIAL.
              READ TABLE gt_bp_info WITH KEY zcct_role = gt_ac_config-saknr.
              IF sy-subrc EQ 0.
                ls_accountrec-customer = gt_bp_info-partner.
              ELSE.
                ls_accountrec-customer = gt_ac_config-saknr.
              ENDIF.
            ELSEIF  gv_cct_ac_kunnr IS NOT INITIAL.
              ls_accountrec-customer = gv_cct_ac_kunnr.
            ENDIF.
            ls_accountrec-bline_date = gv_post_date.
            ls_accountrec-profit_ctr = gt_ac_config-prctr.
            ls_accountrec-tax_code   = gt_ac_config-mwskz.
            IF lv_clean_pay_taxcode IS NOT INITIAL.
              CLEAR ls_accountrec-tax_code.
            ENDIF.
*            ls_accountpay-costcenter = gt_ac_config-kostl.
            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_accountrec-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_accountrec INTO lt_accountrec.
            ELSE.
              ls_accountrec-itemno_acc = lv_item.
              APPEND ls_accountrec TO lt_accountrec.
            ENDIF.
            CLEAR:ls_accountrec.
          WHEN 'K'. "供应商统御
            IF gt_ac_config-saknr IS NOT INITIAL.
              READ TABLE gt_bp_info WITH KEY zcct_role = gt_ac_config-saknr.
              IF sy-subrc EQ 0.
                ls_accountpay-vendor_no = gt_bp_info-partner.
              ELSE.
                ls_accountpay-vendor_no = gt_ac_config-saknr.
              ENDIF.
            ELSEIF  gv_cct_ac_lifnr IS NOT INITIAL.
              ls_accountpay-vendor_no = gv_cct_ac_lifnr.
            ENDIF.
            ls_accountpay-bline_date = gv_post_date.
            ls_accountpay-profit_ctr = gt_ac_config-prctr.
            ls_accountpay-tax_code   = gt_ac_config-mwskz.
            IF lv_clean_pay_taxcode IS NOT INITIAL.
              CLEAR ls_accountpay-tax_code.
            ENDIF.
            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_accountpay-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_accountpay INTO lt_accountpay.
            ELSE.
              ls_accountpay-itemno_acc = lv_item.
              APPEND ls_accountpay TO lt_accountpay.
            ENDIF.
            CLEAR:ls_accountpay.
          WHEN ' '. "总帐
            ls_item-gl_account = gt_ac_config-saknr.
            ls_item-tax_code   = gt_ac_config-mwskz.
*          ls_item-sales_ord = gt_data-vbeln_va.
*          ls_item-s_ord_item = gt_data-posnr_va.
            ls_item-orderid = gt_data-aufnr.

            IF gv_cct_type = '2004' OR gv_cct_type = '4010'.
              CLEAR ls_item-sales_ord.
              CLEAR ls_item-s_ord_item.
              CLEAR ls_item-orderid.
            ENDIF.

            ls_item-costcenter = gt_ac_config-kostl.
            IF ls_item-costcenter = 'DATA'.
              ls_item-costcenter = gt_data-kostl.
            ENDIF.
*            ls_item-costcenter = gt_ac_config-kostl.

            IF gt_ac_config-prctr IS INITIAL.
              ls_item-profit_ctr = gv_cct_prctr.
            ELSE.
              ls_item-profit_ctr = gt_ac_config-prctr.
            ENDIF.


            IF gv_cct_bl_kunnr IS NOT INITIAL.
              ls_item-customer = gv_cct_bl_kunnr .
            ELSEIF gv_cct_ac_kunnr IS NOT INITIAL.
              ls_item-customer = gv_cct_ac_kunnr.
            ENDIF.

            PERFORM fix_aufnr CHANGING ls_item.
*            ls_item-CUSTOMER = gt_ac_config-kostl.
            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_item-itemno_acc = lt_lines-posnr_acc.
              READ TABLE lt_item INTO DATA(ls1_item) WITH KEY itemno_acc = lt_lines-posnr_acc.
              IF sy-subrc EQ 0 AND ls1_item <> ls_item.
                CLEAR flag_coll.
                LOOP AT lt_lines WHERE zcct_line = gt_ac_config-zcct_line.
                  READ TABLE lt_item INTO ls1_item WITH KEY itemno_acc = lt_lines-posnr_acc.
                  IF sy-subrc EQ 0.
                    ls_item-itemno_acc = lt_lines-posnr_acc.
                    IF ls_item = ls1_item.
                      flag_coll = 'X'.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
                IF flag_coll IS INITIAL.
                  LOOP AT lt_lines .
                  ENDLOOP.
                  ADD 1 TO lt_lines-posnr_acc.
                  APPEND lt_lines.
                ENDIF.
              ELSE.

              ENDIF.

              ls_item-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_item INTO lt_item.
            ELSE.
              ls_item-material_long   = gt_data-matnr.
              SELECT SINGLE meins INTO ls_item-base_uom
                FROM mara WHERE matnr = gt_data-matnr.
              ls_item-quantity   = gt_data-menge.
              ls_item-itemno_acc = lv_item.  "凭证行项目
              APPEND ls_item TO lt_item.
            ENDIF.
            CLEAR ls_item.
        ENDCASE.

*        CLEAR ls_bapidoccur.
*        CASE gt_ac_config-zcct_amount_type.
*          WHEN 'A' OR 'C'.
*            ls_bapidoccur = gv_last_amount_netpr.
*          WHEN 'B' OR 'D'.
*            ls_bapidoccur = gv_last_amount_cmpre.
*        ENDCASE.


        IF gt_ac_config-shkzg = 'H'.
          ls_bapidoccur = - ls_bapidoccur.
          lv_tax = - lv_tax.
        ENDIF.
*        PERFORM fix_tax_code USING gv_cct_type ls_header-comp_code CHANGING   gt_ac_config-mwskz.

        CASE gt_ac_config-ztax_split.
          WHEN ''.

            ls_curritem-amt_doccur = ls_bapidoccur .
            ls_curritem-currency = gt_ac_config-waers.
            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_curritem-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_curritem INTO lt_curritem.
            ELSE.
              ls_curritem-itemno_acc = lv_item.
              APPEND ls_curritem TO lt_curritem.
            ENDIF.

            CLEAR ls_curritem.
          WHEN '+'.
            CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT' "净价拆分含税价和税，并获取税科目
              EXPORTING
                i_bukrs = ls_header-comp_code
                i_mwskz = gt_ac_config-mwskz
                i_waers = 'CNY'
                i_wrbtr = ls_bapidoccur
              TABLES
                t_mwdat = lt_mwdat.

            CLEAR ls_kbetr.
            CLEAR sum_kbetr.
            LOOP AT lt_mwdat INTO ls_mwdat.
              ls_kbetr = ls_mwdat-wmwst / 100.
              sum_kbetr = sum_kbetr + ls_kbetr.
              ls_mwdat-wmwst = ls_kbetr * 100.
              MODIFY lt_mwdat FROM ls_mwdat.
            ENDLOOP.


            ls_curritem-amt_doccur = ls_bapidoccur + sum_kbetr.
            ls_curritem-currency = gt_ac_config-waers.

            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_curritem-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_curritem INTO lt_curritem.
            ELSE.
              ls_curritem-itemno_acc = lv_item.
              APPEND ls_curritem TO lt_curritem.
            ENDIF.

            CLEAR ls_curritem.

            DATA:lvm_item TYPE posnr_acc.
            CLEAR lvm_item.
            LOOP AT lt_mwdat INTO ls_mwdat."插入税金会计凭证行
              lvm_item = lt_lines-posnr_acc + sy-tabix.
              CLEAR ls_accounttax.

              ls_accounttax-tax_code = gt_ac_config-mwskz.
              ls_accounttax-acct_key   = ls_mwdat-ktosl.
              ls_accounttax-cond_key   = ls_mwdat-kschl.
              ls_accounttax-taxjurcode = ls_mwdat-txjcd.
              ls_accounttax-taxjurcode_deep  = ls_mwdat-txjcd_deep.
              ls_accounttax-taxjurcode_level = ls_mwdat-txjlv.

              CLEAR ls_curritem.

              ls_curritem-currency   = gt_ac_config-waers.
              ls_curritem-amt_doccur = - ls_mwdat-wmwst / 100.
              ls_curritem-amt_base = - ls_mwdat-kawrt / 100.

              IF gt_ac_config-actgroup IS NOT INITIAL.
                IF lv_item <= lvm_item.
                  lv_item = lvm_item + 1.
                ENDIF.
                ls_accounttax-itemno_acc = lvm_item.
                ls_curritem-itemno_acc = lvm_item.
                COLLECT ls_accounttax INTO lt_accounttax.
                COLLECT ls_curritem INTO lt_curritem.
              ELSE.
                lv_item = lv_item + 1.
                ls_accounttax-itemno_acc = lv_item.
                ls_curritem-itemno_acc = lv_item.
                APPEND ls_accounttax TO lt_accounttax.
                APPEND ls_curritem TO lt_curritem.
              ENDIF.


              CLEAR ls_accounttax.
              CLEAR ls_curritem.
            ENDLOOP.

            CLEAR ls_curritem.
          WHEN '-' .
            CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT' "净价拆分含税价和税，并获取税科目
              EXPORTING
                i_bukrs = ls_header-comp_code
                i_mwskz = gt_ac_config-mwskz
                i_waers = gt_ac_config-waers
                i_wrbtr = ls_bapidoccur
              TABLES
                t_mwdat = lt_mwdat.

            CLEAR ls_kbetr.
            CLEAR sum_kbetr.
            LOOP AT lt_mwdat INTO ls_mwdat.
              ls_kbetr = ls_mwdat-wmwst / 100.
              sum_kbetr = sum_kbetr + ls_kbetr.
              ls_mwdat-wmwst = ls_kbetr * 100.
              MODIFY lt_mwdat FROM ls_mwdat.
*              ls_kbetr = ls_kbetr + ls_mwdat-wmwst / 100.
            ENDLOOP.


            ls_curritem-amt_doccur = ls_bapidoccur - sum_kbetr.
            ls_curritem-currency = gt_ac_config-waers.

            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_curritem-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_curritem INTO lt_curritem.
            ELSE.
              ls_curritem-itemno_acc = lv_item.
              APPEND ls_curritem TO lt_curritem.
            ENDIF.

            CLEAR ls_curritem.

            CLEAR lvm_item.
            LOOP AT lt_mwdat INTO ls_mwdat."插入税金会计凭证行
              lvm_item = lt_lines-posnr_acc + sy-tabix.
              CLEAR ls_accounttax.

              ls_accounttax-tax_code = gt_ac_config-mwskz.
              ls_accounttax-acct_key   = ls_mwdat-ktosl.
              ls_accounttax-cond_key   = ls_mwdat-kschl.
              ls_accounttax-taxjurcode = ls_mwdat-txjcd.
              ls_accounttax-taxjurcode_deep  = ls_mwdat-txjcd_deep.
              ls_accounttax-taxjurcode_level = ls_mwdat-txjlv.

              CLEAR ls_curritem.

              ls_curritem-currency   = gt_ac_config-waers.
              ls_curritem-amt_doccur = ls_mwdat-wmwst / 100.
              ls_curritem-amt_base = ls_mwdat-kawrt / 100.

              IF gt_ac_config-actgroup IS NOT INITIAL.
                IF lv_item <= lvm_item.
                  lv_item = lvm_item + 1.
                ENDIF.
                ls_accounttax-itemno_acc = lvm_item.
                ls_curritem-itemno_acc = lvm_item.
                COLLECT ls_accounttax INTO lt_accounttax.
                COLLECT ls_curritem INTO lt_curritem.
              ELSE.
                lv_item = lv_item + 1.
                ls_accounttax-itemno_acc = lv_item.
                ls_curritem-itemno_acc = lv_item.
                APPEND ls_accounttax TO lt_accounttax.
                APPEND ls_curritem TO lt_curritem.
              ENDIF.


              CLEAR ls_accounttax.
              CLEAR ls_curritem.
            ENDLOOP.

            CLEAR ls_curritem.

          WHEN 'L' .
            CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT' "净价拆分含税价和税，并获取税科目
              EXPORTING
                i_bukrs = ls_header-comp_code
                i_mwskz = gt_ac_config-mwskz
                i_waers = gt_ac_config-waers
                i_wrbtr = ls_bapidoccur
              TABLES
                t_mwdat = lt_mwdat.


            CLEAR ls_kbetr.
            CLEAR sum_kbetr.
            LOOP AT lt_mwdat INTO ls_mwdat.
              ls_kbetr = lv_tax.
              sum_kbetr = sum_kbetr + lv_tax.
              ls_mwdat-wmwst = lv_tax.
              MODIFY lt_mwdat FROM ls_mwdat.
*              ls_kbetr = ls_kbetr + ls_mwdat-wmwst / 100.
            ENDLOOP.


            ls_curritem-amt_doccur = ls_bapidoccur - lv_tax.
            ls_curritem-currency = gt_ac_config-waers.

            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_curritem-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_curritem INTO lt_curritem.
            ELSE.
              ls_curritem-itemno_acc = lv_item.
              APPEND ls_curritem TO lt_curritem.
            ENDIF.

            CLEAR ls_curritem.

            CLEAR lvm_item.
            LOOP AT lt_mwdat INTO ls_mwdat."插入税金会计凭证行
              lvm_item = lt_lines-posnr_acc + sy-tabix.
              CLEAR ls_accounttax.

              ls_accounttax-tax_code = gt_ac_config-mwskz.
              ls_accounttax-acct_key   = ls_mwdat-ktosl.
              ls_accounttax-cond_key   = ls_mwdat-kschl.
              ls_accounttax-taxjurcode = ls_mwdat-txjcd.
              ls_accounttax-taxjurcode_deep  = ls_mwdat-txjcd_deep.
              ls_accounttax-taxjurcode_level = ls_mwdat-txjlv.

              CLEAR ls_curritem.

              ls_curritem-currency   = gt_ac_config-waers.
              ls_curritem-amt_doccur = lv_tax.
              ls_curritem-amt_base = lv_tax.

              IF gt_ac_config-actgroup IS NOT INITIAL.
                IF lv_item <= lvm_item.
                  lv_item = lvm_item + 1.
                ENDIF.
                ls_accounttax-itemno_acc = lvm_item.
                ls_curritem-itemno_acc = lvm_item.
                COLLECT ls_accounttax INTO lt_accounttax.
                COLLECT ls_curritem INTO lt_curritem.
              ELSE.
                lv_item = lv_item + 1.
                ls_accounttax-itemno_acc = lv_item.
                ls_curritem-itemno_acc = lv_item.
                APPEND ls_accounttax TO lt_accounttax.
                APPEND ls_curritem TO lt_curritem.
              ENDIF.


              CLEAR ls_accounttax.
              CLEAR ls_curritem.
            ENDLOOP.

            CLEAR ls_curritem.

          WHEN 'M'.

            CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT' "净价拆分含税价和税，并获取税科目
              EXPORTING
                i_bukrs = ls_header-comp_code
                i_mwskz = gt_ac_config-mwskz
                i_waers = gt_ac_config-waers
                i_wrbtr = ls_bapidoccur
              TABLES
                t_mwdat = lt_mwdat.


            CLEAR ls_kbetr.
            CLEAR sum_kbetr.
            LOOP AT lt_mwdat INTO ls_mwdat.
              ls_kbetr = lv_tax.
              sum_kbetr = sum_kbetr + lv_tax.
              ls_mwdat-wmwst = lv_tax.
              MODIFY lt_mwdat FROM ls_mwdat.
*              ls_kbetr = ls_kbetr + ls_mwdat-wmwst / 100.
            ENDLOOP.


            ls_curritem-amt_doccur = ls_bapidoccur + lv_tax.
            ls_curritem-currency = gt_ac_config-waers.

            IF gt_ac_config-actgroup IS NOT INITIAL.
              ls_curritem-itemno_acc = lt_lines-posnr_acc.
              COLLECT ls_curritem INTO lt_curritem.
            ELSE.
              ls_curritem-itemno_acc = lv_item.
              APPEND ls_curritem TO lt_curritem.
            ENDIF.

            CLEAR ls_curritem.

            CLEAR lvm_item.
            LOOP AT lt_mwdat INTO ls_mwdat."插入税金会计凭证行
              lvm_item = lt_lines-posnr_acc + sy-tabix.
              CLEAR ls_accounttax.

              ls_accounttax-tax_code = gt_ac_config-mwskz.
              ls_accounttax-acct_key   = ls_mwdat-ktosl.
              ls_accounttax-cond_key   = ls_mwdat-kschl.
              ls_accounttax-taxjurcode = ls_mwdat-txjcd.
              ls_accounttax-taxjurcode_deep  = ls_mwdat-txjcd_deep.
              ls_accounttax-taxjurcode_level = ls_mwdat-txjlv.

              CLEAR ls_curritem.

              ls_curritem-currency   = gt_ac_config-waers.
              ls_curritem-amt_doccur = - lv_tax.
              ls_curritem-amt_base = - lv_tax.

              IF gt_ac_config-actgroup IS NOT INITIAL.
                IF lv_item <= lvm_item.
                  lv_item = lvm_item + 1.
                ENDIF.
                ls_accounttax-itemno_acc = lvm_item.
                ls_curritem-itemno_acc = lvm_item.
                COLLECT ls_accounttax INTO lt_accounttax.
                COLLECT ls_curritem INTO lt_curritem.
              ELSE.
                lv_item = lv_item + 1.
                ls_accounttax-itemno_acc = lv_item.
                ls_curritem-itemno_acc = lv_item.
                APPEND ls_accounttax TO lt_accounttax.
                APPEND ls_curritem TO lt_curritem.
              ENDIF.


              CLEAR ls_accounttax.
              CLEAR ls_curritem.
            ENDLOOP.

            CLEAR ls_curritem.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    PERFORM fix_ac_extension TABLES lt_item
                                    lt_extension2.

    LOOP AT lt_curritem INTO ls_curritem WHERE amt_doccur = 0.
      IF lt_ac_config-zallow_zero = 'X'.
        DELETE lt_curritem.

        READ TABLE lt_item TRANSPORTING NO FIELDS WITH KEY itemno_acc = ls_curritem-itemno_acc.
        IF sy-subrc EQ 0.
          DELETE lt_item INDEX sy-tabix.
          CONTINUE.
        ENDIF.

        READ TABLE lt_accountpay TRANSPORTING NO FIELDS WITH KEY itemno_acc = ls_curritem-itemno_acc.
        IF sy-subrc EQ 0.
          DELETE lt_accountpay INDEX sy-tabix.
          CONTINUE.
        ENDIF.

        READ TABLE lt_accountrec TRANSPORTING NO FIELDS WITH KEY itemno_acc = ls_curritem-itemno_acc.
        IF sy-subrc EQ 0.
          DELETE lt_accountrec INDEX sy-tabix.
          CONTINUE.
        ENDIF.

        READ TABLE lt_accounttax TRANSPORTING NO FIELDS WITH KEY itemno_acc = ls_curritem-itemno_acc.
        IF sy-subrc EQ 0.
          DELETE lt_accounttax INDEX sy-tabix.
          CONTINUE.
        ENDIF.

      ELSE.
        PERFORM add_message USING 'E' 'ZCCT' '001' '会计凭证金额不能为0' '' '' ''.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc EQ 0 AND lt_curritem[] IS INITIAL.
      PERFORM add_message USING 'W' 'ZCCT' '001' '会计凭证金额0已跳过' '' '' ''.
      RETURN.
    ENDIF.

    IF gv_error = 'X'.
      RETURN.
    ENDIF.

   "add 20220914 修复公司间凭证，客户错误,取ZCCT_BL_CONFIG-KUNNR 替代

    IF lt_ac_config-bukrs ne lt_ac_config-zcct_vkorg
      and lt_ac_config-blart = 'Z5' and  lt_ac_config-saknr = '6401010100' .

      DATA:ls_item1 LIKE LINE OF lt_item.
      select
        kunnr into @DATA(ls_kunnr)
        from  zcct_bl_config
        where
             zcct_bl_config~zcct_werks  =  @lt_ac_config-zcct_werks
         and zcct_bl_config~zcct_vkorg  =  @lt_ac_config-zcct_vkorg
         and zcct_bl_config~zcct_type   =  @lt_ac_config-zcct_type
         and zcct_bl_config~vkorg       =  @lt_ac_config-bukrs.
       ENDSELECT.

      READ TABLE lt_item into ls_item1 index 1.
      if sy-subrc eq 0.
       if ls_item1-customer is not INITIAL and  ls_kunnr is not INITIAL and ls_kunnr ne ls_item1-customer.
           ls_item1-customer = ls_kunnr.
           MODIFY lt_item from ls_item1  index 1.
           CLEAR:ls_item1.
       endif.
      endif.
      clear:ls_kunnr .
    ENDIF.
    "end 20220914 修复公司间凭证，客户错误,取ZCCT_BL_CONFIG-KUNNR 替代

    BREAK h-wangf.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "会计凭证过账
      EXPORTING
        documentheader    = ls_header
      IMPORTING
        obj_type          = obj_type
        obj_key           = obj_key
        obj_sys           = obj_sys
      TABLES
        accountgl         = lt_item
        accountpayable    = lt_accountpay
        accountreceivable = lt_accountrec
        currencyamount    = lt_curritem
        accounttax        = lt_accounttax
        return            = lt_return
        extension2        = lt_extension2.

    LOOP AT lt_return  WHERE type = 'E' OR type = 'A' OR type = 'X'.
      IF lt_return-id = 'RW' AND lt_return-number = '609'.
        CONTINUE.
      ENDIF.
      APPEND lt_return TO ot_return.
    ENDLOOP.
    IF sy-subrc EQ 0.
      gv_error = 'X'.
      RETURN.
    ELSE.
      l_docnumber = obj_key+0(10).
      l_year = obj_key+14(4).
      IF lt_ac_config-bukrs IS NOT INITIAL.
        gt_borident-objkey = lt_ac_config-bukrs && l_docnumber && l_year."处理会计凭证关系浏览器
      ELSE.
        gt_borident-objkey = gv_cct_bukrs && l_docnumber && l_year."处理会计凭证关系浏览器
      ENDIF.
      gt_borident-objtype = 'BKPF'.
      APPEND gt_borident.
      CLEAR gt_borident.

      PERFORM add_message USING 'S' 'ZCCT' '006' l_docnumber lt_ac_config-bukrs l_year ''.
    ENDIF.

  ENDLOOP.

ENDFORM.




FORM binary_relation.

  CHECK gt_borident[] IS NOT INITIAL.

  DATA:   lt_foldoc        TYPE TABLE OF wpusa_foldoc WITH HEADER LINE.
  DATA:   ls_foldoc_attr   TYPE wpusa_foldoc_attr.


  READ TABLE gt_borident INTO DATA(ls_borident) WITH KEY objtype = 'IDOC'  .
  IF sy-subrc NE 0.
    READ TABLE gt_borident INTO ls_borident WITH KEY objtype = 'ZAFO'.
    IF sy-subrc NE 0.
      READ TABLE gt_borident INTO ls_borident WITH KEY objtype = 'BUS2017'.
      IF sy-subrc NE 0.
        READ TABLE gt_borident INTO ls_borident INDEX 1.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE gt_borident INTO DATA(idoc_borident) WITH KEY objtype = 'IDOC'.


  LOOP AT gt_borident .
    IF gt_borident-objtype <> 'IDOC'.
      ls_foldoc_attr-forceplus = 'X'.
      lt_foldoc-key = gt_borident-objkey.
      lt_foldoc-objtype = gt_borident-objtype.
      lt_foldoc-attr = ls_foldoc_attr.
      lt_foldoc-level = 1.
      APPEND lt_foldoc.
      CLEAR lt_foldoc.
    ENDIF.


    IF gt_borident-objtype = ls_borident-objtype
      AND gt_borident-objkey = ls_borident-objkey.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BINARY_RELATION_CREATE'
      EXPORTING
        obj_rolea      = ls_borident
        obj_roleb      = gt_borident
        relationtype   = 'VORL'
*       FIRE_EVENTS    = 'X'
*     IMPORTING
*       BINREL         =
*     TABLES
*       BINREL_ATTRIB  =
      EXCEPTIONS
        no_model       = 1
        internal_error = 2
        unknown        = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDLOOP.

  IF idoc_borident IS NOT INITIAL AND lt_foldoc[] IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(ls_edidc) FROM edidc WHERE docnum = @gv_docnum.


    CALL FUNCTION 'POS_SA_RELATE_DOCUMENTS'
      EXPORTING
        docnum     = ls_edidc-docnum
        segnum     = 1
        segnum_end = 999999
        mestyp     = ls_edidc-mestyp
        sndprn     = ls_edidc-sndprn
        uploadnr   = 'VORL'
        uploaddate = sy-datum
      TABLES
        foldoc     = lt_foldoc.

  ENDIF.






ENDFORM.

FORM add_message USING type id number msg1 msg2 msg3 msg4.
  CLEAR ot_return.
  IF type = 'E'.
    gv_error = 'X'.
  ENDIF.
  ot_return-type = type.
  ot_return-id = id.
  ot_return-number = number.
  ot_return-message_v1 = msg1.
  ot_return-message_v2 = msg2.
  ot_return-message_v3 = msg3.
  ot_return-message_v4 = msg4.
  MESSAGE ID ot_return-id TYPE ot_return-type NUMBER ot_return-number
     INTO ot_return-message WITH ot_return-message_v1
                                 ot_return-message_v2
                                 ot_return-message_v3
                                 ot_return-message_v4.
  APPEND ot_return.

ENDFORM.

FORM add_message_row USING type id number msg1 msg2 msg3 msg4 row.
  CLEAR ot_return.
  ot_return-type = type.
  ot_return-id = id.
  ot_return-number = number.
  ot_return-message_v1 = msg1.
  ot_return-message_v2 = msg2.
  ot_return-message_v3 = msg3.
  ot_return-message_v4 = msg4.
  ot_return-row = row.
  MESSAGE ID ot_return-id TYPE ot_return-type NUMBER ot_return-number
     INTO ot_return-message WITH ot_return-message_v1
                                 ot_return-message_v2
                                 ot_return-message_v3
                                 ot_return-message_v4.
  APPEND ot_return.

ENDFORM.
FORM clear_amount .
  CLEAR gv_last_netpr.
  CLEAR gv_last_amount_netpr.
  CLEAR gv_last_cmpre.
  CLEAR gv_last_amount_cmpre.
ENDFORM.


FORM get_next_amount USING amount_type rebate u_werks.


*  IF rebate IS INITIAL.
*    rebate = 1.
*  ENDIF.
*  IF amount_type = 'A'.
*    gv_last_netpr = gt_data-netwr .
*    gv_last_amount_netpr = gt_data-netwr *  rebate.
*  ENDIF.
*
*  IF amount_type = 'B'.
*    gv_last_cmpre = gt_data-cmpre.
*    gv_last_amount_cmpre = gt_data-cmpre *  rebate.
*  ENDIF.
*
*  IF amount_type = 'C'.
*    gv_last_netpr = gv_last_netpr * rebate.
*    gv_last_amount_netpr = gv_last_amount_netpr * rebate.
*  ENDIF.
*
*
*  IF amount_type = 'D'.
*    READ TABLE gt_cost WITH KEY matnr = gt_data-matnr werks = u_werks menge = gt_data-menge.
*    IF sy-subrc EQ 0 AND gt_cost-verpr IS NOT INITIAL.
*      gv_last_cmpre = gt_cost-verpr / gt_data-menge.
*      gv_last_amount_cmpre = gt_cost-verpr.
*    ELSE.
*      PERFORM add_message USING 'E' 'ZCCT' '012' gt_data-matnr u_werks '' ''.
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM add_message_emkpf USING type id number msg1 msg2 msg3 msg4 CHANGING emkpf STRUCTURE emkpf.
  CLEAR emkpf.
  IF type = 'E'.
    gv_error = 'X'.
  ENDIF.
  emkpf-msgty = type.
  emkpf-msgid = id.
  emkpf-msgno = number.
  emkpf-msgv1 = msg1.
  emkpf-msgv2 = msg2.
  emkpf-msgv3 = msg3.
  emkpf-msgv4 = msg4.
ENDFORM.

FORM exchangerate USING waers_x waers_y date CHANGING netwr.

  DATA: cw_rate LIKE bapi1093_0.
  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      rate_type  = 'M'
      from_curr  = waers_x
      to_currncy = waers_y
      date       = date
    IMPORTING
      exch_rate  = cw_rate.

  netwr = netwr * cw_rate-exch_rate.

ENDFORM.

FORM frm_get_exchangerate USING waers_x waers_y date CHANGING netwr.

  DATA: cw_rate LIKE bapi1093_0.
  date = date+0(6) && '01'.
  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      rate_type  = 'M'
      from_curr  = waers_x
      to_currncy = waers_y
      date       = date
    IMPORTING
      exch_rate  = cw_rate.
  netwr =  cw_rate-exch_rate.
ENDFORM.

FORM frm_add_zero CHANGING snum.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = snum
    IMPORTING
      output = snum.
ENDFORM.
