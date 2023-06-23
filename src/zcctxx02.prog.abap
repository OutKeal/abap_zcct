*&---------------------------------------------------------------------*
*& 包含               ZCCTXX02
*&---------------------------------------------------------------------*

FORM move_return_emkpf TABLES i_return STRUCTURE bapiret2
                              CHANGING c_emkpf TYPE emkpf.
  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    c_emkpf-msgid = i_return-id.
    c_emkpf-msgno = i_return-number.
    c_emkpf-msgty = i_return-type.
    c_emkpf-msgv1 = i_return-message_v1.
    c_emkpf-msgv2 = i_return-message_v2.
    c_emkpf-msgv3 = i_return-message_v3.
    c_emkpf-msgv4 = i_return-message_v4.
  ENDIF.
ENDFORM.
FORM move_return_emseg TABLES i_return STRUCTURE bapiret2
                              e_emseg STRUCTURE emseg.
  READ TABLE e_emseg INDEX 1.
  CLEAR e_emseg[].
  LOOP AT i_return.
    e_emseg-msgid = i_return-id.
    e_emseg-msgno = i_return-number.
    e_emseg-msgty = i_return-type.
    e_emseg-msgv1 = i_return-message_v1.
    e_emseg-msgv2 = i_return-message_v2.
    e_emseg-msgv3 = i_return-message_v3.
    e_emseg-msgv4 = i_return-message_v4.
    APPEND e_emseg.
  ENDLOOP.
ENDFORM.

FORM move_value_notnull USING s1 f1 f2 CHANGING s2.
  FIELD-SYMBOLS <fs_value1> TYPE any.
  FIELD-SYMBOLS <fs_value2> TYPE any.
  UNASSIGN <fs_value1>.
  UNASSIGN <fs_value2>.
  ASSIGN COMPONENT f1 OF STRUCTURE s1 TO <fs_value1>.
  ASSIGN COMPONENT f2 OF STRUCTURE s2 TO <fs_value2>.
  IF <fs_value1> IS ASSIGNED AND <fs_value2> IS ASSIGNED.
    IF <fs_value1> IS NOT INITIAL.
      <fs_value2> = <fs_value1>.
    ENDIF.
  ENDIF.


ENDFORM.
FORM check_zcct_business TABLES imseg STRUCTURE imseg USING imkpf TYPE imkpf.

  DATA: lt_t001l TYPE TABLE OF t001l WITH HEADER LINE.
  DATA: lt_t320 TYPE TABLE OF t320 WITH HEADER LINE.
  DATA: lt_afpo TYPE TABLE OF afpo WITH HEADER LINE.
  DATA: lt_ekpo TYPE TABLE OF ekpo WITH HEADER LINE.
  DATA: lt_zcct_cf TYPE TABLE OF zcct_config WITH HEADER LINE.
  DATA: ewm_flag TYPE char1.
  DATA: no_ewm_flag TYPE char1.
  CLEAR lt_zcct_cf[].
  CLEAR lt_zcct_cf.

  CLEAR gv_zcct_active .

  LOOP AT imseg WHERE ( ebeln <> '' OR aufnr <> '' ) AND matnr <> ' '.
    IF imseg-bwart = '261' OR imseg-bwart = '262' OR imseg-bwart = 'Z65' OR imseg-bwart = 'Z66'.
      CONTINUE.
    ENDIF.
    lt_zcct_cf-zcct_werks = imseg-werks.
*    lt_zcct_cf-zcct_umwrk  = imseg-kzbew.
    lt_zcct_cf-zcct_type = imseg-bwart && imseg-kzbew.
    COLLECT lt_zcct_cf.
    CLEAR lt_zcct_cf.

    lt_t001l-werks = imseg-werks.
    lt_t001l-lgort = imseg-lgort.
    COLLECT lt_t001l.
    CLEAR lt_t001l.

    IF imseg-aufnr IS NOT  INITIAL.
      lt_afpo-aufnr = imseg-aufnr.
      COLLECT lt_afpo.
      CLEAR lt_afpo.
    ENDIF.

    IF imseg-ebeln IS NOT INITIAL.
      lt_ekpo-ebeln = imseg-ebeln.
      lt_ekpo-ebelp = imseg-ebelp.
      COLLECT lt_ekpo.
      CLEAR lt_ekpo.
    ENDIF.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  SELECT * FROM t320 INTO TABLE lt_t320 FOR ALL ENTRIES IN lt_t001l
    WHERE werks = lt_t001l-werks AND lgort = lt_t001l-lgort.
  IF sy-subrc EQ 0.
    SELECT * FROM t340d INTO TABLE @DATA(lt_t340d) FOR ALL ENTRIES IN
      @lt_t320 WHERE lgnum = @lt_t320-lgnum.
  ENDIF.

  CLEAR :ewm_flag,no_ewm_flag.
  LOOP AT lt_t001l.
    READ TABLE lt_t320 WITH KEY werks = lt_t001l-werks lgort = lt_t001l-lgort.
    IF sy-subrc EQ 0.
      READ TABLE lt_t340d INTO DATA(ls_t340d) WITH KEY lgnum = lt_t320-lgnum.
      IF sy-subrc EQ 0 AND ls_t340d-decsy = 'E'.
        ewm_flag = 'X'.
      ELSE.
        no_ewm_flag = 'X'.
      ENDIF.
    ELSE.
      no_ewm_flag = 'X'.
    ENDIF.
  ENDLOOP.

  IF ewm_flag = 'X'  AND no_ewm_flag = 'X'.
    PERFORM add_message USING 'E' 'ZCCT' '00' 'EWM仓与非EWMS仓无法同时记账' '' '' '' .
    RETURN.
  ENDIF.

  IF ewm_flag = 'X' .
    IF imkpf-spe_logsys IS INITIAL .
      RETURN.
    ENDIF.
  ENDIF.



  IF lt_afpo[] IS NOT INITIAL.
    SELECT aufnr ,pwerk, dwerk FROM afpo INTO TABLE @DATA(it_afpo)
      FOR ALL ENTRIES IN @lt_afpo WHERE aufnr = @lt_afpo-aufnr.

    DELETE ADJACENT DUPLICATES FROM it_afpo COMPARING pwerk dwerk.

    DESCRIBE TABLE it_afpo LINES DATA(ls_lines).
    IF ls_lines > 1.
      PERFORM add_message USING 'E' 'ZCCT' '00' '生产收货工厂与发货工厂的组合不唯一' '' '' '' .
      RETURN.
    ENDIF.

    READ TABLE it_afpo INDEX 1 INTO DATA(is_afpo).

    CHECK is_afpo-pwerk IS NOT INITIAL AND is_afpo-pwerk <> is_afpo-dwerk.

    gv_cct_werks = is_afpo-dwerk.
    gv_cct_vkorg = is_afpo-pwerk.

    gv_zcct_active = 'X'.

  ENDIF.


  IF lt_ekpo[] IS NOT INITIAL.
    SELECT ebeln ,ebelp, werks AS dwerk
*      , pwerk
        FROM ekpo
        INTO TABLE @DATA(it_ekpo)
      FOR ALL ENTRIES IN @lt_ekpo WHERE ebeln = @lt_ekpo-ebeln AND ebelp = @lt_ekpo-ebelp.

    DELETE ADJACENT DUPLICATES FROM it_ekpo COMPARING
*    pwerk
    dwerk.

    DESCRIBE TABLE it_ekpo LINES ls_lines.
    IF ls_lines > 1.
      PERFORM add_message USING 'E' 'ZCCT' '00' '采购收货工厂与发货工厂的组合不唯一' '' '' '' .
      RETURN.
    ENDIF.

    READ TABLE it_ekpo INDEX 1 INTO DATA(is_ekpo).

*    CHECK is_ekpo-pwerk IS NOT INITIAL AND is_ekpo-pwerk <> is_ekpo-dwerk.

    gv_cct_werks = is_ekpo-dwerk.
*    gv_cct_umwrk = is_ekpo-pwerk.

    gv_zcct_active = 'X'.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM lt_zcct_cf.
  IF sy-subrc EQ 0.
    PERFORM add_message USING 'E' 'ZCCT' '00' '生产工厂移动类型必须唯一' '' '' ''  .
    RETURN.
  ENDIF.
  READ TABLE  lt_zcct_cf INDEX 1.
  gv_cct_type = lt_zcct_cf-zcct_type.

ENDFORM.

FORM set_gt_data TABLES imseg STRUCTURE imseg USING cmkpf TYPE imkpf.

  DATA: ls_netpr TYPE netpr.
  gv_post_date = cmkpf-budat.
  gv_bktxt = cmkpf-bktxt.

  SELECT
     matnr,
     bwkey ,
     vprsv ,
     verpr ,
     stprs
    INTO TABLE @DATA(lt_mbew)
    FROM mbew FOR ALL ENTRIES IN @imseg
    WHERE matnr = @imseg-matnr AND bwkey = @gv_cct_werks.

  LOOP AT imseg .
    gt_data-line_id = imseg-line_id.
    gt_data-matnr = imseg-matnr.
    gt_data-charg = imseg-charg.
    IF imseg-menge IS INITIAL.
      gt_data-menge = imseg-erfmg.
    ELSE.
      gt_data-menge = imseg-menge.
    ENDIF.
    gt_data-netWr = imseg-dmbtr.

    READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = imseg-matnr.
    IF sy-subrc EQ 0.
      IF ls_mbew-vprsv = 'S'.
        ls_netpr = ls_mbew-stprs.
        IF ls_mbew-stprs IS INITIAL.
          PERFORM add_message USING 'E' 'ZCCT' '013' imseg-matnr gv_cct_werks '' ''.
        ENDIF.
      ELSEIF ls_mbew-vprsv = 'V'.
        ls_netpr = ls_mbew-verpr.
      ENDIF.

    ENDIF.

    gt_data-netWr = ls_netpr * gt_data-menge.
*    gt_data-CMPRE =
    gt_data-aufnr = imseg-aufnr.
    gt_data-ebeln = imseg-ebeln.
    gt_data-ebelp = imseg-ebelp.
    APPEND gt_data.
    CLEAR gt_data.
  ENDLOOP.
ENDFORM.

FORM mb_doc_modify TABLES imseg STRUCTURE imseg.
  DATA: lv_line TYPE char6.
  DATA: new_imseg TYPE TABLE OF imseg WITH HEADER LINE.
  CLEAR new_imseg[].

  lv_line = lines( imseg ).

  LOOP AT imseg.
    CLEAR new_imseg.
    READ TABLE gt_data WITH KEY ebeln = imseg-ebeln ebelp = imseg-ebelp aufnr = imseg-aufnr matnr = imseg-matnr .
    LOOP AT gt_mb_config.

*      PERFORM get_next_amount USING"计算金额，金额=基础值*折扣(默认1).基础值四种类型，传入的成本额，销售额，或者上一步滚动计算的成本额/销售额
*             gt_mb_config-zcct_amount_type gt_mb_config-zcct_rebate gt_mb_config-werks.

      IF gt_mb_config-zcct_line = 0 OR gt_mb_config-zcct_line = '999'.
        new_imseg = imseg.
        new_imseg-insmk = gt_mb_config-insmk.
        new_imseg-ablad = new_imseg-lgort.
        new_imseg-kzbew = gt_mb_config-kzbew.
        PERFORM move_value_notnull USING gt_mb_config 'BWART' 'BWART' CHANGING new_imseg.
*        PERFORM move_value_notnull USING gt_mb_config 'KZBEW' 'KZBEW' CHANGING new_imseg.x
        PERFORM move_value_notnull USING gt_mb_config 'SOBKZ' 'SOBKZ' CHANGING new_imseg.
        new_imseg-werks = gt_mb_config-werks.
*        PERFORM move_value_notnull USING gt_mb_config 'INSMK' 'INSMK' CHANGING new_imseg.
        PERFORM move_value_notnull USING gt_mb_config 'LGORT' 'LGORT' CHANGING new_imseg.
        PERFORM move_value_notnull USING gt_mb_config 'UMWRK' 'UMWRK' CHANGING new_imseg.
        PERFORM move_value_notnull USING gt_mb_config 'UMLGO' 'UMLGO' CHANGING new_imseg.
        PERFORM move_value_notnull USING gt_mb_config 'PRCTR' 'PRCTR' CHANGING new_imseg.
        PERFORM move_value_notnull USING gt_mb_config 'KOSTL' 'KOSTL' CHANGING new_imseg.

*        CASE gt_mb_config-zcct_amount_type.
*          WHEN 'A' OR 'C'.
*            new_imseg-exbwr = gv_last_amount_netpr.
*          WHEN 'B' OR 'D'.
*            new_imseg-exbwr = gv_last_amount_cmpre.
*        ENDCASE.
        CASE gt_mb_config-kzbew.
          WHEN 'F'.
            new_imseg-aufnr = imseg-aufnr.

          WHEN 'B'.
            new_imseg-ebeln = imseg-ebeln.
            new_imseg-ebelp = imseg-ebelp.
        ENDCASE.


        IF new_imseg-bwart <> '122' AND new_imseg-grund  = '0001'.
          CLEAR new_imseg-grund.
        ENDIF.

        APPEND new_imseg .
        CLEAR new_imseg.
      ELSE.
        new_imseg = imseg.
        lv_line = lv_line + 1.
        new_imseg-line_id = lv_line.
        new_imseg-parent_id = imseg-line_id .
        new_imseg-insmk = gt_mb_config-insmk.
        new_imseg-matnr = imseg-matnr.
        new_imseg-charg = imseg-charg.
        new_imseg-erfmg = imseg-erfmg.
        new_imseg-menge = imseg-menge.
        new_imseg-erfme = imseg-erfme.
        new_imseg-meins = imseg-meins.
*        new_imseg-insmk = gt_mb_config-insmk.
        new_imseg-bwart = gt_mb_config-bwart.
        new_imseg-kzbew = gt_mb_config-kzbew.
        new_imseg-sobkz = gt_mb_config-sobkz.
        new_imseg-werks = gt_mb_config-werks.
        new_imseg-prctr = gt_mb_config-prctr.
        new_imseg-kostl = gt_mb_config-kostl.

        new_imseg-called_by = imseg-called_by.

        CASE gt_mb_config-sobkz.
          WHEN ' '.
            IF gt_mb_config-lgort IS INITIAL.
              new_imseg-lgort = imseg-lgort.
            ELSE.
              new_imseg-lgort = gt_mb_config-lgort.
            ENDIF.
          WHEN 'O'.
            new_imseg-lifnr = imseg-lifnr.
          WHEN 'W'.
            new_imseg-kunnr = imseg-kunnr.
        ENDCASE.
        CLEAR new_imseg-aufnr.
        CLEAR new_imseg-ebeln.
        CLEAR new_imseg-ebelp.

        CASE gt_mb_config-kzbew.
          WHEN 'F'.
            new_imseg-aufnr = imseg-aufnr.

          WHEN 'B'.
            new_imseg-ebeln = imseg-ebeln.
            new_imseg-ebelp = imseg-ebelp.
        ENDCASE.

        new_imseg-umwrk = gt_mb_config-umwrk.
        new_imseg-umlgo = gt_mb_config-umlgo.
        new_imseg-umcha = imseg-umcha.

*        CASE gt_mb_config-zcct_amount_type.
*          WHEN 'A' OR 'C'.
*            new_imseg-exbwr = gv_last_amount_netpr.
*          WHEN 'B' OR 'D'.
*            new_imseg-exbwr = gv_last_amount_cmpre.
*        ENDCASE.

        IF new_imseg-bwart <> '122' AND new_imseg-grund  = '0001'.
          CLEAR new_imseg-grund.
        ENDIF.

        APPEND new_imseg .
        CLEAR new_imseg.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CLEAR imseg[].
  imseg[] = new_imseg[].
ENDFORM.
