*&---------------------------------------------------------------------*
*& Include ZSCMR0001_SCR_PAI
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'TAG9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tag9000_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tag9000-tab1.
      g_tag9000-pressed_tab = c_tag9000-tab1.
    WHEN c_tag9000-tab2.
      g_tag9000-pressed_tab = c_tag9000-tab2.
    WHEN c_tag9000-tab3.
      g_tag9000-pressed_tab = c_tag9000-tab3.
    WHEN c_tag9000-tab4.
      g_tag9000-pressed_tab = c_tag9000-tab4.
    WHEN c_tag9000-tab5.
      g_tag9000-pressed_tab = c_tag9000-tab5.
    WHEN c_tag9000-tab6.
      g_tag9000-pressed_tab = c_tag9000-tab6.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOD_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_exit INPUT.
  CLEAR ok_code.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  save_ok = sy-ucomm.
  CLEAR:sy-ucomm.
  CLEAR:gv_status,gv_msg,gt_return.

  CASE save_ok.
    WHEN 'SEARCH'. " 查询修改
      IF gv_mod <> 'A'.
        PERFORM f_get_data.
      ENDIF.
    WHEN 'CLEAR'. " CLEAR
      PERFORM f_clear_data.
    WHEN 'CHECK'. " 检查
      PERFORM f_check_data.
    WHEN 'SAVE'. " 保存
      PERFORM f_save_data.
    WHEN 'TJ'."提交
      PERFORM f_tj_data.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_clear_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_clear_data .
  CLEAR:zscmt0001.
*   CLEAR:gs_data,gt_data.
  CLEAR:gv_status,gv_msg.
  CLEAR:gv_redol,gt_return.
  CLEAR:gt_1002,gt_1003,gt_1004,gt_1005.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_save_data .

  DATA:lr_root TYPE REF TO cx_root.
  DATA:ls_0001 LIKE zscmt0001.

  PERFORM f_check_data.

  IF gv_status = 'E'.

    MESSAGE e899(mg) WITH gv_msg.

  ELSE.

*---修改的时候也可以保存
    IF zscmt0001-zwhno IS INITIAL.
      PERFORM get_scanid CHANGING zscmt0001-zwhno.
    ENDIF.

    IF zscmt0001-partner IS INITIAL.
*      PERFORM get_partner_no CHANGING zscmt0001-partner.
    ENDIF.

    TRY .
        CLEAR:ls_0001.


        zscmt0001-zbp_status = 'A'.

        IF zscmt0001-erdat IS INITIAL.
          zscmt0001-erdat = sy-datum.
          zscmt0001-erzet = sy-uzeit.
          zscmt0001-ernam = sy-uname.
        ENDIF.

*        IF gv_mod = 'B'.
        zscmt0001-modat = sy-datum.
        zscmt0001-mozet = sy-uzeit.
        zscmt0001-monam = sy-uname.
*        ENDIF.

        MOVE zscmt0001 TO ls_0001.

        MODIFY zscmt0001 FROM ls_0001.
        IF sy-subrc = 0.
          gv_redol = 'X'.
          COMMIT WORK.

          MESSAGE s899(mg) WITH '保存成功'.
        ELSE.
          zscmt0001-zbp_status = ''.
          ROLLBACK WORK.

          MESSAGE e899(mg) WITH '保存失败'.
        ENDIF.
      CATCH cx_root INTO lr_root.
        zscmt0001-zbp_status = ''.

        gv_msg = lr_root->get_text( ).

        MESSAGE e899(mg) WITH gv_msg.
    ENDTRY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_check_data .

  CASE gv_mod.
    WHEN 'A'."
      PERFORM auth_check USING '01'.

      SELECT SINGLE * FROM zscmt0010
        WHERE zname1 = @zscmt0001-zname1
        into @data(ls_zscmt0010).
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO gt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
        <fs_return>-type = 'E'.
        <fs_return>-id = 'MG'.
        <fs_return>-number = '899'.
        <fs_return>-message =
        <fs_return>-message_v1 =
        '该合作伙伴已存在'.
      ENDIF.
    WHEN 'B'.
      PERFORM auth_check USING '02'.
    WHEN OTHERS.
  ENDCASE.

*---创建的时候BP号已经存在，不允许创建
  IF gv_mod = 'A' AND zscmt0001-partner IS NOT INITIAL.

    PERFORM dup_check.

  ENDIF.

  PERFORM check_tab_value.

  PERFORM pop_msg .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MOD_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_data_check INPUT.

  IF zscmt0001-zterm_cus IS NOT INITIAL AND
     zscmt0001-zterm_ven IS INITIAL.
    zscmt0001-zterm_ven = zscmt0001-zterm_cus.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data .
  DATA:ls_0001 LIKE zscmt0001.
  DATA:ls_0010 LIKE zscmt0010.

  IF zscmt0001-partner IS NOT INITIAL.

    SELECT SINGLE * FROM zscmt0001 INTO ls_0001
     WHERE partner = zscmt0001-partner.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM zscmt0010 INTO  ls_0010
      WHERE partner = zscmt0001-partner.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_0010 TO zscmt0001.
        zscmt0001-zbp_status = 'B'.
      ELSE.
        PERFORM f_clear_data.
        MESSAGE e899(mg) WITH '未找到满足条件的数据'.
      ENDIF.
    ELSE.
      MOVE ls_0001 TO zscmt0001 .
      zscmt0001-zbp_status = 'A'.
    ENDIF.

  ELSEIF zscmt0001-zwhno IS NOT INITIAL.
    SELECT SINGLE * FROM zscmt0001 INTO ls_0001
     WHERE zwhno = zscmt0001-zwhno.
    IF sy-subrc NE 0.
      PERFORM f_clear_data.
      MESSAGE e899(mg) WITH '未找到满足条件的数据'.
    ELSE.
      MOVE ls_0001 TO zscmt0001 .
      zscmt0001-zbp_status = 'A'.
    ENDIF.
  ELSE.
    PERFORM f_clear_data.
    MESSAGE e899(mg) WITH '请至少输入一个查询条件'.
  ENDIF.

  CASE gv_mod.
    WHEN 'B'."
      PERFORM auth_check USING '02'.
    WHEN 'C'.
      PERFORM auth_check USING '03'.
    WHEN OTHERS.
  ENDCASE.

  IF gt_return[] IS NOT INITIAL.
    PERFORM pop_msg .
    PERFORM f_clear_data.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_tj_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_tj_data .

  CLEAR:gv_status,gv_msg.

  PERFORM f_check_data.

  CHECK gv_status NE 'E'.

  IF zscmt0001-zwhno IS INITIAL.
    PERFORM get_scanid CHANGING zscmt0001-zwhno.
  ENDIF.

  IF zscmt0001-erdat IS INITIAL.
    zscmt0001-erdat = sy-datum.
    zscmt0001-erzet = sy-uzeit.
    zscmt0001-ernam = sy-uname.
  ENDIF.

*    IF gv_mod = 'B'.
  zscmt0001-modat = sy-datum.
  zscmt0001-mozet = sy-uzeit.
  zscmt0001-monam = sy-uname.
*    ENDIF.

  IF sy-binpt = 'X' AND sy-calld = 'X'.
    CLEAR sy-binpt .
    CLEAR sy-calld .
    CALL FUNCTION 'ZSCM_BP_CREATE' "IN BACKGROUND TASK
      IMPORTING
        ev_status = gv_status
      TABLES
        et_return = gt_return
      CHANGING
        cs_data   = zscmt0001.
    sy-binpt = 'X'.
    sy-calld = 'X'.

    MESSAGE s899(mg) WITH '已后台处理'.
  ELSE.

    CALL FUNCTION 'ZSCM_BP_CREATE'
      IMPORTING
        ev_status = gv_status
      TABLES
        et_return = gt_return
      CHANGING
        cs_data   = zscmt0001.
  ENDIF.
  IF gv_status = 'S'.
    gv_redol = 'X'."提交后不允许修改
    zscmt0001-zbp_status = 'B'.
    MESSAGE s899(mg) WITH '创建成功'.
  ELSE.
    PERFORM pop_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form dup_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM dup_check .

  FIELD-SYMBOLS:<fs_return> LIKE LINE OF gt_return.

  IF save_ok = 'CHECK' OR save_ok = 'SAVE'.
    SELECT COUNT(*) FROM zscmt0001
     WHERE partner = zscmt0001-partner.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO gt_return ASSIGNING <fs_return>.
      <fs_return>-type = 'E'.
      <fs_return>-id = 'MG'.
      <fs_return>-number = '899'.
      <fs_return>-message =
      <fs_return>-message_v1 =
      'BP编号' && zscmt0001-partner && '已经存在'.
*        MESSAGE e899(mg) WITH 'BP编号' zscmt0001-partner '已经存在'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT COUNT(*) FROM zscmt0010
   WHERE partner = zscmt0001-partner.
  IF sy-subrc = 0.
    APPEND INITIAL LINE TO gt_return ASSIGNING <fs_return>.
    <fs_return>-type = 'E'.
    <fs_return>-id = 'MG'.
    <fs_return>-number = '899'.
    <fs_return>-message =
    <fs_return>-message_v1 =
    'BP编号' && zscmt0001-partner && '已经存在'.
*        MESSAGE e899(mg) WITH 'BP编号' zscmt0001-partner '已经存在'.
    EXIT.
  ENDIF.

  SELECT COUNT(*) FROM but001
   WHERE partner = zscmt0001-partner.
  IF sy-subrc = 0.
    APPEND INITIAL LINE TO gt_return ASSIGNING <fs_return>.
    <fs_return>-type = 'E'.
    <fs_return>-id = 'MG'.
    <fs_return>-number = '899'.
    <fs_return>-message =
    <fs_return>-message_v1 =
    'BP编号' && zscmt0001-partner && '已经存在'.
*        MESSAGE e899(mg) WITH 'BP编号' zscmt0001-partner '已经存在'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM auth_check USING pv_actvt.

  FIELD-SYMBOLS:<fs_return> LIKE LINE OF gt_return.

  AUTHORITY-CHECK OBJECT 'F_KNA1_GRP'
               ID 'KTOKD' FIELD  zscmt0001-bu_group
               ID 'ACTVT' FIELD pv_actvt.
  IF sy-subrc NE 0.
    APPEND INITIAL LINE TO gt_return ASSIGNING <fs_return>.
    <fs_return>-type = 'E'.
    <fs_return>-id = 'MG'.
    <fs_return>-number = '899'.
    <fs_return>-message =
    <fs_return>-message_v1 =
    '没有分组' && zscmt0001-bu_group && '的权限'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'F_KNA1_BUK'
               ID 'BUKRS' FIELD  zscmt0001-bukrs
               ID 'ACTVT' FIELD pv_actvt.
  IF sy-subrc NE 0.
    APPEND INITIAL LINE TO gt_return ASSIGNING <fs_return>.
    <fs_return>-type = 'E'.
    <fs_return>-id = 'MG'.
    <fs_return>-number = '899'.
    <fs_return>-message =
    <fs_return>-message_v1 =
    '没有公司' && zscmt0001-bukrs && '的权限'.
  ENDIF.

ENDFORM.

MODULE f4_zterm_costomer INPUT.

  PERFORM f4_zterm USING 'D' CHANGING zscmt0001-zterm_cus.

ENDMODULE.                 " f4_zterm  INPUT

MODULE f4_account_costomer INPUT.

  PERFORM f4_account USING 'D' CHANGING zscmt0001-akont_cus.

ENDMODULE.                 " f4_recon_account  INPUT

MODULE f4_zterm_vendor INPUT.

  PERFORM f4_zterm USING 'K' CHANGING zscmt0001-zterm_ven.

ENDMODULE.                 " f4_zterm  INPUT

MODULE f4_account_vendor INPUT.

  PERFORM f4_account USING 'K' CHANGING zscmt0001-akont_ven.

ENDMODULE.                 " f4_recon_account  INPUT

*&---------------------------------------------------------------------*
*&      Form  f4_zterm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_zterm USING i_koart CHANGING u_zterm.


  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
      i_koart = i_koart
      i_xshow = ''
    IMPORTING
      e_zterm = u_zterm
    EXCEPTIONS
      OTHERS  = 0.

ENDFORM.                    "f4_zterm



FORM f4_account USING i_mitkz CHANGING u_akont.
  CALL FUNCTION 'FI_F4_AKONT'
    EXPORTING
      i_bukrs  = zscmt0001-bukrs
      i_mitkz  = i_mitkz
*     i_akont  = zscmt0001-akont_cus
*     i_xshow  = 'X'
    IMPORTING
      e_akont  = u_akont
      e_akont0 = u_akont.

ENDFORM.
