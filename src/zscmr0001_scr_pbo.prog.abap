*&---------------------------------------------------------------------*
*& Include ZSCMR0001_SCR_PBO
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAG9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tag9000_active_tab_set OUTPUT.
  tag9000-activetab = g_tag9000-pressed_tab.
  CASE g_tag9000-pressed_tab.
    WHEN c_tag9000-tab1.
      g_tag9000-subscreen = '9010'.
    WHEN c_tag9000-tab2.
      g_tag9000-subscreen = '9011'.
    WHEN c_tag9000-tab3.
      g_tag9000-subscreen = '9012'.
    WHEN c_tag9000-tab4.
      g_tag9000-subscreen = '9013'.
    WHEN c_tag9000-tab5.
      g_tag9000-subscreen = '9014'.
    WHEN c_tag9000-tab6.
      g_tag9000-subscreen = '9015'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9000 OUTPUT.
  DATA:lv_t1 TYPE string.
  DATA:lt_extab TYPE slis_t_extab.

  CLEAR:lv_t1,lt_extab.

  CASE gv_mod.
    WHEN 'A'.
      lv_t1 = 'BP创建'.
    WHEN 'B'.
      lv_t1 = 'BP维护'.
    WHEN 'C'.
      lv_t1 = 'BP显示'.
    WHEN OTHERS.
  ENDCASE.

  PERFORM set_fcode CHANGING lt_extab.

  SET PF-STATUS 'STA9000' EXCLUDING lt_extab.
  SET TITLEBAR 'T1' WITH lv_t1.

  PERFORM f_modi_scren_9000 USING gv_mod.

  PERFORM f_val_list."下拉菜单

  PERFORM f_sp_led.  " 状态判断

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_MODI_SCREN_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_modi_scren_9000 USING pv_mod.

  LOOP AT SCREEN.

*    IF screen-name+0(7) = 'TAG9000'.
*       CONTINUE.
*    ENDIF.
    IF screen-group1 CA 'TG'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    IF gv_redol = 'X'.
      screen-input = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    CASE screen-name.
      WHEN 'ZSCMT0001-BU_GROUP'.
        IF pv_mod = 'A' AND zscmt0001-bu_group IS INITIAL.
          CONTINUE.
        ELSE.
        ENDIF.
      WHEN 'ZSCMT0001-PARTNER'.
        IF  pv_mod = 'C' AND zscmt0001-partner IS INITIAL.
          CONTINUE.
        ELSEIF pv_mod = 'B' AND zscmt0001-partner IS INITIAL
               AND zscmt0001-zwhno IS INITIAL.
          CONTINUE.
        ENDIF.
      WHEN 'ZSCMT0001-ZWHNO'.
        "只要有值必须灰掉，否则只改维护号保存会造成重号
        IF pv_mod = 'B' AND zscmt0001-zwhno IS INITIAL
            AND zscmt0001-partner IS INITIAL.
          CONTINUE.
        ELSEIF pv_mod = 'C'.
          screen-active = '0'.
        ENDIF.
      WHEN OTHERS.

    ENDCASE.

    screen-input = '0'.
    MODIFY SCREEN.

  ENDLOOP.

  CHECK zscmt0001-bu_group IS NOT INITIAL.

  PERFORM f_bp_no USING pv_mod."bp编号的输入控制

  PERFORM f_pbo_tg."页签控制

  PERFORM f_pbo_field."字段的控制参数

  PERFORM f_field_def."字段的默认值
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_pbo_tg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pbo_tg .

  CLEAR:gt_1001,gs_tag.

  SELECT * FROM zscmt1001 INTO TABLE gt_1001
   WHERE bu_group = zscmt0001-bu_group.

  gs_tag-tg1 = gs_tag-tg2 = gs_tag-tg3 = gs_tag-tg4 = gs_tag-tg5 = gs_tag-tg6 = 0.
  LOOP AT gt_1001 INTO gs_1001.
    CASE gs_1001-role .
      WHEN 'BASIC'.
        gs_tag-tg1 = '1'."基本信息
      WHEN 'FLVN00' .
        gs_tag-tg2 = '1'."零售信息
        zscmt0001-flvn00 = 'X'.
      WHEN 'FLVN01'.
        gs_tag-tg2 = '1'."零售信息
        zscmt0001-flvn01 = 'X'.
      WHEN 'FLCU00' .
        gs_tag-tg3 = '1'."商场信息
        zscmt0001-flcu00 = 'X'.
      WHEN  'FLCU01'.
        gs_tag-tg3 = '1'."商场信息
        zscmt0001-flcu01 = 'X'.
      WHEN 'MANAGE'.
        gs_tag-tg4 = '1'."管理信息
      WHEN 'EXT'.
        gs_tag-tg5 = '1'."外招信息
      WHEN 'BRAND'.
        gs_tag-tg6 = '1'."品牌信息
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  IF zscmt0001-flcu00 = 'X' OR zScmt0001-flcu01 = 'X'.
    gs_tag-tg3 = '1'."零售信息
  ENDIF.

  IF zscmt0001-flvn00 = 'X' OR zScmt0001-flvn01 = 'X'.
    gs_tag-tg2 = '1'."零售信息
  ENDIF.
  LOOP AT SCREEN.
    CHECK screen-group1 IS NOT INITIAL.

    CASE screen-group1.
      WHEN 'TG1'.
        PERFORM mod_tag USING gs_tag-tg1
                        CHANGING screen.
      WHEN 'TG2'.
        PERFORM mod_tag USING gs_tag-tg2
                        CHANGING screen.
      WHEN 'TG3'.
        PERFORM mod_tag USING gs_tag-tg3
                        CHANGING screen.
      WHEN 'TG4'.
        PERFORM mod_tag USING gs_tag-tg4
                        CHANGING screen.
      WHEN 'TG5'.
        PERFORM mod_tag USING gs_tag-tg5
                        CHANGING screen.
      WHEN 'TG6'.
        PERFORM mod_tag USING gs_tag-tg6
                        CHANGING screen.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MOD_TAG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_TAG_TG1
*&      <-- SCREEN_ACTIVE
*&---------------------------------------------------------------------*
FORM mod_tag  USING    pv_tg
              CHANGING cv_screen.

  DATA:lv_fname TYPE string.
  FIELD-SYMBOLS:<fs_value>.

  IF pv_tg = '1'.
    lv_fname = 'ACTIVE'.
    ASSIGN COMPONENT lv_fname OF STRUCTURE cv_screen TO <fs_value>.
    <fs_value> = '1'.

    lv_fname = 'INVISIBLE'.
    ASSIGN COMPONENT lv_fname OF STRUCTURE cv_screen TO <fs_value>.
    <fs_value> = '0'.

    lv_fname = 'INPUT'.
    ASSIGN COMPONENT lv_fname OF STRUCTURE cv_screen TO <fs_value>.
    <fs_value> = '1'.

  ELSE.
    lv_fname = 'ACTIVE'.
    ASSIGN COMPONENT lv_fname OF STRUCTURE cv_screen TO <fs_value>.
    <fs_value> = '0'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9010 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9010 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9011 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9011 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9012 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9012 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.

*---运动城/商场的提示隐藏
  READ TABLE gt_1003 INTO gs_1003 WITH KEY zfname = 'ZYDC_SC_NO'
                                        BINARY SEARCH.
  IF sy-subrc = 0 AND gs_1003-zhide = 'X'.
    LOOP AT SCREEN.
      CHECK screen-name = 'TEXT-SJQD'.

      screen-active = '0'.
      MODIFY SCREEN.

      EXIT.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9013 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9013 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9014 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9014 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MOD_PBO_9015 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_pbo_9015 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM scr_field_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SCR_FIELD_PROC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM scr_field_proc .

  DATA:lv_sname TYPE string."结构名
  DATA:lv_fname LIKE zscmt1003-zfname."字段名

*---字段状态
  IF gv_mod = 'C' OR gv_redol = 'X'.

    LOOP AT SCREEN .

      CLEAR:lv_sname,lv_fname.

      IF screen-name+(12) EQ 'ZSCMT0001-FL'.
        screen-input = 0.
        screen-active = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      screen-input = 0.
      MODIFY SCREEN.

      SPLIT screen-name AT '-' INTO lv_sname lv_fname.

      CHECK lv_sname = 'ZSCMT0001'.

      READ TABLE gt_1003 INTO gs_1003 WITH KEY zfname = lv_fname
                                      BINARY SEARCH.
      IF sy-subrc NE 0.
        screen-active = 0.
        MODIFY SCREEN.
      ELSE.
        IF gs_1003-zhide = 'X'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ELSE."根据配置的屏幕属性设置

    LOOP AT SCREEN.
      CLEAR:lv_sname,lv_fname.

      SPLIT screen-name AT '-' INTO lv_sname lv_fname.


      CHECK lv_sname = 'ZSCMT0001'.

      IF screen-name+(12) EQ 'ZSCMT0001-FL'.

        READ TABLE gt_1001 INTO DATA(ls_1001) WITH KEY role = lv_fname.
        IF sy-subrc EQ 0.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
        screen-active = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.



      READ TABLE gt_1003 INTO gs_1003 WITH KEY zfname = lv_fname
                                      BINARY SEARCH.
      IF sy-subrc = 0.
        IF gs_1003-zedit = 'X'.
          screen-input = '1'.
*            screen-active = '1'.
*            screen-invisible = '0'.
        ELSE.
          screen-input = '0'.
*            screen-active = '1'.
*            screen-invisible = '0'.
        ENDIF.

        IF gs_1003-zreq = 'X'.
          screen-required = '1'.
*            screen-input = '1'.
*            screen-active = '1'.
*            screen-invisible = '0'.
        ELSE.
          screen-required = '0'.
        ENDIF.

        IF gs_1003-zhide = 'X'.
*            screen-input = '0'.
          screen-active = '0'.
*            screen-invisible = '1'.
        ENDIF.
      ELSE.
        screen-input = '0'.
        screen-active = '0'.
*          screen-invisible = '0'.
      ENDIF.

      MODIFY SCREEN .
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_pbo_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pbo_field .

  CLEAR:gt_1002,gt_1003,gt_1004,gt_1005.

  SELECT * FROM zscmt1002 INTO TABLE gt_1002
   WHERE bu_group = zscmt0001-bu_group
     AND tcode = sy-tcode.

  SELECT * FROM zscmt1004 INTO TABLE gt_1004
   WHERE bu_group = zscmt0001-bu_group
     AND defty = 'A'.

  IF gt_1002 IS NOT INITIAL.

    SELECT * FROM zscmt1003 INTO TABLE gt_1003
       FOR ALL ENTRIES IN gt_1002
     WHERE zzdzt_group = gt_1002-zzdzt_group.

  ENDIF.

  SELECT * FROM zscmt1005 INTO TABLE gt_1005
   WHERE bu_group = zscmt0001-bu_group.

  SORT gt_1003 BY zfname.
  SORT gt_1005 BY bu_group fieldname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bp_no
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_MOD
*&---------------------------------------------------------------------*
FORM f_bp_no  USING  pv_mod.

  DATA:
    ls_tb001 TYPE tb001,
    ls_nriv  TYPE nriv.


  CHECK pv_mod = 'A'.

  SELECT SINGLE *
     INTO ls_tb001
     FROM tb001
     WHERE bu_group = zscmt0001-bu_group.
  IF sy-subrc = 0.
    SELECT SINGLE *
    INTO ls_nriv
    FROM nriv
    WHERE object = 'BU_PARTNER'
      AND nrrangenr = ls_tb001-nrrng."号码段
    IF sy-subrc = 0 AND ls_nriv-externind = 'X'
        AND gv_redol = ''.
      ""AND zscmt0001-partner IS INITIAL."外部给号
      LOOP AT SCREEN.

        CHECK screen-name =  'ZSCMT0001-PARTNER'.

        screen-input = '1'.
        screen-required = '1'.

        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_field_def
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_field_def .

  FIELD-SYMBOLS:<fs_value>.

  LOOP AT gt_1004 INTO gs_1004.

    ASSIGN COMPONENT gs_1004-fname OF STRUCTURE zscmt0001 TO <fs_value>.

    CHECK sy-subrc = 0 AND <fs_value> IS INITIAL.

    <fs_value> = gs_1004-fvalue.

  ENDLOOP.

*----公司名称
  IF zscmt0001-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO gv_scr_butxt
      FROM t001
     WHERE bukrs = zscmt0001-bukrs.
  ELSE.
    gv_scr_butxt = ''.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_val_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_val_list USING pv_id TYPE vrm_id
                          pt_val TYPE vrm_values.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = pv_id
      values          = pt_val
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_val_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_val_list .

*---分销渠道
  IF gt_vtweg[] IS INITIAL.
    SELECT * FROM tvtwt INTO TABLE @DATA(lt_tvtwt)
     WHERE spras = 1.
    LOOP AT lt_tvtwt INTO DATA(ls_tvtwt).
      APPEND INITIAL LINE TO gt_vtweg ASSIGNING FIELD-SYMBOL(<fs_vtweg>).
      <fs_vtweg>-key  = ls_tvtwt-vtweg.
      <fs_vtweg>-text = ls_tvtwt-vtext.
    ENDLOOP.
  ENDIF.

  CLEAR:gv_vrmid.gv_vrmid = 'ZSCMT0001-VTWEG'.
  PERFORM f_set_val_list USING gv_vrmid gt_vtweg."下拉菜单设置

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_sp_led
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_sp_led .
  CLEAR:gv_scr_sp_led.

  CASE zscmt0001-zbp_status.
    WHEN ''.
      gv_scr_sp_led = '@0A@'.
      gv_scr_sta_txt = '未处理'.
    WHEN 'A'.
      gv_scr_sp_led = '@09@'.
      gv_scr_sta_txt = '已保存'.
    WHEN 'B'.
      gv_scr_sp_led = '@08@'.
      gv_scr_sta_txt = '已提交'.
    WHEN 'C'.
      gv_scr_sp_led = '@03@'.
      gv_scr_sta_txt = '已作废'.
    WHEN OTHERS.
      gv_scr_sp_led = '@0A@'.
      gv_scr_sta_txt = '未处理'.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fcode
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXTAB
*&---------------------------------------------------------------------*
FORM set_fcode  CHANGING ct_extab TYPE slis_t_extab.

  DATA:ls_extab TYPE slis_extab.

  CASE gv_mod.
    WHEN 'A'.
      CLEAR:ls_extab.
      ls_extab-fcode = 'SEARCH'.
      APPEND ls_extab TO ct_extab.

      CASE zscmt0001-zbp_status.
        WHEN ''.
          CLEAR:ls_extab.
          ls_extab-fcode = 'TJ'.
          APPEND ls_extab TO ct_extab.
        WHEN 'A'.
        WHEN 'B'.
          CLEAR:ls_extab.
          ls_extab-fcode = 'TJ'.
          APPEND ls_extab TO ct_extab.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'B'.
      IF zscmt0001-zbp_status = 'B'.
        CLEAR:ls_extab.
        ls_extab-fcode = 'TJ'.
        APPEND ls_extab TO ct_extab.
      ENDIF.
    WHEN 'C'.
      CLEAR:ls_extab.
      ls_extab-fcode = 'SAVE'.
      APPEND ls_extab TO ct_extab.

      CLEAR:ls_extab.
      ls_extab-fcode = 'TJ'.
      APPEND ls_extab TO ct_extab.

      CLEAR:ls_extab.
      ls_extab-fcode = 'CHECK'.
      APPEND ls_extab TO ct_extab.
    WHEN OTHERS.
  ENDCASE.

  IF zscmt0001-zbp_status = 'C'.
    CLEAR:ls_extab.
    ls_extab-fcode = 'SAVE'.
    APPEND ls_extab TO ct_extab.

    CLEAR:ls_extab.
    ls_extab-fcode = 'TJ'.
    APPEND ls_extab TO ct_extab.

    CLEAR:ls_extab.
    ls_extab-fcode = 'CHECK'.
    APPEND ls_extab TO ct_extab.

    CLEAR:ls_extab.
    ls_extab-fcode = 'SEARCH'.
    APPEND ls_extab TO ct_extab.
  ENDIF.

  SORT ct_extab BY fcode.
  DELETE ADJACENT DUPLICATES FROM ct_extab COMPARING fcode.

ENDFORM.

MODULE zscmt0001-bukrs.
  IF zscmt0001-bukrs IS NOT INITIAL.

    SELECT SINGLE werks,vkorg,ekorg,kkber
      INTO CORRESPONDING FIELDS OF
      @zscmt0001
      FROM zscmt1006
      WHERE bu_group = @zscmt0001-bu_group
      AND bukrs = @zscmt0001-bukrs.


  ENDIF.


ENDMODULE.


MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  DATA:manager TYPE REF TO cl_gos_manager.
  DATA:it_gos_sels TYPE tgos_sels.
  DATA:is_gos_sels TYPE sgos_sels.



  CHECK zscmt0001-partner IS NOT INITIAL .

  SELECT SINGLE partner INTO @DATA(ls_partner)
    FROM but000 WHERE partner = @zscmt0001-partner.
  CHECK sy-subrc EQ 0.

*    PERFORM frm_set_gos_sels TABLES it_gos_sels.

  obj-objtype = 'BUS1006'.

  obj-objkey = zscmt0001-partner .

  IF manager IS INITIAL .
    CREATE OBJECT manager
      EXPORTING
*       IP_START_DIRECT      = 'X'
*       it_service_selection = it_gos_sels
*       ip_no_instance       = 'X'
        is_object = obj
*       ip_no_commit         = 'X'
      EXCEPTIONS
        OTHERS    = 1.
    .


  ENDIF.



  cl_gui_cfw=>flush( ).


ENDMODULE.                    "CREATE_GOS_SERVICE
