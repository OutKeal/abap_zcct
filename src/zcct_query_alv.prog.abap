*&---------------------------------------------------------------------*
*& 包含               LZJS_COSTF01
*&---------------------------------------------------------------------*





DATA: g_grid_up           TYPE REF TO cl_gui_alv_grid,
      g_grid_down         TYPE REF TO cl_gui_alv_grid,
      gt_fcat_up          TYPE lvc_t_fcat,
      gt_fcat_down        TYPE lvc_t_fcat,
      gs_layout_down      TYPE lvc_s_layo,
      gs_layout_up        TYPE lvc_s_layo,
      gt_sort             TYPE lvc_t_sort,
      gt_exclude          TYPE ui_functions,
      g_docking_container TYPE REF TO cl_gui_docking_container,
      g_cumtom_container  TYPE REF TO cl_gui_custom_container,
      g_container_1       TYPE REF TO cl_gui_container,
      g_container_2       TYPE REF TO cl_gui_container,
      g_splitter          TYPE REF TO cl_gui_splitter_container,
      g_toolbar           TYPE REF TO cl_gui_toolbar.

DATA:index TYPE sy-tabix.

DATA:
  gt_fieldcat_alv TYPE slis_t_fieldcat_alv WITH HEADER LINE, "定义存放字段信息的内表
  gs_layout       TYPE slis_layout_alv, "定义存放画面布局控制数据的工作区
  gv_grid         TYPE REF TO cl_gui_alv_grid,
  gv_repid        LIKE sy-repid VALUE sy-repid. "报表ID


CLASS:
  lcl_event_receiver_grid DEFINITION DEFERRED.

CONSTANTS:
  cns_extension TYPE i VALUE 3000.  "Docking size
DATA:
  g_event_receiver_grid   TYPE REF TO lcl_event_receiver_grid.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
*    METHODS: handle_data_changed
*                FOR EVENT data_changed OF cl_gui_alv_grid
*      IMPORTING er_data_changed
*                e_onf4.
    METHODS handle_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

    METHODS  handle_click
      FOR EVENT hotspot_click
      OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no.
    METHODS   toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_double_click
      FOR EVENT double_click
      OF cl_gui_alv_grid
      IMPORTING e_row e_column.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid IMPLEMENTATION.
* DATA CHANGED
*  METHOD handle_data_changed.
*    PERFORM f_handle_data_changed
*      USING er_data_changed
*            e_onf4.
*  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_changed_finished.
    PERFORM f_changed_finished USING et_good_cells .

  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_click.
    DATA:ls_modi TYPE lvc_s_modi.
    DATA stbl TYPE lvc_s_stbl.

    PERFORM frm_hotspot_click USING e_column_id
                                    e_row_id  .


  ENDMETHOD.

  METHOD toolbar.
    PERFORM f_toolbar
      USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command
      USING e_ucomm.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_handle_double_click USING e_row e_column.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*&---------------------------------------------------------------------*
*& 包含               ZJS_COST_ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET TITLEBAR 'TT001'.
  SET PF-STATUS 'ST001'." EXCLUDING lt_excu.
ENDMODULE.


MODULE create_object_0100 OUTPUT.



  IF g_grid_up IS INITIAL.
**-- CREATE container
    PERFORM f_create_container.
**-- Layout
    PERFORM f_create_grid_layout.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_exclude_toolbar  CHANGING gt_exclude[].

    PERFORM f_set_grid_field_catalog_up.

    PERFORM f_set_sort_up.

    PERFORM f_assign_grid_handlers_up CHANGING g_grid_up.

    PERFORM f_register_grid_event USING g_grid_up.

***-- Display Grid Alv
*    CALL METHOD cl_gui_cfw=>flush.
    PERFORM f_display_grid_alv_up .

  ELSE .
    PERFORM f_refresh_grid_alv USING g_grid_up.

  ENDIF.

  IF g_grid_down IS INITIAL.

    CREATE OBJECT g_grid_down
      EXPORTING
        i_parent = g_container_2.
**-- Field_Catalog Define
    PERFORM f_set_grid_field_catalog_down.
**-- Grid event handler Define
    PERFORM f_assign_grid_handlers_down CHANGING g_grid_down.
**-- REGISTER EVENT
    PERFORM f_register_grid_event USING g_grid_down.
****-- Display Grid Alv
*    CALL METHOD cl_gui_cfw=>flush.
    IF gv_m = 'A'.
      PERFORM f_display_grid_alv_down USING gt_item_dis[].
    ELSEIF gv_m = 'B'.
      PERFORM f_display_grid_alv_down USING gt_msg_dis[].
    ELSEIF gv_m = 'C'.
      PERFORM f_display_grid_alv_down USING gt_vgbel_dis[].
    ENDIF.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_down.
  ENDIF.



ENDMODULE.


MODULE exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


MODULE user_command_0100 INPUT.

  DATA:ok_code TYPE sy-ucomm.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'ITEM'.
      IF gv_m <> 'A'.
        PERFORM frm_free_grid_down.
      ENDIF.
      gv_m = 'A'.
      PERFORM f_display_detail.
    WHEN 'MSG'.
      IF gv_m <> 'B'.
        PERFORM frm_free_grid_down.
      ENDIF.
      gv_m = 'B'.
      PERFORM f_display_detail.
    WHEN 'VGBEL'.
      IF gv_m <> 'C'.
        PERFORM frm_free_grid_down.
      ENDIF.
      gv_m = 'C'.
      PERFORM f_display_detail.

    WHEN 'REFRESH'.

      PERFORM frm_get_data.

      IF gt_head_dis[] IS INITIAL.
        MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
      ENDIF.

      PERFORM frm_deal_data.

    WHEN 'POSMON'.
      PERFORM frm_call_posmon.

    WHEN 'POST'.
      PERFORM frm_idoc_post.

    WHEN 'CANCEL'.
      PERFORM frm_idoc_cancel.

  ENDCASE.


ENDMODULE.


FORM f_create_container .



  IF g_docking_container IS INITIAL.
    CREATE OBJECT g_docking_container
      EXPORTING
        style     = cl_gui_control=>ws_child
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = g_docking_container->dock_at_left
        lifetime  = cl_gui_control=>lifetime_imode
        extension = cns_extension
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* SPLITTER CONTAINER
  IF g_splitter IS INITIAL.
    CREATE OBJECT g_splitter
      EXPORTING
        parent  = g_docking_container
        rows    = 2
        columns = 1.


    CALL METHOD g_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 50.
*
*    CALL METHOD g_splitter->set_row_height
*      EXPORTING
*        id     = 2
*        height = 400.

    g_container_1  = g_splitter->get_container( row = 1 column = 1 ).
    CREATE OBJECT g_grid_up
      EXPORTING
        i_parent = g_container_1.

    g_container_2  = g_splitter->get_container( row = 2 column = 1 ).

  ENDIF.





ENDFORM.


FORM f_set_grid_field_catalog_up .

  REFRESH: gt_fcat_up.
  REFRESH: gt_fieldcat_alv[].


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*     I_PROGRAM_NAME         =
*     I_INTERNAL_TABNAME     =
      i_structure_name       = 'ZCCT_HEAD_DIS'
      i_client_never_display = 'X'
*     I_INCLNAME             =
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = gt_fieldcat_alv[]
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE = 1
*     PROGRAM_ERROR          = 2
*     OTHERS                 = 3
    .


  PERFORM f_transfer_slis_to_lvc
          CHANGING gt_fieldcat_alv[]
                   gt_fcat_up[].

  LOOP AT gt_fcat_up ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_fcat>-col_pos <= 6.
      <fs_fcat>-fix_column = 'X'.
      <fs_fcat>-emphasize = 'C710'.
    ENDIF.

    IF <fs_fcat>-fieldname = 'DOCNUM'.
      <fs_fcat>-hotspot = 'X'.
    ENDIF.
*    READ TABLE lt_dd03l WITH KEY fieldname = <fs_fcat>-fieldname.
*    IF sy-subrc = 0 AND lt_dd03l-checktable IS NOT INITIAL.
*      <fs_fcat>-f4availabl = 'X'.
*    ENDIF.
  ENDLOOP.

ENDFORM.


FORM f_set_grid_field_catalog_down .

  REFRESH: gt_fcat_down.
  REFRESH: gt_fieldcat_alv[].
  DATA:ls_tabname TYPE dd02l-tabname.
  IF gv_m = 'A'.
    ls_tabname = 'ZCCT_ITEM_DIS'.
  ELSEIF gv_m = 'B'.
    ls_tabname = 'ZCCT_MSG_DIS'.
  ELSEIF gv_m = 'C'.
    ls_tabname = 'ZCCT_VGBEL_DIS'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*     I_PROGRAM_NAME         =
*     I_INTERNAL_TABNAME     =
      i_structure_name       = ls_tabname
      i_client_never_display = 'X'
*     I_INCLNAME             =
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = gt_fieldcat_alv[]
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE = 1
*     PROGRAM_ERROR          = 2
*     OTHERS                 = 3
    .


  PERFORM f_transfer_slis_to_lvc
          CHANGING gt_fieldcat_alv[]
                   gt_fcat_down[].

  LOOP AT gt_fcat_down ASSIGNING FIELD-SYMBOL(<fs_fcat>).


    CASE gv_m .
      WHEN 'A'.
        IF <fs_fcat>-col_pos <= 3.
          <fs_fcat>-fix_column = 'X'.
          <fs_fcat>-emphasize = 'C710'.
        ENDIF.

      WHEN 'B'.
        IF <fs_fcat>-col_pos <= 6.
          <fs_fcat>-fix_column = 'X'.
          <fs_fcat>-emphasize = 'C710'.
        ENDIF.
      WHEN 'C'.
        IF <fs_fcat>-col_pos <= 3.
          <fs_fcat>-fix_column = 'X'.
          <fs_fcat>-emphasize = 'C710'.
        ENDIF.
        IF <fs_fcat>-fieldname = 'DOCNR' OR <fs_fcat>-fieldname = 'C_DOCNR'.
          <fs_fcat>-hotspot = 'X'.
        ENDIF.
    ENDCASE.



  ENDLOOP.

ENDFORM.


FORM f_transfer_slis_to_lvc
  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
           ct_fcat     TYPE lvc_t_fcat..

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
    EXPORTING
      it_fieldcat = ct_fieldcat
    IMPORTING
      et_fieldcat = lt_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lt_fieldcat
    IMPORTING
      et_fieldcat_lvc   = ct_fcat.

ENDFORM.

FORM f_create_grid_exclude_toolbar
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO c_t_toolbar.
ENDFORM.

FORM f_assign_grid_handlers_down
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid.

  SET HANDLER g_event_receiver_grid->handle_changed_finished
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
  SET HANDLER g_event_receiver_grid->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid->handle_click
          FOR c_grid .

  SET HANDLER g_event_receiver_grid->toolbar FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_set_sort_up.
  CLEAR gt_sort[].
  DATA:ls_sort TYPE lvc_s_sort.

  ls_sort-spos = 1.
  ls_sort-fieldname = 'CREDAT'.
  ls_sort-DOWN = 'X'.
  APPEND ls_sort TO gt_sort.
  CLEAR ls_sort.

  ls_sort-spos = 2.
  ls_sort-fieldname = 'CRETIM'.
  ls_sort-DOWN = 'X'.
  APPEND ls_sort TO gt_sort.
  CLEAR ls_sort.



ENDFORM.

FORM f_assign_grid_handlers_up
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid.

  SET HANDLER g_event_receiver_grid->handle_changed_finished
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
  SET HANDLER g_event_receiver_grid->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid->handle_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid->handle_double_click
        FOR c_grid .

*  SET HANDLER g_event_receiver_grid->toolbar FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.



FORM f_register_grid_event
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* Enter event
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** Modify event
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*** double click event
*  CALL METHOD u_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>evt_dblclick_row_col.

ENDFORM.

FORM f_display_grid_alv_up .


*  data: ls_variant like disvariant.
*  ls_variant-report = sy-repid.

  CALL METHOD g_grid_up->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ' '
      is_layout            = gs_layout_up
      it_toolbar_excluding = gt_exclude[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_head_dis[]
      it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat_up[].

ENDFORM.

FORM f_display_grid_alv_down USING tab.

*  DATA: ls_variant LIKE disvariant.
*  ls_variant-report = sy-repid.

  CALL METHOD g_grid_down->set_table_for_first_display
    EXPORTING
*     is_variant           = ls_variant
      i_save               = ' '
      is_layout            = gs_layout_down
      it_toolbar_excluding = gt_exclude[]
      i_default            = 'X'
    CHANGING
      it_outtab            = tab
*     it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat_down[].


ENDFORM.

FORM f_refresh_grid_alv
   USING u_grid TYPE REF TO cl_gui_alv_grid..

  DATA: ls_scroll TYPE lvc_s_stbl.

  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.


  CALL METHOD u_grid->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_down.
  CALL METHOD u_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_scroll
      i_soft_refresh = 'X'.

ENDFORM.

FORM f_create_grid_layout .

  CLEAR: gs_layout_down , gs_layout_up.
  gs_layout_down-sel_mode   = 'A'.
  gs_layout_down-cwidth_opt = 'X'.
  gs_layout_down-zebra      = 'X'.
*  gs_layout-no_rowmark = 'X'.
*  gs_layout_down-box_fname = 'SELECTED'.
  IF g_grid_up IS NOT INITIAL.
    gs_layout_up-sel_mode   = 'A'.
    gs_layout_up-cwidth_opt = 'X'.
    gs_layout_up-zebra      = 'X'.
  ENDIF.

*  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

*  GS_LAYOUT-NUMC_TOTAL = CNS_CHAR_X.

*  GS_LAYOUT_DOWN-SGL_CLK_HD    = 'X'.
*  GS_LAYOUT_DOWN-TOTALS_BEF    = 'X'.             " 合计显示在上面
*  GS_LAYOUT_DOWN-NO_HGRIDLN    = ' '.
*  GS_LAYOUT_DOWN-NO_VGRIDLN    = ' '.
*  GS_LAYOUT_DOWN-NO_TOOLBAR    = SPACE.
*  GS_LAYOUT_DOWN-GRID_TITLE    = ' '.
*  GS_LAYOUT_DOWN-SMALLTITLE    = ' '.
*  GS_LAYOUT_DOWN-EXCP_FNAME    = 'ICON'.          " LED
*  GS_LAYOUT_DOWN-INFO_FNAME    = 'COLOR'.         " LINE COLOR
*  GS_LAYOUT_DOWN-CTAB_FNAME    = ' '.             " Cell color
*  GS_LAYOUT_DOWN-BOX_FNAME     = ' '.
*  GS_LAYOUT_DOWN-DETAILINIT    = ' '.


ENDFORM.

FORM f_changed_finished USING et_good_cells TYPE lvc_t_modi.
  DATA: ls_good_cells TYPE lvc_s_modi.
*    BREAK-POINT.
  DATA stbl TYPE lvc_s_stbl.

*  LOOP AT et_good_cells INTO ls_good_cells.
*    READ TABLE gt_item_dis ASSIGNING <gs_item_dis> INDEX ls_good_cells-row_id.
*    IF sy-subrc EQ 0.
*      <gs_item_dis>-icon = icon_led_yellow.
*      <gs_item_dis>-msg = '已修改'.
*
*      READ TABLE gt_item ASSIGNING <gs_item> WITH KEY zjs_vgbel = <gs_item_dis>-zjs_vgbel cost_line = <gs_item_dis>-cost_line.
*      IF sy-subrc EQ 0.
*        <gs_item> = <gs_item_dis>.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*  IF sy-subrc = 0.
*    PERFORM f_refresh_grid_alv USING g_grid_down.
*  ENDIF.

ENDFORM.

FORM f_toolbar USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

*  IF gv_readonly = 'X'.   "不能修改时不加载按钮
*    RETURN.
*  ENDIF.
*  MOVE 'ADD' TO ls_toolbar-function.
**    MOVE ICON_DISPLAY_TEXT TO LS_TOOLBAR-ICON.
*  MOVE '新增行' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '新增行' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*
*
*  CLEAR ls_toolbar.
*  MOVE 'DELETE' TO ls_toolbar-function.
**    MOVE ICON_DISPLAY_TEXT TO LS_TOOLBAR-ICON.
*  MOVE '删除行' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '删除行' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*
*  CLEAR ls_toolbar.

ENDFORM.

FORM f_user_command  USING   VALUE(iv_ucomm) TYPE sy-ucomm.

  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  CASE iv_ucomm.
*    WHEN 'ADD'.
*      CLEAR gt_item_add[].
*      CLEAR zjs_cost_item.
*      CALL SCREEN 200 STARTING AT 40 5
*                      ENDING AT 110 15 .    "弹窗选择款式和次数
*
*      PERFORM f_refresh_grid_alv USING g_grid_down.
*
*    WHEN 'DELETE'.
*      CALL METHOD g_grid_down->get_selected_rows
*        IMPORTING
*          et_index_rows = lt_index_rows
*          et_row_no     = lt_row_no.
*
*      IF lt_index_rows[] IS INITIAL.
*        MESSAGE '请至少选择一行明细数据做删除' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*
*      LOOP AT lt_index_rows INTO ls_index_rows.
*        READ TABLE gt_item_dis ASSIGNING <gs_item_dis> INDEX ls_index_rows-index.
*        IF sy-subrc EQ 0.
*          IF <gs_item_dis>-cost_line < 900000 AND <gs_item_dis>-zjs_vgbel IS NOT INITIAL.
*            MOVE-CORRESPONDING <gs_item_dis> TO gt_item_delete.
*            APPEND gt_item_delete.
*          ENDIF.
*
*          <gs_item_dis>-flag = 'D'.
*          READ TABLE gt_item ASSIGNING <gs_item> WITH KEY zjs_vgbel = gt_item_dis-zjs_vgbel cost_line = gt_item_dis-cost_line.
*          IF sy-subrc EQ 0.
*            <gs_item>-flag = 'D'.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*      DELETE gt_item_dis WHERE flag = 'D'.
*      DELETE gt_item WHERE flag = 'D'.
*
*      PERFORM f_refresh_grid_alv USING g_grid_down.


  ENDCASE.
ENDFORM.

FORM f_handle_double_click  USING    pv_row TYPE lvc_s_row
                                      pv_column.
  CLEAR:gt_item_dis[].
  CLEAR:gt_msg_dis[].
  CLEAR:gt_vgbel_dis[].



  READ TABLE gt_head_dis ASSIGNING <gs_head_dis> INDEX pv_row-index .
  IF sy-subrc EQ  0.

    LOOP AT gt_item WHERE docnum = <gs_head_dis>-docnum.
      APPEND gt_item TO gt_item_dis.
    ENDLOOP.


    LOOP AT gt_msg WHERE docnum = <gs_head_dis>-docnum.
      APPEND gt_msg TO gt_msg_dis.
    ENDLOOP.

    LOOP AT gt_vgbel WHERE docnum = <gs_head_dis>-docnum.
      APPEND gt_vgbel TO gt_vgbel_dis.
    ENDLOOP.
  ENDIF.


  PERFORM f_refresh_grid_alv USING g_grid_down.

ENDFORM.






FORM frm_free_grid_down.
  CALL METHOD g_grid_down->free.
  FREE  g_grid_down.
ENDFORM.
