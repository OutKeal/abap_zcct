FUNCTION zge_doma_get .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(SPRAS) TYPE  SPRAS DEFAULT '1'
*"  TABLES
*"      DD04VV STRUCTURE  DD04VV OPTIONAL
*"      DD04T STRUCTURE  DD04T OPTIONAL
*"      DOMA_VALUE STRUCTURE  ZDOMA_VALUE OPTIONAL
*"----------------------------------------------------------------------
  CHECK dd04vv[] IS NOT INITIAL.

  DATA: dd04vv_def TYPE TABLE OF dd04vv WITH HEADER LINE.
  DATA: dd04vv_tab TYPE TABLE OF dd04vv WITH HEADER LINE.
  DATA: dd08l TYPE TABLE OF dd08l WITH HEADER LINE.
  DATA: it_dd03l TYPE TABLE OF dd03l WITH HEADER LINE.
  DATA: dd07t TYPE TABLE OF dd07t WITH HEADER LINE.
  DATA: tabname TYPE tabname.
  DATA: fieldname TYPE fieldname.
  DATA: valfname TYPE fieldname.
  DATA: sprasname TYPE fieldname.

  DATA: l_field(100) TYPE c,
*      l_table(10) TYPE c,
        l_cond(100)  TYPE c,
        l_cond1(100) TYPE c.



  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any,
                 <txt_field> TYPE any,
                 <val_field> TYPE any.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA: val_field TYPE char40.
  DATA: txt_field TYPE char40.

  CHECK dd04vv[] IS NOT INITIAL.

  SELECT * INTO TABLE dd04vv FROM dd04vv
    FOR ALL ENTRIES IN dd04vv
    WHERE rollname = dd04vv-rollname ."AND  dtelmaster = spras.

  CHECK sy-subrc EQ 0.
  SELECT * INTO TABLE dd04t FROM dd04t
   FOR ALL ENTRIES IN dd04vv
   WHERE rollname = dd04vv-rollname .


  LOOP AT dd04vv.                                   "带域的数据元素
    IF dd04vv-valexi = 'X'.                         "存在固定值
      dd04vv_def = dd04vv.
      APPEND dd04vv_def.
      CLEAR dd04vv_def.
    ELSEIF dd04vv-entitytab IS NOT INITIAL.         "值表不为空
      dd04vv_tab = dd04vv.
      APPEND dd04vv_tab.
      CLEAR dd04vv_tab.
    ENDIF.
  ENDLOOP.

  IF dd04vv_def[] IS NOT INITIAL.                   "有固定域值的情况下
    SELECT * FROM dd07t INTO TABLE dd07t            "表： 用于域固定值的文本（语言相关），根据域名取数-对应文本
      FOR ALL ENTRIES IN dd04vv_def
       WHERE domname = dd04vv_def-domname
      AND ddlanguage = spras.
    IF sy-subrc EQ 0.
      LOOP AT dd07t.
        READ TABLE dd04vv_def WITH KEY domname = dd07t-domname.
        IF sy-subrc EQ 0.
          doma_value-rollname = dd04vv_def-rollname.      "数据元素
          doma_value-domval = dd07t-domvalue_l.         "上限单值
          doma_value-ddtext = dd07t-ddtext.             "固定文本
          APPEND doma_value.
          CLEAR doma_value.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF dd04vv_tab[] IS NOT INITIAL.
    SELECT * FROM dd08l INTO TABLE dd08l
      FOR ALL ENTRIES IN dd04vv_tab
      WHERE checktable = dd04vv_tab-entitytab           "值表
      AND frkart = 'TEXT'                               "外文建语义
      AND as4local = 'A'.                               "激活的
    IF dd08l[] IS NOT INITIAL.
      SELECT * FROM dd03l INTO TABLE it_dd03l
        FOR ALL ENTRIES IN dd08l
        WHERE tabname = dd08l-tabname.

    ENDIF.
    SELECT * FROM dd03l APPENDING TABLE it_dd03l
    FOR ALL ENTRIES IN dd04vv_tab
    WHERE tabname = dd04vv_tab-entitytab.

    DELETE  it_dd03l WHERE fieldname = 'MANDT' OR fieldname = 'SPRAS'.

    SORT it_dd03l BY tabname position.

    LOOP AT dd04vv_tab.
      READ TABLE dd08l WITH KEY checktable = dd04vv_tab-entitytab.
      IF sy-subrc NE 0.
        tabname = dd04vv_tab-entitytab.
      ELSE.
        tabname = dd08l-tabname.
      ENDIF.

      READ TABLE it_dd03l WITH KEY tabname = tabname keyflag = ' '.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        fieldname = it_dd03l-fieldname.
      ENDIF.

      READ TABLE it_dd03l WITH KEY tabname = tabname rollname = dd04vv_tab-rollname.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        valfname = it_dd03l-fieldname.
      ENDIF.



      IF <dyn_table> IS ASSIGNED.
        UNASSIGN <dyn_table>.
      ENDIF.
*创建动态表结构
      CREATE DATA dyn_table TYPE TABLE OF (tabname).
*创建动态内表
      ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
      CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
      ASSIGN dyn_wa->* TO <dyn_wa>.

      IF <dyn_table> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      DATA:ls_dd03l TYPE dd03l.

      READ TABLE dd08l WITH KEY tabname = tabname.
      IF sy-subrc EQ 0.
        SELECT SINGLE * INTO ls_dd03l
          FROM dd03l WHERE tabname = tabname AND rollname = 'SPRAS'.
        sprasname = ls_dd03l-fieldname.
        CONCATENATE  '''' spras '''' INTO  l_cond1.
        CONCATENATE sprasname '= ' l_cond1 INTO l_cond SEPARATED BY ' '.
        SELECT  * FROM (tabname) INTO TABLE  <dyn_table>
        WHERE (l_cond).
      ELSE.
        SELECT  * FROM (tabname) INTO TABLE  <dyn_table>.
      ENDIF.
      IF sy-subrc EQ 0.
        LOOP AT <dyn_table> INTO <dyn_wa>.
          CLEAR txt_field.
          IF <txt_field> IS ASSIGNED.
            UNASSIGN <txt_field>.
          ENDIF.

          CONCATENATE '<DYN_WA>-' fieldname INTO txt_field.
          CONCATENATE '<DYN_WA>-' valfname INTO val_field.
          ASSIGN (txt_field) TO <txt_field>.
          ASSIGN (val_field) TO <val_field>.
          IF <txt_field> IS ASSIGNED AND <val_field> IS ASSIGNED.
            doma_value-rollname = dd04vv_tab-rollname.

            doma_value-domval = <val_field>.
            doma_value-ddtext = <txt_field>.
            APPEND doma_value.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM doma_value COMPARING rollname domval.



ENDFUNCTION.
