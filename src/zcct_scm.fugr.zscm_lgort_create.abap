FUNCTION zscm_lgort_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(PERFIX) TYPE  CHAR1 DEFAULT 'S'
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_LGOBE) TYPE  LGOBE
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  CHANGING
*"     VALUE(I_LGORT) TYPE  LGORT_D
*"----------------------------------------------------------------------
  IF i_werks IS INITIAL.
    SELECT SINGLE * FROM t001W
      INTO @DATA(ls_t001W) WHERE werks = @i_werks.
    IF sy-subrc NE 0.
      PERFORM frm_add_message_single TABLES return
                                     USING 'E' 'ZMDA' '000' '工厂不存在' '' '' ''.
      RETURN.
    ENDIF.
  ENDIF.

  IF i_lgobe IS INITIAL.
    PERFORM frm_add_message_single TABLES return
                             USING 'E' 'ZMDA' '000' '库位描述不能为空' '' '' ''.
  ENDIF.

  IF i_lgort IS NOT INITIAL.
    SELECT SINGLE * FROM t001l
      INTO @DATA(ls_t001l) WHERE werks = @i_werks AND lgort = @i_lgort.
    IF sy-subrc EQ 0.
      PERFORM frm_add_message_single TABLES return
                                   USING 'E' 'ZMDA' '000' '传入库位已存在' '' '' ''.
      RETURN.
    ENDIF.
    PERFORM frm_insert_lgort TABLES return[]
                             USING i_werks
                                   i_lgort
                                   i_lgobe.
    READ TABLE return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
  ELSE.
    PERFORM frm_get_next_lgort TABLES return[]
                               USING i_werks
                                     perfix
                            CHANGING i_lgort.
    READ TABLE return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    PERFORM frm_insert_lgort TABLES return[]
                             USING i_werks
                                   i_lgort
                                   i_lgobe.
    READ TABLE return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.


  ENDIF.







ENDFUNCTION.

FORM frm_get_next_lgort TABLES return STRUCTURE bapireT2
                         USING i_werks
                               perfix
                         CHANGING e_lgort.

  DATA:lt_t0005 TYPE TABLE OF zscms0005 WITH HEADER LINE.
  DATA:l1 TYPE char1.
  DATA:l2 TYPE char1.
  DATA:l3 TYPE char1.
  DATA:l4 TYPE char1.
  DATA:lv_index TYPE sy-tabix.

  DATA:t_lgort TYPE lgort_d.
  t_lgort = perfix && '%'.
  SELECT MAX( lgort ) INTO @DATA(ls_lgort) FROM t001l
                        WHERE werks = @i_werks
                        AND lgort LIKE @t_lgort.
  IF ls_lgort IS INITIAL.
    e_lgort = perfix && '000'.
    RETURN.
  ENDIF.

  PERFORM init_list TABLES lt_t0005 .

  l1 = perfix.
  l2 = ls_lgort+1(1).
  l3 = ls_lgort+2(1).
  l4 = ls_lgort+3(1).

  IF l2 IS INITIAL OR l3 IS INITIAL OR l4 IS INITIAL OR l1 IS INITIAL.
    PERFORM frm_add_message_single TABLES return
                         USING 'E' 'ZMDA' '000' '历史最大库位异常' '' '' ''.
    RETURN.
  ENDIF.

  READ TABLE lt_t0005 WITH KEY value = l4.
  IF sy-subrc NE 0.
    PERFORM frm_add_message_single TABLES return
                     USING 'E' 'ZMDA' '000' '历史最大库位异常' '' '' ''.
    RETURN.
  ELSE.
    lv_index = sy-tabix + 1.
  ENDIF.

  IF l4 <> 'Z'.
    READ TABLE lt_t0005 INDEX lv_index.
    l4 = lt_t0005-value.
    e_lgort = l1 && l2 && l3 && l4 .
    RETURN.
  ELSE.
    l4 = 0.
    READ TABLE lt_t0005 WITH KEY value = l3.
    IF sy-subrc NE 0.
      PERFORM frm_add_message_single TABLES return
                       USING 'E' 'ZMDA' '000' '历史最大库位异常' '' '' ''.
      RETURN.
    ELSE.
      lv_index = sy-tabix + 1.
    ENDIF.
    IF l3 <> 'Z'.
      READ TABLE lt_t0005 INDEX lv_index.
      l3 = lt_t0005-value.
      e_lgort = l1 && l2 && l3 && l4 .
      RETURN.
    ELSE.
      l3 = 0.
      READ TABLE lt_t0005 WITH KEY value = l2.
      IF sy-subrc NE 0.
        PERFORM frm_add_message_single TABLES return
                         USING 'E' 'ZMDA' '000' '历史最大库位异常' '' '' ''.
        RETURN.
      ELSE.
        lv_index = sy-tabix + 1.
      ENDIF.
      IF l2 <> 'Z'.
        READ TABLE lt_t0005 INDEX lv_index.
        l2 = lt_t0005-value.
        e_lgort = l1 && l2 && l3 && l4 .
        RETURN.
      ELSE.
        PERFORM frm_add_message_single TABLES return
                    USING 'E' 'ZMDA' '000' '库位编码已满' '' '' ''.
        RETURN.
      ENDIF.

    ENDIF.




  ENDIF.







ENDFORM.

FORM init_list TABLES lt_t0005 STRUCTURE zscms0005.
  CLEAR lt_t0005.
  lt_t0005-value = '0'. APPEND lt_t0005.
  lt_t0005-value = '1'. APPEND lt_t0005.
  lt_t0005-value = '2'. APPEND lt_t0005.
  lt_t0005-value = '3'. APPEND lt_t0005.
  lt_t0005-value = '4'. APPEND lt_t0005.
  lt_t0005-value = '5'. APPEND lt_t0005.
  lt_t0005-value = '6'. APPEND lt_t0005.
  lt_t0005-value = '7'. APPEND lt_t0005.
  lt_t0005-value = '8'. APPEND lt_t0005.
  lt_t0005-value = '9'. APPEND lt_t0005.
  lt_t0005-value = 'A'. APPEND lt_t0005.
  lt_t0005-value = 'B'. APPEND lt_t0005.
  lt_t0005-value = 'C'. APPEND lt_t0005.
  lt_t0005-value = 'D'. APPEND lt_t0005.
  lt_t0005-value = 'E'. APPEND lt_t0005.
  lt_t0005-value = 'F'. APPEND lt_t0005.
  lt_t0005-value = 'G'. APPEND lt_t0005.
  lt_t0005-value = 'H'. APPEND lt_t0005.
  lt_t0005-value = 'I'. APPEND lt_t0005.
  lt_t0005-value = 'J'. APPEND lt_t0005.
  lt_t0005-value = 'K'. APPEND lt_t0005.
  lt_t0005-value = 'L'. APPEND lt_t0005.
  lt_t0005-value = 'M'. APPEND lt_t0005.
  lt_t0005-value = 'N'. APPEND lt_t0005.
  lt_t0005-value = 'O'. APPEND lt_t0005.
  lt_t0005-value = 'P'. APPEND lt_t0005.
  lt_t0005-value = 'Q'. APPEND lt_t0005.
  lt_t0005-value = 'R'. APPEND lt_t0005.
  lt_t0005-value = 'S'. APPEND lt_t0005.
  lt_t0005-value = 'T'. APPEND lt_t0005.
  lt_t0005-value = 'U'. APPEND lt_t0005.
  lt_t0005-value = 'V'. APPEND lt_t0005.
  lt_t0005-value = 'W'. APPEND lt_t0005.
  lt_t0005-value = 'X'. APPEND lt_t0005.
  lt_t0005-value = 'Y'. APPEND lt_t0005.
  lt_t0005-value = 'Z'. APPEND lt_t0005.
ENDFORM.


FORM frm_insert_lgort TABLES return STRUCTURE bapiret2
                      USING i_werks
                            i_lgort
                            i_lgobe.
  DATA:ls_t001l TYPE t001l.
  ls_t001l-werks = i_werks.
  ls_t001l-lgort = i_lgort.
  ls_t001l-lgobe = i_lgobe.
*  ls_t001l-xlong = 'X'.
*  ls_t001l-xblgo = 'X'.
  INSERT t001l FROM ls_t001l.
  IF sy-subrc EQ 0.
    PERFORM frm_add_message_single TABLES return
                             USING 'S' 'ZMDA' '000' '库位创建成功' '' '' ''.
  ENDIF.
ENDFORM.

FORM frm_add_message_single TABLES ot_return STRUCTURE bapiret2
                             USING type id number  msg1 msg2 msg3 msg4.
  DATA: msg TYPE bapi_msg.
  ot_return-type = type.
  ot_return-id = id.
  ot_return-number = number.

  ot_return-message_v1 = msg1.
  ot_return-message_v2 = msg2.
  ot_return-message_v3 = msg3.
  ot_return-message_v4 = msg4.

  MESSAGE ID ot_return-id
          TYPE ot_return-type
          NUMBER ot_return-number
          INTO ot_return-message
          WITH ot_return-message_v1
               ot_return-message_v2
               ot_return-message_v3
               ot_return-message_v4.
  APPEND ot_return.
  CLEAR ot_return.
ENDFORM.
