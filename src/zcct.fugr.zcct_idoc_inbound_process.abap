FUNCTION zcct_idoc_inbound_process.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_DOCNUM) TYPE  EDI_DOCNUM OPTIONAL
*"  EXPORTING
*"     VALUE(E_STATUS) TYPE  EDI_STATUS
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  CLEAR ot_return[]  .

  RANGES: r_docnum FOR edidc-docnum.
  CLEAR r_docnum.

  SELECT SINGLE * INTO @DATA(ls_edidc)
    FROM edidc WHERE docnum = @i_docnum
    AND mestyp = 'ZCCT'.
  IF sy-subrc NE 0 .
    PERFORM add_message USING 'E' 'ZCCT' '000' 'IDOC编号' i_docnum '不存在' ''.
  ELSE.

    r_docnum-sign = 'I'.
    r_docnum-option = 'EQ'.
    r_docnum-low = i_docnum.
    APPEND r_docnum.
    CLEAR r_docnum.

    CASE ls_edidc-status.
      WHEN '64'.
        SUBMIT rbdapp01 WITH docnum IN r_docnum
                        WITH p_output = ''
                        AND RETURN
                        .

        IMPORT ot_return FROM MEMORY ID  'ZCCT_RETURN'.

        SELECT SINGLE status INTO @DATA(lv_status)
          FROM edidc WHERE docnum = @i_docnum.
        IF lv_status = '51'.
          PERFORM add_message USING 'E' 'ZCCT' '000' 'IDOC编号' i_docnum '过账失败' ''.
        ELSEIF lv_status = '53'.
          PERFORM add_message USING 'S' 'ZCCT' '000' 'IDOC编号' i_docnum '处理成功' ''.
        ELSE.
          PERFORM add_message USING 'E' 'ZCCT' '000' 'IDOC编号' i_docnum '状态异常' ''.
        ENDIF.


      WHEN '51'.
        SUBMIT rbdmani2 WITH so_docnu IN r_docnum
                        WITH p_output = ''
                        WITH p_wodial = 'X'
                        AND RETURN.

        IMPORT ot_return FROM MEMORY ID  'ZCCT_RETURN'.

        SELECT SINGLE status INTO @lv_status
             FROM edidc WHERE docnum = @i_docnum.
        IF lv_status = '51'.
          PERFORM add_message USING 'E' 'ZCCT' '000' 'IDOC编号' i_docnum '过账失败' ''.
        ELSEIF lv_status = '53'.
          PERFORM add_message USING 'S' 'ZCCT' '000' 'IDOC编号' i_docnum '处理成功' ''.
        ENDIF.
      WHEN '53'.
        PERFORM add_message USING 'S' 'ZCCT' '000' 'IDOC编号' i_docnum '已处理成功' ''.
      WHEN '68'.
        PERFORM add_message USING 'W' 'ZCCT' '000' 'IDOC编号' i_docnum '已作废，无法处理' ''.
      WHEN OTHERS.
        PERFORM add_message USING 'E' 'ZCCT' '000' 'IDOC编号' i_docnum '状态异常，无法处理' ''.
    ENDCASE.
  ENDIF.
  CLEAR gv_error.
  e_status = lv_status.

  et_return[] = ot_return[].






ENDFUNCTION.
