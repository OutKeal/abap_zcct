class ZCL_IM_IDOC_DATA_MAPPER definition
  public
  final
  create public .

public section.

  interfaces IF_EX_IDOC_DATA_MAPPER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IDOC_DATA_MAPPER IMPLEMENTATION.


  METHOD if_ex_idoc_data_mapper~process.

    DATA: ls_db_h TYPE zcct_db_h.
    DATA: lt_db_i TYPE TABLE OF zcct_db_i.
    DATA: ls_db_i TYPE zcct_db_i.
    DATA: lv_channel TYPE bu_partner.
    DATA: ls_zccth TYPE  zccth.
    DATA: ls_zccti TYPE  zccti.
    DATA: ls_zccte TYPE  zccte.


    CHECK control-idoctp = 'ZCCT'.


    LOOP AT data INTO DATA(ls_data).
      CASE ls_data-segnam.
        WHEN 'ZCCTH'.
          CLEAR ls_zccth.
          MOVE ls_data-sdata TO ls_zccth.
          MOVE-CORRESPONDING ls_zccth TO ls_db_h.

          lv_channel = ls_db_h-channel1.
          IF ls_zccth-zcct_type = '2007'.
            lv_channel = ls_db_h-channel2.
          ENDIF.

          SELECT SINGLE werks bukrs
            FROM zscmt0010
            INTO ( ls_db_h-werks , ls_db_h-bukrs )
            WHERE partner = lv_channel.

          ls_db_h-docnum = control-docnum.

        WHEN 'ZCCTI'.
          CLEAR ls_zccti.
          CLEAR ls_db_i.
          MOVE ls_data-sdata TO ls_zccti.

          MOVE-CORRESPONDING ls_zccti TO ls_db_i.
          ls_db_i-docnum = control-docnum.
          APPEND ls_db_i TO lt_db_i.
          CLEAR ls_db_i.

        WHEN 'ZCCTE'.

      ENDCASE.
    ENDLOOP.


    IF ls_db_h IS NOT INITIAL.
      MODIFY zcct_db_h FROM ls_db_h.
    ENDIF.


    IF lt_db_i[] IS NOT INITIAL.
      MODIFY zcct_db_i FROM TABLE lt_db_i.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
