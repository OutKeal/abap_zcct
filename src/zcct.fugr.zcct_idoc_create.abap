FUNCTION zcct_idoc_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IM_CHANNEL1) TYPE  ZCHANNEL1 OPTIONAL
*"     VALUE(IM_CHANNEL2) TYPE  ZCHANNEL2 OPTIONAL
*"     VALUE(IM_CHANNEL3) TYPE  ZCHANNEL3 OPTIONAL
*"     VALUE(IM_CCT_TYPE) TYPE  ZCCT_TYPE OPTIONAL
*"     VALUE(IM_POST_DATE) TYPE  BUDAT OPTIONAL
*"     VALUE(IM_BKTXT) TYPE  CHAR50 OPTIONAL
*"     VALUE(IM_LONG_VGBEL) TYPE  ZLONG_VGBEL OPTIONAL
*"     VALUE(IM_MESCOD) TYPE  EDIPMESCOD OPTIONAL
*"     VALUE(IM_VKORG) TYPE  VKORG OPTIONAL
*"     VALUE(IM_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(IM_REMARK01) TYPE  CHAR50 OPTIONAL
*"     VALUE(IM_REMARK02) TYPE  CHAR50 OPTIONAL
*"     VALUE(IM_REMARK03) TYPE  CHAR50 OPTIONAL
*"  EXPORTING
*"     VALUE(E_DOCNUM) TYPE  EDI_DOCNUM
*"  TABLES
*"      IM_DATA STRUCTURE  ZCCT_DATA
*"      ET_RETURN STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      IDOC_NOT_SAVED
*"----------------------------------------------------------------------


  DATA:lt_zccth     LIKE TABLE OF zccth,
       ls_zccth     LIKE zccth,
       lt_zccti     LIKE TABLE OF zccti,
       ls_zccti     LIKE zccti,
       lt_zccte     LIKE TABLE OF zccte,
       ls_zccte     LIKE zccte,
       lt_idoc_data TYPE STANDARD TABLE OF edi_dd40,
       ls_idoc_data TYPE edi_dd40.
  DATA:
    ls_edk21  TYPE edk21,
    ls_edp21  TYPE edp21,
    ls_master TYPE edi_dc40.


  ls_zccth-channel1 = im_channel1.
  ls_zccth-channel2 = im_channel2.
  ls_zccth-channel3 = im_channel3.
  ls_zccth-zcct_type = im_cct_type.
  ls_zccth-budat = im_post_date.
  ls_zccth-long_vgbel = im_long_vgbel.
  ls_zccth-vkorg = im_vkorg.
  ls_zccth-werks = im_werks.
  ls_zccth-remark01 = im_remark01.
  ls_zccth-remark02 = im_remark02.
  ls_zccth-remark03 = im_remark03.

  ls_idoc_data-segnam = 'ZCCTH'.
  MOVE  ls_zccth TO ls_idoc_data-sdata.
  APPEND ls_idoc_data TO lt_idoc_data.
  CLEAR  ls_idoc_data.



  "ZCCTH 结构赋值
  LOOP AT im_data.

    ls_zccti-line_id = im_data-line_id.
    ls_idoc_data-segnam = 'ZCCTI'.


    MOVE-CORRESPONDING im_data TO ls_zccti.

    CONDENSE ls_zccti-menge NO-GAPS.
    CONDENSE ls_zccti-netwr NO-GAPS.
    CONDENSE ls_zccti-amount1 NO-GAPS.
    CONDENSE ls_zccti-amount2 NO-GAPS.
    CONDENSE ls_zccti-amount3 NO-GAPS.
    CONDENSE ls_zccti-amount4 NO-GAPS.
    CONDENSE ls_zccti-amount5 NO-GAPS.
    CONDENSE ls_zccti-amount6 NO-GAPS.
    CONDENSE ls_zccti-amount7 NO-GAPS.
    CONDENSE ls_zccti-amount8 NO-GAPS.
    CONDENSE ls_zccti-amount9 NO-GAPS.

    ls_idoc_data-sdata = ls_zccti.



    APPEND ls_idoc_data TO lt_idoc_data.
    CLEAR  ls_idoc_data.
    CLEAR:im_data.

  ENDLOOP.

  SELECT SINGLE sndprn
    INTO ls_edk21-sndprn
    FROM edp21
    WHERE mestyp = 'ZCCT'
    AND sndprt = 'LS'.



  ls_master-mescod = im_mescod.     "接收方合作伙伴类型
  ls_master-rcvprt = 'LS'.     "接收方合作伙伴类型
  ls_master-rcvprn  = ls_edk21-sndprn.       "接收者端口
  ls_master-rcvpor = 'SAP' && sy-sysid.       "接收方的合作伙伴编号
  ls_master-sndpor = ls_edk21-sndprn.       "接收方的合作伙伴编号
  ls_master-sndprn = ls_edk21-sndprn.       "接收方的合作伙伴编号
  ls_master-sndprt = 'LS'.       "接收方的合作伙伴编号


  ls_master-direct = '2'.


  ls_master-idoctyp = 'ZCCT'.  "基本类型
  ls_master-mestyp = 'ZCCT'.  "消息类型
* 发送IDOC
  CALL FUNCTION 'IDOC_INBOUND_SINGLE'
    EXPORTING
      pi_idoc_control_rec_40  = ls_master
*     PI_DO_COMMIT            = 'X'
    IMPORTING
      pe_idoc_number          = e_docnum
*     PE_ERROR_PRIOR_TO_APPLICATION       =
    TABLES
      pt_idoc_data_records_40 = lt_idoc_data
    EXCEPTIONS
      idoc_not_saved          = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    RAISE idoc_not_saved.
  ENDIF.







ENDFUNCTION.
