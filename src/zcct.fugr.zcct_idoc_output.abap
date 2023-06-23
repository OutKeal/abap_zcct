FUNCTION zcct_idoc_output.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IZCCT_TYPE) TYPE  ZCCT_TYPE OPTIONAL
*"  TABLES
*"      T_MSEG STRUCTURE  MSEG OPTIONAL
*"      T_MKPF STRUCTURE  MKPF OPTIONAL
*"----------------------------------------------------------------------
   DATA:
    ls_edk21  TYPE edk21,
    ls_edp21  TYPE edp21,
    ls_master TYPE edi_dc40,
    lv_docnum TYPE edi_docnum.
  DATA:lt_idoc_data TYPE STANDARD TABLE OF edi_dd40,
       ls_idoc_data     LIKE LINE OF lt_idoc_data.


  DATA:lt_zccth         LIKE TABLE OF zccth,
       ls_zccth         LIKE zccth,
       lt_zccti         LIKE TABLE OF zccti,
       ls_zccti         LIKE zccti,
       lt_zccte         LIKE TABLE OF zccte,
       ls_zccte         LIKE zccte,
*       lt_idoc_data     TYPE STANDARD TABLE OF edidd,
*       ls_idoc_data     LIKE LINE OF lt_idoc_data,
       lt_communication TYPE STANDARD TABLE OF edidc,
       ls_communication LIKE LINE OF lt_communication.

  DATA: ls_edk13  TYPE edk13,
        ls_edp13  TYPE edp13,
        ls_master_c TYPE edidc.

  DATA:BEGIN OF imara,
         matnr LIKE mara-matnr,
         satnr LIKE mara-satnr,
         color LIKE mara-color,
         size1 LIKE mara-size1,
         size2 LIKE mara-size2,
       END OF imara.

  DATA: t_mara LIKE TABLE OF mara WITH HEADER LINE.


  "获取款号颜色尺码
  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_mara
  FROM mara
  FOR ALL ENTRIES IN t_mseg[]
   WHERE mara~matnr = t_mseg-matnr.

  SORT t_mara BY  matnr.


  "ZCCTH 结构赋值
  LOOP AT t_mkpf.

    mb_doc = t_mkpf-mblnr.
    mb_year = t_mkpf-mjahr.
    READ TABLE t_mseg WITH  KEY mblnr = t_mkpf-mblnr  Mjahr = t_mkpf-mjahr.
    IF sy-subrc EQ 0.
      SELECT SINGLE partner INTO ls_zccth-channel1
          FROM zscmt0010
          WHERE werks = t_mseg-werks
            AND lgort = t_mseg-lgort.
    ENDIF.
    ls_zccth-zcct_type  = izcct_type.       "IDOC类型
    ls_zccth-long_vgbel = t_mkpf-mblnr.       "物料凭证
    ls_zccth-budat      = t_mkpf-budat.       "过账日期
    ls_zccth-remark01 = t_mkpf-bktxt.       "备注
    ls_idoc_data-segnam = 'ZCCTH'.
    MOVE  ls_zccth TO ls_idoc_data-sdata.
    APPEND ls_idoc_data TO lt_idoc_data.
    CLEAR  ls_idoc_data.


    "ZCCTI 结构赋值
    LOOP AT t_mseg WHERE mblnr = t_mkpf-mblnr AND Mjahr = t_mkpf-mjahr.

      ls_zccti-line_id = t_mseg-line_id.
      ls_idoc_data-segnam = 'ZCCTI'.

      READ TABLE t_mara WITH  KEY matnr = t_mseg-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.

        ls_zccti-satnr = t_mara-satnr.    "款号
        ls_zccti-color = t_mara-color.    "颜色
        ls_zccti-size1 = t_mara-size1.    "尺码1
        ls_zccti-size2 = t_mara-size2.    "尺码2


      ENDIF.

      ls_zccti-menge = t_mseg-menge.    "数量
      SHIFT ls_zccti-menge LEFT DELETING LEADING SPACE.
      ls_zccti-meins = t_mseg-meins.    "单位


      MOVE  ls_zccti TO ls_idoc_data-sdata.
      APPEND ls_idoc_data TO lt_idoc_data.
      CLEAR  ls_idoc_data.
      CLEAR:t_mseg.
    ENDLOOP.

  ENDLOOP.

*  SELECT SINGLE rcvprn
*  INTO ls_edk13-rcvprn
*  FROM edp13
* WHERE mestyp = 'ZCCT'.     "消息类型
*  ls_edk13-rcvprt = 'LS'.      "接收方合作伙伴类型
*  ls_edk13-mestyp = 'ZCCT'.   "消息类型
*  ls_edk13-mandt  = sy-mandt.
*
*  CALL FUNCTION 'EDI_PARTNER_APPL_READ_OUT'
*    EXPORTING
*      rec_edk13           = ls_edk13
*    IMPORTING
*      rec_edp13           = ls_edp13
*    EXCEPTIONS
*      partner_is_inactive = 1
*      partner_is_template = 2
*      partner_not_found   = 3
*      OTHERS              = 4.
*  IF sy-subrc <> 0.
*    " MESSAGE E201(ZMM01).
*  ENDIF.
*
*  ls_master-rcvprt = 'LS'.     "接收方合作伙伴类型
*  ls_master-rcvpor = ls_edp13-rcvpor.       "接收者端口
*  ls_master-rcvprn = ls_edp13-rcvprn.       "接收方的合作伙伴编号
*  ls_master-idoctp = 'ZCCT'.  "基本类型
*  ls_master-mestyp = 'ZCCT'.  "消息类型
  " LT_IDOC_DATA[] = U_IDOC_DATA[].
*---改为入站
      CLEAR:ls_edk21,ls_edp21,ls_master.

      SELECT SINGLE sndprn
      INTO ls_edk21-sndprn
      FROM edp21
      WHERE mestyp = 'ZCCT'
        AND sndprt = 'LS'
        AND mescod = 'CKO'.

      ls_master-mescod = 'CKO'.     "消息变式
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
      CLEAR:lv_docnum.
      CALL FUNCTION 'IDOC_INBOUND_SINGLE'
      EXPORTING
        pi_idoc_control_rec_40  = ls_master
*       PI_DO_COMMIT            = 'X'
      IMPORTING
        pe_idoc_number          = lv_docnum
*       PE_ERROR_PRIOR_TO_APPLICATION       =
      TABLES
        pt_idoc_data_records_40 = lt_idoc_data
      EXCEPTIONS
        idoc_not_saved          = 1
        OTHERS                  = 2.
*    IF sy-subrc <> 0.
*      RAISE idoc_not_saved.
*    ENDIF.

*  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE' "IN UPDATE TASK
*    EXPORTING
*      master_idoc_control            = ls_master
*    TABLES
*      communication_idoc_control     = lt_communication
*      master_idoc_data               = lt_idoc_data
*    EXCEPTIONS
*      error_in_idoc_control          = 1
*      error_writing_idoc_status      = 2
*      error_in_idoc_data             = 3
*      sending_logical_system_unknown = 4.
  IF sy-subrc = 0.

*    READ TABLE lt_communication INTO ls_communication  INDEX 1.  "获取IDOC 号
*    IF sy-subrc EQ 0.

      MOVE-CORRESPONDING ls_master TO ls_master_c.
*      IF ls_communication-docnum  IS NOT INITIAL.  "传入信息
      IF lv_docnum IS NOT INITIAL.
        CLEAR gt_borident.
        gv_docnum = lv_docnum. "ls_communication-docnum.
        gt_borident-objkey = lv_docnum. "ls_communication-docnum."写入关系浏览器对象
        ls_master_c-docnum = lv_docnum. "ls_communication-docnum.
        gt_borident-objtype = 'IDOC'.
        APPEND gt_borident.

      ENDIF.

      CLEAR gt_borident.
      gt_borident-objkey = mb_doc && mb_year."写入关系浏览器对象
      gt_borident-objtype = 'BUS2017'.
      APPEND gt_borident.


*    ENDIF.


    PERFORM binary_relation_output USING ls_master_c."标准关系系浏览器，关联所有对象到第一个单据对象


  ELSE.
    RAISE idoc_not_saved.
  ENDIF.



ENDFUNCTION.



FORM binary_relation_output USING ls_edidc TYPE edidc.

  CHECK gt_borident[] IS NOT INITIAL.

  DATA:   lt_foldoc        TYPE TABLE OF wpusa_foldoc WITH HEADER LINE.
  DATA:   ls_foldoc_attr   TYPE wpusa_foldoc_attr.


  READ TABLE gt_borident INTO DATA(ls_borident) INDEX 1.
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


    IF sy-tabix = 1.
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
