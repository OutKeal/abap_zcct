FUNCTION zscm_bp_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(EV_STATUS) TYPE  CHAR1
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  CHANGING
*"     VALUE(CS_DATA) TYPE  ZSCMT0001
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
  DATA:iv_company_shoulb_be_created,
       iv_wtax_is_needed,
       iv_bankacc_is_needed,
       iv_purchorg_is_needed.
  DATA:ev_partner	TYPE bu_partner.
*----------------------------------------------------------------------*

  DATA:
        lt_data           TYPE cvis_ei_extern_t
      , ls_data           TYPE cvis_ei_extern
      , ls_address_help   TYPE bus_ei_bupa_address
      , ls_address_usages TYPE bus_ei_bupa_addressusage
      , ls_vend_company   TYPE vmds_ei_company
      , ls_with_tax       TYPE vmds_ei_wtax_type
      , ls_role           TYPE bus_ei_bupa_roles
      , lt_return2        TYPE TABLE OF bapiret2
      .

  DATA:
    ls_return        TYPE cvis_bp_return,
    lv_task          TYPE c,  " I新建  U 更新
    lv_task_v        TYPE c, " 供应商逻辑处理
    lv_task_c        TYPE c, " 客户逻辑处理
    ls_but000        TYPE but000,
    ls_kna1          TYPE kna1,
    ls_lfa1          TYPE lfa1,
    ls_alv           TYPE zscmt0001,
    lt_bapiret2      TYPE TABLE OF bapiret2,
    ls_bapiret2      TYPE bapiret2,
    lv_bp            TYPE bu_partner,
    lv_create_applog TYPE boolean.

  DATA: lv_guid    TYPE bu_partner_guid.

  "锁表
*  PERFORM f_enqueue_object USING cs_data.
  CLEAR:ev_status.

*---设置默认值
  PERFORM f_get_def_value CHANGING cs_data.

  SELECT SINGLE * INTO ls_but000
    FROM but000
   WHERE partner = cs_data-partner.
  IF sy-subrc = 0.
    lv_task = 'U'.
  ELSE.
    lv_task = 'I'.
  ENDIF.

*---特殊检查
  PERFORM data_check TABLES et_return
                      USING cs_data
                            lv_task
                   CHANGING ev_status.

*  IF CS_DATA-FLVN00 = 'X' OR CS_DATA-FLVN01 = 'X'.
*    LV_TASK_V = 'X'. "维护供应商
*
*    SELECT SINGLE *
*    INTO LS_LFA1
*    FROM LFA1
*    WHERE LIFNR = CS_DATA-PARTNER.
*    IF SY-SUBRC <> 0.
*      LV_TASK = 'I'.
*    ENDIF.
*
*    SELECT * FROM BUT100 INTO TABLE LT_BUT100
*     WHERE PARTNER =
*  ENDIF.
*
*  IF CS_DATA-FLCU00 = 'X' OR CS_DATA-FLCU01 = 'X'.
*    LV_TASK_C = 'X'."维护客户
*    SELECT SINGLE *
*    INTO LS_KNA1
*    FROM KNA1
*    WHERE KUNNR = CS_DATA-PARTNER.
*    IF SY-SUBRC <> 0.
*      LV_TASK = 'I'.
*    ENDIF.
*  ENDIF.

  lv_bp = cs_data-partner.


*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_bp
*    IMPORTING
*      output = lv_bp.

  CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.



  CASE lv_task.
    WHEN 'I'.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = ls_data-partner-header-object_instance-bpartnerguid.

    WHEN 'U'.
      SELECT SINGLE partner_guid
      FROM but000
      INTO lv_guid
      WHERE partner = lv_bp.

      ls_data-partner-header-object_instance-bpartnerguid = lv_guid.
      ls_data-partner-header-object_instance-bpartner = lv_bp.
    WHEN OTHERS.
  ENDCASE.

  ls_data-partner-header-object_task = lv_task.

  "BP 角色
  PERFORM f_add_role     USING lv_bp
                               cs_data
                      CHANGING ls_data.

  "基本视图
  PERFORM f_add_bp_general USING    lv_bp
                                    lv_task
                                    cs_data
                           CHANGING ls_data.

  "一般地址信息/电话 /传真/邮箱
  PERFORM f_add_bp_address USING    lv_bp
                                    lv_task
                                    cs_data
                           CHANGING ls_data.

  "银行信息
*  PERFORM F_BANK_ACC USING   LV_BP
*                             CS_DATA
*                   CHANGING LS_DATA.

  "供应商信息
  PERFORM f_add_sup_general USING   lv_bp
                                    lv_task
                                    cs_data
                           CHANGING ls_data.

  "客户信息
  PERFORM f_add_cust_general USING lv_bp
                                   lv_task
                                   cs_data
                         CHANGING  ls_data.

  "信贷限额
  PERFORM f_add_ukm_general USING lv_bp
                                   lv_task
                                   cs_data
                         CHANGING  ls_data.

  IF lv_task = 'I'.
    IF lv_task_v = 'X'.
      SELECT COUNT(*) FROM lfa1 WHERE lifnr = lv_bp.
      IF sy-subrc <> 0.
        ls_data-ensure_create-create_vendor   = abap_true.
      ENDIF.
    ENDIF.
    IF lv_task_c = 'X'.
      SELECT COUNT(*) FROM kna1 WHERE kunnr = lv_bp.
      IF sy-subrc <> 0.
        ls_data-ensure_create-create_customer = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  APPEND ls_data TO lt_data.

  cl_md_bp_maintain=>validate_single(
    EXPORTING i_data = ls_data

    IMPORTING et_return_map = DATA(lt_return_map) ).

  LOOP AT lt_return_map ASSIGNING FIELD-SYMBOL(<ls_return_map>)
    WHERE type = 'E' OR  type = 'A'.

    MOVE-CORRESPONDING <ls_return_map> TO et_return.

    APPEND et_return.

    ev_status = 'E'.

  ENDLOOP.

  IF ev_status = 'E'.
    RETURN.
  ELSE.
    cl_md_bp_maintain=>maintain(
      EXPORTING i_data = lt_data
      IMPORTING e_return = DATA(lt_return) ).
  ENDIF.

  LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
    LOOP AT <ls_return>-object_msg ASSIGNING FIELD-SYMBOL(<ls_object_msg>)
      WHERE type = 'E' OR  type = 'A'.

      ev_status = 'E'.

      MOVE-CORRESPONDING <ls_object_msg> TO et_return.
      APPEND et_return.
    ENDLOOP.
  ENDLOOP.

  IF ev_status = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    RETURN.
  ENDIF.


*---创建库位
  PERFORM create_lgort TABLES et_return
                        USING lv_task
                     CHANGING cs_data.
  LOOP AT et_return WHERE type = 'E'
                        OR type = 'A'.
    ev_status = 'E'.
  ENDLOOP.

  IF ev_status = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    RETURN.
  ENDIF.
*---创建库位

*--创建成本中心
  PERFORM create_kostl TABLES et_return
                     CHANGING cs_data.
  LOOP AT et_return WHERE type = 'E'
                        OR type = 'A'.
    ev_status = 'E'.
  ENDLOOP.

  IF ev_status = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ELSE.
    IF cs_data-fkostl = 'X'.
      cs_data-kostl = cs_data-partner.
    ENDIF.
  ENDIF.
*--创建成本中心

  IF ev_status IS INITIAL.
    DATA lv_msg TYPE char50.
    CLEAR et_return[].

    lv_msg    = '合作伙伴' && cs_data-partner && '创建成功'.
    PERFORM set_msg TABLES et_return
                    USING 'S'
                          lv_msg.


    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    lv_guid = ls_data-partner-header-object_instance-bpartnerguid.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner_guid = lv_guid
      IMPORTING
        ev_partner      = ev_partner
      TABLES
        et_return       = lt_return2.

    IF ev_partner IS NOT INITIAL.
      cs_data-partner  =  ev_partner.
      IF lv_task_v = 'X'.
        cs_data-lifnr  =  ev_partner.
      ENDIF.
      IF lv_task_c = 'X'.
        cs_data-kunnr  =  ev_partner.
      ENDIF.
    ENDIF.


    PERFORM modify_vbund USING cs_data.

*----写自定义表
    PERFORM f_update_data_table USING cs_data.

    ev_status = 'S'.

*    PERFORM send_idoc USING cs_data.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

*  PERFORM F_DEQUEUE_OBJECT USING C_ALV.

ENDFUNCTION.

FORM f_enqueue_object USING u_alv TYPE zscmt0001 .
  DATA: lv_msg TYPE string.

  " 锁维护码
*  CALL FUNCTION 'ENQUEUE_EZ_ZSCMT0001'
*    EXPORTING
*      mode_zscmt0001  = 'E'
*      mandt          = sy-mandt
*      zwhm           = u_alv-zwhm
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
    MESSAGE lv_msg TYPE 'E'.
  ENDIF.



ENDFORM.

FORM f_add_role  USING
                        u_bp  TYPE char10
                        u_alv TYPE zscmt0001
               CHANGING
                        c_data TYPE cvis_ei_extern.


  DATA: lt_but100 TYPE TABLE OF but100,
        ls_but100 TYPE but100,
        lt_roles  TYPE bus_ei_bupa_roles_t,
        ls_roles  TYPE bus_ei_bupa_roles.

  SELECT *
   INTO TABLE lt_but100
   FROM but100
   WHERE partner = u_bp.

  "财务供应商角色
  READ TABLE lt_but100 INTO ls_but100 WITH KEY partner = u_bp
                                               rltyp   = 'FLVN00'.
  IF sy-subrc <> 0.
    IF u_alv-flvn00 = 'X'.
      CLEAR ls_roles.
      ls_roles-task             = 'I'.
      ls_roles-data_key         = 'FLVN00'.
      ls_roles-data-valid_from  = sy-datum      .
      ls_roles-data-valid_to    = '99991231'    .
      APPEND ls_roles TO lt_roles.
    ENDIF.
  ENDIF.

  "财务客户角色
  READ TABLE lt_but100 INTO ls_but100 WITH KEY partner = u_bp
                                               rltyp   = 'FLCU00'.
  IF sy-subrc <> 0.
    IF u_alv-flcu00 = 'X'.
      CLEAR ls_roles.
      ls_roles-task             = 'I'.
      ls_roles-data_key         = 'FLCU00'.
      ls_roles-data-valid_from  = sy-datum      .
      ls_roles-data-valid_to    = '99991231'    .
      APPEND ls_roles TO lt_roles.
    ENDIF.
  ENDIF.


  "客户角色
  READ TABLE lt_but100 INTO ls_but100 WITH KEY partner = u_bp
                                               rltyp   = 'FLCU01'.
  IF sy-subrc <> 0.
    IF u_alv-flcu01 = 'X' .
      CLEAR ls_roles.
      ls_roles-task             = 'I'.
      ls_roles-data_key         = 'FLCU01'.
      ls_roles-data-valid_from  = sy-datum      .
      ls_roles-data-valid_to    = '99991231'    .
      APPEND ls_roles TO lt_roles.
    ENDIF.
  ENDIF.

  "采购角色

  READ TABLE lt_but100 INTO ls_but100 WITH KEY partner = u_bp
                                           rltyp   = 'FLVN01'.
  IF sy-subrc <> 0 .
    IF u_alv-flvn01 = 'X' .
      CLEAR ls_roles.
      ls_roles-task             = 'I'.
      ls_roles-data_key         = 'FLVN01'.
      ls_roles-data-valid_from  = sy-datum      .
      ls_roles-data-valid_to    = '99991231'    .
      APPEND ls_roles TO lt_roles.
    ENDIF.
  ENDIF.

  "信用角色

  READ TABLE lt_but100 INTO ls_but100 WITH KEY partner = u_bp
                                           rltyp   = 'UKM000'.
  IF sy-subrc <> 0 .
    IF u_alv-fukm000 = 'X' .
      CLEAR ls_roles.
      ls_roles-task             = 'I'.
      ls_roles-data_key         = 'UKM000'.
      ls_roles-data-valid_from  = sy-datum      .
      ls_roles-data-valid_to    = '99991231'    .
      APPEND ls_roles TO lt_roles.
    ENDIF.
  ENDIF.

  IF lt_roles IS NOT INITIAL.
    c_data-partner-central_data-role-roles = lt_roles.
  ENDIF.

ENDFORM.

FORM f_add_bp_general  USING
                                u_bp  TYPE char10
                                u_task TYPE char1
                                u_alv TYPE zscmt0001
                       CHANGING c_data TYPE cvis_ei_extern.


  c_data-partner-header-object_instance-bpartner = u_bp.
  c_data-partner-header-object_task = u_task.

  c_data-partner-central_data-common-data-bp_control-category = '2'."类别
  c_data-partner-central_data-common-data-bp_control-grouping = u_alv-bu_group."分组 控制视图 号码段
  c_data-partner-central_data-common-data-bp_centraldata-searchterm1 = u_alv-bu_sort1_txt ."搜索项1
*  c_data-partner-central_data-common-data-bp_centraldata-searchterm2 = u_alv-sortl2 ."名称2 ."搜索项2
  c_data-partner-central_data-common-data-bp_centraldata-partnerlanguage = u_alv-langu ."语言
*  c_data-partner-central_data-common-data-bp_centraldata-centralblock = u_alv-zdelete."集中冻结
  c_data-partner-central_data-common-data-bp_organization-name1 = u_alv-name1."'  ."名称1
  c_data-partner-central_data-common-data-bp_organization-name2 = u_alv-name2 ."名称2
* c_data-partner-central_data-common-data-bp_organization-name3 = u_alv-name3 ."名称3

  IF u_task = 'U' .
    c_data-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true ."搜索项1
*    c_data-partner-central_data-common-datax-bp_centraldata-searchterm2 = abap_true ."名称2 ."搜索项2
    c_data-partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true .
    c_data-partner-central_data-common-datax-bp_centraldata-centralblock = abap_true.
    c_data-partner-central_data-common-datax-bp_organization-name1 = abap_true.
    c_data-partner-central_data-common-datax-bp_organization-name2 = abap_true ."名称2
*    c_data-partner-central_data-common-datax-bp_organization-name3 = abap_true ."名称3
  ENDIF.

ENDFORM.

FORM f_add_bp_address  USING    u_bp TYPE char10
                                 u_task TYPE char1
                                u_alv TYPE zscmt0001
                       CHANGING c_data TYPE cvis_ei_extern.

  DATA:
    lv_guid           TYPE bu_partner_guid,
    ls_but000         TYPE but000,
    ls_address_help   TYPE bus_ei_bupa_address,
    ls_address_usages TYPE bus_ei_bupa_addressusage,
    ls_phone          TYPE bus_ei_bupa_telephone,
    lt_phone          TYPE bus_ei_bupa_telephone_t,
    ls_fax            TYPE bus_ei_bupa_fax,
    lt_fax            TYPE bus_ei_bupa_fax_t,
    ls_smtp           TYPE bus_ei_bupa_smtp,
    lt_smtp           TYPE bus_ei_bupa_smtp_t.

  ls_address_help-task = u_task.

  CLEAR lv_guid.
  CASE u_task.
    WHEN 'U'.
      SELECT SINGLE address_guid
      FROM but020
      INTO lv_guid
      WHERE partner = u_bp.

      ls_address_help-data_key-guid = lv_guid.
    WHEN 'I'.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = ls_address_help-data_key-guid.
    WHEN OTHERS.
  ENDCASE.


  ls_address_help-task          = u_task.
  ls_address_help-data-postal-data-city             = u_alv-city1  ."城市
  ls_address_help-data-postal-data-standardaddress  = 'X'.
  PERFORM frm_set_cod1 USING u_alv-country CHANGING u_alv-post_code1.
  ls_address_help-data-postal-data-postl_cod1       = u_alv-post_code1."邮政编码/城市
  ls_address_help-data-postal-data-street           = u_alv-street.
*  LS_ADDRESS_HELP-DATA-POSTAL-DATA-HOUSE_NO         = U_ALV-HSNM1   ."门牌号
  ls_address_help-data-postal-data-country          = u_alv-country ."国家
  ls_address_help-data-postal-data-countryiso       = u_alv-country ."国家
*  LS_ADDRESS_HELP-DATA-POSTAL-DATA-REGION           = U_ALV-REGIO   ."地区
  ls_address_help-data-postal-data-time_zone        = 'UTC+8'        ."defalut
  ls_address_help-data-postal-data-langu            = u_alv-langu       ."语言
  ls_address_help-data-postal-data-languiso         = u_alv-langu       ."语言
*  IF U_ALV-POBXCTY IS NOT INITIAL.
*    LS_ADDRESS_HELP-DATA-POSTAL-DATA-POBOX_CTRY = U_ALV-POBXCTY."通讯国家
*    LS_ADDRESS_HELP-DATA-POSTAL-DATA-PO_BOX_REG = U_ALV-POBXREG."通讯地区
*    LS_ADDRESS_HELP-DATA-POSTAL-DATA-POSTL_COD2 = U_ALV-PSTCD2."通讯邮编
*    LS_ADDRESS_HELP-DATA-POSTAL-DATA-PO_BOX_CIT = U_ALV-POBXLOC."通讯城市
*  ENDIF.

  IF u_task = 'U'.
    ls_address_help-data-postal-datax-city             = abap_true.
    ls_address_help-data-postal-datax-standardaddress  = abap_true.
    ls_address_help-data-postal-datax-postl_cod1       = abap_true.
    ls_address_help-data-postal-datax-street           = abap_true.
*    LS_ADDRESS_HELP-DATA-POSTAL-DATAX-HOUSE_NO         = ABAP_TRUE.
    ls_address_help-data-postal-datax-country          = abap_true.
    ls_address_help-data-postal-datax-countryiso       = abap_true.
*    LS_ADDRESS_HELP-DATA-POSTAL-DATAX-REGION           = ABAP_TRUE.
    ls_address_help-data-postal-datax-time_zone        = abap_true.
    ls_address_help-data-postal-datax-langu            = abap_true.
    ls_address_help-data-postal-datax-langu_iso        = abap_true.
*    IF U_ALV-POBXCTY IS NOT INITIAL.
*      LS_ADDRESS_HELP-DATA-POSTAL-DATAX-POBOX_CTRY = ABAP_TRUE.
*      LS_ADDRESS_HELP-DATA-POSTAL-DATAX-PO_BOX_REG = ABAP_TRUE.
*      LS_ADDRESS_HELP-DATA-POSTAL-DATAX-POSTL_COD2 = ABAP_TRUE.
*      LS_ADDRESS_HELP-DATA-POSTAL-DATAX-PO_BOX_CIT = ABAP_TRUE.
*    ENDIF.
  ENDIF.



*  IF U_ALV-TLNMBR1 IS NOT INITIAL.
*    LS_PHONE-CONTACT-TASK = U_TASK.
*    LS_PHONE-CONTACT-DATA-TELEPHONE            = U_ALV-TLNMBR1  ."电话
*    LS_PHONE-CONTACT-DATA-R_3_USER             = '1'           ."电话
*    IF U_TASK = 'U'.
*      LS_PHONE-CONTACT-DATAX-TELEPHONE         = ABAP_TRUE.
*      LS_PHONE-CONTACT-DATAX-R_3_USER          = ABAP_TRUE.
*    ENDIF.
*    APPEND LS_PHONE TO LT_PHONE.
*  ENDIF.

*  IF U_ALV-MBNMBR1 IS NOT INITIAL.
*    LS_PHONE-CONTACT-TASK = U_TASK.
*    LS_PHONE-CONTACT-DATA-TELEPHONE           = U_ALV-MBNMBR1 ."移动电话
*    LS_PHONE-CONTACT-DATA-R_3_USER             = '3'           ."移动电话
*    IF U_TASK = 'U'.
*      LS_PHONE-CONTACT-DATAX-TELEPHONE            = ABAP_TRUE.
*      LS_PHONE-CONTACT-DATAX-R_3_USER             = ABAP_TRUE.
*    ENDIF.
*    APPEND LS_PHONE TO LT_PHONE.
*  ENDIF.

*  IF LT_PHONE IS NOT INITIAL.
*    LS_ADDRESS_HELP-DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE.
*  ENDIF.



*  IF U_ALV-FXNMBR1 IS NOT INITIAL."传真
*    LS_FAX-CONTACT-TASK = U_TASK.
*    LS_FAX-CONTACT-DATA-FAX        = U_ALV-FXNMBR1 .
*
*    IF U_TASK = 'U'.
*      LS_FAX-CONTACT-DATAX-FAX     = ABAP_TRUE.
*    ENDIF.
*
*    APPEND LS_FAX TO LT_FAX.
*    LS_ADDRESS_HELP-DATA-COMMUNICATION-FAX-FAX = LT_FAX.
*  ENDIF.



*  IF U_ALV-SMTPADR IS NOT INITIAL."邮箱
*    LS_SMTP-CONTACT-TASK = U_TASK.
*    LS_SMTP-CONTACT-DATA-E_MAIL     = U_ALV-SMTPADR .
*
*    IF U_TASK = 'U'.
*      LS_SMTP-CONTACT-DATAX-E_MAIL     = ABAP_TRUE.
*    ENDIF.
*
*    APPEND LS_SMTP TO LT_SMTP.
*    LS_ADDRESS_HELP-DATA-COMMUNICATION-SMTP-SMTP = LT_SMTP.
*  ENDIF.


  APPEND ls_address_help TO c_data-partner-central_data-address-addresses.

ENDFORM.

FORM frm_set_cod1 USING land1 CHANGING code.
  IF code = '888888' OR code IS INITIAL.
    CLEAR code.
    SELECT SINGLE lnplz INTO @DATA(ls_LNPLZ) FROM t005
      WHERE land1 = @land1.
    IF ls_lnplz IS NOT INITIAL.
      DO  ls_lnplz TIMES.
        code = code && 8.
      ENDDO.
    ENDIF.
  ENDIF.

ENDFORM.

*FORM F_BANK_ACC  USING     U_BP  TYPE CHAR10
*                           U_ALV  TYPE ZSCMT0001
*                 CHANGING  C_DATA TYPE CVIS_EI_EXTERN.
*  DATA:
*    LT_BUT0BK      TYPE TABLE OF BUT0BK,
*    LS_BUT0BK      TYPE BUT0BK,
*    LS_ALV         TYPE TY_ALV,
*    LT_ZMMT0316    TYPE TABLE OF ZMMT0316,
*    LS_ZMMT0316    TYPE ZMMT0316,
*    LT_BANKDETAILS TYPE BUS_EI_BUPA_BANKDETAIL_T,
*    LS_BANK_ACC    TYPE BUS_EI_BUPA_BANKDETAIL.
*
*
*  SELECT *
*  INTO TABLE LT_ZMMT0316
*  FROM ZMMT0316
*  WHERE ZWHM = U_ALV-ZWHM.
*
*  SELECT *
*  FROM BUT0BK
*  INTO TABLE LT_BUT0BK
*  WHERE PARTNER = U_BP.
*
*  LOOP AT LT_ZMMT0316 INTO LS_ZMMT0316 .
*    READ TABLE  LT_BUT0BK INTO LS_BUT0BK WITH KEY BKVID = LS_ZMMT0316-ITEM.
*    IF SY-SUBRC = 0.
*      LS_BANK_ACC-TASK = 'U'.
*      DELETE LT_BUT0BK WHERE  BKVID = LS_ZMMT0316-ITEM.
*    ELSE.
*      LS_BANK_ACC-TASK = 'I'.
*    ENDIF.
*    LS_BANK_ACC-DATA_KEY             = LS_ZMMT0316-ITEM."ITEM
*    LS_BANK_ACC-DATA-BANK_CTRY       = LS_ZMMT0316-BANKS."银行国家
*    LS_BANK_ACC-DATA-BANK_KEY        = LS_ZMMT0316-BANKK."银行代码
*    LS_BANK_ACC-DATA-BANK_ACCT       = LS_ZMMT0316-BANKN."银行账户
*    LS_BANK_ACC-DATA-CTRL_KEY        = LS_ZMMT0316-BKONT."控制码
*    LS_BANK_ACC-DATA-BANK_REF        = LS_ZMMT0316-BKREF."参考明细
*    LS_BANK_ACC-DATA-ACCOUNTHOLDER   = LS_ZMMT0316-KOINH."帐户持有人姓名
*    LS_BANK_ACC-DATA-BANKACCOUNTNAME = LS_ZMMT0316-BANKA."银行名称
*    LS_BANK_ACC-DATAX-BANK_CTRY       = ABAP_TRUE."银行国家
*    LS_BANK_ACC-DATAX-BANK_KEY        = ABAP_TRUE."银行代码
*    LS_BANK_ACC-DATAX-BANK_ACCT       = ABAP_TRUE."银行账户
*    LS_BANK_ACC-DATAX-CTRL_KEY        = ABAP_TRUE."控制码
*    LS_BANK_ACC-DATAX-BANK_REF        = ABAP_TRUE."参考明细
*    LS_BANK_ACC-DATAX-ACCOUNTHOLDER   = ABAP_TRUE."帐户持有人姓名
*    LS_BANK_ACC-DATAX-BANKACCOUNTNAME = ABAP_TRUE."银行名称
*    APPEND LS_BANK_ACC TO LT_BANKDETAILS.
*  ENDLOOP.
*
*  LOOP AT  LT_BUT0BK INTO LS_BUT0BK.
*    LS_BANK_ACC-TASK = 'D'.
*    LS_BANK_ACC-DATA_KEY             = LS_BUT0BK-BKVID."ITEM
*    LS_BANK_ACC-DATA-BANK_CTRY       = LS_BUT0BK-BANKS."银行国家
*    LS_BANK_ACC-DATA-BANK_KEY        = LS_BUT0BK-BANKL."银行代码
*    LS_BANK_ACC-DATA-BANK_ACCT       = LS_BUT0BK-BANKN."银行账户
**    LS_BANK_ACC-DATA-CTRL_KEY        = LS_BUT0BK-BKONT."控制码
**    LS_BANK_ACC-DATA-BANK_REF        = LS_BUT0BK-BKREF."参考明细
**    LS_BANK_ACC-DATA-ACCOUNTHOLDER   = LS_BUT0BK-KOINH."帐户持有人姓名
**    LS_BANK_ACC-DATA-BANKACCOUNTNAME = LS_BUT0BK-BANKA."银行名称
*    LS_BANK_ACC-DATAX-BANK_CTRY       = ABAP_TRUE."银行国家
*    LS_BANK_ACC-DATAX-BANK_KEY        = ABAP_TRUE."银行代码
*    LS_BANK_ACC-DATAX-BANK_ACCT       = ABAP_TRUE."银行账户
**    LS_BANK_ACC-DATAX-CTRL_KEY        = ABAP_TRUE."控制码
**    LS_BANK_ACC-DATAX-BANK_REF        = ABAP_TRUE."参考明细
**    LS_BANK_ACC-DATAX-ACCOUNTHOLDER   = ABAP_TRUE."帐户持有人姓名
**    LS_BANK_ACC-DATAX-BANKACCOUNTNAME = ABAP_TRUE."银行名称
*    APPEND LS_BANK_ACC TO LT_BANKDETAILS.
*  ENDLOOP.
*
*  C_DATA-PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS = LT_BANKDETAILS.
*
*
*ENDFORM.

FORM f_add_sup_general  USING    u_bp TYPE char10
                                 u_task TYPE c
                                 u_alv TYPE zscmt0001
                        CHANGING c_data TYPE cvis_ei_extern.

  DATA: lt_company    TYPE vmds_ei_company_t,
        ls_company    TYPE vmds_ei_company,
        lt_purchasing TYPE vmds_ei_purchasing_t,
        ls_purchasing TYPE vmds_ei_purchasing,
        lt_functions  TYPE vmds_ei_functions_t.


  "供应商基本数据
  IF u_alv-flvn00 = 'X' OR  u_alv-flvn01 = 'X'.

    SELECT SINGLE *
     INTO @DATA(ls_lfa1)
     FROM lfa1
     WHERE lifnr = @u_alv-partner.

    IF sy-subrc EQ 0.
      c_data-vendor-header-object_instance-lifnr = u_bp.
      c_data-vendor-header-object_task = 'U'.
      IF u_alv-bu_group = 'Z100'.
        c_data-vendor-central_data-central-data-vbund   =  u_alv-partner+4(6)."贸易伙伴
        c_data-vendor-central_data-central-datax-vbund = abap_true.
      ENDIF.
*      C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATAX-STENR  =  ABAP_TRUE."税号
*      C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATAX-STCEG  =  ABAP_TRUE."增值税登记号  20位
*      C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATAX-FITYP  =  ABAP_TRUE."税收类型
*      C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATAX-STCDT  =  ABAP_TRUE."税号类型\
    ELSE.
      c_data-vendor-header-object_instance-lifnr = u_bp.
      c_data-vendor-header-object_task = 'I'.
      IF u_alv-bu_group = 'Z100'.
        c_data-vendor-central_data-central-data-vbund   =  u_alv-partner+4(6)."贸易伙伴
        c_data-vendor-central_data-central-datax-vbund = abap_true.
*        c_data-vendor-central_data-central-datax-werks = abap_true.
      ENDIF.
    ENDIF.
*

*    C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATA-STCEG   =  U_ALV-STCEG."增值税登记号  20位
*    C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATA-FITYP   =  U_ALV-AFITP."税收类型
*    C_DATA-VENDOR-CENTRAL_DATA-CENTRAL-DATA-STCDT   =  U_ALV-ATOID."税号类型
  ENDIF.


  " 供应商公司数据
  IF u_alv-flvn00 = 'X'.
    SELECT SINGLE *
     INTO @DATA(ls_lfb1)
     FROM lfb1
     WHERE lifnr = @u_alv-partner
       AND bukrs = @u_alv-bukrs.
    IF sy-subrc = 0.
      ls_company-task           =  'U'.
      ls_company-datax-akont    =  abap_true."统御科目
      ls_company-datax-zterm    =  abap_true."付款条件
*      LS_COMPANY-DATAX-SPERR    =  ABAP_TRUE."所选公司代码 过账冻结
*      LS_COMPANY-DATAX-GUZTE    =  ABAP_TRUE."信用记录条件
*      LS_COMPANY-DATAX-ALTKN    =  ABAP_TRUE."先前的账户号
*      LS_COMPANY-DATAX-ZWELS    =  ABAP_TRUE."付款方式
    ELSE.
      ls_company-task          =  'I'.
    ENDIF.

    ls_company-data_key      =  u_alv-bukrs.
    ls_company-data-akont    =  u_alv-akont_ven."统御科目
    ls_company-data-zterm    =  u_alv-zterm_ven."付款条件
*    LS_COMPANY-DATA-SPERR    =  U_ALV-SPERB_B."所选公司代码 过账冻结
*    LS_COMPANY-DATA-GUZTE    =  U_ALV-GUZTE."信用记录条件
*    LS_COMPANY-DATA-ALTKN    =  U_ALV-ALTKN."先前的账户号
*    LS_COMPANY-DATA-ZWELS    =  U_ALV-ZWELS."付款方式
    APPEND ls_company TO lt_company.
    c_data-vendor-company_data-company = lt_company.
  ENDIF.


  "供应商采购数据
  IF u_alv-flvn01 = 'X'.
    SELECT SINGLE *
    INTO @DATA(ls_lfm1)
    FROM lfm1
    WHERE lifnr = @u_bp
      AND ekorg = @u_alv-ekorg.
    IF sy-subrc = 0.
      ls_purchasing-task   = 'U'.
      ls_purchasing-datax-waers   = abap_true. "货币.
      ls_purchasing-datax-kalsk   = abap_true. "方案组.
*      LS_PURCHASING-DATAX-LFABC   = ABAP_TRUE. "供应商等级
      ls_purchasing-datax-webre   = abap_true. "基于收货的发票验证
*      LS_PURCHASING-DATAX-SPERM   = ABAP_TRUE. "采购冻结在采购组
    ELSE.
      ls_purchasing-task   = 'I'.
      PERFORM f_add_sup_purch_functions USING   u_alv  " 供应商合作伙伴功能
                                       CHANGING lt_functions.
      IF lt_functions IS NOT INITIAL.
        ls_purchasing-functions-functions = lt_functions.
      ENDIF.
    ENDIF.

    ls_purchasing-data_key     = u_alv-ekorg.
    ls_purchasing-data-waers   = u_alv-waers. "货币.
    ls_purchasing-data-kalsk   = u_alv-kalsk. "方案组.
*    LS_PURCHASING-DATA-LFABC   = U_ALV-LFABC. "供应商等级
    ls_purchasing-data-webre   = u_alv-webre. "基于收货的发票验证
*    LS_PURCHASING-DATA-SPERM   = U_ALV-SPERM_M. "采购冻结在采购组
    APPEND ls_purchasing TO  lt_purchasing.
    c_data-vendor-purchasing_data-purchasing = lt_purchasing.
  ENDIF.

ENDFORM.

FORM f_add_sup_purch_functions  USING
                                         u_alv   TYPE zscmt0001
                                CHANGING
                                         ct_function TYPE  vmds_ei_functions_t.


  DATA: ls_partner_function    TYPE wyt3,
        ls_sup_purch_functions TYPE cvis_supplier_purch_func,
        ls_functions           TYPE vmds_ei_functions.



  CALL METHOD cvi_default_values_classic=>get_default_vend_functions
    EXPORTING
      i_account_group  = u_alv-bu_group "供应商账户组
    RECEIVING
      r_functions_list = DATA(lt_partner_function).

  LOOP AT lt_partner_function INTO ls_partner_function.

    ls_functions-task = 'I'.
    ls_functions-data_key-ltsnr = ls_partner_function-ltsnr.
    ls_functions-data_key-werks = ls_partner_function-werks.
    ls_functions-data_key-parvw = ls_partner_function-parvw.
    ls_functions-data_key-parza = ls_partner_function-parza.
    ls_functions-data-defpa     = ls_partner_function-defpa.


    ls_functions-data-partner   = u_alv-partner.

    APPEND ls_functions TO ct_function.
  ENDLOOP.

ENDFORM.

FORM f_add_cust_general  USING
                                   u_bp TYPE char10
                                   u_task TYPE c
                                   u_alv TYPE zscmt0001
                          CHANGING c_data TYPE cvis_ei_extern.


  DATA: lt_company   TYPE cmds_ei_company_t,
        ls_company   TYPE cmds_ei_company,
        lt_sales     TYPE cmds_ei_sales_t,
        ls_sales     TYPE cmds_ei_sales,
        lt_texts     TYPE cvis_ei_text_t,
        ls_texts     TYPE cvis_ei_text,
        lt_functions TYPE cmds_ei_functions_t,
        lt_tax       TYPE cmds_ei_tax_ind_t.




  "客户销售组织数据
  IF u_alv-flcu01 = 'X' OR u_alv-flcu00 = 'X'.
    SELECT SINGLE *
      INTO @DATA(ls_kna1)
      FROM kna1
      WHERE kunnr = @u_bp.
    IF sy-subrc EQ 0.
      c_data-customer-header-object_instance-kunnr = u_bp.
      c_data-customer-header-object_task = 'U'.
*      IF u_alv-bu_group = 'Z100'.
*        c_data-customer-central_data-central-datax-vbund   =  'X'."贸易伙伴

*      ENDIF.
    ELSE.
      c_data-customer-header-object_task = 'I'.
      c_data-customer-header-object_instance-kunnr = u_bp.
*      IF u_alv-bu_group = 'Z100'.
*        c_data-customer-central_data-central-data-vbund   =  u_alv-partner+4(6)."贸易伙伴
*      ENDIF.

    ENDIF.

    IF u_alv-flcu01 = 'X'.


      SELECT SINGLE *
      INTO @DATA(ls_knvv)
      FROM knvv
      WHERE kunnr = @u_bp
        AND vkorg = @u_alv-vkorg
        AND vtweg = @u_alv-vtweg
        AND spart = @u_alv-spart.

      IF sy-subrc = 0.
        ls_sales-task = 'U'.
*      ls_sales-datax-kdgrp      = abap_true."运营方式
        ls_sales-datax-kalks      = abap_true."定价过程
        ls_sales-datax-konda      = abap_true."客户定价组
        ls_sales-datax-bzirk      = abap_true."销售地区
        ls_sales-datax-waers      = abap_true."币种
        ls_sales-datax-vsbed      = abap_true."装运条件
        ls_sales-datax-kkber      = abap_true."信控范围
        ls_sales-datax-vwerk      = abap_true."交货工厂
        ls_sales-datax-vkbur      = abap_true."销售办事处
        ls_sales-datax-vkgrp      = abap_true."销售组
        ls_sales-datax-kdgrp      = abap_true."客户组
        ls_sales-datax-zterm      = abap_true."付款条件
*        ls_sales-datax-ktgrd      = abap_true."账户分配组
*      ls_sales-datax-aufsd      = abap_true."冻结
*      ls_sales-datax-lifsd      = abap_true."
*      ls_sales-datax-faksd      = abap_true."
      ELSE.
        ls_sales-task = 'I'.
        PERFORM f_add_sale_functions USING      u_alv  " 客户伙伴功能
                                     CHANGING   lt_functions.
        IF lt_functions IS NOT INITIAL.
          ls_sales-functions-functions = lt_functions.
        ENDIF.
      ENDIF.

      ls_sales-data_key-vkorg   = u_alv-vkorg.
      ls_sales-data_key-vtweg   = u_alv-vtweg.
      ls_sales-data_key-spart   = u_alv-spart..
*    ls_sales-data-kdgrp       = u_alv-kdgrp."运营方式
      ls_sales-data-kalks       = u_alv-kalks."定价过程
      ls_sales-data-konda       = u_alv-konda."客户定价组
      ls_sales-data-bzirk       = u_alv-bzirk."销售地区
      ls_sales-data-waers       = u_alv-waers."币种
      ls_sales-data-vsbed       = u_alv-vsbed."装运条件
      ls_sales-data-kkber       = u_alv-kkber."信控范围
      ls_sales-data-vwerk       = u_alv-werks."交货工厂
      ls_sales-data-vkbur       = u_alv-vkbur."销售办事处
      ls_sales-data-vkgrp       = u_alv-vkgrp."销售组
      ls_sales-data-ktgrd       = u_alv-ktgrd."账户分配组
      ls_sales-data-kdgrp       = u_alv-kdgrp."客户组
      ls_sales-data-zterm       = u_alv-zterm_cus."付款条件

*    ls_sales-data-aufsd       = u_alv-aufsd."冻结
*    ls_sales-data-lifsd       = u_alv-lifsd."
*    ls_sales-data-faksd       = u_alv-faksd."
      APPEND ls_sales TO lt_sales.
      c_data-customer-sales_data-sales = lt_sales.
    ENDIF.


    " 客户公司代码
    IF  u_alv-flcu00 = 'X' .
      SELECT SINGLE *
       INTO @DATA(ls_knb1)
       FROM knb1
       WHERE kunnr = @u_bp
         AND bukrs = @u_alv-bukrs.
      IF sy-subrc = 0.
        ls_company-task          =  'U'.
        ls_company-datax-akont    =  abap_true."统御科目
        ls_company-datax-zterm    =  abap_true."付款条件
*      ls_company-datax-sperr    =  abap_true."所选公司代码 过账冻结
      ELSE.
        ls_company-task          =  'I'.
      ENDIF.

      ls_company-data_key      =  u_alv-bukrs.
      ls_company-data-akont    =  u_alv-akont_cus."统御科目
      ls_company-data-zterm    =  u_alv-zterm_cus."付款条件
*    ls_company-data-sperr    =  u_alv-sperb_b_k."所选公司代码 过账冻结

      APPEND ls_company TO lt_company.
      c_data-customer-company_data-company = lt_company.


      PERFORM f_add_sale_tax_typ USING      u_alv  " 客户伙伴功能
                                           u_bp
                               CHANGING   lt_tax.

      c_data-customer-central_data-tax_ind-tax_ind = lt_tax.

    ENDIF.

  ENDIF.
ENDFORM.

FORM f_add_ukm_general USING
                             u_bp TYPE char10
                             u_task TYPE c
                             u_alv TYPE zscmt0001
                    CHANGING c_data TYPE cvis_ei_extern.

  DATA:ls_profile  TYPE ukm_ei_bp_cms.
  DATA:lt_segments TYPE ukmt_ei_bp_cms_sgm.
  DATA:ls_segments TYPE ukm_ei_bp_cms_sgm.

  IF u_alv-fukm000 = 'X' AND u_task = 'I'.

    ls_profile-data-check_rule = '01'.
    ls_profile-data-limit_rule = 'B2B-EXIST'.
    ls_profile-data-risk_class = 'A'.
    ls_profile-data-credit_group = '0001'.

    ls_profile-datax-check_rule = 'X'.
    ls_profile-datax-limit_rule = 'X'.
    ls_profile-datax-risk_class = 'X'.
    ls_profile-datax-credit_group = 'X'.

    c_data-partner-ukmbp_data-profile = ls_profile.


*    IF u_alv-credit > 0.
    ls_segments-task = u_task.

    ls_segments-data_key-partner = u_bp.
    ls_segments-data_key-credit_sgmnt = 'Z001'.

    ls_segments-data-credit_limit = u_alv-credit.
    ls_segments-data-limit_valid_date = '99991231'.

    APPEND ls_segments TO lt_segments.


    c_data-partner-ukmbp_data-segments-segments = lt_segments.
*    ENDIF.

  ENDIF.

ENDFORM.

FORM f_add_sale_functions  USING   u_alv   TYPE zscmt0001
                         CHANGING  ct_functions TYPE  cmds_ei_functions_t.

  DATA: ls_knvp                 TYPE knvp,
        ls_functions            TYPE cmds_ei_functions,
        ls_cust_sales_functions TYPE cvis_customer_sales_func.
  DATA: lv_part_up TYPE zscmt0010-partner.

  CALL METHOD cvi_default_values_classic=>get_default_cust_functions
    EXPORTING
      i_account_group  = u_alv-bu_group "账户组
    RECEIVING
      r_functions_list = DATA(lt_partner_function).

*  IF u_alv-zydc_sc_no IS NOT INITIAL.
*    lv_part_up = u_alv-zydc_sc_no.
*    SELECT SINGLE * FROM zscmt0010 INTO @DATA(ls_0010)
*     WHERE partner = @lv_part_up."店中店上上级
*    IF ls_0010-bu_group = 'K006' AND ls_0010-zydc_sc_no IS NOT INITIAL.
*      lv_part_up = ls_0010-zydc_sc_no.
*    ENDIF.
*  ENDIF.


  LOOP AT lt_partner_function INTO ls_knvp.
    ls_functions-task = 'I'.
    ls_functions-data_key-parvw = ls_knvp-parvw.
    ls_functions-data_key-parza = ls_knvp-parza.
*    LS_FUNCTIONS-DATA-PARTNER = LS_KNVP-PARNR.
    ls_functions-data-defpa = ls_knvp-defpa.
    ls_functions-data-knref = ls_knvp-knref.

    IF lv_part_up IS NOT INITIAL.
      IF ls_functions-data_key-parvw = 'RE' OR ls_functions-data_key-parvw = 'RG' .
        ls_functions-data-partner = lv_part_up.
*     ELSE.
*        ls_functions-data-partner = u_alv-partner.
      ENDIF.
*    ELSE.
*      ls_functions-data-partner = u_alv-partner.
    ENDIF.

    APPEND ls_functions TO ct_functions.
    CLEAR:ls_functions.
  ENDLOOP.

ENDFORM.


FORM f_add_sale_tax_typ USING   u_alv   TYPE zscmt0001
                                u_bp    TYPE char10
                         CHANGING  ct_tax TYPE  cmds_ei_tax_ind_t.

  DATA: ls_knvi               TYPE knvi,
        ls_area               TYPE cvis_sales_area,
        ls_cust_sales_tax_typ TYPE cvis_customer_sales_func,
        lt_knvi               TYPE TABLE OF knvi,
        ls_tax                TYPE cmds_ei_tax_ind.


  SELECT *
    INTO TABLE lt_knvi
    FROM knvi
    WHERE kunnr = u_bp.

  ls_area-sales_org    = u_alv-vkorg.
  ls_area-dist_channel = u_alv-vtweg.
  ls_area-division     = u_alv-spart.


  CALL METHOD cvi_default_values_classic=>get_default_cust_tax_inds
    EXPORTING
      i_sales_area     = ls_area "账户组
    RECEIVING
      r_tax_indicators = DATA(lt_partner_tax_typ).


  LOOP AT lt_partner_tax_typ INTO DATA(ls_partner_tax_typ).
    READ TABLE lt_knvi INTO ls_knvi WITH KEY aland = ls_partner_tax_typ-aland
                                             tatyp = ls_partner_tax_typ-tatyp.
    IF sy-subrc = 0.
      ls_tax-task = 'U'.
      ls_tax-data-taxkd     = u_alv-taxkd.
      ls_tax-datax-taxkd    = abap_true.
      DELETE lt_knvi WHERE aland = ls_partner_tax_typ-aland AND
                           tatyp = ls_partner_tax_typ-tatyp.
    ELSE.
      ls_tax-task = 'I'.
    ENDIF.
    ls_tax-data-taxkd     = u_alv-taxkd.
    ls_tax-data_key-aland = ls_partner_tax_typ-aland.
    ls_tax-data_key-tatyp = ls_partner_tax_typ-tatyp.
    APPEND ls_tax TO ct_tax.
    CLEAR ls_tax.
  ENDLOOP.

  LOOP AT lt_knvi INTO ls_knvi.
    ls_tax-task = 'D'.
    ls_tax-data-taxkd     = ls_knvi-taxkd.
    ls_tax-data_key-aland = ls_knvi-aland.
    ls_tax-data_key-tatyp = ls_knvi-tatyp.
    APPEND ls_tax TO ct_tax.
    CLEAR ls_tax.
  ENDLOOP.



ENDFORM.

FORM f_get_def_value CHANGING u_alv TYPE zscmt0001.

  FIELD-SYMBOLS:<fs_value>.

  SELECT * FROM zscmt1004
    WHERE bu_group = @u_alv-bu_group
     AND defty = 'B'
    INTO TABLE @DATA(lt_1004).

  LOOP AT lt_1004 INTO DATA(ls_1004).

    ASSIGN COMPONENT ls_1004-fname OF STRUCTURE u_alv TO <fs_value>.

    CHECK sy-subrc = 0 AND <fs_value> IS INITIAL.

    <fs_value> = ls_1004-fvalue.

  ENDLOOP.


*---确定工厂，销售组织，采购组织
  SELECT SINGLE *  FROM zscmt1006
    WHERE bu_group = @u_alv-bu_group
      AND bukrs    = @u_alv-bukrs
    INTO @DATA(ls_1006).
  IF sy-subrc = 0.
    u_alv-vkorg = ls_1006-vkorg.
    u_alv-werks = ls_1006-werks.
    u_alv-ekorg = ls_1006-ekorg.
  ENDIF.

*----扩展视图配置
  SELECT * FROM zscmt1001
    WHERE bu_group = @u_alv-bu_group
    INTO TABLE @DATA(lt_1001).

  LOOP AT lt_1001 INTO DATA(ls_1001).

    CASE ls_1001-role.
      WHEN 'FLVN00'.
        u_alv-flvn00 = 'X'.
      WHEN 'FLVN01'.
        IF u_alv-ekorg IS NOT INITIAL.
          u_alv-flvn01 = 'X'.
        ELSE.
          u_alv-flvn01 = ''.
        ENDIF.
      WHEN 'FLCU00'.
        u_alv-flcu00 = 'X'.
      WHEN 'FLCU01'.
        IF u_alv-vkorg IS NOT INITIAL.
          u_alv-flcu01 = 'X'.
        ELSE.
          u_alv-flcu01 = ''.
        ENDIF.
      WHEN 'KOSTL'.
        u_alv-fkostl = 'X'.
      WHEN 'LGORT'.
        u_alv-flgort = 'X'.
      WHEN 'UKM000'.
        u_alv-fukm000 = 'X'.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.


*---确定利润中心
*  IF u_alv-zsyb IS NOT INITIAL.
*
*    SELECT SINGLE prctr INTO u_alv-prctr
*      FROM zscm_syb
*     WHERE zsyb = u_alv-zsyb.
*
*  ENDIF.

*  IF u_alv-bu_group = 'K007'.
*    u_alv-zydc_brand = u_alv-brand_id.
*  ENDIF.

*---加前导零
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_alv-partner
    IMPORTING
      output = u_alv-partner.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_alv-kunnr
    IMPORTING
      output = u_alv-kunnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_alv-lifnr
    IMPORTING
      output = u_alv-lifnr.

  u_alv-name1 = u_alv-zname1.
  u_alv-name2 = u_alv-zname2.
  u_alv-street = u_alv-zaddress.


ENDFORM.


FORM create_lgort TABLES ct_return STRUCTURE bapiret2
                   USING u_task TYPE char1
                  CHANGING u_alv TYPE zscmt0001.

  DATA:lv_lgobe LIKE t001l-lgobe.

  CHECK u_alv-flgort = 'X'.

  IF u_task = 'U'  AND
     u_alv-werks IS NOT INITIAL AND
     u_alv-lgort IS NOT INITIAL.

    SELECT COUNT(*) FROM t001l
     WHERE werks = u_alv-werks
       AND lgort = u_alv-lgort.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

  ENDIF.

  lv_lgobe = u_alv-name1.

  CALL FUNCTION 'ZSCM_LGORT_CREATE'
    EXPORTING
*     i_partner = u_alv-partner
      perfix  = 'S'
      i_werks = u_alv-werks
      i_lgobe = lv_lgobe
    TABLES
      return  = ct_return
    CHANGING
      i_lgort = u_alv-lgort.

ENDFORM.

FORM data_check TABLES ct_return STRUCTURE bapiret2
                 USING u_alv TYPE zscmt0001
                       u_task TYPE char1
              CHANGING cv_status TYPE char1.

  DATA:lv_msg TYPE string.

  IF u_alv-flgort = 'X' AND u_task = 'I' AND
     u_alv-werks IS NOT INITIAL AND
     u_alv-lgort IS NOT INITIAL.
*---在新建（通过BP有没有判断）的时候，新建的时候就必须要创建库位
    SELECT COUNT(*) FROM t001l
     WHERE werks = u_alv-werks
       AND lgort = u_alv-lgort.
    IF sy-subrc = 0.
      cv_status = 'E'.
      lv_msg    = '库位' && u_alv-lgort && '已经存在'.

      PERFORM set_msg TABLES ct_return
                      USING cv_status
                            lv_msg.
    ENDIF.
  ELSE.
    SELECT SINGLE lgort FROM zscmt0010
      WHERE partner = @u_alv-partner
      INTO @DATA(lv_lgort).
    IF sy-subrc EQ 0.
      u_alv-lgort = lv_lgort.
    ENDIF.
  ENDIF.

ENDFORM.

FORM set_msg TABLES ct_return STRUCTURE bapiret2
              USING pv_status
                    pv_msg.

  DATA:ls_return LIKE bapiret2.

  ls_return-type = pv_status.
  ls_return-id = 'ZMDA'.
  ls_return-number = '000'.
  ls_return-message = pv_msg.
  ls_return-message_v1 = pv_msg.

  APPEND ls_return TO ct_return.

ENDFORM.

FORM create_kostl TABLES ct_return STRUCTURE bapiret2
                  CHANGING u_alv TYPE zscmt0001.

  DATA:lv_co_area TYPE bapi0012_gen-co_area.
  DATA:lt_input TYPE STANDARD TABLE OF bapi0012_ccinputlist.
  DATA:ls_input TYPE bapi0012_ccinputlist.
  DATA:lv_kostl LIKE csks-kostl.
  DATA:lt_extensionin LIKE STANDARD TABLE OF bapiparex.
  DATA:ls_ext TYPE bapi_te_csks.
  DATA:lt_return LIKE STANDARD TABLE OF bapiret2.
*  FIELD-SYMBOLS:<fs_1012> LIKE zscmt1012.

  CHECK u_alv-fkostl = 'X'.

  lv_kostl = u_alv-partner.

*  SELECT * FROM zscmt1012 INTO TABLE @DATA(lt_1012)
*   WHERE bukrs = @u_alv-bukrs.
*  IF sy-subrc NE 0.
*    CLEAR:lt_return.
*    PERFORM set_msg TABLES lt_return
*                    USING 'E'
*                          '请配置需要扩展的成本中心公司'.
*
*    IF lt_return[] IS NOT INITIAL.
*      APPEND LINES OF lt_return TO ct_return.
*    ENDIF.
*
*    RETURN.
*  ENDIF.

*  READ TABLE lt_1012 TRANSPORTING NO FIELDS
*                     WITH KEY bukrs    = u_alv-bukrs
*                              bukrs_co = u_alv-bukrs."自身要有，没有自动补
*  IF sy-subrc NE 0.
*    APPEND INITIAL LINE TO lt_1012 ASSIGNING <fs_1012>.
*
*    <fs_1012>-bukrs = u_alv-bukrs.
*    <fs_1012>-bukrs_co = u_alv-bukrs.
*  ENDIF.
*
*  LOOP AT lt_1012 ASSIGNING <fs_1012>.
*
*    CLEAR:lv_co_area,ls_input,lt_input,lt_extensionin,ls_ext.
*
*    IF u_alv-bu_group = 'K012'.
*      IF <fs_1012>-bukrs <> <fs_1012>-bukrs_co.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    SELECT COUNT(*) FROM csks
*    WHERE kokrs = <fs_1012>-bukrs_co
*      AND kostl = lv_kostl.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    lv_co_area = <fs_1012>-bukrs_co.
*
*    ls_input-costcenter = lv_kostl.
*    ls_input-valid_from = sy-datum.
*    ls_input-valid_to = '99991231'.
*
*
*    ls_input-costctr_hier_grp = <fs_1012>-bukrs_co.
*
*    SELECT SINGLE phinr INTO ls_input-costctr_hier_grp
*      FROM tka01 WHERE kokrs =  <fs_1012>-bukrs_co.
*
*    ls_input-comp_code = <fs_1012>-bukrs_co.
**     ls_input-bus_area = '1000'.
*    ls_input-person_in_charge = u_alv-zcontact.
*
*    IF u_alv-bu_group = 'K012'.
*      ls_input-costcenter_type = 'L'.
*      ls_input-func_area = '2000'.
*      ls_input-department = u_alv-name2.
*
*
*    ELSE.
*      ls_input-costcenter_type = 'V'.
*      ls_input-func_area = '1000'.
*      IF ls_input-person_in_charge IS INITIAL.
*        ls_input-person_in_charge = lv_kostl.
*      ENDIF.
*    ENDIF.
*
*    ls_input-profit_ctr = u_alv-prctr.
**    ls_input-lock_ind_actual_revenues   = 'X'.
**    ls_input-lock_ind_plan_revenues     = 'X'.
*    ls_input-lock_ind_commitment_update = 'X'.
*
*
*    SELECT SINGLE waers INTO ls_input-currency
*     FROM t001
*     WHERE bukrs = ls_input-comp_code.
*
*    ls_input-name = u_alv-name1.
*
*    APPEND ls_input TO lt_input.
*
**---扩展结构
*
*    APPEND INITIAL LINE TO lt_extensionin ASSIGNING FIELD-SYMBOL(<fs_ext>).
*
*    MOVE 'BAPI_TE_CSKS' TO <fs_ext>-structure.
*    MOVE-CORRESPONDING u_alv TO ls_ext.
*    ls_ext-kunnr = u_alv-partner.
*    ls_ext-costcenter = lv_kostl.
*
*    CALL METHOD cl_abap_container_utilities=>fill_container_c
*      EXPORTING
*        im_value     = ls_ext
*      IMPORTING
*        ex_container = <fs_ext>-valuepart1.
*
*    CHECK lt_input IS NOT INITIAL.
*
*    CLEAR:lt_return.
*    CALL FUNCTION 'BAPI_COSTCENTER_CREATEMULTIPLE'
*      EXPORTING
*        controllingarea = lv_co_area
**       TESTRUN         = ' '
**       MASTER_DATA_INACTIVE       = ' '
**       LANGUAGE        =
*      TABLES
*        costcenterlist  = lt_input
*        return          = lt_return
*        extensionin     = lt_extensionin
**       EXTENSIONOUT    =
*      .
*    IF lt_return[] IS NOT INITIAL.
*      APPEND LINES OF lt_return TO ct_return.
*    ENDIF.
*  ENDLOOP.

ENDFORM.

FORM f_update_data_table USING u_alv TYPE zscmt0001.

  DATA:ls_0001 LIKE zscmt0001."临时表-已保存
  DATA:ls_0010 LIKE zscmt0010."正式表-已提交
  DATA:ls_0010_log LIKE zscmt0010_log."日志表-成功才存储

*----批导程序的时候可能会不写0001临时表
  IF u_alv-zwhno IS NOT INITIAL.
    SELECT SINGLE * FROM zscmt0001 INTO ls_0001
     WHERE partner = u_alv-partner
       AND bukrs = u_alv-bukrs.
    IF sy-subrc = 0.
      DELETE zscmt0001 FROM ls_0001.
    ENDIF.
  ENDIF.

  IF u_alv-erdat IS INITIAL.
    u_alv-erdat = sy-datum.
    u_alv-erzet = sy-uzeit.
    u_alv-ernam = sy-uname.
  ENDIF.

  IF u_alv-modat IS INITIAL.
    u_alv-modat = sy-datum.
    u_alv-mozet = sy-uzeit.
    u_alv-monam = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING u_alv TO ls_0010.
  MOVE-CORRESPONDING u_alv TO ls_0010_log.



  MODIFY zscmt0010 FROM ls_0010.
  MODIFY zscmt0010_log FROM ls_0010_log.

  PERFORM set_others_org USING ls_0010.

  COMMIT WORK AND WAIT .

  u_alv-zbp_status = 'B'.

ENDFORM.

FORM set_others_org USING us_0010 STRUCTURE zscmt0010.
  DATA:ls_SCMS000 TYPE zscms000.
  SELECT * INTO TABLE @DATA(lt_scmt0010)
  FROM zscmt0010
  WHERE partner = @us_0010-partner
  AND bukrs <> @us_0010-bukrs.
  IF sy-subrc EQ 0.
    LOOP AT lt_scmt0010 INTO DATA(ls_scmt0010).
      MOVE-CORRESPONDING us_0010 TO ls_SCMS000.
      MOVE-CORRESPONDING ls_SCMS000 TO    ls_scmt0010.                                         .
      ls_scmt0010-name1         = us_0010-name1         .
      ls_scmt0010-name2         = us_0010-name2         .
      ls_scmt0010-bu_group      = us_0010-bu_group      .
      ls_scmt0010-brand_id      = us_0010-brand_id      .
      ls_scmt0010-bu_sort1_txt  = us_0010-bu_sort1_txt  .
      ls_scmt0010-post_code1    = us_0010-post_code1    .
      ls_scmt0010-street        = us_0010-street        .
      ls_scmt0010-city1         = us_0010-city1         .
      ls_scmt0010-country       = us_0010-country       .
      ls_scmt0010-langu         = us_0010-langu         .
      MODIFY lt_scmt0010 FROM ls_scmt0010.
    ENDLOOP.
    MODIFY zscmt0010 FROM TABLE lt_scmt0010.
  ENDIF.

ENDFORM.

FORM send_idoc USING u_alv TYPE zscmt0001.

  DATA:lv_task.

  lv_task = u_alv-partner.
*  RANGES:
*           rs_part     FOR zscmt0001-partner,
*           rs_bug      FOR zscmt0001-bu_group,
*           rs_erdat    FOR zscmt0001-erdat,
*           rs_modat    FOR zscmt0001-modat.
*
*  CHECK u_alv-partner IS NOT INITIAL.
*
*  rs_part-sign = 'I'.
*  rs_part-option = 'EQ'.
*  rs_part-low = u_alv-partner.
*  APPEND rs_part.
*
*  SUBMIT zscmif0001 WITH s_part  IN rs_part
*                    WITH s_bug   IN rs_bug
*                    WITH s_erdat IN rs_erdat
*                    WITH s_modat IN rs_modat
*                    AND RETURN.

  CALL FUNCTION 'ZSCM_BP_SEND' STARTING NEW TASK lv_task
    EXPORTING
      iv_partner = u_alv-partner
      iv_bukrs   = u_alv-bukrs.

ENDFORM.

FORM modify_vbund USING u_alv TYPE zscmt0001.
  CHECK u_alv-bu_group = 'A001'.
  UPDATE kna1 SET vbund = u_alv-partner+4(6)
   WHERE kunnr = u_alv-partner.
  UPDATE lfa1 SET vbund = u_alv-partner+4(6)
   WHERE lifnr = u_alv-partner.
  COMMIT WORK AND WAIT.
ENDFORM.
