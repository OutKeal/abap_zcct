FUNCTION ZCCT_SD_POS_BL_READ_DOCUMENTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJTYPE) LIKE  SWOTRTIME-OBJTYPE
*"     VALUE(KEY) TYPE  WPUSA_DOC_KEY
*"     VALUE(LEVEL) TYPE  N
*"     VALUE(SEGNUM) LIKE  EDIDD-SEGNUM
*"     VALUE(SNDPRN) LIKE  EDIDC-SNDPRN
*"  TABLES
*"      T_FOLDOC TYPE  WPUSA_T_FOLDOC
*"----------------------------------------------------------------------
*{   INSERT         LNDK905316                                        1

  DATA: BEGIN OF L_T_ACC_DOC OCCURS 0.
          INCLUDE STRUCTURE ACC_DOC.
  DATA: END OF L_T_ACC_DOC.
  DATA: L_KUNNR      LIKE T001W-KUNNR.

  DATA: L_YEAR(4).
  STATICS: L_T000    LIKE T000.
  STATICS: l_vbrk    type vbrk.
  DATA: l_awref      type awref.
  DATA: l_awsys      type awsys.
  DATA: l_awtyp   TYPE awtyp.
  DATA: l_aworg   TYPE aworg.
  data: l_s_bseg  type bseg.
  data: l_s_object type BORIDENT.
  DATA: l_t_neighbors   TYPE table of neighbor.
  data: l_t_posted_docs type table of neighbor.
  data: l_t_fidcc2_docs type table of neighbor.
  DATA: lv_linked_object TYPE swo_typeid.
  DATA: lv_level       TYPE n LENGTH 2.

  DATA: l_upload_key TYPE wpusa_uploadkey.
  DATA: l_dialog     LIKE boole.
  DATA: l_kokrs      TYPE kokrs.
  DATA: l_erkrs      TYPE erkrs.
  DATA: l_vbeln      TYPE vbeln.
  data: l_s_tvko     type tvko.
  DATA: l_s_vbrk     TYPE vbrk.
  DATA: l_belnr      LIKE bkpf-belnr.
  DATA: l_copa_read  TYPE c.
  DATA: l_awtyp_len  TYPE i.
  DATA: l_awref_len  TYPE i.
  DATA: l_erkrs_len  TYPE i.
  DATA: l_offset     TYPE i.
  DATA: l_bukrs_len  TYPE i.
  DATA: l_belnr_len  TYPE i.
  DATA: l_gjahr_len  TYPE i.
  DATA: l_kokrs_len  TYPE i.

  CONSTANTS:
        l_c_incoming_idoc LIKE breltyp-reltype  VALUE 'IDC1',
        l_c_billing_doc   LIKE borident-objtype VALUE 'VBRK',
        l_c_idoc          like borident-objtype value 'IDOC',
        l_c_outgoing_doc  type ROLETYPE value 'OUTBELEG'.

  FIELD-SYMBOLS:
        <l_s_neighbor> TYPE neighbor,
        <l_s_posted_docs> type neighbor,
        <l_s_fidcc2_docs> type neighbor.

* Determine key field lengths for reference key generation
  DESCRIBE FIELD l_awtyp LENGTH l_awtyp_len IN CHARACTER MODE.
  DESCRIBE FIELD l_awref LENGTH l_awref_len IN CHARACTER MODE.
  DESCRIBE FIELD l_erkrs LENGTH l_erkrs_len IN CHARACTER MODE.
  DESCRIBE FIELD l_t_acc_doc-bukrs LENGTH l_bukrs_len IN CHARACTER MODE.
  DESCRIBE FIELD l_s_bseg-belnr LENGTH l_belnr_len IN CHARACTER MODE.
  DESCRIBE FIELD l_s_bseg-gjahr LENGTH l_gjahr_len IN CHARACTER MODE.
  DESCRIBE FIELD l_s_bseg-kokrs LENGTH l_kokrs_len IN CHARACTER MODE.


  L_YEAR = SY-DATUM.
  l_kunnr = sndprn.


  IF L_T000-MANDT NE SY-MANDT.
    SELECT SINGLE * FROM T000
      INTO L_T000
      WHERE MANDT EQ SY-MANDT.
  ENDIF.

  L_AWSYS = L_T000-LOGSYS.

  IF OBJTYPE EQ C_OBJTYPE_MAT_DOC.
    L_AWTYP = 'MKPF'.
    L_AWREF = KEY.
    L_AWORG = L_YEAR.
  ELSE.
    L_AWTYP = 'VBRK'.
    L_AWREF = KEY.
    CLEAR L_AWORG.
    l_vbeln = key.

    call function 'OI0_SD_VBRK_READ_DB'
      exporting
        iv_vbeln        = l_vbeln
      importing
        es_vbrk         = l_s_vbrk
      exceptions
        not_found       = 1
        others          = 2.
              .

    call function 'TVKO_SINGLE_READ'
      exporting
        vkorg          = l_s_vbrk-vkorg
      importing
        wtvko          = l_s_tvko
      exceptions
        not_found      = 1
        others         = 2.

  ENDIF.

  L_DIALOG = ' '.

  CALL FUNCTION 'AC_DOCUMENT_RECORD'
    EXPORTING
      I_AWTYP      = L_AWTYP
      I_AWREF      = L_AWREF
      I_AWORG      = L_AWORG
      i_bukrs      = l_s_tvko-bukrs
      I_AWSYS      = L_AWSYS
      X_DIALOG     = L_DIALOG
    TABLES
      T_DOCUMENTS  = L_T_ACC_DOC
    EXCEPTIONS
      NO_REFERENCE = 1
      NO_DOCUMENT  = 2
      OTHERS       = 3.

    LOOP AT l_t_acc_doc.

      CASE l_t_acc_doc-awtyp.

        WHEN 'BKPF'.
          t_foldoc-objtype = 'BKPF'.
          CLEAR t_foldoc-key.
          t_foldoc-key(l_bukrs_len) = l_t_acc_doc-bukrs.
          t_foldoc-key+l_bukrs_len(l_belnr_len) = l_t_acc_doc-docnr.
          l_offset = l_bukrs_len + l_belnr_len.
          t_foldoc-key+l_offset(l_gjahr_len) = l_t_acc_doc-ac_gjahr.

        WHEN 'COBK'.
          if not l_s_tvko-bukrs is initial
             and ( l_t_acc_doc-bukrs <> l_s_tvko-bukrs ).
            continue.
          endif.
          t_foldoc-objtype = 'BUS2072'.
          CLEAR t_foldoc-key.

          CALL FUNCTION 'RK_KOKRS_FIND'
            EXPORTING
              bukrs                  = l_t_acc_doc-bukrs
            IMPORTING
              kokrs                  = l_kokrs
            EXCEPTIONS
              assignment_not_allowed = 1
              insufficient_input     = 2
              no_kokrs_assigned      = 3
              no_kokrs_for_bukrs     = 4
              no_kokrs_for_bu_gb     = 5
              wrong_kokrs_for_bukrs  = 6
              wrong_kokrs_for_bu_gb  = 7
              OTHERS                 = 8.

          t_foldoc-key(l_kokrs_len) = l_kokrs.
          t_foldoc-key+l_kokrs_len(l_belnr_len) = l_t_acc_doc-docnr.

        WHEN 'COPA'.
*         Display only one CO-PA document in the main screen
          IF l_copa_read IS INITIAL.
            t_foldoc-objtype = '/ATU/RCOPA'.
            CLEAR t_foldoc-key.

*           Get the Operatin Concern for the company code
            CALL FUNCTION 'COPA_ERKRS_FIND'
              EXPORTING
                bukrs              = l_t_acc_doc-bukrs
              IMPORTING
                erkrs              = l_erkrs
              EXCEPTIONS
                error_kokrs_find   = 1
                kokrs_wrong        = 2
                no_erkrs_defined   = 3
                no_erkrs_for_kokrs = 4
                OTHERS             = 5.

            IF sy-subrc = 0.
*             Create the reference key for BO '/ATU/RCOPA'
              l_offset = l_erkrs_len + l_awtyp_len.

              t_foldoc-key(l_erkrs_len) = l_erkrs.
              t_foldoc-key+l_erkrs_len(l_awtyp_len) = l_c_billing_doc.
              t_foldoc-key+l_offset(l_awref_len) = l_awref.

              l_copa_read = 'X'.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.

        WHEN 'COPCA'.
*         Create the reference key for BO '/ATU/COPCA'
          CLEAR t_foldoc-key.
          t_foldoc-objtype = '/ATU/COPCA'.
          t_foldoc-key(l_awtyp_len) = l_c_billing_doc.
          t_foldoc-key+l_awtyp_len(l_awref_len) = l_awref.

        WHEN 'FILC'.
          t_foldoc-objtype = 'BUS1026'.
          t_foldoc-key = l_t_acc_doc-docnr.

        WHEN 'GLX'.
          t_foldoc-objtype = 'BUS1021'.
          t_foldoc-key = l_t_acc_doc-docnr.

      ENDCASE.

*      t_foldoc-uploadkey = l_upload_key.
      T_FOLDOC-LEVEL     = '01'.
      T_FOLDOC-SEGNUM    = segnum.
      APPEND t_foldoc.

    ENDLOOP.


  l_s_object-objtype = l_c_billing_doc.
  l_s_object-objkey  = key.

  CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
    EXPORTING
      object               = l_s_object
      roletype             = l_c_outgoing_doc
    TABLES
      neighbors            = l_t_neighbors
    EXCEPTIONS
      internal_error       = 1
      no_logsys            = 2
      OTHERS               = 3.

 loop at l_t_neighbors assigning <l_s_neighbor>.
   l_s_object-objtype = <l_s_neighbor>-objtype.
   l_s_object-objkey  = <l_s_neighbor>-objkey.
   t_foldoc-objtype   = <l_s_neighbor>-objtype.
   t_foldoc-key       = <l_s_neighbor>-objkey.
   t_foldoc-level     = '01'.
   APPEND t_foldoc.

   CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
     EXPORTING
       object               = l_s_object
       relationtype         = l_c_incoming_idoc
     TABLES
       neighbors            = l_t_posted_docs
     EXCEPTIONS
       internal_error       = 1
       no_logsys            = 2
       OTHERS               = 3.

   loop at l_t_posted_docs assigning <l_s_posted_docs>.
      t_foldoc-objtype = <l_s_posted_docs>-objtype.
      t_foldoc-key     = <l_s_posted_docs>-objkey.
      t_foldoc-level   = '02'.
      append t_foldoc.
      if <l_s_posted_docs>-objtype = 'IDOC'.
        l_s_object-objtype = <l_s_posted_docs>-objtype.
        l_s_object-objkey  = <l_s_posted_docs>-objkey.
        CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
          EXPORTING
            object               = l_s_object
            relationtype         = l_c_incoming_idoc
          TABLES
            neighbors            = l_t_fidcc2_docs
          EXCEPTIONS
            internal_error       = 1
            no_logsys            = 2
            OTHERS               = 3.

          loop at l_t_fidcc2_docs assigning <l_s_fidcc2_docs>.
            read table t_foldoc transporting no fields with key objtype = <l_s_fidcc2_docs>-objtype
                                                                key     = <l_s_fidcc2_docs>-objkey.
            if sy-subrc = 0.
              continue.
            else.
              t_foldoc-objtype = <l_s_fidcc2_docs>-objtype.
              t_foldoc-key     = <l_s_fidcc2_docs>-objkey.
              t_foldoc-level   = '03'.
              append t_foldoc.
            endif.
         endloop.
      endif.
   endloop.

 endloop.
*}   INSERT
ENDFUNCTION.
