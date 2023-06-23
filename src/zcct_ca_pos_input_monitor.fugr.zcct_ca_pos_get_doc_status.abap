FUNCTION zcct_ca_pos_get_doc_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOCNUM) LIKE  EDIDC-DOCNUM
*"     VALUE(SEGNUM) LIKE  EDIDD-SEGNUM OPTIONAL
*"     VALUE(SEGNUM_END) LIKE  EDIDD-SEGNUM OPTIONAL
*"     VALUE(MESTYP) LIKE  EDIDC-MESTYP
*"     VALUE(HISTORY_FLAG) TYPE  C OPTIONAL
*"     VALUE(INBOUND) TYPE  C OPTIONAL
*"     VALUE(SNDPRN) LIKE  EDIDC-SNDPRN OPTIONAL
*"  EXPORTING
*"     VALUE(STATUS) LIKE  POSWPSA-STATUS
*"     VALUE(STATUS_DISP) LIKE  POSWPSA-STATUS
*"     VALUE(VERARBEND) LIKE  WPTST-VERARBEND
*"     VALUE(VERBUCHUNG) LIKE  WPTST-VERBUCHUNG
*"     VALUE(POSTED) LIKE  POSWPSA-POSTED
*"  TABLES
*"      DOC_STATUS TYPE  WPUSA_T_DOC_STATUS OPTIONAL
*"      DOC_HISTORY TYPE  WPUSA_T_DOC_HISTORY OPTIONAL
*"      DOC_REASON TYPE  WPUSA_T_DOC_REASON OPTIONAL
*"      DOC_CHANGES TYPE  WPUSA_T_DOC_CHANGES OPTIONAL
*"      T_EDIDS TYPE  WPUSA_T_EDIDS OPTIONAL
*"      T_WPTST TYPE  WPUSA_T_WPTST OPTIONAL
*"      T_WPLST TYPE  WPUSA_T_WPLST OPTIONAL
*"      T_EXTDOC TYPE  WPUSA_T_EXTDOC OPTIONAL
*"      T_UPLDOC TYPE  WPUSA_T_UPLDOC OPTIONAL
*"      T_FOLDOC TYPE  WPUSA_T_FOLDOC OPTIONAL
*"  EXCEPTIONS
*"      IDOC_NOT_EXIST
*"      FOREIGN_LOCK
*"      UNKNOWN_EXCEPTION
*"      RANGE_NOT_EXIST
*"----------------------------------------------------------------------

  DATA: t_int_status TYPE type_t_int_status WITH HEADER LINE,
        t_contob     LIKE sww_contob       OCCURS 1 WITH HEADER LINE,
        idoc_status  LIKE edidc-status.
  CLEAR: status.
  REFRESH: doc_status, doc_reason, t_edids, t_wptst, t_wplst,
           t_extdoc, t_upldoc, t_foldoc.

  CALL FUNCTION 'ZCCT_CA_POS_GET_MSGTYP_PARAMS'
    EXPORTING
      mestyp = mestyp
      sndprn = sndprn
    IMPORTING
      msgpro = g_msgpro.

* Read all necessary information for status determination and compile
* it in the internal status table T_INT_STATUS
  PERFORM f05_read_upldoc TABLES t_int_status t_upldoc t_contob
       USING docnum.
  PERFORM f05_read_wptst  TABLES t_int_status t_wptst
       USING docnum CHANGING verarbend.
  PERFORM f05_read_edids
       TABLES   t_int_status t_edids
       USING    docnum inbound
       CHANGING idoc_status.
  PERFORM f05_read_wplst  TABLES t_int_status t_wplst USING docnum.

* Analyze internal status information to get the document status
  LOOP AT t_int_status.
    PERFORM f05_doc_status USING t_int_status CHANGING doc_status.
    APPEND doc_status.
  ENDLOOP.

* Write status change history
  IF NOT ( history_flag IS INITIAL ).
    PERFORM f05_write_history TABLES t_edids t_int_status
          doc_history doc_reason doc_changes
          USING segnum segnum_end.
  ENDIF.

* Fill the tables with the external and follow-on documents
  PERFORM f05_write_doc
       TABLES t_contob doc_status t_upldoc t_extdoc t_foldoc
       USING  docnum g_msgpro-extobjtype verarbend.

* Get status for a single range requested in import parameters
  PERFORM f05_get_single_range_status TABLES   doc_status
                                      USING    inbound
                                               segnum segnum_end
                                      CHANGING status
                                               status_disp
                                               verbuchung
                                               posted.

ENDFUNCTION.


*&---------------------------------------------------------------------*
*&      Form  F05_RAISE
*&---------------------------------------------------------------------*
*       Raise own exceptions depending on exceptions from called
*       function modules. SY_SUBRC contains the exception code.
*----------------------------------------------------------------------*
FORM f05_raise
USING function_module TYPE c.

* Ignore returncodes < 1000; they are handled by the application.
  CHECK sy-subrc >= 1000.

  CASE sy-subrc.
    WHEN 1000.
      RAISE idoc_not_exist.
    WHEN 1001.
      RAISE foreign_lock.
    WHEN 9999.
      MESSAGE ID 'WP' TYPE 'E' NUMBER '752'
           WITH function_module 'POS_SA_GET_DOCUMENT_STATUS'
           RAISING unknown_exception.
  ENDCASE.

ENDFORM.                                                    " F05_RAISE


*&---------------------------------------------------------------------*
*&      Form  F05_CHECK_INT_STATUS
*&---------------------------------------------------------------------*
*       Make shure that the internal status table is filled correctly
*----------------------------------------------------------------------*
*      -->P_T_INT_STATUS  internal status table
*      -->P_DOCNUM        IDoc number
*      -->P_SEGNUM        Range begin
*      -->P_SEGNUM_END    Range end
*      -->P_INBOUND       Inbound processing flag
*----------------------------------------------------------------------*
FORM f05_check_int_status
TABLES   p_t_int_status TYPE type_t_int_status
USING    p_segnum       LIKE edidd-segnum
         p_segnum_end   LIKE edidd-segnum
         p_inbound.

  DATA: l_lines LIKE sy-tabix.

  DESCRIBE TABLE p_t_int_status LINES l_lines.
  IF l_lines = 0.
*   Internal status table is empty
    IF p_inbound IS INITIAL.
*     During normal processing, the status table must not be empty.
*     Either an upload number or a error range must be there.
      MESSAGE ID 'WP' TYPE 'E' NUMBER '768' RAISING range_not_exist.
    ELSE.
*     During preprocessing, the internal status table may be empty
*     during the first processing of the IDoc. In this case, a dummy
*     entry is created.
      CLEAR p_t_int_status.
      p_t_int_status-segnum     = p_segnum.
      p_t_int_status-segnum_end = p_segnum_end.
      APPEND p_t_int_status.
    ENDIF.
  ENDIF.

ENDFORM.                               " F05_CHECK_INT_STATUS


*&---------------------------------------------------------------------*
*&      Form  F05_READ_EDIDS
*&---------------------------------------------------------------------*
*       Read all EDI status records of current IDoc, get status
*       information for current IDoc range.
*----------------------------------------------------------------------*
FORM f05_read_edids
TABLES   t_int_status TYPE type_t_int_status
         t_edids      TYPE wpusa_t_edids
USING    docnum       LIKE edidc-docnum
         inbound
CHANGING idoc_status  LIKE edidc-status.

  DATA: edids_hide TYPE wpusa_edids_hide,
        segnum     LIKE edids-segnum,
        status     LIKE poswpsa-status.

*-----------------------------------------------------------------------
* read all EDI status records
*-----------------------------------------------------------------------

  IF inbound IS INITIAL.
    CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_READ'
      EXPORTING
        document_number         = docnum
        db_read_option          = 'N'
      EXCEPTIONS
        document_foreign_lock   = 1001
        document_not_exist      = 1000
        document_number_invalid = 1000
        OTHERS                  = 9999.
    PERFORM f05_raise USING 'EDI_DOCUMENT_OPEN_FOR_READ'.
  ENDIF.

  CALL FUNCTION 'EDI_DOCUMENT_READ_ALL_STATUS'
    EXPORTING
      document_number        = docnum
    TABLES
      int_edids              = t_edids
    EXCEPTIONS
      no_status_record_found = 2
      OTHERS                 = 9999.
  PERFORM f05_raise USING 'EDI_DOCUMENT_READ_ALL_STATUS'.

* Get current IDoc status
  DESCRIBE TABLE t_edids LINES sy-tabix.
  IF sy-tabix > 0.
    READ TABLE t_edids INDEX sy-tabix.
    idoc_status = t_edids-status.
  ENDIF.

  IF inbound IS INITIAL.
    CALL FUNCTION 'EDI_DOCUMENT_CLOSE_READ'
      EXPORTING
        document_number = docnum
      EXCEPTIONS
        OTHERS          = 9999.
    PERFORM f05_raise USING 'EDI_DOCUMENT_CLOSE_READ'.
  ENDIF.

*-----------------------------------------------------------------------
* Fill internal status table with information from EDIDS
*-----------------------------------------------------------------------
* Prior Release 4.0, the EDI messages were stored in STACOD. To
* simplify processing, they are copied to the new fields.
*{   DELETE         LNDK905093                                        3
*\  LOOP AT t_edids WHERE NOT ( stacod IS INITIAL ).
*\    t_edids-stamqu = t_edids-stacod+0(3).
*\    t_edids-stamid = t_edids-stacod+3(2).
*\    t_edids-stamno = t_edids-stacod+5(3).
*\    MODIFY t_edids.
*\  ENDLOOP.
*}   DELETE

  LOOP AT t_edids WHERE stamqu =  'SAP'
                    AND stamid =  'WP'
                    AND ( stamno(2) = '78' OR
                          stamno    = '791' )
                    AND stamno <> '781'.
*{   DELETE         LNDK905093                                        2
*\    IF t_edids-stamno = '791'.
*\      status = pos_status_process_no_check.
*\    ELSE.
*\*     The EDI status records containing the POS status information
*\*     have message numbers beginning with WP78. The last digit of the
*\*     message number is the POS status.
*\      status = t_edids-stamno+2(1).
*\    ENDIF.
*}   DELETE
*   Parameter 4 is used to keep internal information. The messages
*   WP78* do not use parameter 4.
    edids_hide = t_edids-stapa4.
*   Get range (begin and end segment) of status record
    IF status = pos_status_correct_system.
*     Status corrected by sytem can only occur before the actual inbound
*     processing. These EDIDS records contain only the number of the
*     modified/faulty segment. Luckily, they can be assigned to a
*     range by searching a range that includes this segment.
      LOOP AT t_int_status
           WHERE segnum     <= edids_hide-segnum
             AND segnum_end >= edids_hide-segnum_end.
        PERFORM f05_write_edids_to_int_status
             USING t_edids status CHANGING t_int_status.
        MODIFY t_int_status.
      ENDLOOP.
    ELSE.
*     Only store the newest status in the internal status table
      READ TABLE t_int_status
           WITH KEY segnum     = edids_hide-segnum
                    segnum_end = edids_hide-segnum_end
           BINARY SEARCH.
      IF sy-subrc = 0.
        PERFORM f05_write_edids_to_int_status
             USING t_edids status CHANGING t_int_status.
        MODIFY t_int_status INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Search EDI status record for a record that indicates a change of
* the IDoc by a user through the EDI standard IDoc editor. The external
* document with the changed segment counts as 'changed by user'.

** bi 551650 23.09.02
  LOOP AT t_edids WHERE status = c_idoc_status_copy
                    AND stamqu = 'SAP'
                    AND stamid = 'E0'
                    AND stamno = '184'.
    segnum = t_edids-stapa1.
* Check for exception range first
    LOOP AT t_int_status WHERE countr     <  t_edids-countr
                           AND segnum     <= segnum
                           AND segnum_end >= segnum
                           AND segnum_end <> '999999'.
      t_int_status-status_disp = pos_status_correct_user.
      t_int_status-status      = pos_status_correct_user.
      MODIFY t_int_status.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                               " F05_READ_EDIDS
** ei 551650 23.09.02

** bd 551650 23.09.02
*  loop at t_edids where status = c_idoc_status_copy
*                    and stamqu = 'SAP'
*                    and stamid = 'E0'
*                    and stamno = '184'.
*    segnum = t_edids-stapa1.
*      loop at t_int_status where segnum     <= segnum
*                           and segnum_end >= segnum
*                           and countr    <  t_edids-countr.
*      t_int_status-status_disp = pos_status_correct_user.
*      modify t_int_status.
*      endloop.
*  endloop.
*endform.                               " F05_READ_EDIDS
** ed 551650 23.09.02


*&---------------------------------------------------------------------*
*&      Form  F05_READ_WPTST
*&---------------------------------------------------------------------*
*       Read all entrys from WPTST for the current IDoc and add this
*       information to the internal status table.
*----------------------------------------------------------------------*
FORM f05_read_wptst
TABLES   t_int_status TYPE type_t_int_status
         t_wptst      TYPE wpusa_t_wptst
USING    docnum       LIKE edidc-docnum
CHANGING verarbend    LIKE wptst-verarbend.

  DATA: lv_del_default_status TYPE xfeld.

* Read all WPTST entrys
  SELECT * FROM wptst INTO TABLE t_wptst WHERE docnum = docnum.
  SORT t_wptst BY rngbegin rngend.

* Get last processed segment number
  READ TABLE t_wptst INDEX 1.
  IF sy-subrc = 0 AND t_wptst-rngbegin = 0 AND t_wptst-rngend = 0.
    verarbend = t_wptst-verarbend.
    DELETE t_wptst INDEX sy-tabix.
  ELSE.
    verarbend = 999999.
  ENDIF.

  LOOP AT t_wptst.
*   wptst status found. Remove default status filling.
    lv_del_default_status = 'X'.
*   Add posting state 'VERBUCHUNG' to internal status table
    READ TABLE t_int_status
         WITH KEY segnum     = t_wptst-rngbegin
                  segnum_end = t_wptst-rngend   BINARY SEARCH.
    t_int_status-verbuchung = t_wptst-verbuchung.
    t_int_status-wptst_flag = 'X'.
    IF sy-subrc = 0.
      MODIFY t_int_status INDEX sy-tabix.
    ELSE.
      t_int_status-segnum     = t_wptst-rngbegin.
      t_int_status-segnum_end = t_wptst-rngend.
      INSERT t_int_status INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  IF lv_del_default_status = 'X'.
    DELETE t_int_status WHERE segnum     = 1
                          AND segnum_end = 999999
                          AND wptst_flag = space
                          AND status     = space.

  ENDIF.
ENDFORM.                               " F05_READ_WPTST


*&---------------------------------------------------------------------*
*&      Form  F05_READ_WPLST
*&---------------------------------------------------------------------*
*       Read all error messages of current IDoc from WPLST and add
*       this information to the internal status table
*----------------------------------------------------------------------*
FORM f05_read_wplst
TABLES   t_int_status TYPE type_t_int_status
         t_wplst      TYPE wpusa_t_wplst
USING    docnum       LIKE edidc-docnum.

  SELECT * FROM wplst INTO TABLE t_wplst WHERE docnum = docnum
       ORDER BY docnum segnum.

  LOOP AT t_wplst.
    IF t_wplst-msgid = 'WP' AND t_wplst-msgnr = '751'.
*     The message WP751 is created when the status of an IDoc range is
*     changed to 'reversed' or 'changed by user' together with the
*     corresponding WPTST entry. This message gets deleted during the
*     next inbound processing. Thus, if the message is still there, this
*     means that the reverse/change is not processed, it is still
*     planned.
      LOOP AT t_int_status WHERE segnum     <= t_wplst-segnum
                             AND segnum_end >= t_wplst-segnum.
        t_int_status-planned = 'X'.
        MODIFY t_int_status.
      ENDLOOP.
    ENDIF.
    IF t_wplst-msgid = 'WP' AND t_wplst-msgnr = '613'.
*     The message WP613 is is in WPLST as long as there is at least
*     one workitem for the IDoc. It is valid for the whole IDoc.
*     Thus, all T_INT_STATUS need to be marked.
      t_int_status-workitem = 'X'.
      MODIFY t_int_status TRANSPORTING workitem WHERE workitem <> 'X'.
    ENDIF.
    IF t_wplst-fehlertyp CA 'FJ'.
*     The message type F/J are created when the IDoc range was processed
*     but workitems were created. If the workitems were processed,
*     these messages get deleted. As long as there are workitems,
*     the range gets the status 'Workitem created' instead of 'Ok'.
      LOOP AT t_int_status WHERE segnum     <= t_wplst-segnum
                             AND segnum_end >= t_wplst-segnum.
        t_int_status-workitem = 'X'.
        MODIFY t_int_status.
      ENDLOOP.
    ENDIF.

    IF t_wplst-fehlertyp = 'E'.
      LOOP AT t_int_status WHERE segnum     <= t_wplst-segnum
                            AND segnum_end >= t_wplst-segnum.
        t_int_status-status = '1'.
        MODIFY t_int_status.
      ENDLOOP.
    ENDIF.
  ENDLOOP.



ENDFORM.                               " F05_READ_WPLST


*&---------------------------------------------------------------------*
*&      Form  F05_DOC_STATUS
*&---------------------------------------------------------------------*
*       Analyse the information stored in the internal status table
*       and write the POS document status.
*----------------------------------------------------------------------*
FORM f05_doc_status
USING    int_status TYPE type_int_status
CHANGING doc_status TYPE wpusa_doc_status.

  CLEAR doc_status.
  MOVE-CORRESPONDING int_status TO doc_status.

*-----------------------------------------------------------------------
* Get POS status of document
*-----------------------------------------------------------------------

  IF int_status-wptst_flag = 'X'.
*   There is a WPTST entry for the IDoc range
    doc_status-verbuchung = int_status-verbuchung.
    CASE int_status-verbuchung.
      WHEN '2' OR '3'.
*       IDoc range has been rejected.
*       It gets the POS status 'rejected by user' or 'resubmit' only if
*       there is a EDIDS entry. In all other cases, the POS status is
*       'rejected by system' because the parser only looks at the
*       posting status in WPTST.
        IF int_status-status IS INITIAL.
          doc_status-status = pos_status_reject_system.
        ELSE.
          doc_status-status = int_status-status.
        ENDIF.

      WHEN '1'.
*       IDoc range has been reversed.
        IF int_status-planned = 'X'.
*         The reversal has not been processed yet
          doc_status-status = pos_status_reverse_planned.
        ELSE.
*         The message indicating that the reversal is planned is gone,
*         but the WPTST entry is still there. This happens if there is
*         an error during the inbound processing of the reversal.
          doc_status-status = pos_status_error.
        ENDIF.

      WHEN '4'.
*       IDoc range has to be processed again, with no checks
        doc_status-status = pos_status_process_no_check.

      WHEN OTHERS.
*       IDoc range has to be processed again
        IF int_status-planned = 'X'.
*         The range has to be processed again because it was changed
          doc_status-status = pos_status_process_planned.
        ELSE.
*         There is no message indicating a planned change, but the
*         WPTST entry is still there. This means that there was an
*         error during the inbound processing.
          doc_status-status = pos_status_error.
        ENDIF.
    ENDCASE.

  ELSE.

*   There is no WPTST entry for the IDoc range. In this case, the
*   POS status can be taken from EDIDS. If there is no EDIDS record,
*   this means that the range is ok.
    IF int_status-status IS INITIAL OR
       int_status-status CA pos_status_process_all.
      doc_status-status = pos_status_ok.
    ELSE.
      doc_status-status = int_status-status.
    ENDIF.

*   If there are workitems for a processed IDoc, set status to
*   'Workitem created'
    IF ( doc_status-status = pos_status_ok OR
         doc_status-status = pos_status_reversed )
       AND int_status-workitem = 'X'.
      doc_status-status = pos_status_workitem_created.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* Get posted status of document
*-----------------------------------------------------------------------
** bd 551650 18.09.02
*  if int_status-wptst_flag = 'X'.
*    if int_status-verbuchung ca '13'.
*      doc_status-posted = 'X'.
*    endif.
*  else.
*    if doc_status-status = pos_status_ok.
*      doc_status-posted = 'X'.
*    endif.
*  endif.
** ed 551650 18.09.02

** BD 773592 29.09.04
** bi 551650 18.09.02
** Both "pos_status_ok( = '9')" and "pos_status_correct_user( = '8')"
** must be taken into account to get the right status display
*  IF int_status-wptst_flag = 'X'.
*    IF int_status-verbuchung CA '13'.
*      doc_status-posted = 'X'.
*    ENDIF.
*  ELSE.
*    IF ( doc_status-status = pos_status_ok
*         OR ( doc_status-status = pos_status_correct_user ) ).
*      doc_status-posted = 'X'.
*    ENDIF.
*  ENDIF.
** ei 551650 18.09.02
** ED 773592 29.09.04

* BI 773592 29.09.04
* Both "pos_status_ok( = '9')" and "pos_status_correct_user( = '8')"
* must be taken into account to get the right status display and the
* fact that there could be an e1wpg01 segment which had been already
* posted but relapsed to status 1 during a cancellation ...
  IF int_status-wptst_flag = 'X'.
    IF ( int_status-verbuchung CA '13'
         AND doc_status-status <> pos_status_error ).
      doc_status-posted = 'X'.
    ENDIF.
  ELSE.
    IF ( doc_status-status = pos_status_ok
         OR ( doc_status-status = pos_status_correct_user ) ).
      doc_status-posted = 'X'.
    ENDIF.
  ENDIF.
* EI 773592 29.09.04

*-----------------------------------------------------------------------
* Get display POS status
*-----------------------------------------------------------------------
* The display status only differs from the current status if the
* current status is 'ok' and the display status is filled. In all
* other cases, the current status is used as display status.
  IF doc_status-status = pos_status_ok AND
     NOT ( int_status-status_disp IS INITIAL ).
    doc_status-status_disp = int_status-status_disp.
  ELSE.
    doc_status-status_disp = doc_status-status.
  ENDIF.

ENDFORM.                               " F05_DOC_STATUS


*&---------------------------------------------------------------------*
*&      Form  F05_READ_UPLDOC
*&---------------------------------------------------------------------*
*       Read upload documents that were written to the BOR
*----------------------------------------------------------------------*
FORM f05_read_upldoc
TABLES t_int_status TYPE      type_t_int_status " <->
       t_upldoc     TYPE      wpusa_t_upldoc " -->
*BD644889
*       t_contob     structure neighbor
*ED644889
*BI644889
       t_contob     TYPE      tt_sww_contob
*EI644889
  USING  docnum       LIKE      edidc-docnum.

  STATICS idoc_obj LIKE borident.

  DATA: upldoc_element TYPE wpusa_upldoc_element.
  DATA: extdoc_element TYPE wpusa_extdoc_element.
*BD644889
* data: t_contob_old   type table of sww_contob with header line.
*ED644889
*BI644889
  DATA: t_contob_new LIKE neighbor OCCURS 0 WITH HEADER LINE.
*EI644889

* Indicates if the default range (1-999999) needs to be created.
  DATA  create_default_range VALUE 'X'.

* read external documents and follow-on documents from container
  idoc_obj-objtype = c_objtype_idoc.
  idoc_obj-objkey  = docnum.

* read document links
*BD644889
*  call function 'SREL_GET_NEXT_NEIGHBORS'
*    EXPORTING
*      object    = idoc_obj
*    TABLES
*      neighbors = t_contob
*    EXCEPTIONS
*      others    = 3.
*ED644889
*BI644889
  REFRESH t_contob.

  CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
    EXPORTING
      object    = idoc_obj
    TABLES
      neighbors = t_contob_new
    EXCEPTIONS
      OTHERS    = 3.
*EI644889
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING unknown_exception.
  ENDIF.

* fill ouptput tables

* Delete object from other logical systems, the same IDoc number
* might have been used there too.
*BD644889
* delete t_contob where logsys <> idoc_obj-logsys.
*ED644889
*BI644889
  DELETE t_contob_new WHERE logsys <> idoc_obj-logsys.
*EI644889

*BD644889
*  loop at t_contob where objtype = c_objtype_posuploaddoc.
**   write upload document
*    clear t_upldoc.
*    upldoc_element      = t_contob-objkey+38(32).
*    t_upldoc-segnum     = upldoc_element-segnum.
*    t_upldoc-segnum_end = upldoc_element-segnum_end.
*    t_upldoc-uploadkey  = t_contob-objkey.
*    t_upldoc-attr       = upldoc_element-attr.
*    append t_upldoc.
*    if g_msgpro-type = 'N' and g_msgpro-extobjtype is initial.
**     Use upload documents to fill internal status table only if there
**     are no external documents available for a splittable IDoc
*      clear create_default_range.
**     fill internal status table
*      read table t_int_status
*           with key segnum     = t_upldoc-segnum
*                    segnum_end = t_upldoc-segnum_end   binary search.
*      if sy-subrc <> 0.
*        t_int_status-segnum     = t_upldoc-segnum.
*        t_int_status-segnum_end = t_upldoc-segnum_end.
*        insert t_int_status index sy-tabix.
*      endif.
*    endif.
*  endloop.
*ED644889
*BI644889
  LOOP AT t_contob_new WHERE objtype = c_objtype_posuploaddoc.
*   write upload document
    CLEAR t_upldoc.
    upldoc_element      = t_contob_new-objkey+38(32).
    t_upldoc-segnum     = upldoc_element-segnum.
    t_upldoc-segnum_end = upldoc_element-segnum_end.
    t_upldoc-uploadkey  = t_contob_new-objkey.
    t_upldoc-attr       = upldoc_element-attr.
    APPEND t_upldoc.
    IF g_msgpro-type = 'N' AND g_msgpro-extobjtype IS INITIAL.
*     Use upload documents to fill internal status table only if there
*     are no external documents available for a splittable IDoc
      CLEAR create_default_range.
*     fill internal status table
      READ TABLE t_int_status
           WITH KEY segnum     = t_upldoc-segnum
                    segnum_end = t_upldoc-segnum_end   BINARY SEARCH.
      IF sy-subrc <> 0.
        t_int_status-segnum     = t_upldoc-segnum.
        t_int_status-segnum_end = t_upldoc-segnum_end.
        INSERT t_int_status INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
*EI644889


* In case there are no entries using binary relation, check SWW_CONTOB
* for old object links instead
  IF sy-subrc <> 0.
*BD644889
*    perform f05_read_upldoc_old tables t_int_status
*                                       t_upldoc
*                                       t_contob_old
*                                using  docnum.
*ED644889
*BI644889
    PERFORM f05_read_upldoc_old TABLES t_int_status
                                       t_upldoc
                                       t_contob
                                USING  docnum.
*{   DELETE         LNDK905093                                        1
*\    EXIT.
*}   DELETE
*EI644889
*BD644889
*    refresh t_contob.
*    loop at t_contob_old.
*      t_contob-objtype = t_contob_old-objtype.
*      t_contob-objkey  = t_contob_old-objkey.
*      append t_contob.
*    endloop.
*    exit.
*ED644889
  ENDIF.


  SORT t_upldoc BY uploadkey-uploadnr.

  CLEAR t_int_status.
  IF g_msgpro-type = 'N' AND NOT ( g_msgpro-extobjtype IS INITIAL ).
*   If external documents are available for splittable IDocs, use
*   them instead of the upload documents to fill the internal
*   status table. This is necessarry because for the compressed
*   WPUBON, upload ranges and external document ranges differ.
*   Because the follow-on documents use the ranges defined by the
*   external documents, this range must be used.
*BD644889
*    loop at t_contob where objtype = g_msgpro-extobjtype.
*      clear create_default_range.
**     Write external document to t_int_status
*      extdoc_element = t_contob-objkey+38(32).
**     fill internal status table
*      read table t_int_status
*           with key segnum     = extdoc_element-segnum
*                    segnum_end = extdoc_element-segnum_end
*           binary search.
*      if sy-subrc <> 0.
*        t_int_status-segnum     = extdoc_element-segnum.
*        t_int_status-segnum_end = extdoc_element-segnum_end.
*        insert t_int_status index sy-tabix.
*      endif.
*    endloop.
*ED644889
*BI644889
    LOOP AT t_contob_new WHERE objtype = g_msgpro-extobjtype.
      CLEAR create_default_range.
*     Write external document to t_int_status
      extdoc_element = t_contob_new-objkey+38(32).
*     fill internal status table
      READ TABLE t_int_status
           WITH KEY segnum     = extdoc_element-segnum
                    segnum_end = extdoc_element-segnum_end
           BINARY SEARCH.
      IF sy-subrc <> 0.
        t_int_status-segnum     = extdoc_element-segnum.
        t_int_status-segnum_end = extdoc_element-segnum_end.
        INSERT t_int_status INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
*EI644889
  ENDIF.

  IF create_default_range = 'X'.
*   Create the default range because no other range already exists.
*   This always happens with splittable IDocs or if something goes
*   wrong with non-splittable IDocs.
    READ TABLE t_int_status
         WITH KEY segnum     = 1
                  segnum_end = 999999
         BINARY SEARCH.
    IF sy-subrc <> 0.
      t_int_status-segnum     = 1.
      t_int_status-segnum_end = 999999.
      INSERT t_int_status INDEX sy-tabix.
    ENDIF.
  ENDIF.

*BI644889
  LOOP AT t_contob_new.
    t_contob-objtype = t_contob_new-objtype.
    t_contob-objkey  = t_contob_new-objkey(38).
    t_contob-element = t_contob_new-objkey+38(32).
* do not show double documents because relationship
    IF t_contob-element IS INITIAL.
      CONTINUE.
    ENDIF.
    APPEND t_contob.
  ENDLOOP.
*EI644889

ENDFORM.                               " F05_READ_UPLDOC


*&---------------------------------------------------------------------*
*&      Form  F05_READ_UPLDOC_OLD
*&---------------------------------------------------------------------*
*       Read upload documents that were written to the BOR
*----------------------------------------------------------------------*
FORM f05_read_upldoc_old
TABLES t_int_status TYPE      type_t_int_status " <->
       t_upldoc     TYPE      wpusa_t_upldoc " -->
       t_contob     STRUCTURE sww_contob " -->
USING  docnum       LIKE      edidc-docnum.

  STATICS idoc_obj LIKE swotobjid.

  DATA: upldoc_element TYPE wpusa_upldoc_element.
  DATA: l_t_contob     TYPE SORTED TABLE OF sww_contob
                       WITH UNIQUE KEY wi_id.
  DATA: extdoc_element TYPE wpusa_extdoc_element.
* Indicates if the default range (1-999999) needs to be created.
  DATA  create_default_range VALUE 'X'.

* read external documents and follow-on documents from container
  idoc_obj-objtype = c_objtype_idoc.
  idoc_obj-objkey  = docnum.
  IF idoc_obj-logsys IS INITIAL.
    SELECT SINGLE * FROM t000 WHERE mandt = sy-mandt.
    idoc_obj-logsys = t000-logsys.
  ENDIF.

* read document links from BOR
  CALL FUNCTION 'SWW_WI_READ_CONTAINERS_OF_OBJ'
    EXPORTING
      object                    = idoc_obj
    TABLES
      related_container_objects = t_contob
    EXCEPTIONS
      object_not_found          = 1
      OTHERS                    = 2.

* fill ouptput tables

  IF sy-subrc = 0.
*   Delete object from other logical systems, the same IDoc number
*   might have been used there too.
    DELETE t_contob WHERE logsys <> idoc_obj-logsys.
*   Ignore all workitems that do not contain an element 'IDOC', because
*   they were not created by POS inbound.
    LOOP AT t_contob WHERE element = c_objtype_idoc AND
                           objtype = c_objtype_idoc.
      INSERT t_contob INTO TABLE l_t_contob.
    ENDLOOP.
    LOOP AT t_contob.
      READ TABLE l_t_contob WITH KEY wi_id = t_contob-wi_id
                            TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE t_contob.
      ENDIF.
    ENDLOOP.

    LOOP AT t_contob WHERE objtype = c_objtype_posuploaddoc.
*     write upload document
      CLEAR t_upldoc.
      upldoc_element    = t_contob-element.
      t_upldoc-segnum     = upldoc_element-segnum.
      t_upldoc-segnum_end = upldoc_element-segnum_end.
      t_upldoc-uploadkey = t_contob-objkey.
      t_upldoc-attr = upldoc_element-attr.
      APPEND t_upldoc.
      IF g_msgpro-type = 'N' AND g_msgpro-extobjtype IS INITIAL.
*       Use upload documents to fill internal status table only if there
*       are no external documents available for a splittable IDoc
        CLEAR create_default_range.
*       fill internal status table
        READ TABLE t_int_status
             WITH KEY segnum     = t_upldoc-segnum
                      segnum_end = t_upldoc-segnum_end   BINARY SEARCH.
        IF sy-subrc <> 0.
          t_int_status-segnum     = t_upldoc-segnum.
          t_int_status-segnum_end = t_upldoc-segnum_end.
          INSERT t_int_status INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT t_upldoc BY uploadkey-uploadnr.

    CLEAR t_int_status.
    IF g_msgpro-type = 'N' AND NOT ( g_msgpro-extobjtype IS INITIAL ).
*     If external documents are available for splittable IDocs, use
*     them instead of the upload documents to fill the internal
*     status table. This is necessarry because for the compressed
*     WPUBON, upload ranges and external document ranges differ.
*     Because the follow-on documents use the ranges defined by the
*     external documents, this range must be used.
      LOOP AT t_contob WHERE objtype = g_msgpro-extobjtype.
        CLEAR create_default_range.
*       Write external document to t_int_status
        extdoc_element = t_contob-element.
*       fill internal status table
        READ TABLE t_int_status
             WITH KEY segnum     = extdoc_element-segnum
                      segnum_end = extdoc_element-segnum_end
             BINARY SEARCH.
        IF sy-subrc <> 0.
          t_int_status-segnum     = extdoc_element-segnum.
          t_int_status-segnum_end = extdoc_element-segnum_end.
          INSERT t_int_status INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF create_default_range = 'X'.
*     Create the default range because no other range already exists.
*     This always happens with splittable IDocs or if something goes
*     wrong with non-splittable IDocs.
      READ TABLE t_int_status
           WITH KEY segnum     = 1
                    segnum_end = 999999
           BINARY SEARCH.
      IF sy-subrc <> 0.
        t_int_status-segnum     = 1.
        t_int_status-segnum_end = 999999.
        INSERT t_int_status INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " F05_READ_UPLDOC_OLD



*&---------------------------------------------------------------------*
*&      Form  F05_WRITE_DOC
*&---------------------------------------------------------------------*
*       Analyse the links from the BOR and fill the tables with the
*       external and follow-on documents
*----------------------------------------------------------------------*
FORM f05_write_doc
*BD644889
*tables t_contob    structure neighbor" <-- IDoc object links
*ED644889
*BI644889
TABLES t_contob    TYPE tt_sww_contob  " <-- IDoc object links
*EI644889
       doc_status  TYPE wpusa_t_doc_status  " <-- ext. doc. status
       t_upldoc    TYPE wpusa_t_upldoc " <-- upload documents
       t_extdoc    TYPE wpusa_t_extdoc " --> external documents
       t_foldoc    TYPE wpusa_t_foldoc " --> follow-on docs
USING  docnum      LIKE edidc-docnum
       ext_objtype LIKE objectconn-objecttype
       verarbend   LIKE wptst-verarbend.

  DATA: extdoc_element TYPE wpusa_extdoc_element,
        foldoc_element TYPE wpusa_foldoc_element.

  LOOP AT t_contob WHERE objtype <> c_objtype_posuploaddoc AND
                         objtype <> c_objtype_idoc.
    CLEAR t_extdoc.
*BD644889
*    extdoc_element = t_contob-objkey+38(32).
*ED644889
*BI644889
    extdoc_element = t_contob-element.
*EI644889

    IF t_contob-objtype = ext_objtype.

*     Container object is an external document
      t_extdoc-segnum = extdoc_element-segnum.
      t_extdoc-segnum_end = extdoc_element-segnum_end.
      t_extdoc-objtype = ext_objtype.
      t_extdoc-key = t_contob-objkey.
      t_extdoc-attr = extdoc_element-attr.

*     Write data of external document to all status records in its range
      READ TABLE doc_status WITH KEY segnum = t_extdoc-segnum
                            BINARY SEARCH.
      LOOP AT doc_status FROM sy-tabix.
        IF doc_status-segnum     >= t_extdoc-segnum AND
           doc_status-segnum_end <= t_extdoc-segnum_end.
          doc_status-objtype = t_extdoc-objtype.
          doc_status-key     = t_extdoc-key.
          MODIFY doc_status.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

*   Insert current record into table of the external documents
      READ TABLE t_extdoc WITH KEY segnum     = t_extdoc-segnum
                                   segnum_end = t_extdoc-segnum_end
                                   BINARY SEARCH
                                   TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY t_extdoc INDEX sy-tabix.
      ELSE.
        IF sy-tabix = 0.
          APPEND t_extdoc.
        ELSE.
          INSERT t_extdoc INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ELSE.

*     Container object is a follow-on document
      CLEAR t_foldoc.
*BD644889
*      foldoc_element     = t_contob-objkey+38(32).
*ED644889
*BI644889
      foldoc_element     = t_contob-element.
*EI644889
      READ TABLE t_upldoc WITH KEY
           uploadkey-uploadnr = foldoc_element-uploadnr BINARY SEARCH.
      IF sy-subrc = 0.
        t_foldoc-segnum     = t_upldoc-segnum.
        t_foldoc-segnum_end = t_upldoc-segnum_end.
        t_foldoc-uploadkey  = t_upldoc-uploadkey.
      ELSE.
        t_foldoc-segnum = 1.
        t_foldoc-segnum_end = verarbend.
        t_foldoc-uploadkey-uploadnr = foldoc_element-uploadnr.
      ENDIF.
      t_foldoc-objtype  = t_contob-objtype.
      t_foldoc-key      = t_contob-objkey.
      t_foldoc-index    = foldoc_element-index.
      t_foldoc-level    = foldoc_element-level.
      t_foldoc-attr     = foldoc_element-attr.
      APPEND t_foldoc.

    ENDIF.
  ENDLOOP.

  SORT t_extdoc BY segnum segnum_end key.
  SORT t_upldoc BY segnum segnum_end uploadkey.
  SORT t_foldoc BY segnum segnum_end uploadkey index objtype key.

ENDFORM.                               " F05_WRITE_DOC


*&---------------------------------------------------------------------*
*&      Form  F05_WRITE_HISTORY
*&---------------------------------------------------------------------*
*       Write status change history to output tables
*----------------------------------------------------------------------*
FORM f05_write_history
TABLES t_edids      TYPE wpusa_t_edids
       t_int_status TYPE type_t_int_status
       doc_history  TYPE wpusa_t_doc_history
       doc_reason   TYPE wpusa_t_doc_reason
       doc_changes  TYPE wpusa_t_doc_changes
USING  VALUE(p_segnum)       LIKE edidd-segnum
       VALUE(p_segnum_end)   LIKE edidd-segnum.

  DATA: edids_hide TYPE wpusa_edids_hide.

  LOOP AT t_edids WHERE   stamqu = 'SAP'
                    AND ( stamid = 'E0' AND stamno = '184' OR
                          stamid = 'WP' AND stamno(1) = '7' ).
*   There are two groups of EDI status records which are relevant for
*   the history:
*   1. If the IDoc was changes by the EDI standard IDoc editor, there
*      is a status record with the message E0184. It contains the user
*      name and the changed segment.
*   2. If the status change was done by the function module
*      pos_sa_set_document_status, there is a status record with a
*      message beginning with WP78. If there were any reason codes or
*      field changes, they come after this message. Because
*      of this, the range of the history record is used for the reasons
*      and changes too (see below).
    IF t_edids-stamno = '184'.
*     Write history entry from EDI standard editor message
      CLEAR doc_history.
      doc_history-status     = pos_status_correct_user.
*     The segment number is stored in stapa1.
      doc_history-segnum     = t_edids-stapa1.
      doc_history-countr     = t_edids-countr.
      doc_history-date       = t_edids-logdat.
      doc_history-time       = t_edids-logtim.
*     Search the range this segment belongs to
      LOOP AT t_int_status
           WHERE segnum     <= doc_history-segnum
             AND segnum_end >= doc_history-segnum.
        doc_history-segnum     = t_int_status-segnum.
        doc_history-segnum_end = t_int_status-segnum_end.
      ENDLOOP.
*     Write status to status history
      READ TABLE doc_history WITH KEY
           segnum     = doc_history-segnum
           segnum_end = doc_history-segnum_end
           countr     = doc_history-countr
           BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-tabix = 0.
        APPEND doc_history.
      ELSE.
        INSERT doc_history INDEX sy-tabix.
      ENDIF.

    ELSEIF t_edids-stamno(2) = '78' OR t_edids-stamno = '791'.
*     Write history entry
*     The EDI status records containing the POS status information
*     have message numbers beginning with WP78. The last digit of the
*     message number is the POS status.
*     The range is stored in STAPA4, which is not used for this message
      edids_hide             = t_edids-stapa4.
      IF NOT ( edids_hide-no_history IS INITIAL ).
*       This status record has no relevance for the history
        CONTINUE.
      ENDIF.
      CLEAR doc_history.
      IF t_edids-stamno = '791'.
        doc_history-status   = pos_status_process_no_check.
      ELSE.
        doc_history-status   = t_edids-stamno+2(1).
      ENDIF.
      doc_history-segnum     = edids_hide-segnum.
      doc_history-segnum_end = edids_hide-segnum_end.
      doc_history-countr     = t_edids-countr.
      doc_history-date       = t_edids-logdat.
      doc_history-time       = t_edids-logtim.
      IF doc_history-status = pos_status_correct_system
      OR doc_history-status = pos_status_error.
*       Status corrected by system can only occur before the inbound
*       processing. Status error (as an EDIDS record) can only occur
*       during it. These EDIDS records contain only the number of the
*       modified/faulty segment. Luckily, they can be assigned to a
*       range by searching a range that includes this segment.
        LOOP AT t_int_status
             WHERE segnum     <= doc_history-segnum
               AND segnum_end >= doc_history-segnum_end.
          doc_history-segnum     = t_int_status-segnum.
          doc_history-segnum_end = t_int_status-segnum_end.
        ENDLOOP.
      ENDIF.
*     Write status to status history
      READ TABLE doc_history WITH KEY
           segnum     = doc_history-segnum
           segnum_end = doc_history-segnum_end
           countr     = doc_history-countr
           BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-tabix = 0.
        APPEND doc_history.
      ELSE.
        INSERT doc_history INDEX sy-tabix.
      ENDIF.

    ELSEIF t_edids-stamno = '757' OR t_edids-stamno = '758'.
*     Write reason code
*     The EDI status records with messages WP757 and WP758 contain
*     reason codes. WP757 says that the error indicated by the reason
*     code is fixed while WP758 says it's not.
      MOVE-CORRESPONDING doc_history TO doc_reason.
      doc_reason-reason = t_edids-stapa1.
      IF t_edids-stamno = '757'.
        CLEAR doc_reason-not_fixed.
      ELSE.
        doc_reason-not_fixed = 'X'.
      ENDIF.
*     Write reason to reason table
      READ TABLE doc_reason WITH KEY
           segnum     = doc_reason-segnum
           segnum_end = doc_reason-segnum_end
           countr     = doc_reason-countr
           BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-tabix = 0.
        APPEND doc_reason.
      ELSE.
        INSERT doc_reason INDEX sy-tabix.
      ENDIF.

    ELSEIF t_edids-stamno = '759'.
*     Write field changes
*     The EDI status record with messag WP759 contains the change of
*     a field.
      MOVE-CORRESPONDING doc_history TO doc_changes.
*     The first 20 characters of the old value of the field are stored
*     in stapa1, the rest in stapa2.
      CONCATENATE t_edids-stapa1 t_edids-stapa2
            INTO doc_changes-old_value.
*     The new field value is stored in stapa3 (only the first 20 chars)
      doc_changes-new_value = t_edids-stapa3.
      doc_changes-segnum_chg = t_edids-segnum.
      doc_changes-segfld = t_edids-segfld.
      edids_hide         = t_edids-stapa4.
      doc_changes-tabname = edids_hide-tabname.
*     Write change to change table
      READ TABLE doc_changes WITH KEY
           segnum     = doc_changes-segnum
           segnum_end = doc_changes-segnum_end
           countr     = doc_changes-countr
           BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-tabix = 0.
        APPEND doc_changes.
      ELSE.
        INSERT doc_changes INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " F05_WRITE_HISTORY


*&---------------------------------------------------------------------*
*&      Form  F05_WRITE_EDIDS_TO_INT_STATUS
*&---------------------------------------------------------------------*
*       Write data from EDIDS to an internal status table entry
*----------------------------------------------------------------------*
FORM f05_write_edids_to_int_status
USING    l_edids LIKE edids
         status  LIKE poswpsa-status
CHANGING int_status TYPE type_int_status.

  IF status CA pos_status_set_post_flag AND
     status <> int_status-status_prev.
    int_status-status_prev2 = int_status-status_prev.
    int_status-status_prev  = status.
  ENDIF.

  int_status-status      = status.
  int_status-countr      = l_edids-countr.
  int_status-date        = l_edids-logdat.
  int_status-time        = l_edids-logtim.
  int_status-uname       = l_edids-uname.
  int_status-key         = l_edids-stapa1.

  IF status = pos_status_correct_user.
    int_status-status_disp = status.
  ENDIF.

ENDFORM.                               " F05_WRITE_EDIDS_TO_INT_STATUS

*&---------------------------------------------------------------------*
*&      Form  F05_GET_SINGLE_RANGE_STATUS
*&---------------------------------------------------------------------*
*      If the status was requested for a particular external document,
*      set export parameters to the values of that external document
*----------------------------------------------------------------------*
FORM f05_get_single_range_status
TABLES   t_doc_status TYPE wpusa_t_doc_status
USING    inbound      TYPE c
         segnum       LIKE edidd-segnum
         segnum_end   LIKE edidd-segnum
CHANGING status       LIKE poswpsa-status
         status_disp  LIKE poswpsa-status
         verbuchung   LIKE wptst-verbuchung
         posted       LIKE poswpsa-posted.

  IF NOT ( segnum IS INITIAL AND segnum_end IS INITIAL ).
*   search record for external document
    READ TABLE t_doc_status WITH KEY segnum     = segnum
                                     segnum_end = segnum_end
                            BINARY SEARCH.
    IF sy-subrc = 0.
*     status record found
      status      = t_doc_status-status.
      status_disp = t_doc_status-status_disp.
      verbuchung  = t_doc_status-verbuchung.
      posted      = t_doc_status-posted.
    ELSE.
*     Range does not exist
      IF inbound = 'X'.
*       During inbound processing, it may happen that a WPTST entry
*       contains several external documents. So if the segment range
*       is within a faulty range, the segment range is faulty also.
        LOOP AT t_doc_status WHERE segnum     <= segnum
                               AND segnum_end >= segnum_end
                               AND status     = pos_status_error.
        ENDLOOP.
        IF sy-subrc = 0.
          status      = pos_status_error.
          status_disp = pos_status_error.
          verbuchung  = t_doc_status-verbuchung.
          posted      = t_doc_status-posted.
        ELSE.
          MESSAGE ID 'WP' TYPE 'E' NUMBER '768' RAISING range_not_exist.
        ENDIF.
      ELSE.
        MESSAGE ID 'WP' TYPE 'E' NUMBER '768' RAISING range_not_exist.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " F05_GET_SINGLE_RANGE_STATUS
