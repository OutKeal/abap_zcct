function ZCCT_CA_POS_SCREEN_STATUS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOCNUM) LIKE  EDIDC-DOCNUM
*"     VALUE(SEGNUM) LIKE  EDIDD-SEGNUM
*"     VALUE(SEGNUM_END) LIKE  EDIDD-SEGNUM
*"     VALUE(KEY) LIKE  OBJECTCONN-OBJECTID
*"     VALUE(MESTYP) LIKE  EDIDC-MESTYP
*"     VALUE(SNDPRN) LIKE  EDIDC-SNDPRN
*"  EXPORTING
*"     VALUE(STATUS) LIKE  POSWPSA-STATUS
*"--------------------------------------------------------------------

  call function 'POS_SA_GET_MSGTYPE_PROCESS'
    EXPORTING
      sndprn = sndprn
    TABLES
      msgpro = g_t_msgpro
    EXCEPTIONS
      others = 1.
  read table g_t_msgpro with key mestyp binary search.
  if sy-subrc <> 0.
    clear g_t_msgpro.
  endif.

  include lwpsawps.

  g_s09-docnum     = docnum.
  g_s09-segnum     = segnum.
  g_s09-segnum_end = segnum_end.
  g_s09-key        = key.
  g_s09-sndprn     = sndprn.

  perform f09_write_status.

  call screen '0900' starting at 1 1 ending at 95 13.

  status = poswpsa-status.

endfunction.

*&---------------------------------------------------------------------*
*&      Form  F09_SET_STATUS
*&---------------------------------------------------------------------*
*       Change status of current external document
*----------------------------------------------------------------------*
form f09_set_status
using status like poswpsa-status.

* BD 551650 15.05.03
*  CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
*       EXPORTING
*            docnum             = g_s09-docnum
*            segnum             = g_s09-segnum
*            segnum_end         = g_s09-segnum_end
*            status_new         = status
*            mestyp             = g_t_msgpro-mestyp
*            sndprn             = g_s09-sndprn
*            ext_objid          = g_s09-key
*       EXCEPTIONS
*            change_not_allowed = 1
*            foreign_lock       = 2
*            idoc_not_exist     = 3
*            db_error           = 4
*            unknown_exception  = 5
*            OTHERS             = 6.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ROLLBACK WORK.
*  ELSE.
**   Commit work now because data will be read immediately
*    COMMIT WORK.
*  ENDIF.
* ED 551650 15.05.03

* BI 551650 15.05.03
  DATA: BEGIN OF l_t_status OCCURS 0,
          docnum LIKE edidc-docnum,
          dstatus TYPE wpusa_doc_status,
          verarbend LIKE wptst-verarbend,
        END OF l_t_status.
  DATA: l_t_reason TYPE wpusa_t_doc_reason.
  DATA: answer(1) TYPE c.
  DATA: l_tabix LIKE sy-tabix.
  DATA: l_status TYPE wpusa_doc_status.
  DATA: l_verarbend LIKE wptst-verarbend.
  DATA: l_t_wptst LIKE wptst OCCURS 0 WITH HEADER LINE.
  DATA: o_t_status TYPE  wpusa_t_doc_status,
        o_t_wplst LIKE wplst OCCURS 0 WITH HEADER LINE.

* transaction WPUBON is explicity checked to have the possibility to
* change the POS-Status for more than one bon with the same status
* in the case that aggregation is switched off ...
  IF ( g_t_msgpro-mestyp = 'WPUBON' ).

    REFRESH: o_t_status, o_t_wplst.

    CALL FUNCTION 'POS_SA_GET_DOCUMENT_STATUS'
      EXPORTING
        docnum            = g_s09-docnum
        mestyp            = g_t_msgpro-mestyp
        sndprn            = g_s09-sndprn
        history_flag      = 'X'
      IMPORTING
        verarbend         = l_verarbend
      TABLES
        doc_status        = o_t_status
        t_wptst           = l_t_wptst
        t_wplst           = o_t_wplst
      EXCEPTIONS
        idoc_not_exist    = 1
        foreign_lock      = 2
        unknown_exception = 3
        range_not_exist   = 4
        error_message     = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
    ELSE.
      l_tabix = 1.
      LOOP AT o_t_status INTO l_status.
        l_t_status-docnum    = g_s09-docnum.
        l_t_status-dstatus   = l_status.
        l_t_status-verarbend = l_verarbend.
        INSERT l_t_status INDEX l_tabix.
        ADD 1 TO l_tabix.
      ENDLOOP.
    ENDIF.

* Check if it is a transaction WPUBON with aggregated bons ...
    READ TABLE l_t_status WITH KEY dstatus-segnum_end = '999999'.
    IF ( sy-subrc = 0 ).
      CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
        EXPORTING
          docnum             = g_s09-docnum
          segnum             = g_s09-segnum
          segnum_end         = g_s09-segnum_end
          status_new         = status
          mestyp             = g_t_msgpro-mestyp
          sndprn             = g_s09-sndprn
          ext_objid          = g_s09-key
        EXCEPTIONS
          change_not_allowed = 1
          foreign_lock       = 2
          idoc_not_exist     = 3
          db_error           = 4
          unknown_exception  = 5
          OTHERS             = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ROLLBACK WORK.
      ELSE.
*   Commit work now because data will be read immediately
        COMMIT WORK.
      ENDIF.
    ELSE.

* Popup request if all bons with the same POS-Status shall be taken into
* account ...
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
                defaultoption = 'N'
                textline1     = text-251
                textline2     = text-252
                titel         = text-250
*           START_COLUMN  = 25
*           START_ROW     = 6
           IMPORTING
                answer        = answer
           EXCEPTIONS
                OTHERS        = 1.

* Shall every bon with the same status be changed ...
      IF ( answer = 'J' AND sy-subrc = 0 ).

* EXCEPTION:
* If the bons have been rejected and reversed by user and shall be now
* processed again then only the selected bons either reversed or
* rejected should be set to status '2' processing planned therefore this
* process of elimination is necessary ...
*BD 886624
*        IF ( status = 2 AND poswpsa-status CA '56' ).
*ED 886624
*BI 886624
        IF ( status = '2' AND poswpsa-status CA '56' ).
*EI 886624
          LOOP AT l_t_status
            WHERE dstatus-status = poswpsa-status.
            CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
              EXPORTING
                docnum             = g_s09-docnum
                segnum             = l_t_status-dstatus-segnum
                segnum_end         = l_t_status-dstatus-segnum_end
                status_new         = status
                mestyp             = g_t_msgpro-mestyp
                sndprn             = g_s09-sndprn
                ext_objid          = g_s09-key
              EXCEPTIONS
                change_not_allowed = 1
                foreign_lock       = 2
                idoc_not_exist     = 3
                db_error           = 4
                unknown_exception  = 5
                OTHERS             = 6.

* If one or more POS-Status are different to the poswpsa-status they
* must not changed therefore the rollback!!
            IF sy-subrc <> 0.
              ROLLBACK WORK.
            ELSE.
*   Commit work now because data will be read immediately
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
        ELSE.
* Loop to change every bon with the same status ...
          LOOP AT l_t_status.
            CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
              EXPORTING
                docnum             = g_s09-docnum
                segnum             = l_t_status-dstatus-segnum
                segnum_end         = l_t_status-dstatus-segnum_end
                status_new         = status
                mestyp             = g_t_msgpro-mestyp
                sndprn             = g_s09-sndprn
                ext_objid          = g_s09-key
              EXCEPTIONS
                change_not_allowed = 1
                foreign_lock       = 2
                idoc_not_exist     = 3
                db_error           = 4
                unknown_exception  = 5
                OTHERS             = 6.

* If one or more POS-Status are different to the poswpsa-status they
* must not changed therefore the rollback!!
            IF sy-subrc <> 0.
              ROLLBACK WORK.
            ELSE.
*   Commit work now because data will be read immediately
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
        ENDIF.
* Only the status of one bon shall be changed ...
      ELSE.
        CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
          EXPORTING
            docnum             = g_s09-docnum
            segnum             = g_s09-segnum
            segnum_end         = g_s09-segnum_end
            status_new         = status
            mestyp             = g_t_msgpro-mestyp
            sndprn             = g_s09-sndprn
            ext_objid          = g_s09-key
          EXCEPTIONS
            change_not_allowed = 1
            foreign_lock       = 2
            idoc_not_exist     = 3
            db_error           = 4
            unknown_exception  = 5
            OTHERS             = 6.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ROLLBACK WORK.
        ELSE.
*   Commit work now because data will be read immediately
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.
* set status for all other transactions except WPUBON ...
  ELSE.
    CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
      EXPORTING
        docnum             = g_s09-docnum
        segnum             = g_s09-segnum
        segnum_end         = g_s09-segnum_end
        status_new         = status
        mestyp             = g_t_msgpro-mestyp
        sndprn             = g_s09-sndprn
        ext_objid          = g_s09-key
      EXCEPTIONS
        change_not_allowed = 1
        foreign_lock       = 2
        idoc_not_exist     = 3
        db_error           = 4
        unknown_exception  = 5
        OTHERS             = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ROLLBACK WORK.
    ELSE.
*   Commit work now because data will be read immediately
      COMMIT WORK.
    ENDIF.
  ENDIF.
* EI 551650 15.05.03
endform.                               " F09_SET_STATUS


*&---------------------------------------------------------------------*
*&      Form  F09_WRITE_STATUS
*&---------------------------------------------------------------------*
*       Display status history of current external document
*----------------------------------------------------------------------*
form f09_write_status.



  data: t_edids       type wpusa_t_edids with header line,
        t_wplst       type wpusa_t_wplst with header line,
        message       like message,
        msgno         like sy-msgno,
        msgid         like sy-msgid,
        tab_lines     like sy-tabix.
  data  t_wptst       type wpusa_t_wptst      with header line.
  data  doc_status    type wpusa_t_doc_status with header line.
  data  verarbend     like wptst-verarbend.
  data  status_old    like poswpsa-status.

  refresh: g_t_status_display,
           g_t_doc_history, g_t_doc_reason, g_t_doc_changes.

  call function 'POS_SA_GET_DOCUMENT_STATUS'
    EXPORTING
      docnum       = g_s09-docnum
      segnum       = g_s09-segnum
      segnum_end   = g_s09-segnum_end
      mestyp       = g_t_msgpro-mestyp
      sndprn       = g_s09-sndprn
      history_flag = 'X'
    IMPORTING
      status_disp  = poswpsa-status
      status       = status_old
      verarbend    = verarbend
      verbuchung   = poswpsa-verbuchung
      posted       = poswpsa-posted
    TABLES
      doc_history  = g_t_doc_history
      doc_reason   = g_t_doc_reason
      doc_changes  = g_t_doc_changes
      t_edids      = t_edids
      t_wptst      = t_wptst
      t_wplst      = t_wplst
      doc_status   = doc_status
    EXCEPTIONS
      others       = 9999.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.

  if poswpsa-status is initial.
*   Cursor was not on a line with a POS status -> leave status blank
    clear dd07t-ddtext.
  else.
*   Retrieve POS status description from DD
    call function 'POS_SA_DESCR_FIXED_VALUE_GET'
      EXPORTING
        domname    = 'POSSTATUS'
        domvalue_l = poswpsa-status
      IMPORTING
        ddtext     = dd07t-ddtext.
  endif.

  sort g_t_doc_history by countr.

  loop at g_t_doc_history.
*   Display history records
    read table t_edids with key docnum = g_s09-docnum
                                logdat = g_t_doc_history-date
                                logtim = g_t_doc_history-time
                                countr = g_t_doc_history-countr
                                binary search.
    msgno = t_edids-stamno.
    msgid = t_edids-stamid.
    call function 'WRITE_MESSAGE_NEW'
      EXPORTING
        msgid = msgid
        msgno = msgno
        msgty = 'I'
        msgv1 = t_edids-stapa1
        msgv2 = t_edids-stapa2
        msgv3 = t_edids-stapa3
        msgv4 = t_edids-stapa4
        msgv5 = ' '
      IMPORTING
        messg = message.

    if g_t_doc_history-segnum = 1 and
       g_t_doc_history-segnum_end = 999999.
      concatenate message-msgtx ' *' into message-msgtx.
    endif.

    g_t_status_display-chgdat   = g_t_doc_history-date.
    g_t_status_display-chgtim   = g_t_doc_history-time.
    g_t_status_display-msgtx    = message-msgtx.
    g_t_status_display-msgid    = message-msgid.
    g_t_status_display-msgty    = message-msgty.
    g_t_status_display-msgno    = message-msgno.
    g_t_status_display-stapa1   = t_edids-stapa1.
    g_t_status_display-stapa2   = t_edids-stapa2.
    g_t_status_display-stapa3   = t_edids-stapa3.
    g_t_status_display-stapa4   = t_edids-stapa4.
    read table g_t_doc_reason with key countr = g_t_doc_history-countr
         binary search.
    if sy-subrc = 0.
      g_t_status_display-reason   = g_t_doc_reason-reason.
    else.
      clear g_t_status_display-reason.
    endif.
    append g_t_status_display.
  endloop.

  describe table g_t_status_display lines status_ctrl-lines.

* Exclude the functions that are not available from the popup menu
  refresh t_excl.

  call function 'POS_SA_SET_DOCUMENT_STATUS'
    EXPORTING
      docnum       = g_s09-docnum
      segnum       = g_s09-segnum
      segnum_end   = g_s09-segnum_end
      mestyp       = g_t_msgpro-mestyp
      sndprn       = g_s09-sndprn
      ext_objid    = g_s09-key
      status_new   = pos_status_process
      status_old   = status_old
      check_status = 'X'
      verarbend    = verarbend
    TABLES
      doc_status   = doc_status
      t_edids      = t_edids
      t_wptst      = t_wptst
      t_wplst      = t_wplst
    EXCEPTIONS
      others       = 7.
  if sy-subrc <> 0.
    t_excl-fcode = 'PROC'.
    append t_excl.
    t_excl-fcode = 'PRON'.
    append t_excl.
  endif.

  call function 'POS_SA_SET_DOCUMENT_STATUS'
    EXPORTING
      docnum       = g_s09-docnum
      segnum       = g_s09-segnum
      segnum_end   = g_s09-segnum_end
      mestyp       = g_t_msgpro-mestyp
      sndprn       = g_s09-sndprn
      ext_objid    = g_s09-key
      status_new   = pos_status_reversed
      status_old   = status_old
      check_status = 'X'
      verarbend    = verarbend
    TABLES
      doc_status   = doc_status
      t_edids      = t_edids
      t_wptst      = t_wptst
      t_wplst      = t_wplst
    EXCEPTIONS
      others       = 7.
  if sy-subrc <> 0.
    t_excl-fcode = 'REVE'.
    append t_excl.
  endif.

  if poswpsa-status cn pos_status_rejectable.
    t_excl-fcode = 'REJE'.
    append t_excl.
  endif.

  if poswpsa-status cn pos_status_resubmitable.
    t_excl-fcode = 'RESU'.
    append t_excl.
  endif.

  if poswpsa-status cn pos_status_postable.
    t_excl-fcode = 'POST'.
    append t_excl.
  endif.

*### Exclude the function "Post in background until it's implemented
  t_excl-fcode = 'POSB'.
  append t_excl.

  if g_parameter_wps cs 'POS_ST=C'.
    refresh t_excl.
  endif.

endform.                               " F09_WRITE_STATUS


*&---------------------------------------------------------------------*
*&      Form  F09_DISPLAY_DETAILS
*&---------------------------------------------------------------------*
*       Display details of a status change
*----------------------------------------------------------------------*
form f09_display_details.

  data tabix like sy-tabix.

  refresh: g_t_reason_display, g_t_changes_display.

  get cursor line tabix.

  if tabix <> 0.
    tabix = tabix + status_ctrl-top_line - 1.

    read table g_t_doc_history    index tabix.
    if sy-subrc = 0.
      read table g_t_status_display index tabix.

      loop at g_t_doc_reason where countr = g_t_doc_history-countr.
        move-corresponding g_t_doc_reason to g_t_reason_display.
        call function 'POS_SA_DESCR_FIXED_VALUE_GET'
          EXPORTING
            domname    = 'POSREASON'
            domvalue_l = g_t_doc_reason-reason
          IMPORTING
            ddtext     = g_t_reason_display-descr.
        append g_t_reason_display.
      endloop.

      loop at g_t_doc_changes where countr = g_t_doc_history-countr.
        move-corresponding g_t_doc_changes to g_t_changes_display.
        perform f09_display_details_fieldname.

        append g_t_changes_display.
      endloop.

      call screen '910' starting at 1 1 ending at 80 22.
    endif.
  endif.

endform.                               " F09_DISPLAY_DETAILS


*&---------------------------------------------------------------------*
*&      Form  F09_DISPLAY_DETAILS_FIELDNAME
*&---------------------------------------------------------------------*
*       Get medium screen text of changed field
*----------------------------------------------------------------------*
form f09_display_details_fieldname.
  data: rc       like sy-subrc,
        fieldtab like dfies occurs 0 with header line.

  call function 'GET_FIELDTAB'
    EXPORTING
      langu    = sy-langu
      only     = ' '
      tabname  = g_t_doc_changes-tabname
      withtext = 'X'
    IMPORTING
      rc       = rc
    TABLES
      fieldtab = fieldtab
    EXCEPTIONS
      others   = 9999.

  if sy-subrc = 0 and rc = 0.
    read table fieldtab with key fieldname = g_t_doc_changes-segfld.
    if sy-subrc = 0.
      g_t_changes_display-segfld = fieldtab-scrtext_m.
    endif.
  endif.

endform.                               " F09_DISPLAY_DETAILS_FIELDNAME


*&---------------------------------------------------------------------*
*&      Form  F09_DISPLAY_LONGTEXT
*&---------------------------------------------------------------------*
*       Display long text of a EDI status record message
*----------------------------------------------------------------------*
form f09_display_longtext.

  data: tabix like sy-tabix,
        docu_object like dokhl-object,
        message     like message,
        dummy1 like dselc occurs 0,
        dummy2 like dval  occurs 0,
        help_info   like help_info.

  get cursor line tabix.

  if tabix <> 0.
    tabix = tabix + status_ctrl-top_line - 1.
    read table g_t_status_display index tabix.
    if sy-subrc = 0.
      docu_object   = g_t_status_display-msgid.
      docu_object+2 = g_t_status_display-msgno.

      clear help_info.
      help_info-call       = 'D'.
      help_info-spras      = sy-langu.
      help_info-messageid  = g_t_status_display-msgid.
      help_info-messagenr  = g_t_status_display-msgno.
      help_info-message    = g_t_status_display-msgtx.
      help_info-title      = text-001. "Langtext
      help_info-docuid     = 'NA'.     "Konstante
      help_info-docuobject = docu_object.
      help_info-msgv1      = g_t_status_display-stapa1.
      help_info-msgv2      = g_t_status_display-stapa2.
      help_info-msgv3      = g_t_status_display-stapa3.
      help_info-msgv4      = g_t_status_display-stapa4.

      call function 'HELP_START'
        EXPORTING
          help_infos   = help_info
        TABLES
          dynpselect   = dummy1
          dynpvaluetab = dummy2.
    endif.
  endif.

endform.                               " F09_DISPLAY_LONGTEXT


*&---------------------------------------------------------------------*
*&      Form  F09_PROCESS_IDOC
*&---------------------------------------------------------------------*
*       Start inbound processing of an IDoc
*----------------------------------------------------------------------*
form f09_process_idoc.

  data: l_t_unprocessed_idocs like bdidocs occurs 0 with header line,
        l_t_idoc_data         like edidd   occurs 0,
        l_t_idoc_control      like edidc   occurs 0.

  l_t_unprocessed_idocs-docnum = g_s09-docnum.
  append l_t_unprocessed_idocs.

  call function 'IDOC_INPUT'
    EXPORTING
      mass_processing   = ' '
      direct_call       = ' '
    TABLES
      unprocessed_idocs = l_t_unprocessed_idocs
      idoc_data         = l_t_idoc_data
      idoc_control      = l_t_idoc_control
    EXCEPTIONS
      others            = 9999.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                               " F09_PROCESS_IDOC
