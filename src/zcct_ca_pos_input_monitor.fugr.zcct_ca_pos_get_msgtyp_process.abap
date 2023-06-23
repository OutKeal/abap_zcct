FUNCTION zcct_ca_pos_get_msgtyp_process.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SNDPRN) LIKE  EDIDC-SNDPRN OPTIONAL
*"  TABLES
*"      MSGPRO TYPE  WPUSA_T_MSGPRO OPTIONAL
*"      FOLDESCR TYPE  WPUSA_T_FOLDESCR OPTIONAL
*"--------------------------------------------------------------------

  DATA l_twpir LIKE twpir.
  DATA l_kunnr LIKE kna1-kunnr.

* Entrys for message types
  REFRESH msgpro.

*  CLEAR msgpro.
*  msgpro-mestyp     = 'WGSREQ'.
*  msgpro-docfunc    = 'POS_READ_DOCUMENTS_WGSREQ'.
*  msgpro-extobjtype = c_objtype_idoc.
*  msgpro-function   = c_function_wgsr.
*  msgpro-hints      = 'X'.
*  msgpro-type       = 'S'.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = 'WALREQ'.
*  msgpro-extobjtype = c_objtype_idocextalo.
*  msgpro-function   = c_function_allo.
*  msgpro-type       = 'N'.
*  APPEND msgpro.

*  clear msgpro.
*  msgpro-mestyp     = 'ORDERS'.
*  msgpro-function   = c_function_ords.
*  msgpro-hints      = 'X'.
*  append msgpro.

*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUBON'.
*  msgpro-docfunc    = 'POS_SD_RW_READ_DOCUMENTS'.
*  msgpro-extobjtype = c_objtype_idocextbon.
*  msgpro-reversable = 'X'.
*  msgpro-editable   = 'X'.             "###enable editor
*  msgpro-function   = c_function_fakt.
*  IF sndprn IS INITIAL.
*    msgpro-type     = 'S'.
*  ELSE.
*    l_kunnr = sndprn.
*    CALL FUNCTION 'POS_CUST_SAL_DOC_CONTROL_READ'
*      EXPORTING
*        i_locnr           = l_kunnr
*      IMPORTING
*        o_twpir           = l_twpir
*      EXCEPTIONS
*        no_entry_found    = 425
*        filiale_unbekannt = 401.
*    IF sy-subrc = 0 AND l_twpir-numtr = 0.
*      msgpro-type     = 'N'.
*    ELSE.
*      msgpro-type     = 'S'.
*    ENDIF.
*  ENDIF.
*  msgpro-include_header = 'X'.
*  APPEND msgpro.

*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUUMS'.
*  msgpro-docfunc    = '/ATU/AC_POS_RW_READ_DOCUMENTS'.
*  msgpro-reversable = 'X'.
**BD 381210
*  msgpro-editable   = 'X'.             "###enable editor
**ED 381210
*  msgpro-function   = c_function_fakt.
*  msgpro-type       = 'S'.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUTAB'.
*  msgpro-docfunc    = '/ATU/AC_POS_RW_READ_DOCUMENTS'.
*  msgpro-reversable = 'X'.
**BD 381210
*  msgpro-editable   = 'X'.             "###enable editor
**ED 381210
*  msgpro-function   = c_function_fakt.
*  msgpro-type       = 'N'.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUWBW'.
*  msgpro-reversable = 'X'.
**BD 381210
*  msgpro-editable   = 'X'.             "###enable editor
**ED 381210
*  msgpro-function   = c_function_best.
*  msgpro-hints      = 'X'.
*  msgpro-type       = 'S'.
*  APPEND msgpro.

  CLEAR msgpro.
  msgpro-mestyp     = 'ZCCT'.
  msgpro-reversable = 'X'.
*BD 381210
  msgpro-editable   = 'X'.             "###enable editor
*ED 381210
*  msgpro-function   = c_function_best.
  msgpro-hints      = 'X'.
  msgpro-type       = 'S'.
  APPEND msgpro.

*  CLEAR msgpro.
*  msgpro-mestyp     = 'WVINVE'.
*  msgpro-extobjtype = c_objtype_idocextivu.
*  msgpro-hints      = 'X'.
*  msgpro-type       = 'S'.
*  msgpro-function   = c_function_wvin.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUFIB'.
*  msgpro-docfunc    = '/ATU/AC_POS_RW_READ_DOCUMENTS'.
*  msgpro-reversable = 'X'.
**BD 381210
*  msgpro-editable   = 'X'.             "###enable editor
**ED 381210
*  msgpro-function   = c_function_fibu.
*  msgpro-type       = 'N'.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = 'WPUKSR'.
*  msgpro-reversable = 'X'.
*  msgpro-function   = c_function_stat.
*  msgpro-type       = 'S'.
*  APPEND msgpro.
*
*  CLEAR msgpro.
*  msgpro-mestyp     = '/ATU/RETSAL'.
*  msgpro-docfunc    = '/ATU/AC_POS_RW_READ_DOCUMENTS'.
*  msgpro-reversable = 'X'.
*  msgpro-editable   = ' '.
*  msgpro-function   = c_function_fakt.
*  msgpro-type       = 'S'.
*  APPEND msgpro.

* Other message types with no specialties
  CLEAR msgpro.
  msgpro-type       = 'O'.
  msgpro-mestyp     = 'WP_EAN'.
  APPEND msgpro.
  msgpro-mestyp     = 'WPUERR'.
  APPEND msgpro.
  msgpro-mestyp     = 'WP_PER'.
  APPEND msgpro.
  msgpro-mestyp     = 'WP_PLU'.
  APPEND msgpro.

* Entrys for follow-on document object types
  REFRESH foldescr.

  CLEAR foldescr.
  foldescr-objtype = c_objtype_idocextbed.
  foldescr-display_idoc = 'X'.
  APPEND foldescr.

  CLEAR foldescr.
  foldescr-objtype = c_objtype_mat_doc.
  foldescr-descr   = TEXT-901.
*{   REPLACE        LNDK905450                                        2
*\  foldescr-folfunc = 'POS_SALES_FOLDOC_BILLING'.
  foldescr-folfunc = 'ZCCT_SD_POS_BL_READ_DOCUMENTS'.
*}   REPLACE
  foldescr-offset  = 0.
  foldescr-length  = 10.
  APPEND foldescr.

*  CLEAR foldescr.
*  foldescr-objtype = '/ATU/COPCA'.
*  foldescr-descr   = TEXT-910.
*  foldescr-offset  = 5.
*  foldescr-length  = 10.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = '/ATU/RCOPA'.
*  foldescr-descr   = TEXT-909.
*  foldescr-offset  = 9.
*  foldescr-length  = 10.
*  APPEND foldescr.

  CLEAR foldescr.
  foldescr-objtype = 'BKPF'.
  foldescr-offset  = 4.
  foldescr-length  = 10.
  APPEND foldescr.

  CLEAR foldescr.
  foldescr-objtype = 'BUS2072'.
  foldescr-offset  = 4.
  foldescr-length  = 10.
  APPEND foldescr.

  CLEAR foldescr.
  foldescr-objtype = c_objtype_bill_doc.
  foldescr-descr   = TEXT-900.
*{   REPLACE        LNDK905316                                        1
*\  foldescr-folfunc = 'POS_SALES_FOLDOC_BILLING'.
  foldescr-folfunc = 'ZCCT_SD_POS_BL_READ_DOCUMENTS'.
*}   REPLACE
  APPEND foldescr.
*{   INSERT         LNDK905640                                        3
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_sales_order.
**  foldescr-descr   = text-991.
*  foldescr-folfunc = '/ATU/SD_POS_ORD_READ_DOCUMENTS'.
*  APPEND foldescr.
*}   INSERT
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_useg_doc.
*  foldescr-descr   = TEXT-902.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_useg_doc_neu.
*  foldescr-descr   = TEXT-902.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_inv_doc.
*  foldescr-descr   = TEXT-907.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_av_object_type_wvfb.
*  foldescr-descr   = TEXT-903.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_av_object_type_banf.
*  foldescr-descr   = TEXT-904.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_av_object_type_banf_neu.
*  foldescr-descr   = TEXT-904.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_idocextdoc.
*  foldescr-display_idoc = 'X'.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_av_object_type_best.
*  foldescr-descr   = TEXT-905.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_av_object_type_lief.
*  foldescr-descr   = TEXT-906.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = c_objtype_workitem.
*  foldescr-descr   = TEXT-908.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = 'DUMMY_COPA'.
*  foldescr-descr   = TEXT-909.
*  APPEND foldescr.
*
*  CLEAR foldescr.
*  foldescr-objtype = 'DUMMY_COPCA'.
*  foldescr-descr   = TEXT-910.
*  APPEND foldescr.


  SORT msgpro BY mestyp.
  SORT foldescr BY objtype.

ENDFUNCTION.
