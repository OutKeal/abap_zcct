*----------------------------------------------------------------------*
***INCLUDE ZRET_POS_MONF08 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  IB_ACCDETERR_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ib_accdeterr_detail .
  data: message like message,
         ls_hide    type type_ib_hide,
         l_docnum   type edi_docnum,
         ls_edidc   type edidc,
         ls_edids   type edids,
         l_switch   type c,
         lt_unproc       type table of bdidocs with header line,
         lt_idoc_data    type table of edidd,
         lt_idoc_control type table of edidc.

  ls_hide = g_hide.

  if not ( ls_hide-key is initial ) and ls_hide-objtype = 'IDOC'.
    l_docnum = ls_hide-key.
  elseif not ( ls_hide-docnum is initial ) and ls_hide-objtype is initial.
    l_docnum = ls_hide-docnum.
  else.
    message e767.
    exit.
  endif.

  call function 'EDI_DOCUMENT_OPEN_FOR_PROCESS'
    exporting
      document_number          = l_docnum
    importing
      idoc_control             = ls_edidc
    exceptions
      document_foreign_lock    = 1001
      document_not_exist       = 1000
      document_number_invalid  = 1000
      document_is_already_open = 4
      others                   = 9999.

  call function 'EDI_DOCUMENT_READ_LAST_STATUS'
    exporting
      document_number        = l_docnum
    importing
      status                 = ls_edids
    exceptions
      document_not_open      = 1
      no_status_record_found = 2
      others                 = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  elseif ls_edids-status <> '51'.
    shift l_docnum left deleting leading '0'.
    message e030(/atu/ac_ret_pos) with l_docnum.
    exit.
  endif.

  lt_unproc-docnum = l_docnum.
  append lt_unproc.

  call function 'EDI_DOCUMENT_CLOSE_PROCESS'
    exporting
      document_number = lt_unproc-docnum
    importing
      idoc_control    = ls_edidc
    exceptions
      error_message   = 1
      others          = 2.

  l_switch = 'X'.

*  call function 'ZCCT_AC_POS_INPUT_INTERFACE'
*    exporting
*      action               = 'I'
*    changing
*      px_atu_pos_interface = l_switch.

  call function 'IDOC_INPUT'
    exporting
      mass_processing   = ' '
      input_method      = 'E'
      direct_call       = ' '
    tables
      unprocessed_idocs = lt_unproc
      idoc_data         = lt_idoc_data
      idoc_control      = lt_idoc_control
    exceptions
      others            = 17.

  clear l_switch.
*  call function '/ATU/AC_POS_INPUT_INTERFACE'
*    exporting
*      action               = 'I'
*    changing
*      px_atu_pos_interface = l_switch.

endform.                    " IB_ACCDETERR_DETAIL
