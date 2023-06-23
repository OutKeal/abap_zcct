*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONF04 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  IB_BUILD
*&---------------------------------------------------------------------*
*       Build initial tree
*----------------------------------------------------------------------*
FORM ib_build.

  DATA hide TYPE type_ib_hide.

  CLEAR g_t_nodes.
  hide-area = c_area_inbound.
  g_t_nodes-type       = c_node_area.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 2.
  g_t_nodes-color      = col_key.
  g_t_nodes-text       = text-200.
  g_t_nodes-tlength    = 37.
  g_t_nodes-tcolor     = col_heading.
  g_t_nodes-tintensiv  = c_intensiv.
  APPEND g_t_nodes.

  CLEAR g_t_nodes.
  hide-section = c_section_error.
  g_t_nodes-type       = c_node_section.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 3.
  g_t_nodes-color      = col_key.
  g_t_nodes-text       = text-204.
  g_t_nodes-tlength    = 30.
  g_t_nodes-tcolor     = col_negative.
  g_t_nodes-tintensiv  = c_intensiv.
  g_t_nodes-force_plus = 'X'.
  APPEND g_t_nodes.

  CLEAR g_t_nodes.
  hide-section = c_section_ok.
  g_t_nodes-type       = c_node_section.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 3.
  g_t_nodes-color      = col_key.
  g_t_nodes-text       = text-206.
  g_t_nodes-tlength    = 30.
  g_t_nodes-tcolor     = col_positive.
  g_t_nodes-tintensiv  = c_intensiv.
  g_t_nodes-force_plus = 'X'.
  APPEND g_t_nodes.

ENDFORM.                               " IB_BUILD


*&---------------------------------------------------------------------*
*&      Form  IB_EXPAND_NODE
*&---------------------------------------------------------------------*
*       expand current node and display the children
*----------------------------------------------------------------------*
FORM ib_expand_node
USING command
      node STRUCTURE snodetext.

* append children to all selected leafs
  CASE node-type.
    WHEN c_node_section.
      PERFORM ib_expand_section USING command node.

    WHEN c_node_mestyp.
      PERFORM ib_expand_mestyp USING command node.

    WHEN c_node_foldoc.
      PERFORM ib_expand_foldoc USING command node.

  ENDCASE.

ENDFORM.                               " IB_EXPAND_NODE


*&---------------------------------------------------------------------*
*&      Form  IB_EXPAND_SECTION
*&---------------------------------------------------------------------*
*       Expand the section node
*----------------------------------------------------------------------*
FORM ib_expand_section
USING command
      node STRUCTURE snodetext.

  DATA: l_idoc TYPE type_idoc,
        hide   TYPE type_ib_hide.

  PERFORM read_idocs_inbound.

* process all selected IDOCs
  CLEAR l_idoc.
  LOOP AT g_t_idocs.
    hide = node-hide.

*BI 303301
*    if hide-section = c_section_error.
    IF hide-section = c_section_error AND
       ( g_t_idocs-status = c_idoc_status_ok OR
         g_t_idocs-status = c_idoc_status_nok ).
*     Check if IDoc is faulty
      DATA count TYPE i.
*      SELECT COUNT( * ) FROM wptst INTO count
*           WHERE docnum = g_t_idocs-docnum
*             AND rngbegin <> 0
*             AND rngend   <> 0.
*      IF sy-subrc <> 0 OR count = 0.
        SELECT COUNT( * ) FROM wplst INTO count
           WHERE docnum = g_t_idocs-docnum.
*{   REPLACE        LNDK905093                                        1
*\             AND msgid  = 'WP'
*\             AND msgnr  = '613'.
*             AND msgid  = '/ATU/CA_RET_COMMON'.
*}   REPLACE
        IF sy-subrc <> 0 OR count = 0.
          CONTINUE.
        ENDIF.
*      ENDIF.
    ENDIF.
*EI 303301

*   append store line if the store is different from the previous IDoc
    hide-sndprn = g_t_idocs-sndprn.
    IF l_idoc-sndprn <> g_t_idocs-sndprn.
      PERFORM ib_append_store USING hide.
      CLEAR l_idoc-mestyp. " Make shure that the msg.type gets appended
    ENDIF.

*   Append message type line item if the message type is different
*   from the previous IDOC
    hide-mestypnr = g_t_idocs-mestypnr.
    IF l_idoc-mestyp <> g_t_idocs-mestyp.
      PERFORM ib_append_mestyp USING hide.
    ENDIF.

    l_idoc = g_t_idocs. " Store current IDOC as previous IDOC
  ENDLOOP.                             " at g_t_idocs.

ENDFORM.                               " IB_EXPAND_SECTION


*&---------------------------------------------------------------------*
*&      Form  IB_EXPAND_MESTYP
*&---------------------------------------------------------------------*
*       Expand the message type node
*----------------------------------------------------------------------*
FORM ib_expand_mestyp
USING command
      node STRUCTURE snodetext.

  DATA: hide       TYPE type_ib_hide,
        doc_node   LIKE snodetext,
        idoc_check.

  hide = node-hide.

* search processing information for current message type
  READ TABLE g_t_msgpro INDEX hide-mestypnr.
  IF sy-subrc <> 0.
    MESSAGE a802(wp) WITH 'SAPMWPER' 'IB_EXPAND_MESTYP' '#1'.
  ENDIF.

  READ TABLE g_t_idocs
       WITH KEY sndprn = hide-sndprn
                mestyp = g_t_msgpro-mestyp BINARY SEARCH.
  CHECK sy-subrc = 0.

  LOOP AT g_t_idocs FROM sy-tabix
    WHERE sndprn = hide-sndprn AND
          mestyp = g_t_msgpro-mestyp.

    hide-docnum = g_t_idocs-docnum.

*   Check if IDoc should be displayed in current section
    PERFORM ib_check_idoc USING hide CHANGING idoc_check.

    IF idoc_check = 'X'.
      IF g_t_msgpro-type <> 'O'.
*       IDocs with POS inbound protocol
        IF hide-section = c_section_error AND
           g_t_idocs-status <> c_idoc_status_ok AND
           g_t_idocs-status <> c_idoc_status_nok.
*          For all IDoc status except ok and nok, create a node that
*          displays this IDoc with its status in the error section
          PERFORM ib_append_idoc_status USING hide.
        ENDIF.
*       Display IDoc details
        PERFORM ib_append_idoc USING hide.
      ELSE.
*       IDocs without POS inbound protocol are simply displayed with
*       their status
        PERFORM ib_append_idoc_status USING hide.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " IB_EXPAND_MESTYP


*&---------------------------------------------------------------------*
*&      Form  IB_EXPAND_FOLDOC
*&---------------------------------------------------------------------*
*       Expand the follow-on document type node
*----------------------------------------------------------------------*
FORM ib_expand_foldoc
USING command
      node STRUCTURE snodetext.

  DATA: hide     TYPE type_ib_hide,
        doc_node LIKE snodetext,
        level(2) TYPE n.
  DATA: BEGIN OF t_foldoc OCCURS 0.
          INCLUDE TYPE wpusa_foldoc.
  DATA: END OF t_foldoc.

  hide = node-hide.

  READ TABLE g_t_foldescr WITH KEY objtype = hide-objtype
       BINARY SEARCH.

  IF sy-subrc = 0 AND NOT ( g_t_foldescr-folfunc IS INITIAL ).
    level = node-tlevel - 6 .
    CALL FUNCTION g_t_foldescr-folfunc
      EXPORTING
        objtype  = hide-objtype
        key      = hide-key
        level    = level
        segnum   = hide-segnum
        sndprn   = hide-sndprn
      TABLES
        t_foldoc = t_foldoc
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc = 0.
*     Add range to follow-on documents
      LOOP AT t_foldoc.
        t_foldoc-segnum     = hide-segnum.
        t_foldoc-segnum_end = hide-segnum_end.
        MODIFY t_foldoc.
      ENDLOOP.

*     Insert follow-on documents as children of current node
      PERFORM ib_append_foldocs TABLES t_foldoc USING hide.
    ENDIF.

  ENDIF.

ENDFORM.                               " IB_EXPAND_FOLDOC


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_STORE
*&---------------------------------------------------------------------*
*       Append store line item
*----------------------------------------------------------------------*
FORM ib_append_store
USING  hide  TYPE type_ib_hide.

  hide-sndprn = g_t_idocs-sndprn.

  CLEAR g_t_nodes.
  g_t_nodes-type       = c_node_store.
  g_t_nodes-name       = g_t_idocs-sndprn.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 4.
  g_t_nodes-nlength    = 10.
  g_t_nodes-color      = col_key.

  g_t_nodes-text       = g_t_idocs-name1.
  g_t_nodes-tlength    = 30.
  g_t_nodes-tcolor     = col_normal.
  APPEND g_t_nodes.

ENDFORM.                               " IB_APPEND_STORE


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_MESTYP
*&---------------------------------------------------------------------*
*       Append message type line item
*----------------------------------------------------------------------*
FORM ib_append_mestyp
USING  hide  TYPE type_ib_hide.

  DATA: lv_descrp TYPE edimsgt-descrp.

  hide-mestypnr = g_t_idocs-mestypnr.

  CLEAR g_t_nodes.
  g_t_nodes-type       = c_node_mestyp.
  g_t_nodes-name       = g_t_idocs-mestyp.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 5.
  g_t_nodes-nlength    = 6.
  g_t_nodes-color      = col_key.
  g_t_nodes-force_plus = 'X'.

  CALL FUNCTION 'MESSAGE_TYPE_GET_TEXT'
    EXPORTING
      mestyp = g_t_idocs-mestyp
    IMPORTING
      descrp = lv_descrp.

  g_t_nodes-text = lv_descrp.

  g_t_nodes-tlength  = 50.
  g_t_nodes-tcolor   = col_normal.
  APPEND g_t_nodes.

ENDFORM.                               " IB_APPEND_MESTYP


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_IDOC_STATUS
*&---------------------------------------------------------------------*
*       Append a node for IDocs without further detailed information
*       available
*----------------------------------------------------------------------*
FORM ib_append_idoc_status
USING  hide  TYPE type_ib_hide.

  DATA: descrp LIKE teds2-descrp.

* No external document available, use IDOC as external document
  hide-objtype = c_objtype_idoc.
  hide-key     = hide-docnum.

  CLEAR g_t_nodes.
  g_t_nodes-type        = c_node_extdoc.
  g_t_nodes-hide        = hide.
  g_t_nodes-tlevel      = 6.
  g_t_nodes-color       = col_key.

* external document key
  g_t_nodes-text1       = hide-key.
  g_t_nodes-tlength1    = 16.
  g_t_nodes-tcolor1     = col_key.

* description of external object type
  PERFORM ib_get_objtype_descr
*{   REPLACE        LNDK906038                                        1
*\       USING hide-objtype CHANGING g_t_nodes-text2.
       USING hide-objtype space CHANGING g_t_nodes-text2.
*}   REPLACE
  g_t_nodes-tlength2    = 20.
  g_t_nodes-tcolor2     = col_normal.

* Creation date of IDoc
  WRITE g_t_idocs-credat TO g_t_nodes-text3.
  g_t_nodes-tlength3    = 10.
  g_t_nodes-tcolor3     = col_normal.
  g_t_nodes-tintensiv3  = c_intensiv.

  g_t_nodes-text4      = g_t_idocs-status.
  g_t_nodes-tlength4   = 2.
  CASE g_t_idocs-status.
    WHEN c_idoc_status_ok.
      g_t_nodes-tcolor4  = col_positive.
    WHEN c_idoc_status_nok.
      g_t_nodes-tcolor4  = col_negative.
    WHEN OTHERS.
      g_t_nodes-tcolor4  = col_warning.
  ENDCASE.

  CALL FUNCTION 'POS_EDI_STATUS_READ'
    EXPORTING
      langua = sy-langu
    IMPORTING
      descrp = descrp
    CHANGING
      status = g_t_idocs-status
    EXCEPTIONS
      OTHERS = 9999.

  g_t_nodes-text5      = descrp.
  g_t_nodes-tlength5   = 50.
  g_t_nodes-tcolor5    = g_t_nodes-tcolor4.
  APPEND g_t_nodes.

  PERFORM ib_append_idoc_errors_norm USING hide.

ENDFORM.                               " IB_APPEND_IDOC_STATUS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_IDOC
*&---------------------------------------------------------------------*
*       Append the nodes belonging to an IDOC (the external documents
*       and/or error messages)
*----------------------------------------------------------------------*
FORM ib_append_idoc
USING hide TYPE type_ib_hide.

  DATA: t_extdoc     TYPE wpusa_t_extdoc     WITH HEADER LINE,
        t_upldoc     TYPE wpusa_t_upldoc     WITH HEADER LINE,
        t_foldoc     TYPE wpusa_t_foldoc     WITH HEADER LINE,
        t_doc_status TYPE wpusa_t_doc_status WITH HEADER LINE,
        t_edids      TYPE wpusa_t_edids      WITH HEADER LINE,
        t_wptst      TYPE wpusa_t_wptst      WITH HEADER LINE,
        t_wplst      TYPE wpusa_t_wplst      WITH HEADER LINE,
*       List of messages already displayed for current external document
*       It is used to eliminate duplicates
        t_messages   TYPE wpusa_t_wplst      WITH HEADER LINE,
        prev_status  LIKE poswpsa-status,
        prev_key     LIKE objectconn-objectid,
        prev_posted,
        l_msgpro     TYPE wpusa_msgpro,
        extdoc_check,
        verarbend    LIKE wptst-verarbend.

  PERFORM ib_get_document_status
  TABLES t_extdoc t_upldoc t_foldoc t_doc_status t_edids t_wptst t_wplst
  USING  hide CHANGING verarbend.

  CLEAR: prev_status, prev_key.

  LOOP AT t_doc_status.
*   Check if external document should be displayed
    PERFORM ib_check_extdoc TABLES   t_wplst t_foldoc
                            USING    t_doc_status hide
                            CHANGING extdoc_check.

    IF extdoc_check = 'X'.
*     Display external documents, but not for each range in the IDoc,
*     instead only once for each combination of external doc/status.
*     For non-splittable IDocs, the external document status is
*     displayed for earch range.
      CALL FUNCTION 'ZCCT_CA_POS_GET_MSGTYP_PARAMS'
        EXPORTING
          mestyp = g_t_msgpro-mestyp
          sndprn = hide-sndprn
        IMPORTING
          msgpro = l_msgpro.
      hide-segnum     = t_doc_status-segnum.
      hide-segnum_end = t_doc_status-segnum_end.
      IF t_doc_status-key    <> prev_key    OR
         t_doc_status-status <> prev_status OR
         t_doc_status-posted <> prev_posted OR
         l_msgpro-type       =  'N'.
        PERFORM ib_append_extdoc TABLES t_extdoc
                                 USING  hide t_doc_status.
        prev_status     = t_doc_status-status.
        prev_key        = t_doc_status-key.
        prev_posted     = t_doc_status-posted.
*       Reset list of displayed messages
        REFRESH t_messages.
      ENDIF.
*     Display follow-on documents, but only in the ok section
      IF hide-section <> c_section_error.
        PERFORM ib_append_foldocs TABLES t_foldoc USING hide.
      ENDIF.
*     Display errors and messages
      PERFORM ib_append_doc_errors TABLES t_wplst t_messages
                                   USING  hide t_doc_status.
    ENDIF.
  ENDLOOP.

**** Special processing for IDoc after replenishment prestep
***  INCLUDE mwperx01.                                              "-sp01042010
  if sy-subrc <> 0 and g_t_idocs-status <> '64'.                    "+sp01042010
*   Clear the error type from the messages so they are not
*   displayed under 'Other messages' - they have already been
*   displayed above.
    clear t_wplst-fehlertyp.                                        "+sp01042010
    modify t_wplst transporting fehlertyp where fehlertyp <> ' '.   "+sp01042010
  endif.                                                            "+sp01042010


* Display errors and messages that are not in a range
  PERFORM ib_append_idoc_errors
          TABLES t_wplst t_messages t_doc_status USING hide.

ENDFORM.                               " IB_APPEND_IDOC


*&---------------------------------------------------------------------*
*&      Form  IB_GET_DOCUMENT_STATUS
*&---------------------------------------------------------------------*
*       Get the external documents in an IDoc, their status and
*       follow-on documents. G_T_MSGPRO must be filled.
*----------------------------------------------------------------------*
FORM ib_get_document_status
TABLES   t_extdoc     TYPE wpusa_t_extdoc
         t_upldoc     TYPE wpusa_t_upldoc
         t_foldoc     TYPE wpusa_t_foldoc
         t_doc_status TYPE wpusa_t_doc_status
         t_edids      TYPE wpusa_t_edids
         t_wptst      TYPE wpusa_t_wptst
         t_wplst      TYPE wpusa_t_wplst
USING    hide TYPE type_ib_hide
CHANGING verarbend    LIKE wptst-verarbend.

  DATA   l_subrc      LIKE sy-subrc.

* Read documents for the current IDOC. At least the upload documents
* are always written to the document flow by the parser.
  CALL FUNCTION 'ZCCT_CA_POS_GET_DOC_STATUS'
    EXPORTING
      docnum            = hide-docnum
      mestyp            = g_t_msgpro-mestyp
      sndprn            = hide-sndprn
    IMPORTING
      verarbend         = verarbend
    TABLES
      doc_status        = t_doc_status
      t_edids           = t_edids
      t_wptst           = t_wptst
      t_wplst           = t_wplst
      t_extdoc          = t_extdoc
      t_upldoc          = t_upldoc
      t_foldoc          = t_foldoc
    EXCEPTIONS
      idoc_not_exist    = 1
      foreign_lock      = 2
      unknown_exception = 3
      range_not_exist   = 4
      OTHERS            = 5.
  l_subrc = sy-subrc.

*changed by liuliying for virtual material document begin 20140403
*  DATA:LV_MBLNR LIKE MKPF-MBLNR.
*  loop at t_foldoc where objtype eq 'BUS2017'.
*    SELECT SINGLE MBLNR
*      INTO LV_MBLNR
*      FROM MKPF
*     WHERE MBLNR = t_foldoc-KEY(10)
*       AND MJAHR = t_foldoc-KEY+10(4).
*    IF SY-SUBRC NE 0.
*      DELETE t_foldoc.
*    ENDIF.
*  endloop.
*changed by liuliying for virtual material document end 20140403

  IF l_subrc = 0 AND NOT ( g_t_msgpro-docfunc IS INITIAL ).
*   Call a function to read external and follow-on documents if they
*   were not or not completely written to the document flow.
    CALL FUNCTION g_t_msgpro-docfunc
      EXPORTING
        docnum      = hide-docnum
        ext_objtype = g_t_msgpro-extobjtype
      TABLES
        extdoc      = t_extdoc
        upldoc      = t_upldoc
        foldoc      = t_foldoc
        doc_status  = t_doc_status
        t_edids     = t_edids
        t_wptst     = t_wptst
        t_wplst     = t_wplst.

    SORT t_extdoc BY segnum segnum_end.
    SORT t_upldoc BY segnum segnum_end uploadkey.
    SORT t_foldoc BY segnum segnum_end uploadkey index objtype key.
  ENDIF.

* check whether enhancement for non merch items is active
*  DATA: lr_badi                 TYPE REF TO badi_pos_nonmerch_items.
*
*  TRY.
*      GET BADI lr_badi.
**  BADI Exceptions
*    CATCH cx_badi_not_implemented.
*    CATCH cx_badi_multiply_implemented.
*  ENDTRY.
*
*  IF lr_badi IS BOUND.
*    CALL BADI lr_badi->display_documents
*      EXPORTING
*        i_docnum = hide-docnum
*        i_mestyp = g_t_msgpro-mestyp
*      CHANGING
*        c_extdoc = t_extdoc[]
*        c_foldoc = t_foldoc[]
*        c_upldoc = t_upldoc[].
*  ENDIF.

  SORT t_doc_status BY objtype status_disp posted segnum segnum_end.

ENDFORM.                               " IB_GET_DOCUMENT_STATUS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_EXTDOC
*&---------------------------------------------------------------------*
*       Append external document line item
*----------------------------------------------------------------------*
FORM ib_append_extdoc
TABLES t_extdoc     TYPE wpusa_t_extdoc
USING    hide       TYPE type_ib_hide
         doc_status TYPE wpusa_doc_status.

  STATICS BEGIN OF t_edimsgt OCCURS 1.
          INCLUDE STRUCTURE edimsgt.
  STATICS END OF t_edimsgt.

  READ TABLE t_extdoc WITH KEY segnum     = doc_status-segnum
                               segnum_end = doc_status-segnum_end
                      BINARY SEARCH.

  IF doc_status-objtype IS INITIAL.
*   No external document available, use IDOC as external document
    hide-objtype = c_objtype_idoc.
    hide-key     = hide-docnum.
  ELSE.
    hide-objtype = doc_status-objtype.
    hide-key     = doc_status-key.
  ENDIF.

  CLEAR g_t_nodes.
  g_t_nodes-type        = c_node_extdoc.
  g_t_nodes-hide        = hide.
  g_t_nodes-tlevel      = 6.
  g_t_nodes-color       = col_key.

** Icon
*  G_T_NODES-TEXT    = 'icon_set_state'.
*  G_T_NODES-TLENGTH = 2.
*  G_T_NODES-KIND    = 'I'.
*  G_T_NODES-HOTSPOT = 'X'.

* external document key
  g_t_nodes-text1       = hide-key.
  g_t_nodes-tlength1    = 16.
  g_t_nodes-tcolor1     = col_key.

* description of external object type
  PERFORM ib_get_objtype_descr
*{   REPLACE        LNDK906038                                        1
*\       USING hide-objtype CHANGING g_t_nodes-text2.
       USING hide-objtype space CHANGING g_t_nodes-text2.
*}   REPLACE
  g_t_nodes-tlength2    = 20.
  g_t_nodes-tcolor2     = col_normal.

* Creation date of IDoc
  WRITE g_t_idocs-credat TO g_t_nodes-text3.
  g_t_nodes-tlength3    = 10.
  g_t_nodes-tcolor3     = col_normal.
  g_t_nodes-tintensiv3  = c_intensiv.

  PERFORM ib_append_extdoc_status
       USING g_t_nodes doc_status-status_disp.

  IF g_parameter_wps CS 'POSMON=E'.
*   Segment range
    g_t_nodes-text6       = hide-segnum.
    g_t_nodes-tlength6    = 6.
    g_t_nodes-tcolor6     = col_normal.
    g_t_nodes-text7       = hide-segnum_end.
    g_t_nodes-tlength7    = 6.
    g_t_nodes-tcolor7     = col_normal.

*   Posted flag
    g_t_nodes-text8       = doc_status-posted.
    g_t_nodes-tlength8    = 1.
    g_t_nodes-tcolor8     = col_normal.
  ENDIF.

* Posted flag
  IF doc_status-posted IS INITIAL.
    g_t_nodes-text8       = 'icon_booking_stop'.
  ELSE.
    g_t_nodes-text8       = 'icon_booking_ok'.
  ENDIF.
  g_t_nodes-kind8         = 'I'.
  g_t_nodes-tlength8      = 2.

  IF hide-segnum = 1 AND hide-segnum_end = 999999.
*   Mark whole IDoc for splittable IDocs
    g_t_nodes-text9       = '*'.
    g_t_nodes-tlength9    = 1.
  ENDIF.

  APPEND g_t_nodes.

ENDFORM.                               " IB_APPEND_EXTDOC


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_EXTDOC_STATUS
*&---------------------------------------------------------------------*
*       Append external document line item status fields
*----------------------------------------------------------------------*
FORM ib_append_extdoc_status
USING  node   LIKE snodetext
       status LIKE poswpsa-status.

* document status
  node-text4       = status.
  node-tlength4    = 1.
  CASE status.
    WHEN pos_status_error.
      node-tcolor4    = col_negative.
    WHEN pos_status_ok.
      node-tcolor4    = col_positive.
    WHEN OTHERS.
      node-tcolor4    = col_warning.
  ENDCASE.
  node-hotspot4 = 'X'.

* despription of document status
  READ TABLE g_t_status_descr WITH KEY domvalue_l = status.
  node-text5       = g_t_status_descr-ddtext.
  node-tlength5    = 30.
  node-tcolor5     = node-tcolor4.
  node-hotspot5    = 'X'.

ENDFORM.                               " IB_APPEND_EXTDOC_STATUS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_FOLDOCS
*&---------------------------------------------------------------------*
*       Append follow-on document nodes for one external document
*----------------------------------------------------------------------*
FORM ib_append_foldocs
TABLES t_foldoc TYPE wpusa_t_foldoc
USING  hide     TYPE type_ib_hide.

  DATA l_msgpro TYPE wpusa_msgpro.

  READ TABLE g_t_msgpro INDEX hide-mestypnr.

  CALL FUNCTION 'ZCCT_CA_POS_GET_MSGTYP_PARAMS'
    EXPORTING
      mestyp = g_t_msgpro-mestyp
      sndprn = hide-sndprn
    IMPORTING
      msgpro = l_msgpro.

  IF l_msgpro-type = 'N'.
*   For non-splittable IDocs, show only follow-on documents belonging
*   to current range
    READ TABLE t_foldoc WITH KEY segnum     = hide-segnum
                                 segnum_end = hide-segnum_end
                         BINARY SEARCH.
    CHECK sy-subrc = 0.

    LOOP AT t_foldoc FROM sy-tabix.
      IF t_foldoc-segnum     <> hide-segnum OR
         t_foldoc-segnum_end <> hide-segnum_end.
        EXIT.
      ENDIF.
      PERFORM ib_append_foldoc USING t_foldoc hide.
    ENDLOOP.
  ELSE.
*   For splittable IDocs, show all follow-on documents under the
*   whole IDoc.
    CHECK hide-segnum = 1 AND hide-segnum_end = 999999.
    LOOP AT t_foldoc.
      PERFORM ib_append_foldoc USING t_foldoc hide.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " IB_APPEND_FOLDOCS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_FOLDOC
*&---------------------------------------------------------------------*
*       Append a single follow-on document node
*----------------------------------------------------------------------*
FORM ib_append_foldoc
USING  foldoc   TYPE wpusa_foldoc
       hide     TYPE type_ib_hide.

  DATA: foldoc_attr TYPE wpusa_foldoc_attr,
        text(8).   " Name of text element with comment
  FIELD-SYMBOLS <fs_text> TYPE c.   " Field symbol for TEXT

  CLEAR g_t_nodes.
  hide-objtype = foldoc-objtype.
  hide-key     = foldoc-key.
  g_t_nodes-type       = c_node_foldoc.
  g_t_nodes-hide       = hide.
  g_t_nodes-tlevel     = 7 + foldoc-level.
  foldoc_attr          = foldoc-attr.
  g_t_nodes-force_plus = foldoc_attr-forceplus.

* description of follow-on document
  PERFORM ib_get_objtype_descr USING foldoc-objtype
*{   INSERT         LNDK906038                                        2
                                     foldoc-key
*}   INSERT
                               CHANGING g_t_nodes-text.
  g_t_nodes-tcolor     = col_normal.
  g_t_nodes-tlength    = 20.

* key of follow-on document
  IF g_t_foldescr-length > 0.
    g_t_nodes-name =
         foldoc-key+g_t_foldescr-offset(g_t_foldescr-length).
  ELSE.
    g_t_nodes-name      = foldoc-key.
  ENDIF.
  g_t_nodes-nlength   = 16.
  g_t_nodes-color     = col_key.

* comment for follow-on document
  IF NOT ( foldoc_attr-comment IS INITIAL ).
    CONCATENATE 'text-' foldoc_attr-comment INTO text.
    ASSIGN (text) TO <fs_text>.
    g_t_nodes-text1      = <fs_text>.
    g_t_nodes-tlength1   = 20.
    g_t_nodes-tcolor1    = col_warning.
  ENDIF.

  APPEND g_t_nodes.
  IF foldoc-objtype = '/ATU/RCOPA'.
    INSERT INITIAL LINE INTO TABLE gt_copa_nodes ASSIGNING <gf_copa_node>.
    MOVE-CORRESPONDING foldoc TO <gf_copa_node>.
    MOVE-CORRESPONDING g_t_nodes TO <gf_copa_node>.
  ENDIF.

ENDFORM.                               " IB_APPEND_FOLDOC


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_DOC_ERRORS
*&---------------------------------------------------------------------*
*       Append error nodes for one external document
*----------------------------------------------------------------------*
FORM ib_append_doc_errors
TABLES t_wplst    TYPE wpusa_t_wplst
       t_messages TYPE wpusa_t_wplst
USING  hide       TYPE type_ib_hide
       doc_status TYPE wpusa_doc_status.

  DATA: workitem_msg_created.

  LOOP AT t_wplst WHERE segnum >= doc_status-segnum
                    AND segnum <= doc_status-segnum_end.

    IF hide-section = c_section_error.
*     Check for workitem messages in the error section
      IF doc_status-status = pos_status_workitem_created AND
         t_wplst-fehlertyp = 'F'.
*       Message type F: Display ALL messages of type F of the whole
*       IDoc together as children of the message WP613. This message
*       is always found at segment 2.
*       Check if WP613 is still there. If not, all messages of type F
*       F have already been displayed, so simply go on.
        READ TABLE t_wplst WITH KEY segnum = 2
                                    msgid = 'WP'
                                    msgnr = '613'.
        IF sy-subrc = 0.
*         WP613 still there, delete it so that the messages of type F
*         are displayed only once
          DELETE t_wplst INDEX sy-tabix.
          PERFORM ib_append_error USING hide t_wplst.
*         Display all F-messages as children of the message WP613
          LOOP AT t_wplst WHERE fehlertyp = 'F'.
            PERFORM ib_append_error_workitem USING hide t_wplst.
          ENDLOOP.
        ENDIF.
        CONTINUE.
      ENDIF.

*     Display all messages with message type 'J' as children of
*     a message WP613 that exists only in the display, not in WPLST.
      IF t_wplst-fehlertyp = 'J' AND workitem_msg_created IS INITIAL.
        workitem_msg_created = 'X'.
        g_t_wplst-segnum = doc_status-segnum.
        PERFORM ib_append_error USING hide g_wplst_wp613.
        LOOP AT t_wplst WHERE fehlertyp = 'J' AND
            segnum BETWEEN doc_status-segnum AND doc_status-segnum_end.
          PERFORM ib_append_error_workitem USING hide t_wplst.
        ENDLOOP.
      ENDIF.
    ENDIF.

*   Ignore message WP613 for normal processing (see above)
    IF t_wplst-msgid = 'WP' AND t_wplst-msgnr = '613'.
      CONTINUE.
    ENDIF.

*   Display each message only once unless it has a different LFDNR
*   or REFNR
    IF t_wplst-msgid = 'WP' AND t_wplst-msgnr = '751'.
*     Ignore parameters for message WP751
      READ TABLE t_messages WITH KEY
           fehlertyp  = t_wplst-fehlertyp
           msgid      = t_wplst-msgid
           msgnr      = t_wplst-msgnr
           lfdnr      = t_wplst-lfdnr     BINARY SEARCH.
    ELSE.
      READ TABLE t_messages WITH KEY
           fehlertyp  = t_wplst-fehlertyp
           msgid      = t_wplst-msgid
           msgnr      = t_wplst-msgnr
           lfdnr      = t_wplst-lfdnr
           parameter1 = t_wplst-parameter1
           parameter2 = t_wplst-parameter2
           parameter3 = t_wplst-parameter3
           parameter4 = t_wplst-parameter4
           refnr      = t_wplst-refnr      BINARY SEARCH.
    ENDIF.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      MOVE t_wplst TO t_messages.
      INSERT t_messages INDEX sy-tabix.
    ENDIF.

    IF NOT ( hide-section = c_section_ok AND
             ( t_wplst-fehlertyp = 'E' OR
               t_wplst-fehlertyp CA c_hints )
          OR t_wplst-fehlertyp CA 'FJ'
          OR ( doc_status-status = pos_status_workitem_created AND
               t_wplst-fehlertyp = 'E' ) ).
*     Don't display errors or hints in the ok section
*     Don't display messages of type 'F' or 'J' at all
*     Don't display error messages in status "Workitem created" because
*       they belong in status "Error"
      PERFORM ib_append_error USING hide t_wplst.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " IB_APPEND_DOC_ERRORS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_IDOC_ERRORS
*&---------------------------------------------------------------------*
*       Append error nodes that are not in a range
*----------------------------------------------------------------------*
FORM ib_append_idoc_errors
TABLES t_wplst      TYPE wpusa_t_wplst
       t_messages   TYPE wpusa_t_wplst
       t_doc_status TYPE wpusa_t_doc_status
USING  hide         TYPE type_ib_hide.

  DATA first_time VALUE 'X'.

* Display the leftover messages in the section 'Other messages'
* No external document available, use IDOC as external document
  hide-objtype = c_objtype_idoc.
  hide-key     = hide-docnum.

  CLEAR g_t_nodes.
  g_t_nodes-type        = c_node_other.
  g_t_nodes-hide        = hide.
  g_t_nodes-tlevel      = 6.
  g_t_nodes-color       = col_key.

* external document key
  g_t_nodes-text1       = hide-docnum.
  g_t_nodes-tlength1    = 16.
  g_t_nodes-tcolor1     = col_key.

* description of external object type
  PERFORM ib_get_objtype_descr
*{   REPLACE        LNDK906038                                        1
*\       USING c_objtype_idoc CHANGING g_t_nodes-text2.
       USING c_objtype_idoc
             space
    CHANGING g_t_nodes-text2.
*}   REPLACE
  g_t_nodes-tlength2    = 20.
  g_t_nodes-tcolor2     = col_normal.

* Creation date of IDoc
  WRITE g_t_idocs-credat TO g_t_nodes-text3.
  g_t_nodes-tlength3    = 10.
  g_t_nodes-tcolor3     = col_normal.
  g_t_nodes-tintensiv3  = c_intensiv.

* Text 'Other messages'
  g_t_nodes-text4       = text-221.
  g_t_nodes-tlength4    = 30.
  g_t_nodes-tcolor4     = col_warning.

  LOOP AT t_doc_status.
    READ TABLE t_wplst WITH KEY segnum = t_doc_status-segnum
                       BINARY SEARCH.
    IF sy-tabix <> 0.
      LOOP AT t_wplst FROM sy-tabix.
        IF t_wplst-segnum > t_doc_status-segnum_end.
          EXIT.
        ENDIF.
*       Mark the messages in ranges
        CLEAR t_wplst-fehlertyp.
        MODIFY t_wplst.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

* Show the unmarked messages
  LOOP AT t_wplst WHERE NOT ( fehlertyp IS INITIAL ).
*   Don't display errors or hints in the ok section
    IF NOT ( hide-section = c_section_ok AND
             ( t_wplst-fehlertyp = 'E' OR
               t_wplst-fehlertyp CA c_hints ) ).
*   Display each message only once unless it has a different LFDNR
*   or REFNR
      READ TABLE t_messages WITH KEY
           fehlertyp  = t_wplst-fehlertyp
           msgid      = t_wplst-msgid
           msgnr      = t_wplst-msgnr
           lfdnr      = t_wplst-lfdnr
           parameter1 = t_wplst-parameter1
           parameter2 = t_wplst-parameter2
           parameter3 = t_wplst-parameter3
           parameter4 = t_wplst-parameter4
           refnr      = t_wplst-refnr      BINARY SEARCH.
      IF sy-subrc <> 0.
        MOVE t_wplst TO t_messages.
        INSERT t_messages INDEX sy-tabix.

        IF first_time = 'X'.
          APPEND g_t_nodes.
          CLEAR first_time.
        ENDIF.

        PERFORM ib_append_error USING hide t_wplst.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " IB_APPEND_IDOC_ERRORS


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_IDOC_ERRORS_NORM
*&---------------------------------------------------------------------*
*       Append error nodes form normal IDocs (without parser)
*----------------------------------------------------------------------*
FORM ib_append_idoc_errors_norm
USING  hide         TYPE type_ib_hide.

  DATA t_wplst    LIKE wplst OCCURS 0 WITH HEADER LINE.
* List of messages already displayed for current external document
* It is used to eliminate duplicates
  DATA t_messages TYPE wpusa_t_wplst  WITH HEADER LINE.

  SELECT * FROM wplst INTO TABLE t_wplst
       WHERE docnum = hide-docnum.

  LOOP AT t_wplst.
*   Display each message only once unless it has a different LFDNR
*   or REFNR
    READ TABLE t_messages WITH KEY
         fehlertyp  = t_wplst-fehlertyp
         msgid      = t_wplst-msgid
         msgnr      = t_wplst-msgnr
         lfdnr      = t_wplst-lfdnr
         parameter1 = t_wplst-parameter1
         parameter2 = t_wplst-parameter2
         parameter3 = t_wplst-parameter3
         parameter4 = t_wplst-parameter4
         refnr      = t_wplst-refnr      BINARY SEARCH.
    IF sy-subrc <> 0.
      MOVE t_wplst TO t_messages.
      INSERT t_messages INDEX sy-tabix.
      PERFORM ib_append_error USING hide t_wplst.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " IB_APPEND_IDOC_ERRORS_NORM


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_ERROR
*&---------------------------------------------------------------------*
*       Append error node
*----------------------------------------------------------------------*
FORM ib_append_error
USING    hide     TYPE type_ib_hide
         t_wplst  LIKE wplst.

  DATA: messg LIKE message,
        level TYPE i.

* Append a node for each error
  PERFORM write_message USING t_wplst messg.

* The message parameters are stored in a seperate table because the
* hide area of the node is not long enough to keep them. Only the
* index to the table is stored in the hide area. This is necessary
* because the parameters are needed to display the message long text.
  APPEND t_wplst TO g_t_wplst.
  WRITE sy-tabix LEFT-JUSTIFIED TO hide-key.

  CLEAR g_t_nodes.
  g_t_nodes-type     = c_node_error.
  g_t_nodes-hide     = hide.
  g_t_nodes-tlevel   = 7.
  g_t_nodes-color    = col_key.

  IF NOT ( t_wplst-refnr IS INITIAL ).
    g_t_nodes-text1    = t_wplst-refnr.
    g_t_nodes-tlength1 = 15.
    g_t_nodes-tcolor1  = col_key.
    CONCATENATE messg-msgtx ' (' t_wplst-refnr ')' INTO messg-msgtx.
  ENDIF.

  g_t_nodes-text    = messg-msgtx.
  g_t_nodes-tlength = 80.
  IF messg-msgty CA 'ISG'.
    g_t_nodes-tcolor = col_positive.
  ELSEIF messg-msgty CA 'ERA'.
    g_t_nodes-tcolor = col_negative.
  ELSEIF messg-msgty CA 'HW'.
    g_t_nodes-tcolor = col_warning.
  ENDIF.

  APPEND g_t_nodes.

ENDFORM.                               " IB_APPEND_error


*&---------------------------------------------------------------------*
*&      Form  IB_APPEND_ERROR_WORKITEM
*&---------------------------------------------------------------------*
*       Append error node for workitem errors
*----------------------------------------------------------------------*
FORM ib_append_error_workitem
USING    hide     TYPE type_ib_hide
         t_wplst  LIKE wplst.

  DATA: messg LIKE message,
        level TYPE i.

* Append a node for each error
  PERFORM write_message USING t_wplst messg.

  CLEAR g_t_nodes.
  g_t_nodes-type    = c_node_workitem.
  hide-objtype = 'WORKINGWI'.
  IF t_wplst-refnr IS INITIAL.
    hide-key     = t_wplst-proglokali.
  ELSE.
    hide-key     = t_wplst-refnr.
  ENDIF.
  g_t_nodes-hide    = hide.

  g_t_nodes-tlevel  = 8.
  g_t_nodes-text1    = messg-msgtx.
  g_t_nodes-tlength1 = 80.
  g_t_nodes-tcolor1  = col_warning.

  APPEND g_t_nodes.

ENDFORM.                               " IB_APPEND_ERROR_WORKITEM


*&---------------------------------------------------------------------*
*&      Form  IB_INIT
*&---------------------------------------------------------------------*
*       Initialize Sales Audit
*----------------------------------------------------------------------*
FORM  ib_init.

  DATA: tab_lines LIKE sy-tabix,
        node_info LIKE snodetext,
        node      LIKE seucomm,
        hide      TYPE type_hide.

  i_z8  = 0.
  i_z8a = 0.
  REFRESH g_t_idocs.
  REFRESH g_t_wplst.

* read processing parameters for message types
  DESCRIBE TABLE g_t_msgpro LINES tab_lines.
  IF tab_lines = 0.
    CALL FUNCTION 'ZCCT_CA_POS_GET_MSGTYP_PROCESS'
      TABLES
        msgpro   = g_t_msgpro
        foldescr = g_t_foldescr.
  ENDIF.

* read descriptions of document status
  DESCRIBE TABLE g_t_status_descr LINES tab_lines.
  IF tab_lines = 0.
    SELECT * FROM dd07t INTO TABLE g_t_status_descr
        WHERE domname    = 'POSSTATUS'
          AND ddlanguage = sy-langu
          AND as4local   = 'A'.
    SORT g_t_status_descr BY domvalue_l.
  ENDIF.

* read descriptions of idoc status
  DESCRIBE TABLE g_t_idocs_status_descr LINES tab_lines.
  IF tab_lines = 0.
    SELECT * FROM teds2 INTO TABLE g_t_idocs_status_descr
        WHERE langua = sy-langu
        ORDER BY status.
  ENDIF.

  IF NOT ( s_docnum-low IS INITIAL ) AND s_docnum-high IS INITIAL.
*   IDoc number is specified, display this IDoc
    CALL FUNCTION 'RS_TREE_GET_NODE'
      EXPORTING
        node_id      = '000009'
      IMPORTING
        node_info    = node_info
      EXCEPTIONS
        id_not_found = 1
        OTHERS       = 2.
    hide = node_info-hide.
    MOVE-CORRESPONDING node_info TO node.
    PERFORM tree_node_expand USING 'TREP' hide node 99.
  ENDIF.

ENDFORM.                               " IB_INIT


*&---------------------------------------------------------------------*
*&      Form  IB_NODE_PICK
*&---------------------------------------------------------------------*
*       execute pick function on selected node
*----------------------------------------------------------------------*
FORM ib_node_pick
USING node STRUCTURE seucomm.

  DATA: hide      TYPE type_ib_hide,
        object    LIKE obj_record.

  hide = node-hide.

  CASE node-type.
    WHEN '' ."c_node_extdoc.
      IF node-selfield = 'TEXT4' OR node-selfield = 'TEXT5'.
*       Display all status of external document
        PERFORM ib_detail_extdoc_status USING hide node.
      ELSE.
*       Call IDoc display or Sales Audit Editor
        READ TABLE g_t_msgpro INDEX hide-mestypnr.
        IF g_t_msgpro-editable IS INITIAL.
          PERFORM ib_detail_extdoc USING hide node.
        ELSE.
          PERFORM ib_detail_editor USING hide node.
        ENDIF.
      ENDIF.

    WHEN c_node_foldoc OR c_node_extdoc.
      PERFORM ib_detail_document USING hide node.

    WHEN c_node_other.
      PERFORM ib_detail_extdoc USING hide node.

    WHEN c_node_error.
*     display error long text
      PERFORM ib_detail_error  USING hide node.

    WHEN c_node_workitem.
      PERFORM ib_detail_document USING hide node.

  ENDCASE.

ENDFORM.                               " IB_NODE_PICK


*&---------------------------------------------------------------------*
*&      Form  IB_NODE_COMMAND
*&---------------------------------------------------------------------*
*       execute functions for selected node
*----------------------------------------------------------------------*
FORM ib_node_command
USING command TYPE c
      node STRUCTURE seucomm.

  DATA hide TYPE type_ib_hide.

  hide = node-hide.

  CASE command.
*###EDITOR
    WHEN c_command_ib_edit.
      PERFORM ib_mass_editor USING node.

*   when c_command_proc_imm. "###

    WHEN c_command_reject.
      PERFORM ib_status_mass_change USING node pos_status_reject_user.

    WHEN c_command_reverse.
      PERFORM ib_status_mass_change USING node pos_status_reversed.

    WHEN c_command_process.
      PERFORM ib_status_mass_change USING node pos_status_process.

    WHEN c_command_resubmit.
      PERFORM ib_status_mass_change USING node pos_status_resubmit.

    WHEN c_command_disp_seg.
      PERFORM ib_segment_display USING hide node.

    WHEN c_command_status_dialog.
      IF node-type = c_node_extdoc.
*       Display all status of external document
        PERFORM ib_detail_extdoc_status USING hide node.
      ELSE.
*       Cursor is on wrong node type
        MESSAGE e026.
      ENDIF.
  ENDCASE.

ENDFORM.                               " IB_NODE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  IB_SEGMENT_DISPLAY
*&---------------------------------------------------------------------*
*       Display segment
*----------------------------------------------------------------------*
FORM ib_segment_display
USING hide TYPE type_ib_hide
      node STRUCTURE seucomm.

  DATA l_segnum LIKE edidd-segnum.

  IF node-type = c_node_error.
*   read message parameters
    READ TABLE g_t_wplst INDEX hide-key.
    IF sy-subrc = 0.
      l_segnum = g_t_wplst-segnum.
    ENDIF.
  ENDIF.

  IF l_segnum IS INITIAL.
    l_segnum = hide-segnum.
  ENDIF.

  IF l_segnum IS INITIAL.
    MESSAGE e033.
    EXIT.
  ENDIF.

  CALL FUNCTION 'EDI_DATA_RECORD_DISPLAY'
    EXPORTING
      docnum               = hide-docnum
      segnum               = l_segnum
    EXCEPTIONS
      no_data_record_found = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " IB_SEGMENT_DISPLAY


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_ERROR
*&---------------------------------------------------------------------*
*       display error long text
*----------------------------------------------------------------------*
FORM ib_detail_error
USING hide TYPE type_ib_hide
      node STRUCTURE seucomm.

  DATA: docu_object LIKE dokhl-object,
        message     LIKE message.

  DATA: dummy1 LIKE dselc OCCURS 0,
        dummy2 LIKE dval  OCCURS 0.

* read message parameters
  READ TABLE g_t_wplst INDEX hide-key.
  CHECK sy-subrc = 0.

  docu_object   = g_t_wplst-msgid.
  docu_object+2 = g_t_wplst-msgnr.

* Replace the hint message types by standard message types.
  TRANSLATE g_t_wplst-fehlertyp USING 'REHWGSFWJW'.

  PERFORM write_message USING g_t_wplst message.

* Display longtext
  CLEAR help_info.
  help_info-call       = 'D'.
  help_info-spras      = sy-langu.
  help_info-messageid  = g_t_wplst-msgid.
  help_info-messagenr  = g_t_wplst-msgnr.
  help_info-message    = message-msgtx.
  help_info-title      = text-207.     "Langtext
  help_info-docuid     = 'NA'.         "Konstante
  help_info-docuobject = docu_object.
  help_info-msgv1      = g_t_wplst-parameter1.
  help_info-msgv2      = g_t_wplst-parameter2.
  help_info-msgv3      = g_t_wplst-parameter3.
  help_info-msgv4      = g_t_wplst-parameter4.

  CALL FUNCTION 'HELP_START'
    EXPORTING
      help_infos   = help_info
    TABLES
      dynpselect   = dummy1
      dynpvaluetab = dummy2.

ENDFORM.                               " IB_DETAIL_ERROR


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_WPTST
*&---------------------------------------------------------------------*
*       display error details from WPTST
*----------------------------------------------------------------------*
FORM ib_detail_wptst
USING hide TYPE type_ib_hide
      node STRUCTURE seucomm.

* read current message parameters; the header line is used in the
* PBO of screen 200
  READ TABLE g_t_wplst INDEX hide-key.

  IF sy-subrc = 0.
    CALL SCREEN '200' STARTING AT 1 1 ENDING AT 80 16.
  ENDIF.

ENDFORM.                               " IB_DETAIL_WPTST


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_EXTDOC_STATUS
*&---------------------------------------------------------------------*
*       display all external document status and all messages belonging
*       to current external document; user can change the status
*----------------------------------------------------------------------*
FORM ib_detail_extdoc_status
USING hide TYPE type_ib_hide
      node STRUCTURE seucomm.

  DATA: node_tree LIKE snodetext.

  IF sy-subrc = 0.
    READ TABLE g_t_msgpro INDEX hide-mestypnr.
    CALL FUNCTION 'POS_SA_SCREEN_STATUS'
      EXPORTING
        docnum     = hide-docnum
        segnum     = hide-segnum
        segnum_end = hide-segnum_end
        key        = hide-key
        mestyp     = g_t_msgpro-mestyp
        sndprn     = hide-sndprn
      IMPORTING
        status     = poswpsa-status.
  ENDIF.

  node_tree = node.
  PERFORM ib_append_extdoc_status USING node_tree poswpsa-status.

  CALL FUNCTION 'RS_TREE_SET_NODE'
    EXPORTING
      node_info    = node_tree
    EXCEPTIONS
      id_not_found = 1
      OTHERS       = 2.

ENDFORM.                               " IB_DETAIL_EXTDOC_STATUS


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_DOCUMENT
*&---------------------------------------------------------------------*
*       execute display method of object of current document
*----------------------------------------------------------------------*
FORM ib_detail_document
USING hide TYPE      type_ib_hide
      node STRUCTURE seucomm.

  DATA: object    TYPE swc_object,
        container LIKE swcont OCCURS 0,
        l_msgno   LIKE sy-msgno,
        l_message LIKE message,
        lv_awtyp  TYPE awtyp,
        lv_awref  TYPE awref,
        lv_bukrs  TYPE bukrs.

  DATA: BEGIN OF li_documents OCCURS 0,
          include TYPE acc_doc.
  DATA: END OF li_documents.

  CONSTANTS:
        lc_copa TYPE awtyp VALUE 'COPA'.

  READ TABLE g_t_foldescr WITH KEY hide-objtype.
  IF sy-subrc = 0 AND g_t_foldescr-display_idoc = 'X'.
*   For this follow-on document, the IDoc display should be called.
*   This happens for follow-on documents which are only parts of the
*   IDoc.
    PERFORM ib_detail_extdoc USING hide node.
  ELSE.

    IF NOT ( hide-key IS INITIAL ).
      CASE hide-objtype.

        WHEN space.
*      IF hide-objtype IS INITIAL.
*       IDOC without external documents, simply display IDOC
          swc_create_object object c_objtype_idoc hide-key.
*      ELSE.

        WHEN 'DUMMY_COPA'.

          lv_awtyp = hide-key(5).
          lv_awref = hide-key+5(10).
          lv_bukrs = hide-key+15(4).

          CALL FUNCTION 'AC_DOCUMENT_RECORD'
            EXPORTING
              i_awtyp      = lv_awtyp
              i_awref      = lv_awref
              i_awtyp_incl = lc_copa
              i_bukrs      = lv_bukrs
            TABLES
              t_documents  = li_documents
            EXCEPTIONS
              no_reference = 1
              no_document  = 2
              OTHERS       = 3.

        WHEN '/ATU/RCOPA'.
*       The Tree communication structure is unable to hold the full
*       refefence key for /ATU/RCOPA BO. Therefore value is read from
*       global internal table for the SWC display method call.
          READ TABLE gt_copa_nodes WITH KEY hide = hide ASSIGNING <gf_copa_node>.
          IF sy-subrc = 0.
*          Call display method of the object of CO-PA document
            swc_create_object object <gf_copa_node>-objtype <gf_copa_node>-key.
            IF sy-subrc = 0.
              swc_call_method object 'DISPLAY' container.
              UNASSIGN <gf_copa_node>.
            ENDIF.
            IF sy-subrc <> 0.
              l_msgno = sy-subrc - 7200.
              CALL FUNCTION 'WRITE_MESSAGE'
                EXPORTING
                  msgid = 'OL'
                  msgno = l_msgno
                  msgty = 'E'
                  msgv1 = sy-msgv1
                  msgv2 = sy-msgv2
                  msgv3 = sy-msgv3
                  msgv4 = sy-msgv4
                  msgv5 = ''
                IMPORTING
                  messg = l_message.

              CONDENSE l_message.
              sy-msgv1 = l_message+0(50).
              sy-msgv2 = l_message+50(50).
              MESSAGE ID 'WP' TYPE 'E' NUMBER '729' WITH sy-msgv1 sy-msgv2.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
*       Call display method of the object of the follow-on document
          swc_create_object object hide-objtype hide-key.
          IF sy-subrc = 0.
            swc_call_method object 'DISPLAY' container.
          ENDIF.
          IF sy-subrc <> 0.
            l_msgno = sy-subrc - 7200.
            CALL FUNCTION 'WRITE_MESSAGE'
              EXPORTING
                msgid = 'OL'
                msgno = l_msgno
                msgty = 'E'
                msgv1 = sy-msgv1
                msgv2 = sy-msgv2
                msgv3 = sy-msgv3
                msgv4 = sy-msgv4
                msgv5 = ''
              IMPORTING
                messg = l_message.

            CONDENSE l_message.
            sy-msgv1 = l_message+0(50).
            sy-msgv2 = l_message+50(50).
            MESSAGE ID 'WP' TYPE 'E' NUMBER '729' WITH sy-msgv1 sy-msgv2.
          ENDIF.
      ENDCASE.
*      ENDIF.
      swc_free_object   object.
    ENDIF.
  ENDIF.

ENDFORM.                               " IB_DETAIL_DOCUMENT


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_EXTDOC
*&---------------------------------------------------------------------*
*       execute display method of external document (= display IDOC)
*----------------------------------------------------------------------*
FORM ib_detail_extdoc
USING hide TYPE      type_ib_hide
      node STRUCTURE seucomm.

  DATA: object    TYPE swc_object,
        container LIKE swcont OCCURS 0.

* Display IDOC with external document
  swc_create_object object c_objtype_idoc hide-docnum.
  swc_call_method object 'DISPLAY' container.
  swc_free_object   object.

ENDFORM.                               " IB_DETAIL_EXTDOC


*&---------------------------------------------------------------------*
*&      Form  IB_DETAIL_EDITOR
*&---------------------------------------------------------------------*
*       Call Sales Audit Editor for a single node
*----------------------------------------------------------------------*
FORM ib_detail_editor
USING hide TYPE      type_ib_hide
      node STRUCTURE seucomm.

  DATA l_t_idocs LIKE posedcallerinterface OCCURS 0 WITH HEADER LINE.

  l_t_idocs-docnum      = hide-docnum.
  IF hide-objtype <> c_objtype_idoc.
    l_t_idocs-fromsegment = hide-segnum.
    l_t_idocs-tosegment   = hide-segnum_end.
  ENDIF.
  APPEND l_t_idocs.

  CALL FUNCTION 'POSED_CALL'
    TABLES
      i_idocs = l_t_idocs.

ENDFORM.                               " IB_DETAIL_EDITOR


*&---------------------------------------------------------------------*
*&      Form  IB_MASS_EDITOR
*&---------------------------------------------------------------------*
*       Call Sales Audit Editor for set of IDocs
*----------------------------------------------------------------------*
FORM ib_mass_editor
USING node STRUCTURE seucomm.

  DATA l_t_idocs LIKE posedcallerinterface OCCURS 0 WITH HEADER LINE.
  DATA hide        TYPE type_ib_hide.
  DATA cursor_hide TYPE type_ib_hide.
  DATA idoc_check.

  cursor_hide = node-hide.
  IF cursor_hide-area <> c_area_inbound.
    MESSAGE e021 WITH text-200.
*   Bitte positionieren Sie den Cursor im Abschnitt &.
    EXIT.
  ENDIF.

* Read selected IDocs, if necessary
  PERFORM read_idocs_inbound.

* Call the editor with the selected IDocs
  LOOP AT g_t_idocs.

    IF ( cursor_hide-sndprn   = '' OR
         cursor_hide-sndprn   = g_t_idocs-sndprn ) AND
       ( cursor_hide-mestypnr = 0 OR
         cursor_hide-mestypnr = g_t_idocs-mestypnr ) AND
       ( cursor_hide-docnum   = '' OR
         cursor_hide-docnum   = g_t_idocs-docnum ).

*     Get processing parameters for current message type
      IF g_t_msgpro-mestyp <> g_t_idocs-mestyp.
        CLEAR g_t_msgpro.
        READ TABLE g_t_msgpro WITH KEY mestyp = g_t_idocs-mestyp
                              BINARY SEARCH.
      ENDIF.

      CHECK NOT ( g_t_msgpro-editable IS INITIAL ).

      hide = cursor_hide.
      hide-sndprn   = g_t_idocs-sndprn.
      hide-mestypnr = g_t_idocs-mestypnr.
      hide-docnum   = g_t_idocs-docnum.

*     Check if IDoc is selected
      PERFORM ib_check_idoc USING hide CHANGING idoc_check.

      IF idoc_check = 'X'.
        l_t_idocs-docnum = g_t_idocs-docnum.
        APPEND l_t_idocs.
      ENDIF.
    ENDIF.

  ENDLOOP.

  READ TABLE l_t_idocs INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    CALL FUNCTION 'POSED_CALL'
      TABLES
        i_idocs = l_t_idocs.
  ELSE.
    MESSAGE e037(wp).
  ENDIF.

ENDFORM.                               " IB_MASS_EDITOR


*&---------------------------------------------------------------------*
*&      Form  IB_STATUS_MASS_CHANGE
*&---------------------------------------------------------------------*
*       Call the Sales Audit Editor
*----------------------------------------------------------------------*
FORM ib_status_mass_change
USING node       STRUCTURE seucomm     " current node
      status_new LIKE poswpsa-status.

  DATA: hide        TYPE type_ib_hide,
        cursor_hide TYPE type_ib_hide,
        idoc_check,
        ext_docs_changed TYPE i,
        answer.

  cursor_hide = node-hide.
  IF cursor_hide-area <> c_area_inbound.
    MESSAGE e021 WITH text-200.
*   Bitte positionieren Sie den Cursor im Abschnitt &.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption = 'N'
      textline1     = text-251
      textline2     = text-252
      titel         = text-250
*     START_COLUMN  = 25
*     START_ROW     = 6
    IMPORTING
      answer        = answer
    EXCEPTIONS
      OTHERS        = 1.

  CHECK answer = 'J' AND sy-subrc = 0.

* Read selected IDocs, if necessary
  PERFORM read_idocs_inbound.

* Try to change the status of all selected external documents
  LOOP AT g_t_idocs.

    IF ( cursor_hide-sndprn   = '' OR
         cursor_hide-sndprn   = g_t_idocs-sndprn ) AND
       ( cursor_hide-mestypnr = 0 OR
         cursor_hide-mestypnr = g_t_idocs-mestypnr ) AND
       ( cursor_hide-docnum   = '' OR
         cursor_hide-docnum   = g_t_idocs-docnum ).

*     Get processing parameters for current message type
      IF g_t_msgpro-mestyp <> g_t_idocs-mestyp.
        CLEAR g_t_msgpro.
        READ TABLE g_t_msgpro WITH KEY mestyp = g_t_idocs-mestyp
                              BINARY SEARCH.
      ENDIF.

      hide = cursor_hide.
      hide-sndprn   = g_t_idocs-sndprn.
      hide-mestypnr = g_t_idocs-mestypnr.
      hide-docnum   = g_t_idocs-docnum.

*     Check if IDoc is selected
      PERFORM ib_check_idoc USING hide CHANGING idoc_check.

      IF idoc_check = 'X'.
*       Change the status of external documents in current IDoc
        PERFORM ib_status_mass_change_idoc USING hide status_new
                                           CHANGING ext_docs_changed.
      ENDIF.
    ENDIF.

  ENDLOOP.

  MESSAGE s027 WITH ext_docs_changed.

ENDFORM.                               " IB_STATUS_MASS_CHANGE


*&---------------------------------------------------------------------*
*&      Form  IB_STATUS_MASS_CHANGE_IDOC
*&---------------------------------------------------------------------*
*       Append the nodes belonging to an IDOC (the external documents
*       and/or error messages)
*----------------------------------------------------------------------*
FORM ib_status_mass_change_idoc
USING    hide             TYPE type_ib_hide
         status_new       LIKE poswpsa-status
CHANGING ext_docs_changed TYPE i.

  DATA: t_extdoc     TYPE wpusa_t_extdoc     WITH HEADER LINE,
        t_upldoc     TYPE wpusa_t_upldoc     WITH HEADER LINE,
        t_foldoc     TYPE wpusa_t_foldoc     WITH HEADER LINE,
        t_doc_status TYPE wpusa_t_doc_status WITH HEADER LINE,
        t_edids      TYPE wpusa_t_edids      WITH HEADER LINE,
        t_wptst      TYPE wpusa_t_wptst      WITH HEADER LINE,
        t_wplst      TYPE wpusa_t_wplst      WITH HEADER LINE,
        prev_status  LIKE poswpsa-status,
        extdoc_check,
        verarbend    LIKE wptst-verarbend,
        time_diff    TYPE i.
  STATICS time_commit  LIKE sy-uzeit.

  PERFORM ib_get_document_status
  TABLES t_extdoc t_upldoc t_foldoc t_doc_status t_edids t_wptst t_wplst
  USING  hide  CHANGING verarbend.

  CLEAR prev_status.
  LOOP AT t_doc_status.
*   Check if external document is selected
    PERFORM ib_check_extdoc TABLES   t_wplst t_foldoc
                            USING    t_doc_status hide
                            CHANGING extdoc_check.

    IF extdoc_check = 'X'.
      CALL FUNCTION 'POS_SA_SET_DOCUMENT_STATUS'
        EXPORTING
          docnum             = g_t_idocs-docnum
          segnum             = t_doc_status-segnum
          segnum_end         = t_doc_status-segnum_end
          status_new         = status_new
          status_old         = t_doc_status-status
          mestyp             = g_t_msgpro-mestyp
          sndprn             = hide-sndprn
          ext_objid          = t_doc_status-key
          verarbend          = verarbend
        TABLES
          doc_status         = t_doc_status
          t_edids            = t_edids
          t_wptst            = t_wptst
          t_wplst            = t_wplst
        EXCEPTIONS
          change_not_allowed = 1
          foreign_lock       = 2
          idoc_not_exist     = 3
          db_error           = 4
          unknown_exception  = 5
          range_not_exist    = 6
          OTHERS             = 7.

      IF sy-subrc > 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF sy-subrc = 0.
        ADD 1 TO ext_docs_changed.
*       Commit every 150 seconds to prevent time-out
        time_diff = ( sy-uzeit - time_commit ) MOD 86400.
        IF time_diff > 150.
          time_commit = sy-uzeit.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " IB_STATUS_MASS_CHANGE_IDOC


*&---------------------------------------------------------------------*
*&      Form  IB_GET_OBJTYPE_DESCR
*&---------------------------------------------------------------------*
*       Get description of object type document
*----------------------------------------------------------------------*
FORM ib_get_objtype_descr
USING    objtype LIKE objectconn-objecttype
*{   INSERT         LNDK906038                                        3
         key
*}   INSERT
CHANGING descr TYPE c.

  DATA tabix LIKE sy-tabix.
*{   INSERT         LNDK906038                                        2
  data: l_fkart type fkart.
  data: l_auart type auart.
  data: l_blart type blart.
*}   INSERT

  READ TABLE g_t_foldescr WITH KEY objtype = objtype BINARY SEARCH.
  tabix = sy-tabix.
  IF sy-subrc <> 0.
*   no description available, create empty one first
    CLEAR g_t_foldescr.
    g_t_foldescr-objtype = objtype.
    INSERT g_t_foldescr INDEX tabix.
  ENDIF.

  IF g_t_foldescr-descr IS INITIAL.
*   no description available, read the keytext from the OR
    CALL FUNCTION 'SWO_TEXT_OBJTYPE'
      EXPORTING
        language = sy-langu
        objtype  = objtype
      IMPORTING
        keyword  = g_t_foldescr-descr.
    IF g_t_foldescr-descr IS INITIAL.
*     no keyword available, take object type instead
      g_t_foldescr-descr = objtype.
    ENDIF.
    MODIFY g_t_foldescr INDEX tabix.
  ENDIF.

  descr = g_t_foldescr-descr.
*{   INSERT         LNDK906038                                        1

  if objtype = 'VBRK'.
    select single fkart into (l_fkart)
           from vbrk
           where vbeln = key.

    select single vtext into (descr)
           from tvfkt
           where fkart = l_fkart
             and spras = sy-langu.
  elseif objtype = 'BUS2032'.
    select single auart into (l_auart)
             from vbak
             where vbeln = key.
    select single bezei into (descr)
             from tvakt
            where auart = l_auart
              and spras = sy-langu.

  elseif objtype = 'BKPF'.
    select single blart into (l_blart)
           from bkpf
           where bukrs = key(4)
             and belnr = key+4(10)
             and gjahr = key+14(4).
    select single ltext into (descr)
             from t003t
            where blart = l_blart
              and spras = sy-langu.
  endif.
*}   INSERT

ENDFORM.                               " IB_GET_OBJTYPE_DESCR


*&---------------------------------------------------------------------*
*&      Form  IB_CHECK_IDOC
*&---------------------------------------------------------------------*
*       Check if current IDoc (from g_t_idocs) belongs to selection.
*       g_t_msgpro must contain the parameters for the IDoc.
*----------------------------------------------------------------------*
FORM ib_check_idoc
USING    hide TYPE type_ib_hide
CHANGING idoc_check TYPE c.       " 'X' if IDoc belongs to selection

  CLEAR idoc_check.

*{   DELETE         LNDK904281                                        2
*\  IF g_t_msgpro-type = 'O'.
*}   DELETE

*   For the IDocs without POS protocol, simply check IDoc status
  IF hide-section = c_section_error.
    IF g_t_idocs-status <> c_idoc_status_ok.
      idoc_check = 'X'.
    ENDIF.
  ELSE.
    IF g_t_idocs-status = c_idoc_status_ok.
      idoc_check = 'X'.
    ENDIF.
  ENDIF.

*{   DELETE         LNDK904281                                        1
*\  ELSE.
*\
*\    idoc_check = 'X'.
*\
*\  ENDIF.
*}   DELETE

ENDFORM.                               " IB_CHECK_IDOC


*&---------------------------------------------------------------------*
*&      Form  IB_CHECK_EXTDOC
*&---------------------------------------------------------------------*
*       Check if external document is selected
*----------------------------------------------------------------------*
FORM ib_check_extdoc
TABLES   t_wplst    TYPE wpusa_t_wplst
         t_foldoc   TYPE wpusa_t_foldoc
USING    doc_status TYPE wpusa_doc_status
         hide       TYPE type_ib_hide
CHANGING extdoc_check TYPE c.  " 'X' if external document is selected

  DATA nr_extdoc TYPE i.

  CLEAR extdoc_check.

  CHECK doc_status-status IN s_status.

  IF hide-section = c_section_ok.
    IF doc_status-status CN pos_status_section_ok.
      EXIT.
    ENDIF.

*   Only show ranges with workitems in the ok section if there is
*   at least one follow-on document.
    IF doc_status-status = pos_status_workitem_created.
*BD 193106
*  read table t_foldoc with key segnum     = doc_status-segnum
*                               segnum_end = doc_status-segnum_end
*                      binary search.
*ED 193106
*BI 193106
      LOOP AT t_foldoc WHERE segnum >= doc_status-segnum AND
                             segnum_end <= doc_status-segnum_end.

        EXIT.

      ENDLOOP.
*EI 193016
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF hide-section = c_section_error AND
     doc_status-status CN pos_status_section_nok.
*     Documents with a status that are usually not displayed in the
*     error section get displayed if they contain a hint
    IF g_t_msgpro-hints IS INITIAL.
      EXIT.
    ELSE.
      READ TABLE t_wplst WITH KEY segnum = doc_status-segnum.
      LOOP AT t_wplst FROM sy-tabix.
        IF t_wplst-segnum < doc_status-segnum OR
           t_wplst-segnum > doc_status-segnum_end.
          EXIT.
        ENDIF.
        IF t_wplst-fehlertyp CA c_hints.
          ADD 1 TO nr_extdoc.
        ENDIF.
      ENDLOOP.
      IF nr_extdoc = 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  extdoc_check = 'X'.

ENDFORM.                               " IB_CHECK_EXTDOC
