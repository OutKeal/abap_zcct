FUNCTION ZCCT_ca_pos_get_msgtyp_params.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MESTYP) LIKE  EDIDC-MESTYP OPTIONAL
*"     VALUE(OBJTYPE) LIKE  OBJECTCONN-OBJECTTYPE OPTIONAL
*"     VALUE(SNDPRN) LIKE  EDIDC-SNDPRN
*"  EXPORTING
*"     VALUE(MSGPRO) TYPE  WPUSA_MSGPRO
*"     VALUE(FOLDESCR) TYPE  WPUSA_FOLDESCR
*"--------------------------------------------------------------------

  STATICS: l_t_msgpro   TYPE wpusa_t_msgpro,
           l_t_foldescr TYPE wpusa_t_foldescr.
  STATICS  l_sndprn     LIKE edidc-sndprn.

  DATA l_lines LIKE sy-tabix.

* Refresh if sending partner changes
  IF l_sndprn <> sndprn.
    l_sndprn = sndprn.
    REFRESH l_t_msgpro.
  ENDIF.

  DESCRIBE TABLE l_t_msgpro LINES l_lines.

  IF l_lines = 0.
    CALL FUNCTION 'ZCCT_CA_POS_GET_MSGTYP_PROCESS'
      EXPORTING
        sndprn   = sndprn
      TABLES
        msgpro   = l_t_msgpro
        foldescr = l_t_foldescr.
  ENDIF.

  IF NOT ( mestyp IS INITIAL ).
    READ TABLE l_t_msgpro INTO msgpro
         WITH KEY mestyp = mestyp BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR msgpro.
    ENDIF.
  ENDIF.

  IF NOT ( objtype IS INITIAL ).
    READ TABLE l_t_foldescr INTO foldescr
         WITH KEY objtype = objtype BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR foldescr.
    ENDIF.
  ENDIF.

ENDFUNCTION.
