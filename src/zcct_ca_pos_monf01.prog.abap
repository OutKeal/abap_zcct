*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONF01 .
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  READ_STORE_KOMM
*&---------------------------------------------------------------------*
* Check to see if stores have a communication profiles,
* Whether branches electronically exchange data (flag POSDEX)
* Whether subsidiaries for POS are not locked.
*----------------------------------------------------------------------*
FORM read_store_komm.

* Initialization
  CLEAR:   i_z2, i_z3, i_z9, i_z12.

* Communication Profiles
  TABLES: twpfi.

* Auxiliary variable for accessing table IT_WRF1_Valid
  DATA: BEGIN OF h_key,
          mandt LIKE t001w-mandt,
          kunnr LIKE t001w-kunnr,
        END OF h_key.
* Hilfsfeld für Zugriff auf Sperrgrundtabelle twrf11
  DATA: h_sperr(1),
* Hilfsfeld sy-tabix für modify auf IT_T00W-VALID.
        i_t001w_sy-tabix LIKE sy-tabix.

  LOOP AT it_t001w_valid.
    i_t001w_sy-tabix = sy-tabix.
* Zähler für geöffnete Filialen.
    i_z2 = i_z2 + 1.
* Prüfen, ob Filiale gültige Kommunikationsdaten hat.
    h_key-mandt = it_t001w_valid-mandt.
    h_key-kunnr = it_t001w_valid-kunnr.
    READ TABLE it_wrf1_valid WITH KEY h_key.
    IF sy-subrc = 0.
* Update in der Tabelle IT_T001W_Valid zur späteren Anzeige
* (OPEN-FILIALE)
      it_t001w_valid-eroed  = it_wrf1_valid-eroed.
      it_t001w_valid-schld  = it_wrf1_valid-schld.
      MODIFY it_t001w_valid INDEX i_t001w_sy-tabix.
      IF sy-subrc = 0.
* Prüfen, ob Filiale zur Kommunikation gesperrt ist.
        h_sperr = 0.
        PERFORM sperr_act
                USING h_sperr.
        IF h_sperr = 0.

* Prüfen, ob Filiale ein gültiges Kommunikationsprofil hat.
          SELECT SINGLE * FROM  twpfi
                 WHERE  kopro       = it_wrf1_valid-kopro.
          IF sy-subrc = 0.
            IF twpfi-posdex = ik_x.
              it_wrf1_valid-posdex = twpfi-posdex.
              it_wrf1_valid-werks  = it_t001w_valid-werks.
              it_wrf1_valid-name1  = it_t001w_valid-name1.
              it_wrf1_valid-ort01  = it_t001w_valid-ort01.
              MODIFY it_wrf1_valid INDEX sy-tabix.
              IF sy-subrc = 0.
                i_z3 = i_z3 + 1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Gültige Filialen mit Kommunikation nach Filialnummer sortieren.
  SORT it_wrf1_valid BY werks.

* Zähler für geschlossene Filialen
  i_z12 = i_z1 - i_z2.

* Anzahl Filialen ohne Kommunikation ermitteln.
  i_z9 = i_z2 - i_z3.

ENDFORM.                               " READ_STORE_KOMM

*&---------------------------------------------------------------------*
*&      Form  SET_DYNPRO_111
*&---------------------------------------------------------------------*
*       Schreiben des Fehlerübersichtsbildes                           *
*       und interne Tabelle (Position bei späterem Doppelklick) füllen *
*----------------------------------------------------------------------*
FORM set_dynpro_111.

*-----------------------------------------------------------------------
* Filialübersicht aufbereiten
*-----------------------------------------------------------------------
  SKIP 1.
* Titelzeile
  WRITE:  10 TEXT-054 COLOR COL_HEADING.
*  skip.
* Total Filialen
  ULINE AT /09(78).
  WRITE: /09 sy-vline NO-GAP,
             TEXT-001,
          62 sy-vline,
          75 i_z1 COLOR COL_GROUP INTENSIFIED OFF,
          86 sy-vline.
  it_sel-item = 'I_Z1'.
  it_sel-line = sy-linno.
  APPEND it_sel.
* Geöffnete Filialen
  ULINE AT /09(78).
  WRITE: /09 sy-vline NO-GAP,
             TEXT-002,
          62 sy-vline,
          68 i_z2 COLOR COL_TOTAL INTENSIFIED OFF,
          86 sy-vline.
  it_sel-item = 'I_Z2'.
  it_sel-line = sy-linno.
  APPEND it_sel.
* Filialen mit elektronischem Datenaustausch
  ULINE AT /09(78).
  WRITE: /09 sy-vline NO-GAP,
             TEXT-003,
          62 sy-vline,
          63 i_z3 COLOR COL_TOTAL,
          86 sy-vline.
  it_sel-item = 'I_Z3'.
  it_sel-line = sy-linno.
  APPEND it_sel.
* Filialen ohne elektronischen Datenaustausch
  ULINE AT /09(78).
  WRITE: /09 sy-vline NO-GAP,
             TEXT-009,
          62 sy-vline,
          63 i_z9 COLOR COL_TOTAL,
          86 sy-vline.
  it_sel-item = 'I_Z9'.
  it_sel-line = sy-linno.
  APPEND it_sel.
* Geschlossene Filialen
  ULINE AT /09(78).
  WRITE: /09 sy-vline NO-GAP,
             TEXT-012,
          62 sy-vline,
          68 i_z12 COLOR COL_TOTAL INTENSIFIED OFF,
          86 sy-vline.
  it_sel-item = 'I_Z12'.
  it_sel-line = sy-linno.
  APPEND it_sel.
  ULINE AT /09(78).
  SKIP 1.

ENDFORM.                               " SET_DYNPRO_111

*&---------------------------------------------------------------------*
*&      Form  SPERR_ACT
*&---------------------------------------------------------------------*
*       Prüfen, ob Filiale gesperrt und ob Sperrgrund für POS          *
*               relevant ist.                                          *
*----------------------------------------------------------------------*
*  <->  H_sperr  Kennzeichen, ob Filiale gesperrt ist.
*----------------------------------------------------------------------*
FORM sperr_act
     USING h_sperr TYPE c.

* Sperrgründe und Sperrgebiet
  TABLES: twrf11.

  IF NOT it_wrf1_valid-spdab IS INITIAL.
    IF it_wrf1_valid-spdab <= p_dat_v AND
       it_wrf1_valid-spdbi >= p_dat_b OR
       it_wrf1_valid-spdab <= p_dat_v AND
       it_wrf1_valid-spdbi IS INITIAL.

      SELECT SINGLE * FROM  twrf11
             WHERE  spgr1       = it_wrf1_valid-spgr1.
      IF sy-subrc = 0.
        IF twrf11-spmdl <> space.
* Wenn TWRF11-SPMDL (POS-Sperrkennzeichen) ungleich Space, Filiale zur
* POS-Kommunikation gesperrt.
          h_sperr = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " SPERR_ACT

*&---------------------------------------------------------------------*
*&      Form  READ_IDOCS_INBOUND
*&---------------------------------------------------------------------*
*   Ermitteln der selektierten Eingangsidocs.
*   Die IDocs werden in der Tabelle G_T_IDOCS abgelegt.
*&---------------------------------------------------------------------*
FORM read_idocs_inbound.

* Zwischenspeicher für Gruppenverarbeitung bei neuer Filiale.
  DATA: werks      LIKE wptst-werks,
        filiale    LIKE wplst-filiale,
        error_flag,
        lines      LIKE sy-tabix,
        l_t_idocs  TYPE type_idoc OCCURS 0 WITH HEADER LINE,
        l_mestyp   LIKE l_t_idocs-mestyp,
        l_mestypnr LIKE l_t_idocs-mestypnr.
  RANGES  l_s_mestyp FOR edidc-mestyp.
  DATA : skip_store,
         li_store   TYPE zscmt0010_tab,
         lwa_store  LIKE LINE OF li_store.

* exit if internal tables are already filled
  DESCRIBE TABLE g_t_idocs LINES lines.
  CHECK lines = 0.

* Create selection range with the supported EDI message types
  LOOP AT g_t_msgpro.
    l_s_mestyp-sign   = 'I'.
    l_s_mestyp-option = 'EQ'.
    l_s_mestyp-low    = g_t_msgpro-mestyp.
    APPEND l_s_mestyp.
  ENDLOOP.

* Read IDocs according to user selection
* Originals of changed IDocs are ignored.
* Note: Field UPDDAT in the selection doesn't seem to do anything,
*       because it's always >= credat, but it reduces the number
*       of IDocs checked because there is a secondary key with it.
*{   REPLACE        LNDK905093                                        1
*\  SELECT docnum mestyp sndprn credat status FROM edidc    "#EC PORTABLE
  SELECT docnum mestyp rcvprn rcvprt credat status FROM edidc    "#EC PORTABLE
*}   REPLACE
      INTO TABLE l_t_idocs
      WHERE upddat >= p_dat_v
        AND ( credat = p_dat_v AND cretim >= p_time_v OR
              credat > p_dat_v )
        AND ( credat = p_dat_b AND cretim <= p_time_b OR
              credat < p_dat_b )
        AND direct =  '2'
        AND mestyp IN l_s_mestyp
*changed by liuliying for message variant begin 20140403
        AND mescod IN s_mescod
*changed by liuliying for message variant end 20140403
*{   REPLACE        LNDK905719                                        3
*\        AND status <> c_idoc_status_orig
        AND  status <> c_idoc_status_orig
        AND  status <>  c_idoc_status_end
*}   REPLACE
        AND docnum IN s_docnum
        AND mestyp IN s_mestyp
*{   REPLACE        LNDK905093                                        2
*\        AND sndprn IN s_store
*\        AND sndprn IN s_store_class.
        AND rcvprn IN s_store
        AND rcvprn IN s_store_class.
*}   REPLACE

  SORT l_t_idocs BY sndprn mestyp credat docnum.

  CLEAR kna1.
  error_flag = '*'.
  LOOP AT l_t_idocs.
    IF kna1-kunnr <> l_t_idocs-sndprn.
      CLEAR skip_store.
*     Read store data.
      kna1-kunnr = l_t_idocs-sndprn.
      CALL FUNCTION 'KNA1_SINGLE_READ'
        EXPORTING
          kna1_kunnr = kna1-kunnr
        IMPORTING
          wkna1      = kna1
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
*       Sending parter is no store
        IF s_vkorg IS INITIAL AND s_vtweg IS INITIAL.
*         No selection, simply go on without a description
          CLEAR kna1.
        ELSE.
*         There is a selection, and a no-store does not fit it
          skip_store = 'X'.
          CONTINUE.
        ENDIF.
      ELSEIF NOT ( s_vkorg IS INITIAL AND s_vtweg IS INITIAL ).
*       Read site of store to see if it fits the selection
        it_t001w_all-werks = kna1-werks.
        CALL FUNCTION 'T001W_SINGLE_READ'
          EXPORTING
            t001w_werks = it_t001w_all-werks
          IMPORTING
            wt001w      = it_t001w_all
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.

* Get Store related info from store master for a retailstoreid(sending partner).
        CALL FUNCTION '/ATU/STORE_SEARCH'
          EXPORTING
            i_retailstoreid = l_t_idocs-sndprn
          TABLES
            et_store_master = li_store
          EXCEPTIONS
            datanotfound    = 1.

*Read store master record
        READ TABLE li_store INTO lwa_store
                            WITH KEY partner = l_t_idocs-sndprn.

        IF sy-subrc = 0.
          it_t001w_all-vkorg = lwa_store-vkorg.
          it_t001w_all-vtweg = lwa_store-vtweg.
        ENDIF.

        IF sy-subrc <> 0 OR
           NOT ( it_t001w_all-vkorg IN s_vkorg AND
                 it_t001w_all-vtweg IN s_vtweg ).
*         Sending parter has no site or it's site does not fit the
*         the selection. Set flag to skip this store
          skip_store = 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.
* Error flag of the previous branch show
      IF error_flag IS INITIAL.
        ADD 1 TO i_z8a.
      ENDIF.
      IF error_flag = 'X'.
        ADD 1 TO i_z8.
      ENDIF.
* New Branch -> Error flag reset
      CLEAR error_flag.
    ELSEIF NOT ( skip_store IS INITIAL ).
*     Skip records in l_t_idocs with current store
      CONTINUE.
    ENDIF.
    l_t_idocs-name1 = kna1-name1.

* Error flag if at least one branch of an IDOC
* Is faulty
    IF l_t_idocs-status <> c_idoc_status_ok.
      error_flag = 'X'.
    ENDIF.

*   search message type in processing information table; if it's not
*   there, a default processing for unknown IDoc will be used
    IF l_t_idocs-mestyp <> l_mestyp.
      l_mestyp = l_t_idocs-mestyp.
      READ TABLE g_t_msgpro WITH KEY l_mestyp BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR g_t_msgpro.
        g_t_msgpro-mestyp = l_mestyp.
        INSERT g_t_msgpro INDEX sy-tabix.
      ENDIF.
      l_mestypnr = sy-tabix.
    ENDIF.

    l_t_idocs-mestypnr = l_mestypnr.
    APPEND l_t_idocs TO g_t_idocs.

  ENDLOOP.                             " at l_t_idocs

ENDFORM.                               " READ_IDOCS_INBOUND

*&---------------------------------------------------------------------*
*&      Form  READ_ERROR_WPXST
*&---------------------------------------------------------------------*
* WPXST Table includes all of external (POS / FWWS / SCS *
* News reported.                                         *
*----------------------------------------------------------------------*
FORM read_error_wpxst.


  DATA: i_counter LIKE it_wpxst-counter.

  REFRESH: it_wpxst.
  CLEAR:   it_wpxst, i_counter, i_z6.

  SELECT * FROM wpxst
         WHERE  datum      >= p_dat_v
         AND    datum      <= p_dat_b
         AND    filiale    IN s_store
         AND    filiale    IN s_store_class.

    IF NOT ( p_docnr IS INITIAL ).     "only selected IDOCs
      CHECK wpxst-docnum = p_docnr.
    ENDIF.


*   interne Tabelle (IT_WPXST) abfüllen
    MOVE-CORRESPONDING wpxst TO it_wpxst.

    CLEAR it_t001w_all.
    it_t001w_all-werks = wpxst-filiale.
    READ TABLE it_t001w_all.
    IF sy-subrc = 0.
      it_wpxst-name1  = it_t001w_all-name1.
      it_wpxst-ort01  = it_t001w_all-ort01.
      it_wpxst-vkorg  = it_t001w_all-vkorg.
      it_wpxst-vtweg  = it_t001w_all-vtweg.
    ENDIF.
* Branches also work with invalid number is entered in the Table geschr
* The works must be in the select VKORG and his VTWEG.
    IF it_wpxst-vkorg IN s_vkorg AND
       it_wpxst-vtweg IN s_vtweg.
      APPEND it_wpxst.
    ENDIF.
  ENDSELECT.


* Sort the internal table after branch and IDOC number.
  SORT: it_wpxst BY filiale
                    datum.

* Counter (later than the index) to introduce
  LOOP AT it_wpxst.
    i_counter = i_counter + 1.     " rolling counter of messages
    it_wpxst-counter = i_counter.
    MODIFY it_wpxst.
  ENDLOOP.

* External processing error count (numerator I_Z6)
  DESCRIBE TABLE it_wpxst LINES i_z6.


ENDFORM.                               " READ_ERROR_WPXST

*&---------------------------------------------------------------------*
*&      Form  READ_ERROR_WDLS
*&---------------------------------------------------------------------*
* Number of errors in Table WDLS count.                                *
* Table includes all Download WDLS capita Messages                     *
*                                                                      *
*----------------------------------------------------------------------*
FORM read_error_wdls.


* WDLS-completion for status update
  DATA: BEGIN OF pe_t_wdls OCCURS 0.
          INCLUDE STRUCTURE wdls.
  DATA: END OF pe_t_wdls.

* Initialize counter
  i_z4  = 0.
  i_z4a = 0.


  REFRESH it_wdls.
*logged error entries (GESST status) in Table WDLS search

  SELECT * FROM  wdls
         WHERE ( ( erzdt = p_dat_v  AND  erzzt >= p_time_v ) OR
                   erzdt > p_dat_v )
         AND   ( ( erzdt = p_dat_b  AND  erzzt <= p_time_b ) OR
                   erzdt < p_dat_b )
         AND  empfn  IN s_store
         AND  empfn  IN s_store_class.

    IF NOT ( p_systp IS INITIAL ).     " nur selektierte Anwendung
      CHECK wdls-systp = p_systp.
    ENDIF.

* interne Tabelle (IT_WDLS) abfüllen

    MOVE-CORRESPONDING wdls TO it_wdls.
    MOVE-CORRESPONDING wdls TO pe_t_wdls.
    APPEND pe_t_wdls.

    CLEAR it_t001w_all.

    READ TABLE it_t001w_all WITH KEY kunnr = wdls-empfn.

    IF sy-subrc = 0.
      it_wdls-name1  = it_t001w_all-name1.
      it_wdls-ort01  = it_t001w_all-ort01.
      it_wdls-vkorg  = it_t001w_all-vkorg.
      it_wdls-vtweg  = it_t001w_all-vtweg.
      IF it_wdls-vkorg IN s_vkorg AND
         it_wdls-vtweg IN s_vtweg.
        APPEND it_wdls.
      ENDIF.
    ENDIF.

  ENDSELECT.

* If selected for this date records were found.
  IF sy-subrc = 0.

* Send status update
    CALL FUNCTION 'DOWNLOAD_EDI_STATUS_SELECT'
      EXPORTING
        pi_datum_von  = p_dat_v
        pi_datum_bis  = p_dat_b
        pi_protokoll  = ' '
        pi_systp      = p_systp
        pi_docnr      = p_docnr
      TABLES
        pe_t_wdls     = pe_t_wdls
        pe_t_wdlsp    = g_t_wdlsp
      EXCEPTIONS
        no_idocs      = 01
        no_edi_status = 02.

* Update EDI status in the internal enlarged table
    LOOP AT pe_t_wdls.
      READ TABLE it_wdls WITH KEY dldnr = pe_t_wdls-dldnr.
      IF sy-subrc = 0.
        it_wdls-vsest = pe_t_wdls-vsest.
        MODIFY it_wdls INDEX sy-tabix.
      ENDIF.                           " sy-subrc = 0
    ENDLOOP.

*Int delete the table, if not in the WDLSP Reference
    IF NOT ( p_docnr IS INITIAL ).
      LOOP AT it_wdls.
        READ TABLE g_t_wdlsp WITH KEY dldnr = it_wdls-dldnr.
        IF sy-subrc <> 0.
          DELETE it_wdls.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Sort the table by internal branches, etc.
    SORT: it_wdls BY empfn
                     systp
                     erzdt
                     erzzt
                     gesst.

* Download Application: OK / NOK status include
    LOOP AT it_wdls.

      IF it_wdls-gesst = c_status_ok OR
         it_wdls-gesst = c_status_user_hint.
        i_z4a = i_z4a + 1.
      ELSE.
        i_z4 = i_z4 + 1.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.                               " READ_ERROR_WDLS

*&---------------------------------------------------------------------*
*&      Form  TOT_FILIALE
*&---------------------------------------------------------------------*
* All in the company created subsidiaries (plants and VZ) show         *
*----------------------------------------------------------------------*
FORM tot_filiale.

  SET PF-STATUS '101'.

  LOOP AT it_t001w_all.

    WRITE: /    it_t001w_all-kunnr COLOR COL_KEY NO-GAP,
                sy-vline NO-GAP,
                it_t001w_all-name1 COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_all-ort01 COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_all-ekorg COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_all-vkorg COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP.

    HIDE:       it_t001w_all-werks,
                it_t001w_all-ekorg,
                it_t001w_all-vkorg.

  ENDLOOP.
* Hide-area 'AT LINE SELECTION' delete.
  CLEAR it_t001w_all-werks.
ENDFORM.                               " TOT_FILIALE

*&---------------------------------------------------------------------*
*&      Form  FIL_SHOW
*&---------------------------------------------------------------------*
* Display the selected branch                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fil_show USING i_werks TYPE c
                    i_vkorg TYPE c
                    i_vtweg TYPE c.


  TABLES: wr02d, rf02d.

  SET PARAMETER ID 'BUK' FIELD ' '.
  SET PARAMETER ID 'WRK' FIELD i_werks.
  GET PARAMETER ID 'WRK' FIELD wr02d-locnr.
  SET PARAMETER ID 'VKO' FIELD i_vkorg.
  GET PARAMETER ID 'VKO' FIELD rf02d-vkorg.
  SET PARAMETER ID 'VTW' FIELD i_vtweg.
  GET PARAMETER ID 'VTW' FIELD rf02d-vtweg.
  SET PARAMETER ID 'DDY' FIELD '/400/110'.

  CALL TRANSACTION 'WB03' AND SKIP FIRST SCREEN.

ENDFORM.                               " FIL_SHOW

*&---------------------------------------------------------------------*
*&      Form  OPEN_FILIALE
*&---------------------------------------------------------------------*
*       Open Branches Show                                             *
*----------------------------------------------------------------------*
FORM open_filiale.


  SET PF-STATUS '101'.

  LOOP AT it_t001w_valid.

    WRITE: /    it_t001w_valid-kunnr COLOR COL_KEY NO-GAP,
                sy-vline NO-GAP,
                it_t001w_valid-name1 COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_valid-ort01 COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_valid-eroed COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP,
                it_t001w_valid-schld COLOR COL_NORMAL NO-GAP,
                sy-vline NO-GAP.

    HIDE:       it_t001w_valid-werks,
                it_t001w_valid-vkorg,
                it_t001w_valid-vtweg.


  ENDLOOP.

* Hide-area 'AT LINE SELECTION' delete.
  CLEAR it_t001w_valid-werks.
ENDFORM.                               " OPEN_FILIALE

*&---------------------------------------------------------------------*
*&      Form  KOMM_FILIALE
*&---------------------------------------------------------------------*
* Locked offices and branches without valid communication              *
* Display.
*----------------------------------------------------------------------*
FORM komm_filiale.


  SET PF-STATUS '101'.

  LOOP AT it_wrf1_valid.

    IF it_wrf1_valid-posdex = ik_x.

      WRITE: /    it_wrf1_valid-locnr COLOR COL_KEY NO-GAP,
                  sy-vline NO-GAP,
                  it_wrf1_valid-name1 COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP,
                  it_wrf1_valid-ort01 COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP,
                  it_wrf1_valid-eroed COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP,
                  it_wrf1_valid-schld COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP,
                  it_wrf1_valid-kopro COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP.

      HIDE:       it_wrf1_valid-werks,
                  it_wrf1_valid-vkorg,
                  it_wrf1_valid-vtweg.

    ENDIF.
  ENDLOOP.
* Hide-Bereich für 'AT LINE SELECTION' löschen.
  CLEAR it_wrf1_valid-werks.
ENDFORM.                               " KOMM_FILIALE

*&---------------------------------------------------------------------*
*&      Form  CLOSED_FILIALE
*&---------------------------------------------------------------------*
*       Show Branches Closed                                 *
*----------------------------------------------------------------------*
FORM closed_filiale.

  TABLES: wrf1.

* Auxiliary field for accessing table IT_T001W_Valid.
  DATA: BEGIN OF h_key,
          mandt LIKE t001w-mandt,
          werks LIKE t001w-werks,
        END OF h_key.

  SET PF-STATUS '101'.

  LOOP AT it_t001w_all.

* Read all the branches that are listed in Table IT_T001W_VALID are.
* = Closed shops and offices without WERKS data (without WRF1-Rec.)
    CLEAR it_t001w_valid.
    h_key-mandt = it_t001w_all-mandt.
    h_key-werks = it_t001w_all-werks.
    READ TABLE it_t001w_valid WITH KEY h_key.

    IF sy-subrc <> 0.
      WRITE: /    it_t001w_all-kunnr COLOR COL_KEY NO-GAP,
                  sy-vline NO-GAP,
                  it_t001w_all-name1 COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP,
                  it_t001w_all-ort01 COLOR COL_NORMAL NO-GAP,
                  sy-vline NO-GAP.

      HIDE:       it_t001w_all-werks,
                  it_t001w_all-vkorg,
                  it_t001w_all-vtweg.

* If store is closed, whether WRF1 read data created and
* Determine the opening hours and closing times
      DATA:       i_dummy(10) TYPE c.

      SELECT SINGLE * FROM  wrf1
             WHERE  locnr       = it_t001w_all-kunnr.
      IF sy-subrc = 0.

        WRITE:      wrf1-eroed COLOR COL_NORMAL NO-GAP,
                    sy-vline NO-GAP,
                    wrf1-schld COLOR COL_NORMAL NO-GAP,
                    sy-vline NO-GAP.

      ELSE.

        WRITE:      (21) TEXT-080 COLOR COL_NORMAL NO-GAP,
                    sy-vline NO-GAP.

      ENDIF.
    ENDIF.
  ENDLOOP.

* Hide-area 'AT LINE SELECTION' delete.
  CLEAR it_t001w_all-werks.
ENDFORM.                               " CLOSED_FILIALE


*&---------------------------------------------------------------------*
*&      Form  KOPF_103
*&---------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM kopf_103.

  DATA: gesst_text   LIKE dd07t-ddtext.
  DATA: gesst_value  LIKE dd07v-domvalue_l.
  DATA: dlmod_text   LIKE dd07t-ddtext.
  DATA: dlmod_value  LIKE dd07t-domvalue_l.
  DATA: systp_text   LIKE dd07t-ddtext.
  DATA: systp_value  LIKE dd07t-domvalue_l.
  DATA: sbtyp_text   LIKE twbbt.

* Domain fixed values for treatment status read.
  gesst_value = it_wdls-gesst.
  CALL FUNCTION 'SHOW_DOMAIN_VALUES'
    EXPORTING
      domain           = 'GESST'
      langu            = sy-langu
      value            = gesst_value
    IMPORTING
      dtext            = gesst_text
    EXCEPTIONS
      undefined_domain = 1
      OTHERS           = 2.

* Domain fixed values for Download read mode
  dlmod_value = it_wdls-dlmod.
  CALL FUNCTION 'SHOW_DOMAIN_VALUES'
    EXPORTING
      domain           = 'DLMOD'
      langu            = sy-langu
      value            = dlmod_value
    IMPORTING
      dtext            = dlmod_text
    EXCEPTIONS
      undefined_domain = 1
      OTHERS           = 2.

* Domain fixed values for the application to read
  systp_value = it_wdls-systp.
  CALL FUNCTION 'SHOW_DOMAIN_VALUES'
    EXPORTING
      domain           = 'SYSTP'
      langu            = sy-langu
      value            = systp_value
    IMPORTING
      dtext            = systp_text
    EXCEPTIONS
      undefined_domain = 1
      OTHERS           = 2.

* Send the error status read
  teds2-descrp = ' '.
  SELECT SINGLE * FROM teds2
         WHERE  status      = it_wdls-vsest
         AND    langua      = sy-langu.

* If assortment list, assortment list subtype identified
  IF it_wdls-systp = ik_sortliste.                          " SL
    CALL FUNCTION 'ASSORT_LIST_TYPE_GET'
      EXPORTING
        pi_bbtyp   = it_wdls-sbtyp
        pi_langu   = sy-langu
      IMPORTING
        pe_i_twbbt = sbtyp_text
      EXCEPTIONS
        no_text    = 1
        no_type    = 2
        OTHERS     = 3.
  ENDIF.


  WRITE: /    TEXT-030,
           23 it_wdls-mandt COLOR COL_NORMAL,
           40 'POS-Ausgangs-Protokoll'(031),
           63 it_wdls-dldnr COLOR COL_KEY.
  WRITE: /    TEXT-032,
           23 it_wdls-erzdt COLOR COL_NORMAL,
           40 TEXT-033,
           63 it_wdls-erzzt COLOR COL_NORMAL.
  WRITE: /    TEXT-034,
           23 it_wdls-empfn COLOR COL_NORMAL,
           40 TEXT-041,
           63 it_wdls-anzid COLOR COL_NORMAL.
  WRITE: /    TEXT-035,
         23 it_wdls-systp COLOR COL_NORMAL,
         27 systp_text    COLOR COL_NORMAL.
  IF it_wdls-systp = ik_sortliste.                          " SL
    WRITE: /    TEXT-215,
             23 it_wdls-sbtyp COLOR COL_NORMAL,
             27 sbtyp_text-bbtext  COLOR COL_NORMAL.
  ENDIF.
  WRITE: /    'Aufbereitungsmodus'(040),
           23 it_wdls-dlmod COLOR COL_NORMAL,
           27 dlmod_text    COLOR COL_NORMAL.
  WRITE: /    TEXT-043,
           23 it_wdls-gesst COLOR COL_NORMAL,
           27 gesst_text    COLOR COL_NORMAL.
  WRITE: /    TEXT-044,
           23 it_wdls-vsest COLOR COL_NORMAL,
           27 teds2-descrp  COLOR COL_NORMAL.
  ULINE.

ENDFORM.                                                    " KOPF_103

*&---------------------------------------------------------------------*
*&      Form  AT_LINE_SELECTION_101
*&---------------------------------------------------------------------*
*       Processing for screen 101 and AT LINE SELECTION                *
*----------------------------------------------------------------------*
FORM at_line_selection_101.

  DATA: i_werks LIKE t001w-werks,
        i_vkorg LIKE t001w-vkorg,
        i_vtweg LIKE t001w-vtweg.

  CLEAR i_werks.
  CASE it_sel-item.
    WHEN 'I_Z1'.
      IF NOT it_t001w_all-werks IS INITIAL.
        i_werks = it_t001w_all-werks.
        i_vkorg = it_t001w_all-vkorg.
        i_vkorg = it_t001w_all-vtweg.
      ELSE.
        MESSAGE e009.
*       Please position the cursor on a valid row
      ENDIF.
    WHEN 'I_Z2'.
      IF NOT it_t001w_valid-werks IS INITIAL.
        i_werks = it_t001w_valid-werks.
        i_vkorg = it_t001w_valid-vkorg.
        i_vtweg = it_t001w_valid-vtweg.
      ELSE.
        MESSAGE e009.
*       Please position the cursor on a valid row
      ENDIF.
    WHEN 'I_Z3'.
      IF NOT it_wrf1_valid-werks IS INITIAL.
        i_werks = it_wrf1_valid-werks.
        i_vkorg = it_wrf1_valid-vkorg.
        i_vtweg = it_wrf1_valid-vtweg.
      ELSE.
        MESSAGE e009.
*       Please position the cursor on a valid row
      ENDIF.
    WHEN 'I_Z9'.
      IF NOT it_t001w_valid-werks IS INITIAL.
        i_werks = it_t001w_valid-werks.
        i_vkorg = it_t001w_valid-vkorg.
        i_vtweg = it_t001w_valid-vtweg.
      ELSE.
        MESSAGE e009.
*       Please position the cursor on a valid row
      ENDIF.
    WHEN 'I_Z12'.
      IF NOT it_t001w_all-werks IS INITIAL.
        i_werks = it_t001w_all-werks.
        i_vkorg = it_t001w_all-vkorg.
        i_vkorg = it_t001w_all-vtweg.
      ELSE.
        MESSAGE e009.
*       Please position the cursor on a valid row
      ENDIF.
  ENDCASE.
  IF NOT i_werks IS INITIAL.
    PERFORM fil_show USING i_werks
                           i_vkorg
                           i_vtweg.
  ENDIF.
  CLEAR: it_t001w_all-werks,
         it_t001w_valid-werks,
         it_wrf1_valid-werks.

ENDFORM.                               " AT_LINE_SELECTION_101

*&---------------------------------------------------------------------*
*&      Form  AT_LINE_SELECTION_102
*&---------------------------------------------------------------------*
* Processing for screen 102 and AT LINE SELECTION *
* Changed for new output
*----------------------------------------------------------------------*
FORM at_line_selection_102.


  IF NOT it_wdls-dldnr IS INITIAL.

    CASE it_sel-item.
      WHEN 'I_Z4'.
* Download protocol processing
        PERFORM wdlsp_read_proc_new.
      WHEN 'I_Z5'.
* Download Communications Protocol
        PERFORM wdlsp_read_komm_new.
    ENDCASE.

* Hide-area 'AT LINE SELECTION' cache and delete.
    i_wdls = it_wdls.
    CLEAR: wdlsp,
           it_wdls-dldnr.

  ELSE.
    MESSAGE e009.
*    Please position the cursor on a valid row
  ENDIF.
ENDFORM.                               " AT_LINE_SELECTION_102

*&---------------------------------------------------------------------*
*&      Form  AT_LINE_SELECTION_103
*&---------------------------------------------------------------------*
*       Application Log View                                           *
*----------------------------------------------------------------------*
FORM at_line_selection_103.

  DATA: i_wdlsp-dldnr      LIKE wdlsp-dldnr,
        i_wdlsp-doctyp     LIKE wdlsp-doctyp,
        l_column_selection LIKE baldisp.

  IF NOT wdlsp-dldnr IS INITIAL.

    CASE it_sel-item.
      WHEN 'I_Z4'.
* Download processing Applikationslog View
* If no logs for this item have been written.
        IF wdlsp-anloz   = 0.
          i_wdlsp-dldnr  = wdlsp-dldnr.
          i_wdlsp-doctyp = wdlsp-doctyp.
          CLEAR wdlsp.
          MESSAGE e008 WITH i_wdlsp-dldnr i_wdlsp-doctyp.

* No Appl.Log to download the item number & and & exist
        ELSE.
* If there is already a restart for this item was carried out.
          IF wdlsp-rstrt <> space.
            i_wdlsp-doctyp = wdlsp-doctyp.
            MESSAGE i019 WITH i_wdlsp-doctyp.
          ENDIF.                       " wdlsp-rstrt <> space.

          DATA: i_external_number LIKE balhdr-extnumber,
                i_subobject       LIKE balhdr-subobject,
                i_wdls-erzdt      LIKE sy-datum,
                i_wdls-erzzt      LIKE sy-uzeit.

* External number from IDOC number and serial number together
          i_external_number    = wdlsp-dldnr.
          i_external_number+14 = wdlsp-lfdnr.
* Date and time of access to indicate Appl.Log
* Date represents the date of WDLS head record
* Time does not match the WDLS-head record, currently about 000000
          i_wdls-erzdt = it_wdls-erzdt.
          i_wdls-erzzt = '000000'.

* Constant for SubObject determine
          IF it_wdls-systp = 'POS'.
            i_subobject = ik_pos.
          ELSEIF it_wdls-systp CP 'B**'.
            i_subobject = ik_bb.
          ENDIF.

          l_column_selection = '1  1222  22   '.

          CALL FUNCTION 'APPL_LOG_DISPLAY'
            EXPORTING
              object                    = 'W'
              subobject                 = i_subobject
              external_number           = i_external_number
              object_attribute          = 2
              subobject_attribute       = 2
              external_number_attribute = 2
              date_from                 = i_wdls-erzdt
              time_from                 = i_wdls-erzzt
              date_to                   = sy-datum
              time_to                   = sy-uzeit
              title_selection_screen    = ' '
              title_list_screen         = ' '
              column_selection          = l_column_selection
              suppress_selection_dialog = 'X'
            EXCEPTIONS
              no_authority              = 01.

        ENDIF.

      WHEN 'I_Z5'.
* Download Communications: Status Display

*       EDI/Idoc status tracing
        CALL FUNCTION 'RS_TREE_PUSH'.
        CALL FUNCTION 'EDI_DOCUMENT_STATUS_DISPLAY'
          EXPORTING
            docnum                 = wdlsp-docnum
          EXCEPTIONS
            no_status_record_found = 01.

        PERFORM edi_interface.
        CALL FUNCTION 'RS_TREE_POP'.
    ENDCASE.

  ELSE.
    MESSAGE e012.
* Please place the cursor on a valid row IDOC


  ENDIF.

  CLEAR wdlsp.


ENDFORM.                               " AT_LINE_SELECTION_103


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PRINT
*&---------------------------------------------------------------------*
*       Download print processing                                  *
*----------------------------------------------------------------------*
FORM download_print.

  WRITE: / it_wdls-empfn COLOR COL_KEY NO-GAP,
           sy-vline NO-GAP,
           it_wdls-name1 COLOR COL_NORMAL INTENSIFIED NO-GAP,
           sy-vline NO-GAP,
           it_wdls-systp COLOR COL_NORMAL INTENSIFIED,
           sy-vline NO-GAP,
           it_wdls-erzdt UNDER TEXT-022 COLOR COL_NORMAL INTENSIFIED
           NO-GAP,
           sy-vline NO-GAP,
           it_wdls-erzzt COLOR COL_NORMAL INTENSIFIED NO-GAP,
           sy-vline NO-GAP.
  HIDE:    it_wdls.
  CASE it_wdls-gesst.
    WHEN c_status_init.
      WRITE TEXT-024 UNDER TEXT-028 COLOR COL_NEGATIVE.
    WHEN c_status_user_hint.
      WRITE TEXT-025 UNDER TEXT-028 COLOR COL_KEY.
    WHEN c_status_data_missing.
      WRITE TEXT-026 UNDER TEXT-028 COLOR COL_NEGATIVE.
    WHEN c_status_idoc_missing.
      WRITE TEXT-027 UNDER TEXT-028 COLOR COL_NEGATIVE.
    WHEN c_status_ok.
      WRITE TEXT-058 UNDER TEXT-028 COLOR COL_POSITIVE.
  ENDCASE.

ENDFORM.                               " DOWNLOAD_PRINT
