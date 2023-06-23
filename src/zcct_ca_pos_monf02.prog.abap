*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONF02.
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  LONGTEXT
*&---------------------------------------------------------------------*
*       Display the long text of an error message                      *
*----------------------------------------------------------------------*
form longtext using i_lt-msgid type clike
                    i_lt-msgnr type clike
                    i_message like i_message
                    i_lt-para1 type clike
                    i_lt-para2 type clike
                    i_lt-para3 type clike
                    i_lt-para4 type clike.

  data: docu_object like dokhl-object.

  data begin of dummy1 occurs 0.
          include structure dselc.
  data end of dummy1.
  data begin of dummy2 occurs 0.
          include structure dval.
  data end of dummy2.

  docu_object   = i_lt-msgid.
  docu_object+2 = i_lt-msgnr.

* Text Lang
  clear help_info.
  help_info-call       = 'D'.
  help_info-spras      = sy-langu.
  help_info-messageid  = i_lt-msgid.
  help_info-messagenr  = i_lt-msgnr.
  help_info-message    = i_message.
  help_info-title      = text-078.             "long text
  help_info-docuid     = ik_docu_id_msg.       "Constant
  help_info-docuobject = docu_object.
  help_info-msgv1      = i_lt-para1.
  help_info-msgv2      = i_lt-para2.
  help_info-msgv3      = i_lt-para3.
  help_info-msgv4      = i_lt-para4.

  call function 'HELP_START'
       exporting
            help_infos   = help_info
       tables
            dynpselect   = dummy1
            dynpvaluetab = dummy2.
endform.                               " LONGTEXT


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_103
*&---------------------------------------------------------------------*
* Pushbuttons in the query image download processing
*----------------------------------------------------------------------*
form user_command_103.

* IDOC Show (select)
  case save_ok-code.

* Select row
    when 'PICK'.
      perform at_line_selection_103.

* Lines show change
    when 'MEHR'.
      case it_sel-item.
        when 'I_Z4'.                   " Download Processing
          if i_anzeige_p = '1'.
            i_anzeige_p = ' '.
          else.
            i_anzeige_p = '1'.
          endif.
        when 'I_Z5'.                   " Download-Communication
          if i_anzeige_k = '1'.
            i_anzeige_k = ' '.
          else.
            i_anzeige_k = '1'.
          endif.
      endcase.
      it_wdls = i_wdls.
      sy-lsind = sy-lsind - 1.
      perform at_line_selection_102.

* Skip to EDI Status Tracking
    when 'VEDI'.
      call function 'RS_TREE_PUSH'.
      perform edi_interface.
      call function 'RS_TREE_POP'.

* Jump to the EDI data display
    when 'VDAT'.
      call function 'RS_TREE_PUSH'.
      perform edi_interface.
      call function 'RS_TREE_POP'.

* Restart call POS Download
    when 'RDLD'.
      it_wdls = i_wdls.
      perform download_restart.

* General application protocol
    when 'ALOG'.
      perform general_appl_log_display.


  endcase.

endform.                               " USER_COMMAND_103


*&---------------------------------------------------------------------*
*&      Form  NOT_KOMM_FILIALE
*&---------------------------------------------------------------------*
*       Showing what branches can not communicate                      *
*----------------------------------------------------------------------*
form not_komm_filiale.

* Hilfsvariable für Zugriff auf Tabelle IT_WRF1_Valid
  data: begin of h_key,
        mandt like t001w-mandt,
        kunnr like t001w-kunnr,
        end of h_key.

  set pf-status '101'.

* alle gültigen Filialen
  loop at it_t001w_valid.

* Prüfen, ob Filiale gültige Kommunikationsdaten hat.
    h_key-mandt = it_t001w_valid-mandt.
    h_key-kunnr = it_t001w_valid-kunnr.
    read table it_wrf1_valid with key h_key.
    if sy-subrc <> 0 or
       sy-subrc =  0 and it_wrf1_valid-posdex <> ik_x.

      write: /    it_t001w_valid-kunnr color col_key no-gap,
                  sy-vline no-gap,
                  it_t001w_valid-name1 color col_normal no-gap,
                  sy-vline no-gap,
                  it_t001w_valid-ort01 color col_normal no-gap,
                  sy-vline no-gap,
                  it_t001w_valid-eroed color col_normal no-gap,
                  sy-vline no-gap,
                  it_t001w_valid-schld color col_normal no-gap,
                  sy-vline no-gap.

      hide:       it_t001w_valid-werks,
                  it_t001w_valid-vkorg,
                  it_t001w_valid-vtweg.


    endif.
  endloop.
* Hide-Bereich für 'AT LINE SELECTION' löschen.
  clear it_wrf1_valid-werks.
endform.                               " NOT_KOMM_FILIALE


*&---------------------------------------------------------------------*
*&      Form  WDLSP_READ_PROC
*&---------------------------------------------------------------------*
*       Download-Verarbeitung Protokoll aufbereiten                    *
*----------------------------------------------------------------------*
form wdlsp_read_proc.

* protokollierte Positionen (IDOC) je Filiale in Tab. WDLSP suchen
* nur wenn sich der Curser auf gültiger Zeile befindet
  select        * from  wdlsp
         where  dldnr       = it_wdls-dldnr.

    set pf-status '103'.

* IDOC-Bezeichnung lesen
    edidot-descrp = ' '.
    select single * from  edidot
           where  doctyp      = wdlsp-doctyp
           and    langua      = sy-langu.

* Download-IDOC's anzeigen (Positionen und Appl.Log)
    write: / wdlsp-lfdnr color col_key,
             wdlsp-docnum color col_group,
             wdlsp-doctyp color col_normal.
*            edidot-descrp color col_normal.

*   Berücksichtige Sonderfall für Dummy-Initialisierung.
    if wdlsp-doctyp = 'DUMMY'.
      write: 'Dummy-Initialisierung'(105) color col_normal.
*   Es hat keine Dummy-Initialisierung stattgefunden.
    else. " wdlsp-doctyp <> 'DUMMY'.
      write:   edidot-descrp color col_normal.
    endif. " wdlsp-doctyp = 'DUMMY'.

    hide:    wdlsp.
    if i_anzeige_p = '1'.
      write: / wdlsp-anseg under wdlsp-docnum color col_normal,
               wdlsp-anloz color col_normal,
               wdlsp-vsest color col_normal.
      hide:    wdlsp.
      write: / wdlsp-stkey under wdlsp-anseg color col_normal,
               wdlsp-ltkey color col_normal.
      hide:    wdlsp.
    endif.
  endselect.
  if sy-subrc <> 0.
    message w006.
*   Kein Zwischenbeleg (IDOC) vorhanden.
  endif.


endform.                               " WDLSP_READ_PROC

*&---------------------------------------------------------------------*
*&      Form  WDLSP_READ_KOMM
*&---------------------------------------------------------------------*
*       Download-Kommunikation: Protokoll aufbereiten                  *
*----------------------------------------------------------------------*
form wdlsp_read_komm.

* protokollierte Positionen (IDOC) je Filiale in Tab. WDLSP suchen
* nur wenn sich der Curser auf gültiger Zeile befindet
  select        * from  wdlsp
         where  dldnr       = it_wdls-dldnr.

    set pf-status '103'.

* IDOC-Bezeichnung lesen
    edidot-descrp = ' '.
    select single * from  edidot
           where  doctyp      = wdlsp-doctyp
           and    langua      = sy-langu.

* Fehlermeldung des Versendestatus lesen
    teds2-descrp = ' '.
    select single * from teds2
           where  status      = wdlsp-vsest
           and    langua      = sy-langu.

* Download-IDOC's anzeigen (Positionen und Appl.Log)
    write: / wdlsp-lfdnr color col_key,
             wdlsp-docnum color col_group,
             wdlsp-vsest color col_key.
    case wdlsp-vsest.
      when ik_downloadkomm_ok.
        write: 28 teds2-descrp color col_positive.
      when '  '.
        write: 28 text-082 color col_negative.
      when 'X'.
        write 28  text-090 color col_negative.
      when others.
        write: 28 teds2-descrp color col_negative.
    endcase.
    hide:    wdlsp.
    if i_anzeige_k = '1'.
      write: / wdlsp-anseg under wdlsp-docnum color col_normal,
               wdlsp-anloz color col_normal,
               wdlsp-doctyp color col_normal.
*              edidot-descrp color col_normal.

*     Berücksichtige Sonderfall für Dummy-Initialisierung.
      if wdlsp-doctyp = 'DUMMY'.
        write: 'Dummy-Initialisierung'(105) color col_normal.
*     Es hat keine Dummy-Initialisierung stattgefunden.
      else. " wdlsp-doctyp <> 'DUMMY'.
        write:   edidot-descrp color col_normal.
      endif. " wdlsp-doctyp = 'DUMMY'.

      hide:    wdlsp.
      write: / wdlsp-stkey under wdlsp-anseg color col_normal,
               wdlsp-ltkey color col_normal.
      hide:    wdlsp.
    endif.
  endselect.
  if sy-subrc <> 0.
    message w006.
*   Kein Zwischenbeleg (IDOC) vorhanden.
  endif.


endform.                               " WDLSP_READ_KOMM

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PRINT
*&---------------------------------------------------------------------*
*       Upload-Verarbeitung-Protokoll drucken                          *
*----------------------------------------------------------------------*
form upload_print.

  clear: i_message.
* Message-Nummer in ein anderes Datenformat konvertieren
  i_wplst-msgnr = it_wplst-msgnr.

* lesen der Message
  call function 'WRITE_MESSAGE_NEW'
       exporting
            msgid = it_wplst-msgid
            msgno = i_wplst-msgnr
            msgty = it_wplst-fehlertyp
            msgv1 = it_wplst-parameter1
            msgv2 = it_wplst-parameter2
            msgv3 = it_wplst-parameter3
            msgv4 = it_wplst-parameter4
            msgv5 = ' '
       importing
            error = i_error
            messg = i_message
            msgln = i_length.

  write: /  it_wplst-filiale color col_key,
            it_wplst-name1 color col_normal,
            it_wplst-docnum color col_normal,
            it_wplst-erzdt color col_normal.
  hide:     it_wplst.
  if it_wplst-msgid = 'WP' and it_wplst-msgnr = '050'.

    write: at /12(i_length) i_message-msgtx color col_positive
                                                intensified off.
  else.
    write: at /12(i_length) i_message-msgtx color col_negative
                                               intensified off.
  endif.
  hide:      i_message, it_wplst.
  uline.

endform.                               " UPLOAD_PRINT


*eject.
*&---------------------------------------------------------------------*
*&      Form  GENERAL_APPL_LOG_DISPLAY
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form general_appl_log_display.

  data l_column_selection like baldisp.

  l_column_selection = '1  1222  22   '.

  call function 'APPL_LOG_DISPLAY'
       exporting
            object                    = 'W'
            subobject                 = 'W_DOWNLOAD'
            object_attribute          = 2
            date_from                 = p_dat_v
            time_from                 = '000000'
            date_to                   = p_dat_b
            time_to                   = sy-uzeit
            title_selection_screen    = ' '
            title_list_screen         = ' '
            column_selection          = l_column_selection
            suppress_selection_dialog = ' '.

endform.                               " GENERAL_APPL_LOG_DISPLAY


*eject.
*&---------------------------------------------------------------------*
*&      Form  EDI_INTERFACE
*&---------------------------------------------------------------------*
*  call EDI- function modules for one idoc                             *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form edi_interface.

  if not wdlsp-dldnr is initial.

    if not wdlsp-docnum is initial.

      case save_ok-code.
        when 'VEDI'.
*         EDI/Idoc status tracing
          call function 'EDI_DOCUMENT_STATUS_DISPLAY'
               exporting
                    docnum                 = wdlsp-docnum
               exceptions
                    no_status_record_found = 01.

        when 'VDAT'.
*         EDI/Idoc data display
          call function 'EDI_DOCUMENT_DATA_DISPLAY'
               exporting
                    docnum               = wdlsp-docnum
               exceptions
                    no_data_record_found = 01.

      endcase.                         " save_ok-code

    else.                              " not wdlsp-docnum is initial
      message w006.
*D  Kein Zwischenbeleg (IDOC) vorhanden
    endif.                             " not wdlsp-docnum is initial

  else.
    message e012.
*D   Bitte positionieren Sie den Cursor auf eine gültige IDOC-Zeile
  endif.                               " not wdlsp-dldnr is initial

  clear wdlsp.

endform.                               " EDI_INTERFACE


*eject.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RESTART
*&---------------------------------------------------------------------*
* Restartfunktion des POS-Downloads für ein IDOC aufrufen              *
*----------------------------------------------------------------------*
form download_restart.

  data: pi_dldnr like wdls-dldnr,
        dldnr like wdls-dldnr,
        tabix like sy-tabix,
        error_has_occured like wpstruc-modus.

  data: begin of i_pet_errors  occurs 10.
          include structure wperror_rs.
  data: end of i_pet_errors.


  if not ( it_wdls-dldnr is initial ).

    if it_wdls-systp = 'POS'.

      if it_wdls-gesst = c_status_fehlende_daten or
         it_wdls-gesst = c_status_fehlende_idocs.

        call function 'POS_RESTART'
          exporting
               pi_dldnr             = it_wdls-dldnr
*            PI_CREDAT            =
               pi_generate_list     = ' '
          importing
               pe_error_has_occured = error_has_occured
          tables
               pet_errors           = i_pet_errors
          exceptions
               wrong_input          = 1
               no_filia_found       = 2
               others               = 3.

        if sy-subrc <> 0.
          message w020.
          exit.
        endif.

****************************************************
* Geändert von Thomas Roth. 01.08.1996
*       Falls keine Fehler auftraten.
        if error_has_occured = space.
*         Zwischenspeichern der Downloadnummer.
          dldnr = it_wdls-dldnr.

*         Nummer der Tabellenzeile der alten Kopfzeile bestimmen.
          read table it_wdls with key
               dldnr = dldnr
               binary search.

          tabix = sy-tabix.

*         Alte Kopfzeile aus Puffer löschen.
          delete it_wdls index tabix.

*         Neue Kopfzeile von DB lesen und in Puffer einfügen
          select single * from  wdls into it_wdls
                 where dldnr = dldnr.

*         Einfügen der neuen Kopfzeile in die interne Tabelle.
          insert it_wdls index tabix.

*         Alte Positionsdaten aus Puffer löschen.
          delete g_t_wdlsp where dldnr = dldnr.

*         Neue Positionsdaten von DB lesen und in Puffer einfügen
          select * from  wdlsp appending table g_t_wdlsp
                 where dldnr = dldnr.

*         Daten resortieren.
          sort g_t_wdlsp by dldnr descending doctyp lfdnr.


*         Alte Liste löschen.
          sy-lsind = sy-lsind - 1.

*         Neue Liste Generieren.
          perform wdlsp_read_proc_new.

*         'Download wurde wiederholt'.
          message s024.

* Erweitert von Thomas Roth. 13.09.1996
*       Falls Fehler auftraten und der Download nicht wiederholt
*       werden konnte.
        else. " error_has_occured <> SPACE.
*         Lese Fehlermeldung.
          read table i_pet_errors index 1.

          case i_pet_errors-error.
            when '1'.
*             'Für Filiale & ist keine Protokollschreibung vorgesehen'
              message id 'WC' type 'W' number '153'
                      with i_pet_errors-filia.

            when '2'.
*             'Die Stauts-ID & dieser Filiale wurde vom Subsystem noch
*             nicht bearbeitet'
              message id 'WC' type 'W' number '154'
                      with i_pet_errors-dldnr.

            when '3'.
*             'Für die Filiale & ist bereits ein jüngerer'
*              POS-Ausgang vorhanden'
              message id 'WC' type 'W' number '155'
                      with i_pet_errors-filia  i_pet_errors-dldnr.

            when '4'.
*             'Das Kommunikationsprofil für die Filiale & wurde'
*              gelöscht'
              message id 'WC' type 'W' number '156'
                      with i_pet_errors-filia.

            when '5'.
*             'Betrachtungszeitraumende liegt bereits in der'
*              Vergangenheit'
              message id 'WC' type 'W' number '157'
                      with i_pet_errors-filia.

          endcase. " i_pet_errors-error.

        endif. " error_has_occured = SPACE.
****************************************************
      else. " it_wdls-gesst <> c_status_fehlende_daten ...
*       'Die Aufbereitung für diese Filiale ist bereits OK'
         message id 'WC' type 'W' number '158'.

      endif. " it_wdls-gesst = c_status_fehlende_daten ...

    else.                              " it_wdls-systp = 'POS'
      message i151(wb).
*D  Wiederholungsfunktion ist nur für POS vorhanden
    endif.                             " wdls-systp = 'POS'

  else.                                " not wdlsp-docnum is initial
    message w006.
*D  Kein Zwischenbeleg (IDOC) vorhanden
  endif.                               " not wdlsp-docnum is initial


endform.                               " DOWNLOAD_RESTART


*&---------------------------------------------------------------------*
*&      Form  WDLSP_READ_PROC_NEW
*&---------------------------------------------------------------------*
*       Download-Verarbeitung Protokoll aufbereiten                    *
* J.B. 12.6. : WDLSP u. U. schon gelesen u. VSEST aktualisiert
*----------------------------------------------------------------------*
form wdlsp_read_proc_new.
* protokollierte Positionen (IDOC) je Filiale in Tab. WDLSP suchen
* nur wenn sich der Curser auf gültiger Zeile befindet

  data: begin of t_wdlsp occurs 0.
          include structure wdlsp.
  data: end of t_wdlsp.

  data: line_count type i value 0.

* IT_wdls nachlesen
  read table it_wdls with key dldnr = it_wdls-dldnr.

  loop at g_t_wdlsp
       where dldnr = it_wdls-dldnr.
    append g_t_wdlsp to t_wdlsp.
  endloop.                    " at g_t_wdlsp

  if sy-subrc <> 0.
    select * from  wdlsp into table t_wdlsp
         where dldnr = it_wdls-dldnr.
    if sy-subrc <> 0.
*     Kopfdaten anzeigen lassen.
      set pf-status '103'.
      write: / 'keine Positionen vorhanden'(086) color col_normal.
*     MESSAGE W006.
*     Kein Zwischenbeleg (IDOC) vorhanden.
    endif.
  endif.                               " sy-subrc <> 0.

  set pf-status '103'.

* Daten sortieren.
  sort t_wdlsp by doctyp lfdnr.
* sort t_wdlsp descending by anloz.    " fehlerhalfte IDOC vorne

  loop at t_wdlsp.
*   nur wenn kein Referenz-IDOC
    check t_wdlsp-rfpos is initial.
*   IDOC-Bezeichnung lesen
*   PERFORM EDIDOT_SELECT
*           USING T_WDLSP-DOCTYP
*           EDIDOT-DESCRP.

    perform edbast_select
            using t_wdlsp-doctyp
            edbast-descrp.

*   Download-IDOC's anzeigen (Positionen und Appl.Log)
    write: / t_wdlsp-doctyp color col_key.

*   Falls Normalanzeige.
    if i_anzeige_p = '1'.
      write:   t_wdlsp-docnum color col_normal.

*   Falls Detailanzeige.
    else. " i_anzeige_p <> '1'.
*     Falls keine Logzeilen geschrieben wurden.
      if t_wdlsp-anloz = 0.          "korrekt aufbereitetes IDOC
        write: t_wdlsp-docnum color col_positive.

*     Falls Logzeilen geschrieben wurden.
      else. " T_Wdlsp-Anloz > 0.
*       Falls Aufbereitungsstatus bereits auf Positionsebene gepflegt.
        if t_wdlsp-gesst <> space.
*         Falls zu dieser Position bereits ein Restart durchgeführt
*         wurde.
          if t_wdlsp-rstrt <> space.
            write: t_wdlsp-docnum color col_negative intensified off.

*         Falls zu dieser Position noch kein Restart durchgeführt wurde.
          else. " T_Wdlsp-rstrt = space.
            case t_wdlsp-gesst.
*             Falls Benutzerhinweis
              when c_status_user_hint.
                write: t_wdlsp-docnum color col_key.

*             Falls fehlende Daten oder IDOC.
              when others.
                write: t_wdlsp-docnum color col_negative.

            endcase. " t_Wdlsp-gesst.
          endif. " t_wdlsp-rstrt <> space.

*      Falls Aufbereitungsstatus noch nicht auf Positionsebene gepflegt.
        else. " t_wdlsp-gesst = space.
          case it_wdls-gesst.
*         Falls Benutzerhinweis
            when c_status_user_hint.
              write: t_wdlsp-docnum color col_key.

*           Falls fehlende Daten oder IDOC.
            when others.
              write: t_wdlsp-docnum color col_negative.

          endcase. " t_Wdlsp-gesst.
        endif. " t_wdlsp-gesst <> space.
      endif. " T_Wdlsp-Anloz = 0.          "korrekt aufbereitetes IDOC
    endif. " i_anzeige_p = '1'.

*   Berücksichtige Sonderfall für Dummy-Initialisierung.
    if t_wdlsp-doctyp = 'DUMMY'.
      write: 'Dummy-Initialisierung'(105) color col_normal.
*   Es hat keine Dummy-Initialisierung stattgefunden.
    else. " t_wdlsp-doctyp <> 'DUMMY'.
      write:   edbast-descrp color col_normal.
    endif. " t_wdlsp-doctyp = 'DUMMY'.

    wdlsp = t_wdlsp.
    hide:    wdlsp.
    if i_anzeige_p = '1'.
      write: / t_wdlsp-anseg under t_wdlsp-docnum color col_normal.

*     Falls keine Logzeilen geschrieben wurden.
      if t_wdlsp-anloz = 0.          "korrekt aufbereitetes IDOC
        write: t_wdlsp-anloz color col_positive.

*       Falls ein IDOC erzeugt wurde.
        if t_wdlsp-docnum <> 0.
          write: 'Fehler, Zwischenbeleg korrekt erzeugt'(091)
                 color col_positive.

*       Falls kein IDOC erzeugt wurde.
        else. " t_wdlsp-docnum = 0.
          write:
          'Fehler, Zwischenbeleg brauchte nicht erzeugt zu werden'(095)
          color col_positive.
        endif. " t_wdlsp-docnum <> 0.

*     Falls Logzeilen geschrieben wurden.
      else. " T_Wdlsp-Anloz > 0.
*       Falls Aufbereitungsstatus bereits auf Positionsebene gepflegt.
        if t_wdlsp-gesst <> space.
*         Falls zu dieser Position bereits ein Restart durchgeführt
*         wurde.
          if t_wdlsp-rstrt <> space.
            write: t_wdlsp-anloz color col_negative intensified off.

*           Falls fehlende Daten.
            if t_wdlsp-gesst = c_status_data_missing.
              write: 'Warnungen protokolliert'(093)
                     color col_negative intensified off.
*           Falls fehlendes IDOC.
            else. " t_Wdlsp-gesst <> c_status_data_missing.
              write: 'Fehler protokolliert'(092)
                     color col_negative intensified off.
            endif. " t_Wdlsp-gesst = c_status_data_missing.

            write: 51 'Restart bereits durchgeführt'(096)
                      color col_negative intensified off.

*         Falls zu dieser Position noch kein Restart
*         durchgeführt wurde.
          else. " T_Wdlsp-rstrt = space.
            case t_wdlsp-gesst.
*             Falls Benutzerhinweis
              when c_status_user_hint.
                write: t_wdlsp-anloz color col_key,
                       'Hinweise protokolliert'(094)
                       color col_key.

*             Falls fehlende Daten oder IDOC.
              when others.
                write: t_wdlsp-anloz color col_negative.

*               Falls fehlende Daten.
                if t_wdlsp-gesst = c_status_data_missing.
                  write: 'Warnungen protokolliert'(093)
                         color col_negative.
*                 Falls fehlendes IDOC.
                else. " t_wdlsp-gesst <> c_status_data_missing.
                  write: 'Fehler protokolliert'(092)
                         color col_negative.
                endif. " t_wdlsp-gesst = c_status_data_missing.
            endcase. " t_Wdlsp-gesst.
          endif. " T_Wdlsp-rstrt <> space.

*       Falls Aufbereitungsstatus noch nicht auf Positionsebene
*       gepflegt.
        else. " t_wdlsp-gesst = space.
          case it_wdls-gesst.
*           Falls Benutzerhinweis
            when c_status_user_hint.
              write: t_wdlsp-anloz color col_key,
                     'Hinweise protokolliert'(094)
                     color col_key.

*           Falls fehlende Daten oder IDOC.
            when others.
              write: t_wdlsp-anloz color col_negative.

*             Falls fehlende Daten oder IDOC.
              if it_wdls-gesst = c_status_data_missing.
                write: 'Warnungen protokolliert'(093)
                       color col_negative.
*             Falls fehlendes IDOC.
              else. " it_wdls-gesst <> c_status_data_missing.
                write: 'Fehler protokolliert'(092)
                       color col_negative.
              endif. " it_wdls-gesst = c_status_data_missing.
          endcase. " t_Wdlsp-gesst.
        endif. " t_wdlsp-gesst <> space.
      endif. " T_Wdlsp-Anloz = 0.          "korrekt aufbereitetes IDOC

      hide:    wdlsp.
      write: / t_wdlsp-stkey under t_wdlsp-anseg color col_normal,
               t_wdlsp-ltkey color col_normal.
      hide:    wdlsp.
    endif. " i_anzeige_p = '1'.

  endloop.                             " at t_wdlsp

  i_wdls = it_wdls.
  clear: wdlsp.



endform.                               " WDLSP_READ_PROC_NEW


*&---------------------------------------------------------------------*
*&      Form  WDLSP_READ_KOMM_NEW
*&---------------------------------------------------------------------*
*       Download-Kommunikation: Protokoll aufbereiten                  *
* J.B. 12.6. : WDLSP u. U. schon gelesen u. VSEST aktualisiert
*----------------------------------------------------------------------*
form wdlsp_read_komm_new.

* protokollierte Positionen (IDOC) je Filiale in Tab. WDLSP suchen
* nur wenn sich der Curser auf gültiger Zeile befindet

  data: begin of t_wdlsp occurs 0.
          include structure wdlsp.
  data: end of t_wdlsp.

  data: line_count type i value 0.
  data: i_twpfi like twpfi.

* it_wdls nachlesen
  read table it_wdls with key dldnr = it_wdls-dldnr.

  loop at g_t_wdlsp
       where dldnr = it_wdls-dldnr.
    append g_t_wdlsp to t_wdlsp.
    line_count = line_count + 1.
  endloop.                    " at g_t_wdlsp where dldnr = it_wdls-dldnr

  if line_count = 0.
    select * from  wdlsp into table t_wdlsp
         where dldnr = it_wdls-dldnr.
    if sy-subrc <> 0.
*     Kopfdaten anzeigen lassen.
      set pf-status '103'.
      write: / 'keine Positionen vorhanden'(086) color col_normal.

*     message w006.
*     Kein Zwischenbeleg (IDOC) vorhanden.
    endif.
  endif.                               " line_count = 0

  set pf-status '103'.
  sort t_wdlsp descending by vsest.    " fehlerhalfte IDOC vorne
  loop at t_wdlsp.
* IDOC-Bezeichnung lesen
    edbast-descrp = ' '.
    select single * from  edbast
           where  idoctyp     = t_wdlsp-doctyp
           and    langua      = sy-langu.

*    SELECT SINGLE * FROM  EDIDOT
*           WHERE  DOCTYP      = T_WDLSP-DOCTYP
*           AND    LANGUA      = SY-LANGU.

* Fehlermeldung des Versendestatus lesen
    teds2-descrp = ' '.
    select single * from teds2
           where  status      = t_wdlsp-vsest
           and    langua      = sy-langu.

* Download-IDOC's anzeigen (Positionen und Appl.Log)
    write: / t_wdlsp-doctyp color col_key,
             t_wdlsp-docnum color col_group,
             t_wdlsp-vsest color col_key.

* Download-Status überprüfen.
    if t_wdlsp-vsest = '  '.
        write: 'Kein Zwischenbeleg erzeugt '(090) under text-059
               color col_normal.
    else.
      call function 'POS_CUST_COMM_PROFILE_READ'
          exporting
             i_locnr               = it_wdls-empfn
          importing
             o_twpfi               = i_twpfi
          exceptions
             filiale_unbekannt     = 1
             komm_profil_unbekannt = 2
             others                = 3.

      if t_wdlsp-vsest = i_twpfi-oksta.
        write: teds2-descrp under text-059 color col_positive.
      else.
        write: teds2-descrp under text-059 color col_negative.
      endif.
    endif.

    wdlsp = t_wdlsp.
    hide:    wdlsp.
    if i_anzeige_k = ' '.
      write: / t_wdlsp-anseg under t_wdlsp-docnum color col_normal,
               t_wdlsp-anloz color col_normal.

*     Berücksichtige Sonderfall für Dummy-Initialisierung.
      if t_wdlsp-doctyp = 'DUMMY'.
        write: 'Dummy-Initialisierung'(105) color col_normal.
*     Es hat keine Dummy-Initialisierung stattgefunden.
      else. " t_wdlsp-doctyp <> 'DUMMY'.
        write:   edbast-descrp color col_normal.
      endif. " t_wdlsp-doctyp = 'DUMMY'.

      hide:    wdlsp.
    endif.

  endloop.                             " at t_wdlsp

  i_wdls = it_wdls.
  clear: wdlsp.


endform.                               " WDLSP_READ_KOMM_NEW


*eject
************************************************************************
form edidot_select
     using  pi_doctyp  like wdlsp-doctyp
            pe_descrp  like edidot-descrp.
************************************************************************
* Liefert die Bezeichnung zu einem IDOC-Typ.
* Mit interner Pufferung.
* ----------------------------------------------------------------------
* PARAMETER:
* PI_DOCTYP: IDOC-Typ zu dem die Bezeichnung gelesen werden soll.
*
* PE_DESCRP: Bezeichnungs des IDOC-Typs.
* ----------------------------------------------------------------------
* AUTOR(EN):
* Thomas Roth (CAS-Nord)
************************************************************************
  data: tabix like sy-tabix.


* Rücksetze Ausgabestruktur.
  clear: pe_descrp.

* Falls die Kopfzeile des Puffers bereits die nötigen Daten enthält.
  if g_t_edidot-doctyp = pi_doctyp.
    move g_t_edidot-descrp to pe_descrp.

* Falls die Kopfzeile des Puffers nicht die nötigen Daten enthält,
* dann Suche im Puffer nach den Daten.
  else.
    read table g_t_edidot with key
         doctyp = pi_doctyp
         langua = sy-langu
         binary search.

*   Falls die Daten im Puffer gefunden wurden.
    if sy-subrc = 0.
      move g_t_edidot-descrp to pe_descrp.

*   Falls keine Werte gefunden wurden, dann lese von DB.
    else. " sy-subrc <> 0.
      tabix = sy-tabix.

      select single * from  edidot
             where  doctyp = pi_doctyp
             and    langua = sy-langu.

*     Falls Daten gelesen wurden.
      if sy-subrc = 0.
*       Übernehme Daten in den Puffer.
        g_t_edidot = edidot.
        insert g_t_edidot index tabix.

*       Fülle Ausgabestruktur.
        pe_descrp = g_t_edidot-descrp.
      endif. " sy-subrc = 0 auf EDIDOT
    endif. " sy-subrc = 0  auf g_t_edidot
  endif. " g_t_edidot-doctyp = pi_doctyp.
*

endform.                               " edidot_select


*eject
************************************************************************
form edbast_select
     using  pi_doctyp  like wdlsp-doctyp
            pe_descrp  like edbast-descrp.
************************************************************************
* Liefert die Bezeichnung zu einem IDOC-Typ.
* Mit interner Pufferung.
* ----------------------------------------------------------------------
* PARAMETER:
* PI_DOCTYP: IDOC-Typ zu dem die Bezeichnung gelesen werden soll.
*
* PE_DESCRP: Bezeichnungs des IDOC-Typs.
* ----------------------------------------------------------------------
* AUTOR(EN):
* Thomas Roth (CAS-Nord)
************************************************************************
  data: tabix like sy-tabix.


* Rücksetze Ausgabestruktur.
  clear: pe_descrp.

* Falls die Kopfzeile des Puffers bereits die nötigen Daten enthält.
  if g_t_edbast-idoctyp = pi_doctyp.
    move g_t_edbast-descrp to pe_descrp.

* Falls die Kopfzeile des Puffers nicht die nötigen Daten enthält,
* dann Suche im Puffer nach den Daten.
  else.
    read table g_t_edbast with key
         idoctyp = pi_doctyp
         langua = sy-langu
         binary search.

*   Falls die Daten im Puffer gefunden wurden.
    if sy-subrc = 0.
      move g_t_edbast-descrp to pe_descrp.

*   Falls keine Werte gefunden wurden, dann lese von DB.
    else. " sy-subrc <> 0.
      tabix = sy-tabix.

      select single * from  edbast
             where  idoctyp = pi_doctyp
             and    langua  = sy-langu.

*     Falls Daten gelesen wurden.
      if sy-subrc = 0.
*       Übernehme Daten in den Puffer.
        g_t_edbast = edbast.
        insert g_t_edbast index tabix.

*       Fülle Ausgabestruktur.
        pe_descrp = g_t_edbast-descrp.
      endif. " sy-subrc = 0 auf EDbasT
    endif. " sy-subrc = 0  auf g_t_edbast
  endif. " g_t_EdbasT-doctyp = pi_doctyp.
*

endform.                               " edidot_select
