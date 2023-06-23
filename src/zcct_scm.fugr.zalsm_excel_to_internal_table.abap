FUNCTION zalsm_excel_to_internal_table .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_BEGIN_COL) TYPE  I
*"     VALUE(I_BEGIN_ROW) TYPE  I
*"     VALUE(I_END_COL) TYPE  I
*"     VALUE(I_END_ROW) TYPE  I
*"  TABLES
*"      INTERN STRUCTURE  ZALSMEX_TABLINE
*"----------------------------------------------------------------------
*{   INSERT         S4DK900018                                        1

  DATA: excel_tab     TYPE  ty_t_sender.
  DATA: ld_separator  TYPE  c.
  DATA: application TYPE  ole2_object,
        workbook    TYPE  ole2_object,
        range       TYPE  ole2_object,
        worksheet   TYPE  ole2_object.
  DATA: h_cell  TYPE  ole2_object,
        h_cell1 TYPE  ole2_object.
  DATA:
    ld_rc             TYPE i.
*   Rückgabewert der Methode "clipboard_export     "

* Makro für Fehlerbehandlung der Methods
  DEFINE m_message.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WHEN OTHERS. RAISE upload_ole.
    ENDCASE.
  END-OF-DEFINITION.


* check parameters
  IF i_begin_row > i_end_row. RAISE inconsistent_parameters. ENDIF.
  IF i_begin_col > i_end_col. RAISE inconsistent_parameters. ENDIF.

* Get TAB-sign for separation of fields
  CLASS cl_abap_char_utilities DEFINITION LOAD.
  ld_separator = cl_abap_char_utilities=>horizontal_tab.

* open file in Excel
  IF application-header = space OR application-handle = -1.
    CREATE OBJECT application 'Excel.Application'.
    m_message.
  ENDIF.
  CALL METHOD  OF application    'Workbooks' = workbook.
  m_message.
  CALL METHOD  OF workbook 'Open'    EXPORTING #1 = filename.
  m_message.
*  set property of application 'Visible' = 1.
*  m_message.
  GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
  m_message.

* mark whole spread sheet
  CALL METHOD OF worksheet 'Cells' = h_cell
      EXPORTING #1 = i_begin_row #2 = i_begin_col.
  m_message.
  CALL METHOD OF worksheet 'Cells' = h_cell1
      EXPORTING #1 = i_end_row #2 = i_end_col.
  m_message.

  CALL METHOD  OF worksheet 'RANGE' = range
                 EXPORTING #1 = h_cell #2 = h_cell1.
  m_message.
  CALL METHOD OF range 'SELECT'.
  m_message.

* copy marked area (whole spread sheet) into Clippboard
  CALL METHOD OF range 'COPY'.
  m_message.
  IF sy-uname = 'D.WANGFANG'.
    WAIT UP TO  1 SECONDS.
  ENDIF.
* read clipboard into ABAP
  CALL METHOD cl_gui_frontend_services=>clipboard_import
    IMPORTING
      data       = excel_tab
    EXCEPTIONS
      cntl_error = 1
*     ERROR_NO_GUI         = 2
*     NOT_SUPPORTED_BY_GUI = 3
      OTHERS     = 4.


  IF sy-subrc <> 0.
    MESSAGE a037(alsmex).
  ENDIF.



  PERFORM separated_to_intern_convert TABLES excel_tab intern
                                      USING  ld_separator.

* clear clipboard
  REFRESH excel_tab.
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data       = excel_tab
    CHANGING
      rc         = ld_rc
    EXCEPTIONS
      cntl_error = 1
*     ERROR_NO_GUI         = 2
*     NOT_SUPPORTED_BY_GUI = 3
      OTHERS     = 4.

* quit Excel and free ABAP Object - unfortunately, this does not kill
* the Excel process
  CALL METHOD OF application 'QUIT'.
  m_message.

* >>>>> Begin of change note 575877
* to kill the Excel process it's necessary to free all used objects
  FREE OBJECT h_cell.       m_message.
  FREE OBJECT h_cell1.      m_message.
  FREE OBJECT range.        m_message.
  FREE OBJECT worksheet.    m_message.
  FREE OBJECT workbook.     m_message.
  FREE OBJECT application.  m_message.
* <<<<< End of change note 575877
*}   INSERT
ENDFUNCTION.



FORM separated_to_intern_convert TABLES i_tab       TYPE ty_t_sender
                                        i_intern    TYPE ty_t_itab
                                 USING  i_separator TYPE c.
  DATA: l_sic_tabix LIKE sy-tabix,
        l_sic_col   TYPE kcd_ex_col.
  DATA: l_fdpos     LIKE sy-fdpos.

  REFRESH i_intern.

  LOOP AT i_tab.
    l_sic_tabix = sy-tabix.
    l_sic_col = 0.
    WHILE i_tab CA i_separator.
      l_fdpos = sy-fdpos.
      l_sic_col = l_sic_col + 1.
      PERFORM line_to_cell_separat TABLES i_intern
                                   USING  i_tab l_sic_tabix l_sic_col
                                          i_separator l_fdpos.
    ENDWHILE.
    IF i_tab <> space.
      CLEAR i_intern.
      i_intern-row = l_sic_tabix.
      i_intern-col = l_sic_col + 1.
      i_intern-value = i_tab.
      APPEND i_intern.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SEPARATED_TO_INTERN_CONVERT
*---------------------------------------------------------------------*
FORM line_to_cell_separat TABLES i_intern    TYPE ty_t_itab
                          USING  i_line
                                 i_row       LIKE sy-tabix
                                 ch_cell_col TYPE kcd_ex_col
                                 i_separator TYPE c
                                 i_fdpos     LIKE sy-fdpos.
  DATA: l_string   TYPE ty_s_senderline.
  DATA  l_sic_int  TYPE i.

  CLEAR i_intern.
  l_sic_int = i_fdpos.
  i_intern-row = i_row.
  l_string = i_line.
  i_intern-col = ch_cell_col.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
  IF ( i_separator = ';' OR  i_separator = ',' ) AND
       l_string(1) = gc_esc.
    PERFORM line_to_cell_esc_sep USING l_string
                                       l_sic_int
                                       i_separator
                                       i_intern-value.
  ELSE.
    IF l_sic_int > 0.
      i_intern-value = i_line(l_sic_int).
    ENDIF.
  ENDIF.
  IF l_sic_int > 0.
    APPEND i_intern.
  ENDIF.
  l_sic_int = l_sic_int + 1.
  i_line = i_line+l_sic_int.
ENDFORM.

*---------------------------------------------------------------------*
FORM line_to_cell_esc_sep USING i_string
                                i_sic_int      TYPE i
                                i_separator    TYPE c
                                i_intern_value TYPE zalsmex_tabline-value.
  DATA: l_int         TYPE i,
        l_cell_end(2).
  FIELD-SYMBOLS: <l_cell>.
  l_cell_end = gc_esc.
  l_cell_end+1 = i_separator .

  IF i_string CS gc_esc.
    i_string = i_string+1.
    IF i_string CS l_cell_end.
      l_int = sy-fdpos.
      ASSIGN i_string(l_int) TO <l_cell>.
      i_intern_value = <l_cell>.
      l_int = l_int + 2.
      i_sic_int = l_int.
      i_string = i_string+l_int.
    ELSEIF i_string CS gc_esc.
*     letzte Celle
      l_int = sy-fdpos.
      ASSIGN i_string(l_int) TO <l_cell>.
      i_intern_value = <l_cell>.
      l_int = l_int + 1.
      i_sic_int = l_int.
      i_string = i_string+l_int.
      l_int = strlen( i_string ).
      IF l_int > 0 . MESSAGE x001(kx) . ENDIF.
    ELSE.
      MESSAGE x001(kx) . "was ist mit csv-Format
    ENDIF.
  ENDIF.

ENDFORM.
