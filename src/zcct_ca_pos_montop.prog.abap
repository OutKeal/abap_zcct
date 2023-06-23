
PROGRAM  zcct_ca_pos_monitor MESSAGE-ID wp LINE-SIZE 200 NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Update status for table WDLS / WDLSP
INCLUDE lwdldeco.
* POS constants
INCLUDE lwpueeco.
INCLUDE lwpsacon.
* Object Macros
INCLUDE <cntn01>.
* Icons
INCLUDE <icon>.

*-----------------------------------------------------------------------
* Includes other statements on modularization
INCLUDE zcct_ca_pos_mont01.
INCLUDE mwperto2.
INCLUDE mwperto3.
INCLUDE zcct_ca_pos_mont04.

*-----------------------------------------------------------------------
* AT LINE-SELECTION

* Internal table to determine the line
DATA: BEGIN OF it_sel OCCURS 15,
        line     LIKE sy-lilli,
        item(10) TYPE c,
      END OF it_sel.
* Internal field for caching the field name
DATA: i_fname(30).

*-----------------------------------------------------------------------
* Messages to implement the table T100

* Internal auxiliary table
DATA  BEGIN OF i_message.
INCLUDE STRUCTURE message.
DATA  END OF i_message.

*-----------------------------------------------------------------------
* Special relationship structures for BO '/ATU/RCOPA'
DATA  BEGIN OF gt_copa_nodes OCCURS 0.
INCLUDE STRUCTURE snodetext.
INCLUDE TYPE wpusa_foldoc.
DATA  END OF gt_copa_nodes.

FIELD-SYMBOLS <gf_copa_node> LIKE LINE OF gt_copa_nodes.
*-----------------------------------------------------------------------
* Parameters for Reorg
DATA: p_dat_br  LIKE sy-datum,
      p_filiale LIKE t001w-werks.
DATA: c_status_fehlende_daten LIKE wdls-gesst VALUE 'W',
      c_status_fehlende_idocs LIKE wdls-gesst VALUE 'E'.

*-----------------------------------------------------------------------
* Return variables for import parameters of the function block
DATA: i_error(2)     TYPE c,
      i_length       TYPE i,
      i_wplst-msgnr  LIKE sy-msgno,
      i_wpxst-msgnr  LIKE sy-msgno,
      i_anzeige_p(1) TYPE c VALUE ' ',
      i_anzeige_k(1) TYPE c VALUE ' ',
      i_lt-msgid     LIKE wpxst-msgid,
      i_lt-msgnr     LIKE sy-msgno,
      i_lt-para1     LIKE wpxst-para1,
      i_lt-para2     LIKE wpxst-para2,
      i_lt-para3     LIKE wpxst-para3,
      i_lt-para4     LIKE wpxst-para4.


*-----------------------------------------------------------------------
* Tables
TABLES: edidot,                  " IDOC Type Designation
        edbast,                  " IDOC Type Designation
        t001w,                   " Plants
        kna1,                    " Customer master
        wplst,                   " Upload processing protocol
        wptst,                   " Upload touchdown protocol (IDOC-Ber.)
        wpxst,                   "
        wdls,                    " Download processing protocol header
        wdlsp,                   " Download processing protocol Pos.
        zscmt0010.

*-----------------------------------------------------------------------
* Various constants
DATA: ik_x(1)               TYPE c VALUE 'X',
      ik_pos(6)             TYPE c VALUE 'W_PDLD',
      ik_bb(8)              TYPE c VALUE 'W_WBBDLD',
      ik_dat_von            LIKE sy-datum VALUE '19940101',
      ik_docu_id_msg(2)     TYPE c VALUE 'NA',
      ik_bis                TYPE c VALUE '-',
      ik_downloadkomm_ok(2) TYPE c VALUE '12',
      ik_message_typ        TYPE c VALUE 'E',
      ik_sortliste          LIKE wdls-systp VALUE 'SL'.

*-----------------------------------------------------------------------
* Internal tables:

* All branches (table T001W, with VLFKZ = A)
* (A = Werkstyp branches, distribution centers Werkstyp B =)
DATA  BEGIN OF it_t001w_all OCCURS 10.
INCLUDE STRUCTURE t001w.
DATA  END   OF it_t001w_all.

* Selection parameter with all branches, including the restriction
* By the class
RANGES s_store_class FOR wplst-filiale.

* Branches, during the selected period of time in operation.
* (Selective restriction of the table IT_t001w_All)
DATA  BEGIN OF it_t001w_valid OCCURS 10.
INCLUDE STRUCTURE t001w.
DATA: eroed LIKE wrf1-eroed,
      schld LIKE wrf1-schld,
      END   OF it_t001w_valid.

* Advanced plant data to selected retail outlets.
* (Selections Table IT_t001w_VALID analogy)
* If POSDEX = X, Branch with electr. Communications
DATA  BEGIN OF it_wrf1_valid OCCURS 10.
INCLUDE STRUCTURE wrf1.
DATA: posdex LIKE twpfi-posdex,
      werks  LIKE t001w-werks,
      name1  LIKE t001w-name1,
      ort01  LIKE t001w-ort01,
      END   OF it_wrf1_valid.

* Stores with download errors (Table WDLS)
DATA:   BEGIN OF it_wdls OCCURS 10.
          INCLUDE STRUCTURE wdls.
DATA:     name1   LIKE t001w-name1,
          ort01   LIKE t001w-ort01,
          vkorg   LIKE t001w-vkorg,
          vtweg   LIKE t001w-vtweg,
          okno(1) TYPE c,
        END   OF it_wdls.

* Download status positions
DATA: BEGIN OF g_t_wdlsp OCCURS 10.
        INCLUDE STRUCTURE wdlsp.
DATA: END OF g_t_wdlsp.

* Buffer table for IDoc-type designations.
DATA: BEGIN OF g_t_edidot OCCURS 10.
        INCLUDE STRUCTURE edidot.
DATA: END OF g_t_edidot.

* Buffer table for IDoc-type designations.
DATA: BEGIN OF g_t_edbast OCCURS 0.
        INCLUDE STRUCTURE edbast.
DATA: END OF g_t_edbast.

* Stores with upload error (Table WPLST)
DATA:   BEGIN OF it_wplst OCCURS 10.
          INCLUDE STRUCTURE wplst.
DATA:     name1 LIKE t001w-name1,
          ort01 LIKE t001w-ort01,
          vkorg LIKE t001w-vkorg,
          vtweg LIKE t001w-vtweg,
        END   OF it_wplst.

* Div. Upload accounting errors (Table WPTST)
DATA:   BEGIN OF it_wptst OCCURS 10.
          INCLUDE STRUCTURE wptst.
DATA:     name1 LIKE t001w-name1,
          ort01 LIKE t001w-ort01,
          vkorg LIKE t001w-vkorg,
          vtweg LIKE t001w-vtweg,
        END   OF it_wptst.

* External processing protocol (Table WPXST)
DATA:   BEGIN OF it_wpxst OCCURS 10.
          INCLUDE STRUCTURE wpxst.
DATA:     name1   LIKE t001w-name1,
          ort01   LIKE t001w-ort01,
          vkorg   LIKE t001w-vkorg,
          vtweg   LIKE t001w-vtweg,
          counter TYPE i,
        END   OF it_wpxst.

* Value of SET/GET parameter WPS, set at selection
DATA g_parameter_wps(132) VALUE 'X'.

*-----------------------------------------------------------------------
* Auxiliary structure (Used for internal caching)
DATA:   BEGIN OF i_wdls.
          INCLUDE STRUCTURE wdls.
DATA:   END OF i_wdls.



*-----------------------------------------------------------------------
* diverse Z鋒ler
DATA: i_z1   TYPE i,                    " Total Filialen
      i_z2   TYPE i,                    " Ge鰂fnete Filialen
      i_z3   TYPE i,                    " Filialen mit elektr. Komm.
      i_z4   TYPE i,                    " davon fehlerh. Downloadverarb.
      i_z4a  TYPE i,                   " davon OK-Downloadverarb.
      i_z5   TYPE i,                    " davon fehlerh. Downloadkomm.
      i_z5a  TYPE i,                   " davon OK-Downloadkomm.
      i_z6   TYPE i,                    " davon fehlerh. externe Verarb.
      i_z6a  TYPE i,                   " davon OK-Externe-Verarb.
      i_z7   TYPE i,                    " davon fehlerh. Uploadkomm.
      i_z7a  TYPE i,                   " davon OK-Uploadkomm.
      i_z8   TYPE i,                    " davon fehlerh. Uploadverarb.
      i_z8a  TYPE i,                   " davon OK-Uploadverarb.
      i_z9   TYPE i,                    " Filialen ohne elektr. Komm.
      i_z10  TYPE i,                   " davon fehlerh. Downloadaufbereit
      i_z10a TYPE i,                  " davon OK-Downloadaufbereit
      i_z11  TYPE i,                   " davon fehlerh. Uploadaufbereit.
      i_z11a TYPE i,                  " davon OK-Uploadaufbereit.
      i_z12  TYPE i.                   " Geschlossene Filialen

*-----------------------------------------------------------------------
* OK-Codes:
DATA: ok-code(4), save_ok-code(4).
DATA: lv_/atu/ca_posmt TYPE zcct_ca_posmt.
*-----------------------------------------------------------------------
* Selektions-Parameter:
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-106.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-103 FOR FIELD p_dat_v.
    PARAMETERS        p_dat_v      LIKE edidc-credat.
    PARAMETERS:       p_time_v     LIKE edidc-cretim
                                   VISIBLE LENGTH 5.
    SELECTION-SCREEN COMMENT 52(5) TEXT-104 FOR FIELD p_dat_b.
    PARAMETERS        p_dat_b      LIKE edidc-credat.
    PARAMETERS        p_time_b     LIKE edidc-cretim  DEFAULT '235959'
                                   VISIBLE LENGTH 5.
  SELECTION-SCREEN END OF LINE.
  SELECT-OPTIONS:   s_store      FOR zscmt0010-partner.

  SELECT-OPTIONS:   s_vkorg      FOR t001w-vkorg.
  SELECT-OPTIONS:   s_vtweg      FOR t001w-vtweg.
SELECTION-SCREEN END OF BLOCK block1.
* Block 2
*SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-216.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(31) text-035 FOR FIELD p_systp.
PARAMETERS        p_systp      LIKE wdls-systp NO-DISPLAY.
*SELECTION-SCREEN END OF LINE.
PARAMETERS        p_docnr      LIKE edidc-docnum NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK block2.
* Block 3
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE TEXT-217.
  SELECT-OPTIONS:   s_mestyp     FOR lv_/atu/ca_posmt,
*changed by liuliying for message variant begin 20140403
                    s_mescod     FOR edidc-mescod,
*changed by liuliying for message variant end 20140403
                    s_docnum     FOR edidc-docnum MEMORY ID dcn,
                    s_status     FOR poswpsa-status.
SELECTION-SCREEN END OF BLOCK block3.
