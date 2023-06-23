*&---------------------------------------------------------------------*
*&  Include           /ATU/CA_POS_MONE01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&   Event INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  IF sy-calld IS INITIAL.
    p_dat_b = sy-datum.
    p_dat_v = sy-datum - 7.
  ELSE.
    p_dat_v = '19000101'.
    p_dat_b = '99991231'.
  ENDIF.
  REFRESH: it_t001w_all,
           it_t001w_valid,
           it_wrf1_valid.

  CLEAR:   i_z1, i_z2, i_z3, i_z4, i_z5, i_z6, i_z7, i_z8, i_z9, i_z10,
           i_z11.

  g_wplst_wp613-fehlertyp = 'W'.
  g_wplst_wp613-msgid     = 'WP'.
  g_wplst_wp613-msgnr     = '613'.
*&---------------------------------------------------------------------*
*&   Event START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

* Do not add any code here!!!
* Everything that has to be done at START-OF-SELECTION must be
* contained in the Form TREE_INIT, otherwise the refresh function
* will not work correctly! (Only exception: Form TREE_OUTPUT)

  PERFORM tree_init.
  PERFORM tree_output.


*&---------------------------------------------------------------------*
*&   Event AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  DATA l_object LIKE kssk-objek.

  REFRESH s_store_class.
  CLEAR   s_store_class.

  CALL FUNCTION 'LOCATIONS_FOR_DOWNLOAD'
    EXPORTING
      pi_datum_von       = p_dat_v
      pi_datum_bis       = p_dat_b
*     pi_kunnr           =
    TABLES
      t_t001w_valid      = it_t001w_valid
      t_t001w_all        = it_t001w_all
      t_wrf1_valid       = it_wrf1_valid
      t_kunnr            = s_store
      t_vkorg            = s_vkorg
      t_vtweg            = s_vtweg
    EXCEPTIONS
      not_found          = 1
      no_valid_locations = 2
      invalid_parameter  = 3
      no_locations       = 4
      OTHERS             = 5.

*&---------------------------------------------------------------------*
*&   Event AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.

* Name of the cursor attribute cache
  GET CURSOR FIELD i_fname.

  CASE sy-pfkey.

* Branch overview
    WHEN '101'.
      PERFORM at_line_selection_101.

* Appl. View Log
    WHEN '103'.
      PERFORM at_line_selection_103.

* External Updates Log
    WHEN '111'.
      PERFORM at_line_selection_111.


  ENDCASE.
*&---------------------------------------------------------------------*
*&   Event TOP-OF-PAGE DURING LINE-SELECTION
*&---------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.

  CASE sy-pfkey.
    WHEN '101'.
      SET TITLEBAR '101'.
      CASE it_sel-item.
        WHEN 'I_Z1'.
          FORMAT COLOR COL_HEADING.
          WRITE: (10) TEXT-020 NO-GAP, sy-vline NO-GAP,
                 (30) TEXT-013 NO-GAP, sy-vline NO-GAP,
                 (25) TEXT-014 NO-GAP, sy-vline NO-GAP,
                 (4)  TEXT-015 NO-GAP, sy-vline NO-GAP,
                 (4)  TEXT-016 NO-GAP, sy-vline NO-GAP.
          ULINE AT /(78).
        WHEN 'I_Z2'.
          FORMAT COLOR COL_HEADING.
          WRITE: (10) TEXT-020 NO-GAP, sy-vline NO-GAP,
                 (30) TEXT-013 NO-GAP, sy-vline NO-GAP,
                 (25) TEXT-014 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-017 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-018 NO-GAP, sy-vline NO-GAP.
          ULINE AT /(90).
        WHEN 'I_Z3'.
          FORMAT COLOR COL_HEADING.
          WRITE: (10) TEXT-020 NO-GAP, sy-vline NO-GAP,
                 (30) TEXT-013 NO-GAP, sy-vline NO-GAP,
                 (25) TEXT-014 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-017 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-018 NO-GAP, sy-vline NO-GAP,
                 (4)  TEXT-019 NO-GAP, sy-vline NO-GAP.
          ULINE AT /(95).
        WHEN 'I_Z9'.
          FORMAT COLOR COL_HEADING.
          WRITE: (10) TEXT-020 NO-GAP, sy-vline NO-GAP,
                 (30) TEXT-013 NO-GAP, sy-vline NO-GAP,
                 (25) TEXT-014 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-017 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-018 NO-GAP, sy-vline NO-GAP.
          ULINE AT /(90).
        WHEN 'I_Z12'.
          FORMAT COLOR COL_HEADING.
          WRITE: (10) TEXT-020 NO-GAP, sy-vline NO-GAP,
                 (30) TEXT-013 NO-GAP, sy-vline NO-GAP,
                 (25) TEXT-014 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-017 NO-GAP, sy-vline NO-GAP,
                 (10) TEXT-018 NO-GAP, sy-vline NO-GAP.
          ULINE AT /(90).
      ENDCASE.
    WHEN '103'.
      SET TITLEBAR '103'.
      PERFORM kopf_103.
      CASE it_sel-item.
        WHEN 'I_Z4'.                   " Download-treatment
          WRITE: (8)  TEXT-047 COLOR COL_HEADING,
                 (16) TEXT-046 COLOR COL_HEADING,
                      TEXT-081 COLOR COL_HEADING.
          IF i_anzeige_p = '1'.
            WRITE: /10(6) TEXT-048 COLOR COL_HEADING,
                      (6) TEXT-049 COLOR COL_HEADING,
                         TEXT-043 COLOR COL_HEADING.
            WRITE: /10(40) TEXT-051 COLOR COL_HEADING,
                      (40) TEXT-052 COLOR COL_HEADING.
          ENDIF.
        WHEN 'I_Z5'.                   " Download Communications
          WRITE: (8)  TEXT-047 COLOR COL_HEADING,
                 (16) TEXT-046 COLOR COL_HEADING,
                 (2)  TEXT-050 COLOR COL_HEADING,
                      TEXT-059 COLOR COL_HEADING.
          IF i_anzeige_k = ' '.
            WRITE: /10(6) TEXT-048 COLOR COL_HEADING,
                      (6) TEXT-049 COLOR COL_HEADING,
                          TEXT-081 COLOR COL_HEADING.
          ENDIF.
      ENDCASE.
      SKIP.
  ENDCASE.

*&---------------------------------------------------------------------*
*&   Event TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.

  CASE sy-pfkey.
    WHEN '103'.
      SET TITLEBAR '103'.
      PERFORM kopf_103.
      CASE it_sel-item.
        WHEN 'I_Z4'.                   " Download-treatment
          WRITE: (8)  TEXT-047 COLOR COL_HEADING,
                 (16) TEXT-046 COLOR COL_HEADING,
                      TEXT-081 COLOR COL_HEADING.
          IF i_anzeige_p = '1'.
            WRITE: /10(6) TEXT-048 COLOR COL_HEADING,
                      (6) TEXT-049 COLOR COL_HEADING,
                         TEXT-043 COLOR COL_HEADING.
            WRITE: /10(40) TEXT-051 COLOR COL_HEADING,
                      (40) TEXT-052 COLOR COL_HEADING.
          ENDIF.
        WHEN 'I_Z5'.                   " Download Communications
          WRITE: (8)  TEXT-047 COLOR COL_HEADING,
                 (16) TEXT-046 COLOR COL_HEADING,
                 (2)  TEXT-050 COLOR COL_HEADING,
                      TEXT-059 COLOR COL_HEADING.
          IF i_anzeige_k = ' '.
            WRITE: /10(6) TEXT-048 COLOR COL_HEADING,
                      (6) TEXT-049 COLOR COL_HEADING,
                          TEXT-081 COLOR COL_HEADING.
          ENDIF.
      ENDCASE.
      SKIP.
  ENDCASE.

*&---------------------------------------------------------------------*
*&   Event AT USER-COMMAND
*&---------------------------------------------------------------------*
* Image controls on the image status
* Set with the SET PF-STATUS 'XXX'
*&---------------------------------------------------------------------*
AT USER-COMMAND.

  save_ok-code = sy-ucomm.

* Pushbutton query download the image processing (status 103)
  IF sy-pfkey = '103'.
    PERFORM user_command_103.

* Pushbutton retrieve the image display error details
  ELSEIF sy-pfkey = '200'.
    PERFORM user_command_200.

  ENDIF.

  CLEAR: save_ok-code.

*&---------------------------------------------------------------------*
*&      Form  AT_LINE_SELECTION_108
*&---------------------------------------------------------------------*
*       External update log                                            *
*----------------------------------------------------------------------*
FORM at_line_selection_108.


* IT_WPXST read
  READ TABLE it_wpxst INDEX it_wpxst-counter.
* Data conversion for function module
  i_wpxst-msgnr  = it_wpxst-fehlernr.

* Read the message
  CALL FUNCTION 'WRITE_MESSAGE_NEW'
    EXPORTING
      msgid = it_wpxst-msgid
      msgno = i_wpxst-msgnr
      msgty = ik_message_typ
      msgv1 = it_wpxst-para1
      msgv2 = it_wpxst-para2
      msgv3 = it_wpxst-para3
      msgv4 = it_wpxst-para4
      msgv5 = ' '
    IMPORTING
      error = i_error
      messg = i_message
      msgln = i_length.

* Long read
  IF NOT i_message IS INITIAL.
    i_lt-msgid = it_wpxst-msgid.
    i_lt-msgnr = it_wpxst-fehlernr.
    i_lt-para1 = it_wpxst-para1.
    i_lt-para2 = it_wpxst-para2.
    i_lt-para3 = it_wpxst-para3.
    i_lt-para4 = it_wpxst-para4.

    PERFORM longtext USING i_lt-msgid
                           i_lt-msgnr
                           i_message
                           i_lt-para1
                           i_lt-para2
                           i_lt-para3
                           i_lt-para4.
  ELSE.
    MESSAGE e013.
* No long text
  ENDIF.
  CLEAR: it_wpxst, i_message.

ENDFORM.                               " AT_LINE_SELECTION_108
