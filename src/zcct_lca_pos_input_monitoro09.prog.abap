*-------------------------------------------------------------------
***INCLUDE MWPERO09 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  INIT_0900  OUTPUT
*&---------------------------------------------------------------------*
*       Init screen 900
*----------------------------------------------------------------------*
module init_0900 output.

  set titlebar '090'.
  set pf-status '0900' excluding t_excl.

endmodule.                             " INIT_0900  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  FILL_0900  OUTPUT
*&---------------------------------------------------------------------*
*       Fill screen 0900
*----------------------------------------------------------------------*
module fill_0900 output.

* BI 551650 21.02.03
* If the positions are rejected for the "first" time you normally get
* poswpsa-status = '4' back from the perform routine "f09_write_status".
* Due to show the right status after pushing the "Reject"-Button in the
* popup field this enquiry is necessary. If the bon or position was
* really rejected by the system before then displaying '5' is not
* correct but consistent  ...
  IF poswpsa-status = '4'.
    poswpsa-status = '5'.
  ENDIF.
* EI 551650 21.02.03

  read table g_t_status_display into poswpsa_hi
       index status_ctrl-current_line.

  loop at screen.
    if screen-name = 'G_T_STATUS_DISPLAY-TIME' and
       poswpsa_hi-chgtim is initial.
      screen-invisible = '1'.
      modify screen.
    endif.
  endloop.

endmodule.                             " FILL_0900  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  INIT_0910  OUTPUT
*&---------------------------------------------------------------------*
*       Init screen 910
*----------------------------------------------------------------------*
module init_0910 output.

  set titlebar '091'.
  set pf-status '0910'.

endmodule.                             " INIT_0910  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  FILL_0910_REASON  OUTPUT
*&---------------------------------------------------------------------*
*       Fill screen 0910 with reason codes
*----------------------------------------------------------------------*
module fill_0910_reason output.

  read table g_t_reason_display into poswpsa_re
       index reason_ctrl-current_line.

endmodule.                             " FILL_0910_REASON  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  FILL_0910_CHANGES  OUTPUT
*&---------------------------------------------------------------------*
*       Fill screen 0910 with field changes
*----------------------------------------------------------------------*
module fill_0910_changes output.

  read table g_t_changes_display into poswpsa_ch
       index changes_ctrl-current_line.

endmodule.                             " FILL_0910_CHANGES  OUTPUT
