*-------------------------------------------------------------------
***INCLUDE LWPSAI09 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
module user_command_0900 input.

  case sy-ucomm.
    when 'OKAY'.
      leave to screen 0.

    when 'CANC'.
      leave to screen 0.

    when 'PROC'.
      perform f09_set_status using pos_status_process.
      perform f09_write_status.

    when 'PRON'.
      perform f09_set_status using pos_status_process_no_check.
      perform f09_write_status.

    when 'REJE'.
      perform f09_set_status using pos_status_reject_user.
      perform f09_write_status.

    when 'REVE'.
      perform f09_set_status using pos_status_reversed.
      perform f09_write_status.

    when 'RESU'.
      perform f09_set_status using pos_status_resubmit.
      perform f09_write_status.
* BI 551650 06.05.03
      poswpsa-status = pos_status_resubmit.
* EI 551650 06.05.03

    when 'REFR'.
      perform f09_write_status.

    when 'POST'.
      perform f09_process_idoc.
      perform f09_write_status.


    when 'PICK'.
      perform f09_display_details.

    when 'LONG'.
      perform f09_display_longtext.
  endcase.

  clear sy-ucomm.

endmodule.                             " USER_COMMAND_0900  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0910  INPUT
*&---------------------------------------------------------------------*
module user_command_0910 input.

  case sy-ucomm.
    when 'OKAY'.
      leave to screen 0.

    when 'CANC'.
      leave to screen 0.

  endcase.

  clear sy-ucomm.

endmodule.                             " USER_COMMAND_0910  INPUT
