*----------------------------------------------------------------------*
***INCLUDE /ATU/CA_POS_MONT05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module PBO_0100 output.
  SET PF-STATUS 'MAIN'.
  IF G_TREE IS INITIAL.
    " The Tree Control has not been created yet.
    " Create a Tree Control and insert nodes into it.
    PERFORM CREATE_AND_INIT_TREE.
  ENDIF.

endmodule.                 " PBO_0100  OUTPUT
