*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCCT_IDOC_OUTPUT
*   generation date: 2019.12.31 at 10:00:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCCT_IDOC_OUTPUT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
