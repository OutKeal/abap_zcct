*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCCT_IDOC_OUTPUT................................*
DATA:  BEGIN OF STATUS_ZCCT_IDOC_OUTPUT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_IDOC_OUTPUT              .
CONTROLS: TCTRL_ZCCT_IDOC_OUTPUT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCCT_IDOC_OUTPUT              .
TABLES: ZCCT_IDOC_OUTPUT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
