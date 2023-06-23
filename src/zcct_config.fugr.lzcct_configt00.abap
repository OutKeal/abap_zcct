*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCCT_AC_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZCCT_AC_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_AC_CONFIG                .
CONTROLS: TCTRL_ZCCT_AC_CONFIG
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZCCT_BLOCK......................................*
DATA:  BEGIN OF STATUS_ZCCT_BLOCK                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_BLOCK                    .
CONTROLS: TCTRL_ZCCT_BLOCK
            TYPE TABLEVIEW USING SCREEN '0017'.
*...processing: ZCCT_BL_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZCCT_BL_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_BL_CONFIG                .
CONTROLS: TCTRL_ZCCT_BL_CONFIG
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCCT_BP_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZCCT_BP_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_BP_CONFIG                .
CONTROLS: TCTRL_ZCCT_BP_CONFIG
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZCCT_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZCCT_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_CONFIG                   .
CONTROLS: TCTRL_ZCCT_CONFIG
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZCCT_EKGRP......................................*
DATA:  BEGIN OF STATUS_ZCCT_EKGRP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_EKGRP                    .
CONTROLS: TCTRL_ZCCT_EKGRP
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZCCT_JS.........................................*
DATA:  BEGIN OF STATUS_ZCCT_JS                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_JS                       .
CONTROLS: TCTRL_ZCCT_JS
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZCCT_MB_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZCCT_MB_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_MB_CONFIG                .
CONTROLS: TCTRL_ZCCT_MB_CONFIG
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZCCT_MT.........................................*
DATA:  BEGIN OF STATUS_ZCCT_MT                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_MT                       .
CONTROLS: TCTRL_ZCCT_MT
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZCCT_PR_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZCCT_PR_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_PR_CONFIG                .
CONTROLS: TCTRL_ZCCT_PR_CONFIG
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZCCT_TAX........................................*
DATA:  BEGIN OF STATUS_ZCCT_TAX                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCCT_TAX                      .
CONTROLS: TCTRL_ZCCT_TAX
            TYPE TABLEVIEW USING SCREEN '0018'.
*...processing: ZJSQJ...........................................*
DATA:  BEGIN OF STATUS_ZJSQJ                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZJSQJ                         .
CONTROLS: TCTRL_ZJSQJ
            TYPE TABLEVIEW USING SCREEN '0014'.
*...processing: ZJS_BUTYP.......................................*
DATA:  BEGIN OF STATUS_ZJS_BUTYP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZJS_BUTYP                     .
CONTROLS: TCTRL_ZJS_BUTYP
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZPAY_COST.......................................*
DATA:  BEGIN OF STATUS_ZPAY_COST                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPAY_COST                     .
CONTROLS: TCTRL_ZPAY_COST
            TYPE TABLEVIEW USING SCREEN '0016'.
*...processing: ZTPAYTYP........................................*
DATA:  BEGIN OF STATUS_ZTPAYTYP                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPAYTYP                      .
CONTROLS: TCTRL_ZTPAYTYP
            TYPE TABLEVIEW USING SCREEN '0015'.
*.........table declarations:.................................*
TABLES: *ZCCT_AC_CONFIG                .
TABLES: *ZCCT_BLOCK                    .
TABLES: *ZCCT_BL_CONFIG                .
TABLES: *ZCCT_BP_CONFIG                .
TABLES: *ZCCT_CONFIG                   .
TABLES: *ZCCT_EKGRP                    .
TABLES: *ZCCT_JS                       .
TABLES: *ZCCT_MB_CONFIG                .
TABLES: *ZCCT_MT                       .
TABLES: *ZCCT_PR_CONFIG                .
TABLES: *ZCCT_TAX                      .
TABLES: *ZJSQJ                         .
TABLES: *ZJS_BUTYP                     .
TABLES: *ZPAY_COST                     .
TABLES: *ZTPAYTYP                      .
TABLES: ZCCT_AC_CONFIG                 .
TABLES: ZCCT_BLOCK                     .
TABLES: ZCCT_BL_CONFIG                 .
TABLES: ZCCT_BP_CONFIG                 .
TABLES: ZCCT_CONFIG                    .
TABLES: ZCCT_EKGRP                     .
TABLES: ZCCT_JS                        .
TABLES: ZCCT_MB_CONFIG                 .
TABLES: ZCCT_MT                        .
TABLES: ZCCT_PR_CONFIG                 .
TABLES: ZCCT_TAX                       .
TABLES: ZJSQJ                          .
TABLES: ZJS_BUTYP                      .
TABLES: ZPAY_COST                      .
TABLES: ZTPAYTYP                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
