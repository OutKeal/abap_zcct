"Name: \PR:SAPMM06E\EX:MM06EF0N_NEUE_POS_BESTELLUN_07\EI
ENHANCEMENT 0 ZCCT_SET_REPOS.
 IF EKPO-NETPR IS INITIAL.
*   DATA:ls_zcgzp TYPE ZE_ZCGZP.
*   SELECT SINGLE zcgzp
*     INTO ls_zcgzp FROM ZMMT0010
*     WHERE MATNR = EKPO-SATNR.
*     IF SY-SUBRC EQ 0 AND ls_zcgzp = 'A'.
*        CLEAR EKPO-REPOS.
*     ENDIF.

 ENDIF.

ENDENHANCEMENT.