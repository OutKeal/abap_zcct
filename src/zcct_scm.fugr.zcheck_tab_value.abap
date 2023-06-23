FUNCTION zcheck_tab_value.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      TAB
*"      RULE STRUCTURE  ZCHECK_RULE
*"      RET STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <fs_value> TYPE any.
  FIELD-SYMBOLS: <fs_tab> TYPE  STANDARD TABLE.
  DATA tabix TYPE sy-tabix.
  DATA msg TYPE char40.
  DATA: lt_dd04vv LIKE TABLE OF dd04vv WITH HEADER LINE.
  DATA: lt_doma_value LIKE TABLE OF zdoma_value WITH HEADER LINE.


  LOOP AT rule.
    IF rule-rollname IS INITIAL.
      CONTINUE.
    ENDIF.

    lt_dd04vv-rollname = rule-rollname.
    COLLECT lt_dd04vv.
    CLEAR lt_dd04vv.
  ENDLOOP.


  IF lt_dd04vv[] IS NOT INITIAL.

    CALL FUNCTION 'ZGE_DOMA_GET'
*   EXPORTING
*     SPRAS            = '1'
      TABLES
        dd04vv     = lt_dd04vv
*       DD04T      =
        doma_value = lt_doma_value.
  ENDIF.


  SORT lt_doma_value BY rollname domval.

  LOOP AT tab .

    tabix = sy-tabix.
    LOOP AT rule.

      ASSIGN COMPONENT rule-fieldname OF STRUCTURE tab TO <fs_value>.

      IF sy-subrc NE 0.
        PERFORM add_msg TABLES ret USING 'E' 001  0 rule-ddtext ''.
        DELETE rule.
        CONTINUE.
      ENDIF.


      IF <fs_value> IS INITIAL AND rule-notnull = 'X'.
        PERFORM add_msg TABLES ret USING 'E' 002 tabix rule-ddtext ''.
        CONTINUE.
      ENDIF.

      IF rule-rollname IS NOT INITIAL.
        READ TABLE lt_doma_value WITH KEY rollname = rule-rollname
                                          domval = <fs_value> BINARY SEARCH.
        IF sy-subrc NE 0.
          PERFORM add_msg TABLES ret USING 'E' 003  tabix rule-fieldname <fs_value>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDLOOP.
ENDFUNCTION.


FORM add_msg TABLES ret STRUCTURE bapiret2
  USING type number row fieldname value .

  ret-type = type.
  ret-number = number.
  ret-id = 'ZAFO'.
  ret-row = row.
  ret-field = fieldname.
  ret-message_v1 = row.
  ret-message_v2 = fieldname.
  ret-message_v3 = value.
  CONDENSE ret-message_v1 NO-GAPS.
  MESSAGE ID ret-id  TYPE ret-type  NUMBER ret-number
        INTO ret-message
        WITH ret-message_v1 ret-message_v2.
  APPEND ret.
  CLEAR ret.

ENDFORM.
