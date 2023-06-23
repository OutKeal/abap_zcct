FUNCTION zcct_split_amount.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_SA_RULE STRUCTURE  ZCCT_SA_RULE
*"      IT_SA_AMOUNT STRUCTURE  ZCCT_SA_AMOUNT
*"      CT_TAB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_error TYPE char1.
  DATA: lt_sum_amount TYPE TABLE OF  zcct_sa_amount WITH HEADER LINE.
  DATA: lt_sum_amount_l TYPE TABLE OF  zcct_sa_amount WITH HEADER LINE.
  DATA: lt_sum_amount_c TYPE TABLE OF  zcct_sa_amount WITH HEADER LINE.
  DATA: lv_key_value TYPE key_value.
  DATA: lv_amount_value TYPE dmbtr.
  DATA: lv_fr_value TYPE dmbtr.


  DELETE ADJACENT DUPLICATES FROM it_sa_rule COMPARING to_fname.

  FIELD-SYMBOLS: <to_value> TYPE any.

  LOOP AT ct_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
    LOOP AT it_sa_rule.
      CLEAR lt_sum_amount.

      lt_sum_amount-to_fname = it_sa_rule-to_fname.

      PERFORM get_value USING it_sa_rule-key_fname <fs_tab> CHANGING lt_sum_amount-key_value lv_error.
      IF lv_error = 'X'.
        RAISE error.
      ENDIF.

      PERFORM get_value USING it_sa_rule-fr_fname <fs_tab> CHANGING lt_sum_amount-amount lv_error.
      IF lv_error = 'X'.
        RAISE error.
      ENDIF.

      COLLECT lt_sum_amount.
    ENDLOOP.
  ENDLOOP.

  CLEAR lt_sum_amount_l[].

  LOOP AT ct_tab ASSIGNING <fs_tab>.
    LOOP AT it_sa_rule.
      CLEAR lt_sum_amount_l.
      CLEAR lv_key_value.
      CLEAR lv_fr_value.

      PERFORM get_value USING it_sa_rule-key_fname <fs_tab> CHANGING lv_key_value lv_error.
      IF lv_error = 'X'.
        RAISE error.
      ENDIF.

      PERFORM get_value USING it_sa_rule-fr_fname <fs_tab> CHANGING lv_fr_value lv_error.
      IF lv_error = 'X'.
        RAISE error.
      ENDIF.

      ASSIGN COMPONENT it_sa_rule-to_fname OF STRUCTURE <fs_tab> TO <to_value> .
      IF sy-subrc NE 0.
        RAISE error.
      ENDIF.

      READ TABLE it_sa_amount WITH KEY to_fname = it_sa_rule-to_fname key_value = lv_key_value.
      IF sy-subrc EQ 0.
        READ TABLE lt_sum_amount WITH KEY to_fname = it_sa_rule-to_fname key_value = lv_key_value.
        IF sy-subrc EQ 0  AND lt_sum_amount-amount <> 0.
          <to_value> = lv_fr_value * it_sa_amount-amount / lt_sum_amount-amount  .
          lt_sum_amount_l-to_fname = it_sa_rule-to_fname.
          lt_sum_amount_l-key_value = lv_key_value.
          lt_sum_amount_l-amount = <to_value>.
          COLLECT lt_sum_amount_l.
          CLEAR lt_sum_amount_l.
          CLEAR lt_sum_amount.
        ENDIF.
        CLEAR it_sa_amount.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  CLEAR lt_sum_amount_c[].
  LOOP AT it_sa_amount.
    READ TABLE lt_sum_amount_l WITH KEY to_fname = it_sa_amount-to_fname
                                        key_value = it_sa_amount-key_value.
    IF sy-subrc EQ 0.
      MOVE it_sa_amount TO lt_sum_amount_c.
      lt_sum_amount_c-amount = it_sa_amount-amount - lt_sum_amount_l-amount.
      IF lt_sum_amount_c-amount <> 0.
        APPEND lt_sum_amount_c.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_sum_amount_c[] IS NOT INITIAL.
    LOOP AT it_sa_rule.
      LOOP AT lt_sum_amount_c WHERE to_fname = it_sa_rule-to_fname.
        LOOP AT ct_tab ASSIGNING <fs_tab>.
          PERFORM get_value USING it_sa_rule-key_fname <fs_tab> CHANGING lv_key_value lv_error.
          IF lv_error = 'X'.
            RAISE error.
          ENDIF.

          IF lv_key_value <> lt_sum_amount_c-key_value.
            CONTINUE.
          ENDIF.

          ASSIGN COMPONENT it_sa_rule-to_fname OF STRUCTURE <fs_tab> TO <to_value> .
          IF sy-subrc NE 0.
            RAISE error.
          ENDIF.

          <to_value> = <to_value> + lt_sum_amount_c-amount.
          EXIT.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.




ENDFUNCTION.

FORM get_value USING fname line
                CHANGING value error.
  FIELD-SYMBOLS: <fs_value> TYPE any.
  ASSIGN COMPONENT fname OF STRUCTURE line TO <fs_value> .
  IF sy-subrc EQ 0.
    value = <fs_value>.
  ELSE.
    error = 'X'.
  ENDIF.

ENDFORM.
