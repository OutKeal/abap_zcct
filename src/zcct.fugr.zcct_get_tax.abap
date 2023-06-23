FUNCTION zcct_get_tax.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_TAX_CODE) TYPE  MWSKZ OPTIONAL
*"     VALUE(I_TAX_TYPE) TYPE  ZCCT_TAX_DEF OPTIONAL
*"     VALUE(I_TXT_RATE) TYPE  ZCCT_TAX_RATE OPTIONAL
*"  EXPORTING
*"     VALUE(E_TAX_CODE) TYPE  MWSKZ
*"     VALUE(E_TAX_RATE) TYPE  ZCCT_TAX_RATE
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_tax TYPE zcct_tax.
  IF i_tax_code IS NOT INITIAL.
    SELECT SINGLE * INTO ls_tax FROM zcct_tax
      WHERE mwskz = i_tax_code.
    IF sy-subrc NE 0.
      RAISE error.

    ELSE.
      e_tax_code = ls_tax-mwskz.
      e_tax_rate = ls_tax-tax_rate.
      RETURN.
    ENDIF.
  ENDIF.


  IF i_txt_rate IS NOT INITIAL.
    SELECT SINGLE * INTO ls_tax FROM zcct_tax
      WHERE tax_rate = i_txt_rate.
    IF sy-subrc NE 0.
      RAISE error.

    ELSE.
      e_tax_code = ls_tax-mwskz.
      e_tax_rate = ls_tax-tax_rate.
      RETURN.
    ENDIF.
  ENDIF.

  IF i_tax_type IS NOT INITIAL.
    SELECT SINGLE * INTO ls_tax FROM zcct_tax
      WHERE  def = i_tax_type.
    IF sy-subrc NE 0.
      RAISE error.

    ELSE.
      e_tax_code = ls_tax-mwskz.
      e_tax_rate = ls_tax-tax_rate.
      RETURN.
    ENDIF.
  ENDIF.


ENDFUNCTION.
