FUNCTION zmda_get_partner.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_PARTNER) TYPE  BU_PARTNER OPTIONAL
*"  EXPORTING
*"     VALUE(E_SCMT0010) TYPE  ZSCMT0010
*"  EXCEPTIONS
*"      NOTFIND
*"----------------------------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_partner
    IMPORTING
      output = i_partner.


  SELECT SINGLE * INTO e_scmt0010 FROM zscmt0010
    WHERE partner = i_partner.
  IF sy-subrc NE 0.
    RAISE nofind.
  ENDIF.



ENDFUNCTION.
