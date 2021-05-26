*&---------------------------------------------------------------------*
*& Report  Z_MAINTAIN_RATES
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Description     : The purpose of this program is to update Std.Rate 1*
*                   of phases in recipes in C202                       *
*----------------------------------------------------------------------*
* Modification History:                                                *
* =====================                                                *
* Date       User         Transport   RICEF#   Description             *
* ====       ====         =========   ======   =============           *
*----------------------------------------------------------------------*

REPORT zpts_02784_maint_omp_rates.

INCLUDE Z_MAINTAIN_RATES_top.
INCLUDE Z_MAINTAIN_RATES_sel.
INCLUDE Z_MAINTAIN_RATES_f01.

INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'ZREC_PH'
    ID 'ACTVT' FIELD '02'. "Change
  IF sy-subrc NE 0.
    MESSAGE e001(00) WITH 'No Authorization'.   " E  " No Handling Unit archived
    LEAVE LIST-PROCESSING.
  ENDIF.
  
AT SELECTION-SCREEN.
*--------------------------------------------------------------------*
*  Below select query code is to mimic C202 functionality wherein
*  if the key date does not match with the key date in Change Number,
*  a warning is displayed indicating that the Key Date in the
*  selection screen has been replaced with the one in the Change
*  Number
*--------------------------------------------------------------------*
*  Begin of Change number Key Date repace logic
*--------------------------------------------------------------------*
  SELECT SINGLE datuv
    FROM aenr
    INTO v_sttag
      WHERE aennr EQ p_aennr.
  IF v_sttag IS NOT INITIAL.
    IF v_sttag NE p_sttag.
      MESSAGE i108(cp).
    ENDIF.
    p_sttag = v_sttag.
  ELSE.
    MESSAGE e005(cc) WITH p_aennr.
  ENDIF.
*--------------------------------------------------------------------*
*  End of Change number Key Date repace logic
*--------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM f_get_data.

END-OF-SELECTION.
  IF t_output IS INITIAL.
    MESSAGE i001(00) WITH 'No recipe in selected set' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM f_alv.
  ENDIF.
