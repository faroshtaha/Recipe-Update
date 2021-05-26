FUNCTION ZRECIPE_READ.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_PLNTY) TYPE  PLNTY
*"     REFERENCE(IV_AENNR) TYPE  AENNR
*"     REFERENCE(IV_PLNAL) TYPE  PLNAL
*"     REFERENCE(IV_PLNNR) TYPE  PLNNR
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D
*"     REFERENCE(IV_REVLV) TYPE  REVLV
*"  EXPORTING
*"     REFERENCE(ET_PHASE) TYPE  MRTRSTY_PLPO_PH
*"  EXCEPTIONS
*"      NOT_FOUND
*"      KEY_NOT_QUALIFIED
*"      KEY_INVALID
*"      KEY_NUMBER_INVALID
*"      REC_NOT_FOUND
*"      EXISTING
*"      INTERNAL_ERROR
*"      FOREIGN_LOCK
*"      DATA_ERROR
*"      SAVE_ERROR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Description     : The purpose of this FM is to update Phases in a    *
*                   recipe. Currently the FM is only capable of        *
*                   changing Std Value 1 (VGW01), but the FM can be    *
*                   enhanced in future to change more fields.          *
*
*                   How to use this FM:                                *
*                   1) Specify the phase that you want to change in    *
*                      table IT_NEW_PHASE, along with the field that   *
*                      you want to change. For example VGW02           *
*                   2) Set the corresponding update field VGW02_X with *
*                      'X', so that only VGW02 will be updated         *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

*--------------------------------------------------------------------*
*                 !Begin of Data Declaration!
*--------------------------------------------------------------------*
  DATA: lt_mkal         TYPE mrtrsty_mkal,
      lt_operation    TYPE mrtrsty_plpo_opr,
      lt_phase        TYPE mrtrsty_plpo_ph,
      lt_new_phase    TYPE mrtrsty_plpo_ph,
      lt_relation     TYPE mrtrsty_plab,
      lt_sec_resource TYPE mrtrsty_plpo_sres,
      lt_mapl         TYPE mrtrsty_mapl,
      lt_plmz         TYPE mrtrsty_plmz,
      lt_plft         TYPE mrtrsty_plft,
      lt_plfv         TYPE mrtrsty_plfv,
      lt_plmk         TYPE mrtrsty_plmk,
      lt_plmw         TYPE mrtrsty_plmw,
      lt_resclas      TYPE mrtrsty_resclas,
      lt_ltext        TYPE mrtrsty_ltxt.

  DATA: lw_tca01     TYPE tca01,
        lw_tca05     TYPE tca05,
        lw_tca09     TYPE tca09,
        lw_tca11     TYPE tca11,
        lw_t430d     TYPE t430d,
        lw_rc271     TYPE rc271,
        lw_rc27m     TYPE rc27m,
        lw_rc27s     TYPE rc27s,
        lw_xs_rc271  TYPE rc271,
        lw_xs_rc27m  TYPE rc27m,
        lw_xs_rc27s  TYPE rc27s,
        lw_rc27i     TYPE rc27i,
        lw_plkob     TYPE plkob,
        lw_mkal      TYPE mkal.

  DATA: lv_datuv TYPE cp_datub_s.

  CALL FUNCTION 'MRTRS300_MASTER_RECIPE_INIT'
    EXPORTING
      i_plnty   = iv_plnty
    IMPORTING
      es_tca01  = lw_tca01
      es_tca05  = lw_tca05
      es_tca09  = lw_tca09
      es_tca11  = lw_tca11
      es_t430d  = lw_t430d
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING not_found.
  ENDIF.

  SELECT SINGLE datuv FROM aenr
    INTO lv_datuv WHERE aennr EQ iv_aennr.

  lw_rc271-aennr = iv_aennr.
  lw_rc271-revlv = iv_revlv.
  lw_rc271-datuv = lv_datuv.
  lw_rc271-datub = lv_datuv.
  lw_rc271-plnal = iv_plnal.
  lw_rc271-plnty = iv_plnty.
  lw_rc271-plnnr = iv_plnnr.
  lw_rc271-werks = iv_werks.
  lw_rc271-sttag = lv_datuv.

  lw_rc27s-aktyp = 'A'. "for Change mode
  lw_rc27s-aktyp_pic = 'A'. "for Change mode
  lw_rc27s-bldgr = 'C2'.
  lw_rc27s-panel = '4000'.
  lw_rc27s-kopgr = '0006'.
  lw_rc27s-mpool = 'SAPLCPDI'.
  lw_rc27s-ok_code = 'XALU'.
  lw_rc27s-spras = sy-langu.
  lw_rc27s-aennr = lw_rc271-aennr.
  lw_rc27s-sttag = lv_datuv.
  lw_rc27s-flg_clas = abap_true.

*--------------------------------------------------------------------*
*  The reason for below FM CM_FV_MKAL_BT_INITIALIZE is that the
*  MKAL internal table of the Recipe Update FMs do not get cleared.
*  As a result, previous recipes MKAL stays and that gets passed on
*  to current recipe during current recipe's update, which leads to
*  SM13 failure. This FM CM_FV_MKAL_BT_INITIALIZE clears those
*  internal tables
*--------------------------------------------------------------------*
  CALL FUNCTION 'CM_FV_MKAL_BT_INITIALIZE'.
  CALL FUNCTION 'MRTRS300_MASTER_RECIPE_READ'
    EXPORTING
      is_rc271           = lw_rc271
      is_rc27m           = lw_rc27m
      is_rc27s           = lw_rc27s
    IMPORTING
      xs_rc271           = lw_xs_rc271
      xs_rc27m           = lw_xs_rc27m
      xs_rc27s           = lw_xs_rc27s
      es_rc27i           = lw_rc27i
      es_plkob           = lw_plkob
      et_mkal            = lt_mkal
      et_operation       = lt_operation
      et_phase           = lt_phase
      et_relation        = lt_relation
      et_sec_resource    = lt_sec_resource
      et_mapl            = lt_mapl
      et_plmz            = lt_plmz
      et_plft            = lt_plft
      et_plfv            = lt_plfv
      et_plmk            = lt_plmk
      et_plmw            = lt_plmw
      et_resclas         = lt_resclas
      et_ltext           = lt_ltext
    EXCEPTIONS
      key_not_qualified  = 1
      key_invalid        = 2
      key_number_invalid = 3
      not_found          = 4
      existing           = 5
      internal_error     = 6
      foreign_lock       = 7
      OTHERS             = 8.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING key_not_qualified.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING key_invalid.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING key_number_invalid.
      WHEN 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING rec_not_found.
      WHEN 5.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING existing.
      WHEN 6.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING internal_error.
      WHEN 7.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING foreign_lock.
      WHEN 3.
    ENDCASE.
  ENDIF.

  et_phase[] = lt_phase[].

ENDFUNCTION.
