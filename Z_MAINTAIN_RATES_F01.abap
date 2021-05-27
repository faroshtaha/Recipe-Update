*&---------------------------------------------------------------------*
*&  Include           ZPTS_02784_MAINT_OMP_RATES_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program         : ZPTS_02784_MAINT_OMP_RATES_F01                     *
* Title           : Mass Update Master Recipe Rates                    *
* Creation Date   : 26-May-2021                                        *
* Developer       : EYD9748                                            *
* SAP Release     : SAP ECC 6.0                                        *
* Transport No.   : EE1K972672                                         *
*----------------------------------------------------------------------*
* Description     : The purpose of this program is to update Std.Rate 1*
*                   of phases in recipes in C202                       *
*----------------------------------------------------------------------*
* Modification History:                                                *
* =====================                                                *
* Date       User         Transport   RICEF#   Description             *
* ====       ====         =========   ======   =============           *
* 26/05/2021 EYD9748      EE1K972672  02784   Initial Development      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .
  DATA: lt_phase TYPE mrtrsty_plpo_ph.
  DATA: lw_mapl     LIKE LINE OF t_mapl,
        lw_output   TYPE type_output,
        lw_phase    LIKE LINE OF lt_phase,
        lw_r_ktsch  LIKE LINE OF r_ktsch,
        lw_ktsch_f4 LIKE LINE OF t_ktsch_f4.

  CLEAR: r_ktsch, lw_r_ktsch.
  lw_r_ktsch-sign = 'I'.
  lw_r_ktsch-option = 'EQ'.
  LOOP AT t_ktsch_f4 INTO lw_ktsch_f4 WHERE vlsch IN so_ktsch.
    MOVE: lw_ktsch_f4-vlsch TO lw_r_ktsch-low.
    APPEND lw_r_ktsch TO r_ktsch.
  ENDLOOP.

*--------------------------------------------------------------------*
*  Get MAPL data
*--------------------------------------------------------------------*
  SELECT  matnr
          werks
          plnty
          plnnr
          plnal
          FROM mapl
          INTO TABLE t_mapl
            WHERE matnr IN so_matnr
              AND werks IN so_werks
              AND plnty EQ p_plnty
              AND plnnr IN so_plnnr
              AND plnal IN so_plnal
              AND loekz EQ abap_false. "Don't select undeleted (?)
  IF sy-subrc IS INITIAL.
    SORT t_mapl BY matnr werks plnty plnnr plnal.
    DELETE ADJACENT DUPLICATES FROM t_mapl COMPARING ALL FIELDS.
    LOOP AT t_mapl INTO lw_mapl.
      CLEAR: lt_phase, lw_phase, lw_output.
      CALL FUNCTION 'ZPTS_02784_READ_RECIPE'
        EXPORTING
          iv_plnty           = lw_mapl-plnty
          iv_aennr           = p_aennr
          iv_revlv           = p_revlv
          iv_plnal           = lw_mapl-plnal
          iv_plnnr           = lw_mapl-plnnr
          iv_werks           = lw_mapl-werks
        IMPORTING
          et_phase           = lt_phase
        EXCEPTIONS
          not_found          = 1
          key_not_qualified  = 2
          key_invalid        = 3
          key_number_invalid = 4
          rec_not_found      = 5
          existing           = 6
          internal_error     = 7
          foreign_lock       = 8
          data_error         = 9
          save_error         = 10
          OTHERS             = 11.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_phase INTO lw_phase WHERE ktsch IN r_ktsch.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lw_mapl-matnr
            IMPORTING
              output = lw_output-matnr.
          lw_output-plnnr = lw_mapl-plnnr.  "Recipe Group
          lw_output-plnal = lw_mapl-plnal.  "Recipe
          lw_output-werks = lw_mapl-werks.  "Plant
          lw_output-ktsch = lw_phase-ktsch. "Std Text Key
          lw_output-ltxa1 = lw_phase-ltxa1. "Std Text Key Description
          lw_output-vgw01 = lw_phase-vgw01. "Std Rate
          lw_output-vornr = lw_phase-vornr. "Operation
          lw_output-pvznr = lw_phase-pvznr. "Superordinate Operation
          APPEND lw_output TO t_output.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .
  PERFORM f_prepare_fcat.
  PERFORM f_display_alv.
ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_FCAT
*&---------------------------------------------------------------------*
FORM f_prepare_fcat .
  DATA: lw_fcat LIKE LINE OF t_fcat,
        lv_col_pos TYPE sy-cucol VALUE '1'.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'MATNR' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'MAPL'.
  lw_fcat-ref_fieldname = 'MATNR'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'PLNNR' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-seltext_m = 'Recipe Group' . "Specify text to display column header
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'PLNAL' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-seltext_m = 'Recipe' . "Specify text to display column header
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'PVZNR' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'MRTRSS_PLPO_PH'.
  lw_fcat-ref_fieldname = 'PVZNR'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'VORNR' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'PLPO'.
  lw_fcat-ref_fieldname = 'VORNR'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'KTSCH' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'PLPO'.
  lw_fcat-ref_fieldname = 'KTSCH'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'LTXA1' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'PLPO'.
  lw_fcat-ref_fieldname = 'LTXA1'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.

  lw_fcat-col_pos = lv_col_pos . "Specify position of a field
  lw_fcat-fieldname = 'VGW01' . "Specify field name
  lw_fcat-tabname = 'T_OUTPUT' . "Specify internal table name
  lw_fcat-ref_tabname = 'PLPO' . "Specify Reference Table name
  lw_fcat-ref_fieldname = 'VGW01' . "Specify Reference field name
*  lw_fcat-checkbox = 'X'.
  lw_fcat-edit = 'X'.
  APPEND lw_fcat TO t_fcat . "Append to field catalog internal table
  CLEAR lw_fcat.
  lv_col_pos = lv_col_pos + 1.
ENDFORM.                    " F_PREPARE_FCAT
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_alv .
  DATA: lw_layout TYPE slis_layout_alv.
  lw_layout-colwidth_optimize = 'X'.
  lw_layout-zebra = 'X'.
  t_temp_output[] = t_output[].
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = lw_layout
      it_fieldcat             = t_fcat
      i_callback_user_command = 'F_USER_COMMAND'
      i_callback_top_of_page  = 'F_TOP_OF_PAGE'
    TABLES
      t_outtab                = t_output.
ENDFORM.                    " F_DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_user_command USING p_ucomm LIKE sy-ucomm
                          p_selfield TYPE slis_selfield.
  DATA: lt_update_output TYPE STANDARD TABLE OF type_output.
  DATA: lw_temp_output TYPE type_output,
        lw_output TYPE type_output.
  DATA: lv_counter TYPE char3,
        lv_message TYPE string,
        lv_old_plnnr TYPE plpo-plnnr,
        lv_old_plnal TYPE plas-plnal,
        lv_old_werks TYPE plpo-werks,
        lv_old_matnr TYPE mapl-matnr.
  IF sy-ucomm EQ '&DATA_SAVE'.
    lv_counter = '0'.

    READ TABLE t_temp_output INTO lw_temp_output INDEX 1.
    IF sy-subrc IS INITIAL.
      lv_old_matnr = lw_temp_output-matnr.
      lv_old_plnnr = lw_temp_output-plnnr.
      lv_old_plnal = lw_temp_output-plnal.
      lv_old_werks = lw_temp_output-werks.
    ENDIF.

    LOOP AT t_temp_output INTO lw_temp_output.
      IF lw_temp_output-matnr NE lv_old_matnr OR
         lw_temp_output-plnnr NE lv_old_plnnr OR
         lw_temp_output-plnal NE lv_old_plnal OR
         lw_temp_output-werks NE lv_old_werks.
        lv_old_matnr = lw_temp_output-matnr.
        lv_old_plnnr = lw_temp_output-plnnr.
        lv_old_plnal = lw_temp_output-plnal.
        lv_old_werks = lw_temp_output-werks.
        IF lt_update_output IS NOT INITIAL.
          PERFORM f_update_recipe USING lt_update_output.
          CLEAR: lt_update_output.
        ENDIF.
      ENDIF.
      READ TABLE t_output INTO lw_output WITH KEY matnr = lw_temp_output-matnr
                                                  plnnr = lw_temp_output-plnnr
                                                  plnal = lw_temp_output-plnal
                                                  werks = lw_temp_output-werks
                                                  ktsch = lw_temp_output-ktsch.
      IF sy-subrc IS INITIAL.
        IF lw_output NE lw_temp_output.
          lv_counter = lv_counter + 1.
          APPEND lw_output TO lt_update_output.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lt_update_output IS NOT INITIAL.
      PERFORM f_update_recipe USING lt_update_output.
      CLEAR: lt_update_output.
    ENDIF.

    CONCATENATE lv_counter 'Record(s) changed'
           INTO lv_message SEPARATED BY space.

    MESSAGE s001(00) WITH lv_message.
    t_temp_output = t_output.
  ELSEIF sy-ucomm EQ '&REFRESH'.
    p_selfield-refresh = 'X'.
  ENDIF.
ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_RECIPE
*&---------------------------------------------------------------------*
*FORM f_update_recipe USING uw_output TYPE type_output.
FORM f_update_recipe USING ut_output TYPE type_output_tt.
  DATA: lt_new_phase TYPE zptp_t_02784_phase,
        lw_new_phase LIKE LINE OF lt_new_phase,
        lw_output LIKE LINE OF ut_output.
  CLEAR: lw_new_phase, lt_new_phase.
  LOOP AT ut_output INTO lw_output.
    lw_new_phase-vornr = lw_output-vornr.
    lw_new_phase-vgw01 = lw_output-vgw01.
    lw_new_phase-vgw01_x = abap_true.
    APPEND lw_new_phase TO lt_new_phase.
  ENDLOOP.

  CALL FUNCTION 'ZPTS_02784_RECIPE_PHASE_UPD'
    EXPORTING
      iv_plnty     = p_plnty
      iv_aennr     = p_aennr
      iv_revlv     = p_revlv
      iv_plnal     = lw_output-plnal
      iv_plnnr     = lw_output-plnnr
      iv_werks     = lw_output-werks
      iv_matnr     = lw_output-matnr
      it_new_phase = lt_new_phase.
  IF sy-subrc IS INITIAL.
  ENDIF.

ENDFORM.                    " F_UPDATE_RECIPE
*&---------------------------------------------------------------------*
*&      Form  f_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_top_of_page.
  DATA: lt_header     TYPE slis_t_listheader,
        lt_return     TYPE STANDARD TABLE OF bapiret2,
        lw_address    TYPE bapiaddr3,
        lw_header     TYPE slis_listheader,
        lv_line       LIKE lw_header-info,
        lv_lines      TYPE i,
        lv_linesc(10) TYPE c.

* Title
  lw_header-typ  = 'H'.
  lw_header-info = 'Mass Update Master Recipe Rates'.
  APPEND lw_header TO lt_header.
  CLEAR lw_header.

* User
  lw_header-typ  = 'S'.
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = sy-uname
    IMPORTING
      address  = lw_address
    TABLES
      return   = lt_return.

  lw_header-key = 'User: '.
  CONCATENATE  lw_address-fullname ' ' INTO lw_header-info.   "Logged in user
  APPEND lw_header TO lt_header.
  CLEAR lw_header.

* Total No. of Records Selected
  DESCRIBE TABLE  t_output LINES lv_lines.
  lv_linesc = lv_lines.
  CONCATENATE 'Total No. of Records Selected: ' lv_linesc
                    INTO lv_line SEPARATED BY space.
  lw_header-typ  = 'A'.
  lw_header-info = lv_line.
  APPEND lw_header TO lt_header.
  CLEAR lw_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
ENDFORM.                    "f_top_of_page
*&---------------------------------------------------------------------*
*&      Form  F_KTSCH_F4
*&---------------------------------------------------------------------*
FORM f_ktsch_f4 .
  DATA: lw_ktsch_return LIKE LINE OF t_ktsch_return.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VLSCH'
      value_org       = 'S'
    TABLES
      value_tab       = t_ktsch_f4
      return_tab      = t_ktsch_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc IS INITIAL.
    READ TABLE t_ktsch_return INTO lw_ktsch_return INDEX 1.
    IF sy-subrc IS INITIAL.
      so_ktsch-low = lw_ktsch_return-fieldval.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_KTSCH_F4
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_KTSCH
*&---------------------------------------------------------------------*
FORM f_validate_ktsch .
  DATA: lt_ktsch_zbc TYPE STANDARD TABLE OF zbc_const-low.
  SELECT  low
    FROM zbc_const
    INTO TABLE lt_ktsch_zbc
      WHERE progname EQ 'ZPTS_02784_MAINT_OMP_RATES'
        AND fieldname EQ 'ZOMP'
        AND low IN so_ktsch.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE e001(00) WITH 'Invalid Standard Text Key'.
  ENDIF.
ENDFORM.                    " F_VALIDATE_KTSCH
*&---------------------------------------------------------------------*
*&      Form  F_INIT
*&---------------------------------------------------------------------*
FORM f_init .
*--------------------------------------------------------------------*
*  Below select queries are to initialize Std Text Key internal tables
*  which will be used for F4. Only values maintained in ZBC_CONST
*  ZPTS_02784_MAINT_OMP_RATES/ZOMP are allowed.
*--------------------------------------------------------------------*
  SELECT  progname
          fieldname
          low
          FROM zbc_const
          INTO TABLE t_zbc_const_ktsch
            WHERE progname  EQ 'ZPTS_02784_MAINT_OMP_RATES'
              AND fieldname EQ 'ZOMP'.
  IF sy-subrc IS INITIAL.
*--------------------------------------------------------------------*
*  This is to get description of the Std Text Key
*--------------------------------------------------------------------*
    SELECT  vlsch
            txt
            FROM t435t
            INTO TABLE t_ktsch_f4
            FOR ALL ENTRIES IN t_zbc_const_ktsch
              WHERE vlsch EQ t_zbc_const_ktsch-low.
  ENDIF.
ENDFORM.                    " F_INIT
