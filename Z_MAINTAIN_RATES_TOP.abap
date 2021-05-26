*&---------------------------------------------------------------------*
*&  Include           Z_MAINTAIN_RATES_TOP
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
TYPE-POOLS slis . "TO USE FIELD CATALOG WE HAVE TO INCLUDE SLIS TYPE-POOLS

TYPES: BEGIN OF type_zbc_const,
         progname TYPE zbc_const-progname,
         fieldname TYPE zbc_const-fieldname,
         low TYPE t435t-vlsch,
       END OF type_zbc_const,
       BEGIN OF type_ktsch_f4,
         vlsch TYPE t435-vlsch,
         txt   TYPE vlsch_txt,
       END OF type_ktsch_f4,
       BEGIN OF type_mapl,
         matnr TYPE mapl-matnr,
         werks TYPE mapl-werks,
         plnty TYPE mapl-plnty,
         plnnr TYPE mapl-plnnr,
         plnal TYPE mapl-plnal,
       END OF type_mapl,
       BEGIN OF type_plas,
         plnty TYPE plas-plnty,
         plnnr TYPE plas-plnnr,
         plnal TYPE plas-plnal,
         plnfl TYPE plas-plnfl,
         plnkn TYPE plas-plnkn,
       END OF type_plas,
       BEGIN OF type_plpo,
         plnty TYPE plpo-plnty,
         plnnr TYPE plpo-plnnr,
         plnkn TYPE plpo-plnkn,
         zaehl TYPE plpo-zaehl,
         datuv TYPE plpo-datuv,
         aennr TYPE plpo-aennr,
         vornr TYPE plpo-vornr,
         steus TYPE plpo-steus,
         werks TYPE plpo-werks,
         ktsch TYPE plpo-ktsch,
         vgw01 TYPE plpo-vgw01,
       END OF type_plpo,
       BEGIN OF type_output,
         matnr TYPE matnr,
         plnnr TYPE plpo-plnnr,
         plnal TYPE plko-plnal,
         ktsch TYPE plpo-ktsch,
         vgw01 TYPE plpo-vgw01,
         vornr TYPE plpo-vornr,
         werks TYPE plpo-werks,
       END OF type_output.

TYPES: type_output_tt TYPE STANDARD TABLE OF type_output.

DATA: t_zbc_const_ktsch TYPE STANDARD TABLE OF type_zbc_const,
      t_ktsch_f4        TYPE STANDARD TABLE OF type_ktsch_f4,
      t_ktsch_return    TYPE STANDARD TABLE OF DDSHRETVAL,
      t_mapl            TYPE STANDARD TABLE OF type_mapl,
      t_plas            TYPE STANDARD TABLE OF type_plas,
      t_plpo            TYPE STANDARD TABLE OF type_plpo,
      t_output          TYPE STANDARD TABLE OF type_output,
      t_temp_output     TYPE STANDARD TABLE OF type_output,
      t_bdcdata         TYPE STANDARD TABLE OF bdcdata,
      t_messtab         TYPE STANDARD TABLE OF bdcmsgcoll.

DATA: v_matnr TYPE matnr,
      v_werks TYPE werks_d,
      v_plnnr TYPE rc271-plnnr,
      v_plnal TYPE rc271-plnal,
      v_ktsch TYPE ktsch,
      v_sttag TYPE plpo-datuv.

DATA : t_fcat TYPE slis_t_fieldcat_alv . "INTERNAL TABLE FOR FIELD CATALOG
