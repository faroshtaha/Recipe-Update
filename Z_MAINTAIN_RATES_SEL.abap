*&---------------------------------------------------------------------*
*&  Include           Z_MAINTAIN_RATES_SEL
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_plnty TYPE plnty OBLIGATORY.
SELECT-OPTIONS: so_matnr FOR v_matnr,
                so_werks FOR v_werks OBLIGATORY,
                so_plnnr FOR v_plnnr,
                so_plnal FOR v_plnal,
                so_ktsch FOR v_ktsch OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_aennr TYPE rc271-aennr OBLIGATORY,
            p_sttag TYPE sttag,
            p_revlv TYPE revlv.
SELECTION-SCREEN END OF BLOCK b2.
