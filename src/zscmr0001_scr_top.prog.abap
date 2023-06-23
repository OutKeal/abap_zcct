*&---------------------------------------------------------------------*
*& Include ZSCMR0001_SCR_TOP
*&---------------------------------------------------------------------*

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAG9000'
CONSTANTS: BEGIN OF c_tag9000,
             tab1 LIKE sy-ucomm VALUE 'TAG9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAG9000_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAG9000_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAG9000_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAG9000_FC5',
             tab6 LIKE sy-ucomm VALUE 'TAG9000_FC6',
           END OF c_tag9000.
*&SPWIZARD: DATA FOR TABSTRIP 'TAG9000'
CONTROLS:  tag9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tag9000,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZSCMR0001',
             pressed_tab LIKE sy-ucomm VALUE c_tag9000-tab1,
           END OF g_tag9000.
