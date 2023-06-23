*----------------------------------------------------------------------*
*   INCLUDE /ATU/CA_POS_MONT01                                         *
*----------------------------------------------------------------------*

TYPE-POOLS wpusa.

TABLES: edidc,
        edids,
        help_info.

* structure for hide area in tree nodes
TYPES: BEGIN OF type_hide,
       area,
       section,
       filler(73).
TYPES: END OF type_hide.

* Global data to pass node information to screen 200
DATA: g_hide   TYPE type_hide,
      g_node   LIKE seucomm,
      g_linno  LIKE sy-linno,
      g_docnum LIKE edidc-docnum.

* table with nodes for partial  tree
DATA  BEGIN OF g_t_nodes OCCURS 1.
        INCLUDE STRUCTURE snodetext.
DATA  END OF g_t_nodes.


CLASS lcl_application DEFINITION DEFERRED.

TYPES:  item_table_type LIKE STANDARD TABLE OF ZCCT_CA_RET_IB_TREE
        WITH DEFAULT KEY.

DATA:   g_application      TYPE REF TO lcl_application,
        g_custom_container TYPE REF TO cl_gui_custom_container,
        g_tree             TYPE REF TO cl_gui_list_tree.

data:   g_t_cl_node  TYPE treev_ntab,
        g_t_cl_item  TYPE item_table_type.

* current level in tree
DATA  g_curr_level  TYPE i.


* constants for the tree display
CONSTANTS: col_background  TYPE i  VALUE '0',
           col_heading     TYPE i  VALUE '1',
           col_normal      TYPE i  VALUE '2',
           col_warning     TYPE i  VALUE '3',
           col_key         TYPE i  VALUE '4',
           col_positive    TYPE i  VALUE '5',
           col_negative    TYPE i  VALUE '6',
           col_group       TYPE i  VALUE '7',
           c_intensiv      TYPE c  VALUE '1'.

* constants for tree commands
CONSTANTS: c_command_select(4)        VALUE 'TRSI',
           c_command_expand(4)        VALUE 'TREP',
           c_command_position(4)      VALUE 'PLCR',
           c_command_store(4)         VALUE 'STOR',
           c_command_complete(4)      VALUE 'COMP',
           c_command_refresh(4)       VALUE 'REFR',
           c_command_doc_level(4)     VALUE 'DOCL',
           c_command_disp_seg(4)      VALUE 'DISE',
           c_command_proc_imm(4)      VALUE 'STPI',
*###EDITOR
           c_command_ib_edit(4)       VALUE 'IBED',
           c_command_reject(4)        VALUE 'STRJ',
           c_command_reverse(4)       VALUE 'STRV',
           c_command_process(4)       VALUE 'STPR',
           c_command_resubmit(4)      VALUE 'STRS',
           c_command_status_dialog(4) VALUE 'STDI'.

* constants for tree areas
CONSTANTS: c_area_inbound(1)       VALUE 'A',
           c_area_outbound(1)      VALUE 'B',
           c_area_outbound_comm(1) VALUE 'C',
           c_area_message(1)       VALUE 'D',
           c_area_inbound_comm(1)  VALUE 'E'.

* constants für tree sections
CONSTANTS: c_section_ok(1)         VALUE 'O',
           c_section_nok(1)        VALUE 'N'.


* constants for tree node types
CONSTANTS: c_node_root     LIKE snodetext-type  VALUE 'ROOT',
           c_node_area     LIKE snodetext-type  VALUE 'AREA',
           c_node_section  LIKE snodetext-type  VALUE 'SECT',
           c_node_store    LIKE snodetext-type  VALUE 'STOR',
           c_node_header   LIKE snodetext-type  VALUE 'HEAD',
           c_node_mestyp   LIKE snodetext-type  VALUE 'MSGT',
           c_node_extdoc   LIKE snodetext-type  VALUE 'EXTD',
           c_node_other    LIKE snodetext-type  VALUE 'OTHE',
           c_node_foldoc   LIKE snodetext-type  VALUE 'FOLD',
           c_node_error    LIKE snodetext-type  VALUE 'ERRO',
           c_node_workitem LIKE snodetext-type  VALUE 'WOIT'.
