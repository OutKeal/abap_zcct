*-------------------------------------------------------------------
***INCLUDE /ATU/CA_POS_MONF03.
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  TREE_BUILD
*&---------------------------------------------------------------------*
*       Build the initial table of nodes for the document flow tree
*----------------------------------------------------------------------*
FORM tree_build.


*  PERFORM outbound_build.

*  PERFORM outbound_comm_build.

*  PERFORM message_build.

* perform inbound_build.

  PERFORM ib_build.


ENDFORM.                               " TREE_BUILD


*&---------------------------------------------------------------------*
*&      Form  TREE_CONSTRUCT
*&---------------------------------------------------------------------*
*       Construct the tree from the node table
*----------------------------------------------------------------------*
FORM tree_construct
USING insert_id LIKE snode-id
      relationship.

  DATA: nr_nodes LIKE sy-index,
        l_node   LIKE snodetext.

* Check if the table is empty; if so, do not call RS_TREE_CONSTRUCT
  DESCRIBE TABLE g_t_nodes LINES nr_nodes.
  IF nr_nodes = 0.
    CALL FUNCTION 'RS_TREE_GET_NODE'
      EXPORTING
        node_id      = insert_id
      IMPORTING
        node_info    = l_node
      EXCEPTIONS
        id_not_found = 1
        OTHERS       = 2.
    IF sy-subrc = 0 AND NOT ( l_node-force_plus IS INITIAL ).
*     Node has no children. Clear '+'-icon of current node.
      CLEAR l_node-force_plus.
      CALL FUNCTION 'RS_TREE_SET_NODE'
        EXPORTING
          node_info    = l_node
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
    ENDIF.

    EXIT.
  ENDIF.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    EXPORTING
      insert_id    = insert_id
      relationship = relationship
    TABLES
      nodetab      = g_t_nodes.

  LOOP AT g_t_nodes.
*    MOVE-CORRESPONDING g_t_nodes to g_t_cl_node.
*    APPEND g_t_cl_node.
  ENDLOOP.
  REFRESH g_t_nodes.

ENDFORM.                               " TREE_CONSTRUCT


*&---------------------------------------------------------------------*
*&      Form  TREE_OUTPUT
*&---------------------------------------------------------------------*
*       Display the tree
*----------------------------------------------------------------------*
FORM tree_output.

  DATA: e_f15.

  SET PF-STATUS '100'.
  SET TITLEBAR  '100'.

  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program       = sy-repid
      callback_user_command  = 'TREE_COMMANDS'
      callback_text_display  = ''
      callback_color_display = ' '
      status                 = 'OWN'
      check_duplicate_name   = '1'
      color_of_link          = '1'
      color_of_node          = '4'
      lower_case_sensitive   = ' '
      modification_log       = ' '
      node_length            = 30
      text_length            = 75
      text_length1           = 0
      text_length2           = 0
      return_marked_subtree  = ' '
      screen_start_column    = 0
      screen_start_line      = 0
      screen_end_column      = 0
      screen_end_line        = 0
      suppress_node_output   = ' '
    IMPORTING
      f15                    = e_f15.

ENDFORM.                               " TREE_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  TREE_COMMANDS
*&---------------------------------------------------------------------*
*       tree commands exit routine (see documentation of
*       function module RS_TREE_LIST_DISPLAY)
*----------------------------------------------------------------------*
FORM tree_commands TABLES   node         STRUCTURE seucomm
                   USING    command      TYPE clike
                   CHANGING exit         TYPE clike
                            list_refresh TYPE clike.

  DATA: hide         TYPE type_hide,
        node_command TYPE c,      " X if command needs a node
        pe_lsind     LIKE sy-lsind.

  PERFORM tree_functions USING command CHANGING node_command.

* get node under cursor; if there's none, exit
  READ TABLE node INDEX 1.
  IF node-id IS INITIAL AND node_command = 'X'.
    MESSAGE e767.
    EXIT.
  ENDIF.

  g_curr_level = 0.
  hide = node-hide.

  CASE command.
    WHEN c_command_select.
      CASE node-selfield.
        WHEN 'PLUS'.
*         cursor is on a 'plus' icon

          IF node-leaf = 'X'.
*           cursor is on a leaf; create children first and expand node
            PERFORM tree_node_expand USING command hide node 99.
          ELSE.
*           simply expand node, show next level
            PERFORM tree_expand USING node-id.
          ENDIF.

        WHEN 'MINUS'.
*         compress current node
          PERFORM tree_compress USING node-id.

        WHEN OTHERS.
*         cursor is on a node field, execute node specific pick function
          PERFORM tree_node_pick USING command hide node.
      ENDCASE.

    WHEN c_command_expand.
*     completely expand partial tree under cursor
      PERFORM tree_node_expand USING command hide node 99.

    WHEN c_command_doc_level.
*     expand partial tree under cursor to document level
      PERFORM tree_node_expand USING command hide node 5.

    WHEN c_command_position.
*     move line with cursor to top of screen
      pe_lsind = sy-lsind - 1.
      SCROLL LIST TO FIRST PAGE INDEX pe_lsind LINE sy-lilli.
      SET CURSOR LINE sy-lilli OFFSET sy-cucol.
      list_refresh = ' '.

    WHEN OTHERS.
*     execute other functions
      PERFORM tree_node_command USING command hide node.
  ENDCASE.

ENDFORM.                               " TREE_COMMANDS


*&---------------------------------------------------------------------*
*&      Form  TREE_NODE_EXPAND
*&---------------------------------------------------------------------*
*       Expand current node
*----------------------------------------------------------------------*
FORM tree_node_expand
USING command TYPE clike
      hide TYPE type_hide
      node STRUCTURE seucomm
      max_level LIKE streenode-tlevel.

* table of selected nodes
  DATA: BEGIN OF t_sel_node OCCURS 1.
          INCLUDE STRUCTURE snodetext.
  DATA: END OF t_sel_node.
  DATA: nr_nodes_old   LIKE sy-tabix,
        nr_nodes_new   LIKE sy-tabix,
        nr_nodes_first LIKE sy-tabix,
        node_info      LIKE snodetext.

* Get current number of nodes in tree
  PERFORM tree_list TABLES t_sel_node USING node-id.
  DESCRIBE TABLE t_sel_node LINES nr_nodes_first.

  DO.
*   Some nodes may have leafs as children that can be expanded further.
*   So, all nodes of the partial tree must be  if they have to get
*   expanded
    LOOP AT t_sel_node WHERE tlevel <= max_level.
*     Expand tree down to the maximum level
      hide = t_sel_node-hide.

      IF t_sel_node-leaf = 'X' AND t_sel_node-force_plus = 'X'.
        CASE hide-area.
          WHEN c_area_inbound.
            PERFORM ib_expand_node USING command t_sel_node.
          WHEN c_area_outbound.
            PERFORM ob_expand_node USING command t_sel_node.
          WHEN c_area_outbound_comm.
            PERFORM obc_expand_node USING command t_sel_node.
          WHEN c_area_message.
            PERFORM me_expand_node USING command t_sel_node.
        ENDCASE.

*       insert the new partial tree
        PERFORM tree_construct USING t_sel_node-id 'CHILD'.
      ENDIF.

*     expand the currend node
      PERFORM tree_expand USING t_sel_node-id.

    ENDLOOP.
    IF sy-subrc = 4.
*     nothing left to expand, exit loop
      EXIT.
    ENDIF.

    PERFORM tree_list TABLES t_sel_node USING node-id.
    DESCRIBE TABLE t_sel_node LINES nr_nodes_new.
    IF nr_nodes_new = nr_nodes_first AND NOT ( node-leaf IS INITIAL ).
*     No node was expanded at all. Clear '+'-icon of current node.
      CLEAR node-force_plus.
      MOVE-CORRESPONDING node TO node_info.
      CALL FUNCTION 'RS_TREE_SET_NODE'
        EXPORTING
          node_info    = node_info
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
      MESSAGE s765.
      EXIT.
    ELSEIF nr_nodes_old = nr_nodes_new.
*     the tree was not expanded further -> stop expanding
      EXIT.
    ELSEIF command = c_command_select.
*     Only one level has to be expanded, so stop now
      EXIT.
    ELSE.
      nr_nodes_old = nr_nodes_new.
    ENDIF.

  ENDDO.

ENDFORM.                               " TREE_NODE_EXPAND


*&---------------------------------------------------------------------*
*&      Form  TREE_NODE_PICK
*&---------------------------------------------------------------------*
*       Execute pick functions for current node
*----------------------------------------------------------------------*
FORM tree_node_pick
USING command TYPE clike
      hide TYPE type_hide
      node STRUCTURE seucomm.

  CASE hide-area.
    WHEN c_area_inbound.
      PERFORM ib_node_pick USING node.
    WHEN c_area_outbound.
      PERFORM ob_show_mestyp USING command node.
    WHEN c_area_outbound_comm.
      PERFORM obc_show_mestyp USING command node.
    WHEN c_area_message.
      PERFORM me_show_mestyp USING command node.
  ENDCASE.

ENDFORM.                               " TREE_NODE_PICK


*&---------------------------------------------------------------------*
*&      Form  TREE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Execute commands that do not require the cursor to be on a node
*----------------------------------------------------------------------*
FORM tree_functions
USING    command TYPE clike
CHANGING node_command TYPE clike.

  CASE command.
    WHEN c_command_store.
      PERFORM read_store_komm.
      it_sel-item = i_z4.
      CALL SCREEN '111'.

    WHEN c_command_refresh.
      PERFORM tree_init.

    WHEN c_command_complete.
*     Call report for inbound transmission completeness
* bd 487835
*    submit rposcomp via selection-screen
*         with upddat between p_dat_v  and p_dat_b
*         with updtim between p_time_v and p_time_b
* ed 487835
* bi 487835
      SUBMIT rposcomp
         VIA SELECTION-SCREEN
         WITH p_dat_v  = p_dat_v
         WITH p_time_v = p_time_v
         WITH p_dat_b  = p_dat_b
         WITH p_time_b = p_time_b
      AND RETURN.
* ei 487835

    WHEN OTHERS.
*     Command was not node-independend
      node_command = 'X'.

  ENDCASE.

ENDFORM.                               " TREE_FUNCTIONS


*&---------------------------------------------------------------------*
*&      Form  TREE_NODE_COMMAND
*&---------------------------------------------------------------------*
*       Execute command for current node
*----------------------------------------------------------------------*
FORM tree_node_command
USING command TYPE clike
      hide TYPE type_hide
      node STRUCTURE seucomm.

  PERFORM ib_node_command USING command node.

  CASE command.
    WHEN 'NODE'.
*     display message with node type and hide area; this is a function
*     for the developer
      MOVE node-hide(40) TO sy-msgv2.
      MOVE node-hide+40(35) TO sy-msgv3.
      MESSAGE s023 WITH node-type sy-msgv2 sy-msgv3.
*     Node type &1 Contents &2&3

    WHEN 'TECH'.
*     Display technical info.
*     A screen is brought up and each application can write details
*     to a report that is displayed there.
*     Pass data of current node to dialog though global variables.
      g_hide = hide.
      g_node = node.
      CALL SCREEN '200' STARTING AT 1 1 ENDING AT 87 16.
      IF g_linno = 1.
        MESSAGE e766.
      ENDIF.
*{   INSERT         LNDK905448                                        1
* Account Determination Analysis
    WHEN 'ACDE'.
      g_hide = hide.
      g_node = node.
      CASE hide-area.
        WHEN c_area_inbound.
          PERFORM ib_accdeterr_detail.
      ENDCASE.
*}   INSERT
  ENDCASE.

ENDFORM.                               " TREE_NODE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  TREE_EXPAND
*&---------------------------------------------------------------------*
*       Expand partial tree under current node
*----------------------------------------------------------------------*
FORM tree_expand
USING id LIKE snodetext-id.            " id of current node

  CALL FUNCTION 'RS_TREE_EXPAND'
    EXPORTING
      node_id   = id
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

ENDFORM.                               " TREE_EXPAND


*&---------------------------------------------------------------------*
*&      Form  TREE_COMPRESS
*&---------------------------------------------------------------------*
*       compress partial tree under current node
*----------------------------------------------------------------------*
FORM tree_compress
USING id LIKE snodetext-id.            " id of current node

  CALL FUNCTION 'RS_TREE_COMPRESS'
    EXPORTING
      node_id   = id
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

ENDFORM.                               " TREE_COMPRESS


*&---------------------------------------------------------------------*
*&      Form  TREE_LIST
*&---------------------------------------------------------------------*
*       get partial tree
*----------------------------------------------------------------------*
FORM tree_list
TABLES nodes   STRUCTURE snodetext     " nodes of partial tree
USING  node_id LIKE snodetext-id.      " id of root of partial tree

  CALL FUNCTION 'RS_TREE_LIST'
    EXPORTING
      node_id         = node_id
      all             = 'X'
      with_attributes = 'X'
    TABLES
      list            = nodes
    EXCEPTIONS
      cycle_detected  = 1
      node_not_found  = 2
      OTHERS          = 3.

ENDFORM.                               " TREE_LIST

*&---------------------------------------------------------------------*
*&      Form  TREE_INIT
*&---------------------------------------------------------------------*
*       Initialize the document flow                                   *
*----------------------------------------------------------------------*
FORM tree_init.

* delete tree from last call
  CALL FUNCTION 'RS_TREE_DELETE_NODE'
    EXPORTING
      node_id      = '000000'
    EXCEPTIONS
      id_not_found = 1
      OTHERS       = 2.

  PERFORM tree_build.

  PERFORM tree_construct USING '000000' ' '.

  PERFORM tree_expand USING '000002'.
*  perform tree_expand using '000005'.
*  perform tree_expand using '000009'.
* perform tree_expand using '000012'.

  PERFORM ob_init.
  PERFORM ib_init.
  INCLUDE lwpsawps.

ENDFORM.                               " TREE_INIT
