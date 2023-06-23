*----------------------------------------------------------------------*
***INCLUDE /ATU/CA_POS_MONT06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree .
  DATA: node_table TYPE treev_ntab,
        item_table TYPE item_table_type,
        events     TYPE cntl_simple_events,
        event      TYPE cntl_simple_event.

* create a container for the tree control
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = 'TREE_CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a000.
  ENDIF.
* create a list tree
  CREATE OBJECT g_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
      item_selection              = 'X'
      with_headers                = ' '
    EXCEPTIONS
      cntl_system_error           = 1
      create_error                = 2
      failed                      = 3
      illegal_node_selection_mode = 4
      lifetime_error              = 5.
  IF sy-subrc <> 0.
    MESSAGE a000.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  event-eventid = cl_gui_list_tree=>eventid_node_double_click.
  event-appl_event = 'X'.                                   "
  APPEND event TO events.

  " item double click
  event-eventid = cl_gui_list_tree=>eventid_item_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " expand no children
  event-eventid = cl_gui_list_tree=>eventid_expand_no_children.
  event-appl_event = 'X'.
  APPEND event TO events.

  " link click
  event-eventid = cl_gui_list_tree=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " button click
  event-eventid = cl_gui_list_tree=>eventid_button_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " checkbox change
  event-eventid = cl_gui_list_tree=>eventid_checkbox_change.
  event-appl_event = 'X'.
  APPEND event TO events.

  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE a000.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER g_application->handle_node_double_click FOR g_tree.
  SET HANDLER g_application->handle_item_double_click FOR g_tree.
  SET HANDLER g_application->handle_expand_no_children FOR g_tree.
  SET HANDLER g_application->handle_link_click FOR g_tree.
  SET HANDLER g_application->handle_button_click FOR g_tree.
  SET HANDLER g_application->handle_checkbox_change FOR g_tree.

  CALL METHOD g_tree->add_nodes_and_items
    EXPORTING
      node_table                     = node_table
      item_table                     = item_table
      item_table_structure_name      = 'zcct_ca_ret_ib_tree'
    EXCEPTIONS
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.
  IF sy-subrc <> 0.
    MESSAGE a000.
  ENDIF.

ENDFORM.                    " CREATE_AND_INIT_TREE
