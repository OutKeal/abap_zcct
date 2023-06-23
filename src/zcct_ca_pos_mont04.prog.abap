*&---------------------------------------------------------------------*
*&  Include           /ATU/CA_POS_MONF08
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
        OF cl_gui_list_tree
        IMPORTING node_key,
      handle_expand_no_children
        FOR EVENT expand_no_children
        OF cl_gui_list_tree
        IMPORTING node_key,
      handle_item_double_click
        FOR EVENT item_double_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_button_click
        FOR EVENT button_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_link_click
        FOR EVENT link_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_checkbox_change
        FOR EVENT checkbox_change
        OF cl_gui_list_tree
        IMPORTING node_key item_name checked.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
*    G_EVENT = 'NODE_DOUBLE_CLICK'.
*    G_NODE_KEY = NODE_KEY.
  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD  handle_item_double_click.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
*    G_EVENT = 'ITEM_DOUBLE_CLICK'.
*    G_NODE_KEY = NODE_KEY.
*    G_ITEM_NAME = ITEM_NAME.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  handle_link_click.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
*    G_EVENT = 'LINK_CLICK'.
*    G_NODE_KEY = NODE_KEY.
*    G_ITEM_NAME = ITEM_NAME.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  handle_button_click.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
*    G_EVENT = 'BUTTON_CLICK'.
*    G_NODE_KEY = NODE_KEY.
*    G_ITEM_NAME = ITEM_NAME.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD  handle_checkbox_change.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
*    G_EVENT = 'CHECKBOX_CHANGE'.
*    G_NODE_KEY = NODE_KEY.
*    G_ITEM_NAME = ITEM_NAME.
  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE


  METHOD handle_expand_no_children.
    DATA: node_table TYPE treev_ntab,
          node       TYPE treev_node,
          item_table TYPE item_table_type,
          item       TYPE mtreeitm.

* show the key of the expanded node in a dynpro field
*    G_EVENT = 'EXPAND_NO_CHILDREN'.
*    G_NODE_KEY = NODE_KEY.

    IF node_key = 'Child2'.                                 "#EC NOTEXT
* add the children for node with key 'Child2'
* Node with key 'New3'
      CLEAR node.
      node-node_key = 'New3'.                               "#EC NOTEXT
      node-relatkey = 'Child2'.
      node-relatship = cl_gui_list_tree=>relat_last_child.
      APPEND node TO node_table.

* Node with key 'New4'
      CLEAR node.
      node-node_key = 'New4'.                               "#EC NOTEXT
      node-relatkey = 'Child2'.
      node-relatship = cl_gui_list_tree=>relat_last_child.
      APPEND node TO node_table.

* Items of node with key 'New3'
      CLEAR item.
      item-node_key = 'New3'.
      item-item_name = '1'.
      item-class = cl_gui_list_tree=>item_class_text.
      item-length = 11.
      item-usebgcolor = 'X'. "
      item-text = 'SAPTROX1'.
      APPEND item TO item_table.

      CLEAR item.
      item-node_key = 'New3'.
      item-item_name = '2'.
      item-class = cl_gui_list_tree=>item_class_text.
      item-alignment = cl_gui_list_tree=>align_auto.
      item-font = cl_gui_list_tree=>item_font_prop.
      item-text = 'Comment to SAPTROX1'.                    "#EC NOTEXT
      APPEND item TO item_table.

* Items of node with key 'New4'
      CLEAR item.
      item-node_key = 'New4'.
      item-item_name = '1'.
      item-class = cl_gui_list_tree=>item_class_text.
      item-length = 11.
      item-usebgcolor = 'X'. "
      item-text = 'SAPTRIXTROX'.
      APPEND item TO item_table.

      CLEAR item.
      item-node_key = 'New4'.
      item-item_name = '2'.
      item-class = cl_gui_list_tree=>item_class_text.
      item-alignment = cl_gui_list_tree=>align_auto.
      item-font = cl_gui_list_tree=>item_font_prop.
      item-text = 'Comment to SAPTRIXTROX'.                 "#EC NOTEXT
      APPEND item TO item_table.
    ENDIF.

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
  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION
