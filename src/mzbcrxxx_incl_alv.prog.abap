*----------------------------------------------------------------------*
* Confidential and Proprietary
* Copyright 2024,
* All Rights Reserved
*----------------------------------------------------------------------*
* Program Name  : MZBCRXXX_INCL_ALV
* Program Desc  : Include program for ALV Processing
* T-code        : N/A
* WRICEF id     : N/A
* Start Date    : 21.12.2023
* Developer     : Wuthichai L.(Eviden)
* Functional    : N/A
* Tech. Design  : This is include program which contains logic for
*                 displaying report as ALV Grid.
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Search term   :
* Description   :
***********************************************************************

*----------------------------------------------------------------------*
*-- Class
*----------------------------------------------------------------------*
CLASS:
  lcl_event_handler_1 DEFINITION DEFERRED.  "for event handling

*----------------------------------------------------------------------*
*-- Tables
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Type definitions
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Constants
*----------------------------------------------------------------------*
CONSTANTS:
  gc_save_all TYPE char1         VALUE 'A',                 "#EC NEEDED
  gc_save_1   TYPE sy-ucomm      VALUE 'SAVE_1'.            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Internal Tables
*----------------------------------------------------------------------*
DATA:
  gt_fieldcat_1 TYPE lvc_t_fcat,                            "#EC NEEDED
  gt_sort_1     TYPE lvc_t_sort,                            "#EC NEEDED
  gt_tool_exc_1 TYPE ui_functions,                          "#EC NEEDED
  gt_excl       TYPE STANDARD TABLE OF sy-ucomm.            "#EC NEEDED

DATA:
  gt_fieldcat_2 TYPE lvc_t_fcat,                            "#EC NEEDED
  gt_tool_exc_2 TYPE ui_functions,                          "#EC NEEDED
  gt_sort_2     TYPE lvc_t_sort.                            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Work Areas
*----------------------------------------------------------------------*
DATA:
  gs_variant_1  TYPE disvariant,                            "#EC NEEDED
  gs_layout_1   TYPE lvc_s_layo,                            "#EC NEEDED
  gs_fieldcat_1 TYPE lvc_s_fcat,                            "#EC NEEDED
  gs_sort_1     TYPE lvc_s_sort,                            "#EC NEEDED
  gs_excl       TYPE sy-ucomm,                              "#EC NEEDED
  gs_print_1    TYPE lvc_s_prnt.                            "#EC NEEDED

DATA:
  gs_variant_2  TYPE disvariant,                            "#EC NEEDED
  gs_layout_2   TYPE lvc_s_layo,                            "#EC NEEDED
  gs_fieldcat_2 TYPE lvc_s_fcat,                            "#EC NEEDED
  gs_sort_2     TYPE lvc_s_sort,                            "#EC NEEDED
  gs_print_2    TYPE lvc_s_prnt.                            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Variable declarations
*----------------------------------------------------------------------*
DATA:
  gv_container_1          TYPE scrfname,                    "#EC NEEDED
  gv_ok_code_1            TYPE sy-ucomm,                    "#EC NEEDED
  gv_save_ok              TYPE sy-ucomm,                    "#EC NEEDED
  gref_custom_container_1 TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  gref_grid_1             TYPE REF TO cl_gui_alv_grid,      "#EC NEEDED
  gref_grid_2             TYPE REF TO cl_gui_alv_grid,      "#EC NEEDED
  gref_dock_container_1   TYPE REF TO cl_gui_docking_container, "#EC NEEDED
  gref_event_receiver_1   TYPE REF TO lcl_event_handler_1,  "#EC NEEDED
  gref_splitter_1         TYPE REF TO cl_gui_splitter_container, "#EC NEEDED
  gref_container_grid_1   TYPE REF TO cl_gui_container,     "#EC NEEDED
  gref_container_grid_2   TYPE REF TO cl_gui_container,     "#EC NEEDED
  gref_container_html_1   TYPE REF TO cl_gui_container,     "#EC NEEDED
  gref_dyndoc_id_1        TYPE REF TO cl_dd_document,       "#EC NEEDED
  gv_alv_header_1         TYPE flag,                        "#EC NEEDED
  gv_header_hight_1       TYPE i VALUE 20,                  "#EC NEEDED
  gv_second_alv_1         TYPE flag,                        "#EC NEEDED
  gv_alv_height_1         TYPE i VALUE 20,                  "#EC NEEDED
  gv_alv_height_2         TYPE i VALUE 20,                  "#EC NEEDED
  gv_soft_refresh_1       TYPE flag,                        "#EC NEEDED
  gv_soft_refresh_2       TYPE flag,                        "#EC NEEDED
  gv_no_auto_refresh_1    TYPE flag,                        "#EC NEEDED
  gv_no_auto_refresh_2    TYPE flag.                        "#EC NEEDED

*----------------------------------------------------------------------*
*-- Field-Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gfs_list_1> TYPE STANDARD TABLE ##FS_ASSIGN_OK,            "#EC NEEDED
  <gfs_list_2> TYPE STANDARD TABLE                          "#EC FD_ASSGN
  .                                                         "#EC NEEDED

*----------------------------------------------------------------------*
*-- GenC Constants
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Macros
*----------------------------------------------------------------------*

************************************************************************
*-----------------------------------------------------------------------
* CLASS DEFINITION
*-----------------------------------------------------------------------
*Event Handler
CLASS lcl_event_handler_1 DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_print_top_of_page_1 FOR EVENT print_top_of_page OF cl_gui_alv_grid,

      on_top_of_page_1       FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id,

      on_hotspot_click_1     FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_double_click_1      FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      handle_toolbar_1       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command_1  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed_1 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed
                  e_onf4
                  e_onf4_before
                  e_onf4_after
                  e_ucomm,

      on_print_top_of_page_2 FOR EVENT print_top_of_page OF cl_gui_alv_grid,

      on_hotspot_click_2     FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_double_click_2      FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      handle_toolbar_2       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command_2  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_handler DEFINITION

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS lcl_event_handler_1 IMPLEMENTATION.

  METHOD on_top_of_page_1.
    PERFORM f_top_of_page_1 IN PROGRAM (sy-cprog)
      USING e_dyndoc_id
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD on_print_top_of_page_1.
    PERFORM f_print_top_of_page_1 IN PROGRAM (sy-cprog)
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD on_hotspot_click_1.
    PERFORM f_hotspot_click_1 IN PROGRAM (sy-cprog)
      USING e_row_id e_column_id
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD on_double_click_1.
    PERFORM f_hotspot_click_1 IN PROGRAM (sy-cprog)
      USING e_row e_column
      IF FOUND.
  ENDMETHOD.                    "on_double_click

  METHOD handle_toolbar_1.
    PERFORM f_handle_toolbar_1 IN PROGRAM (sy-cprog)
      USING e_object e_interactive
      IF FOUND.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command_1.
    PERFORM f_user_command_1 IN PROGRAM (sy-cprog)
      USING e_ucomm
      IF FOUND.
  ENDMETHOD.                    "handle_user_command_1

  METHOD on_data_changed_1.
    PERFORM f_on_data_changed_1 IN PROGRAM (sy-cprog)
      USING er_data_changed
            e_onf4
            e_onf4_before
            e_onf4_after
            e_ucomm
      IF FOUND.
  ENDMETHOD.

  METHOD on_print_top_of_page_2.
    PERFORM f_print_top_of_page_2 IN PROGRAM (sy-cprog)
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD on_hotspot_click_2.
    PERFORM f_hotspot_click_2 IN PROGRAM (sy-cprog)
      USING e_row_id e_column_id
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD on_double_click_2.
    PERFORM f_hotspot_click_2 IN PROGRAM (sy-cprog)
      USING e_row e_column
      IF FOUND.
  ENDMETHOD.                    "on_double_click

  METHOD handle_toolbar_2.
    PERFORM f_handle_toolbar_2 IN PROGRAM (sy-cprog)
      USING e_object e_interactive
      IF FOUND.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command_2.
    PERFORM f_user_command_2 IN PROGRAM (sy-cprog)
      USING e_ucomm
      IF FOUND.
  ENDMETHOD.                    "handle_user_command_2

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*-- Subroutines
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Form  f_prepare_fieldcat_o
*----------------------------------------------------------------------*
*       Prepare fieldcat method ALV used for online
*----------------------------------------------------------------------*
*  -->  PFD_STRUCTURE  Structure name of the list table
*  <--  PIT_FIELDCAT   Field catalog
*----------------------------------------------------------------------*
FORM f_prepare_fieldcat_o  USING  pv_structure TYPE dd02l-tabname
                         CHANGING pt_fieldcat  TYPE lvc_t_fcat ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = pv_structure
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_prepare_fieldcat_o

*----------------------------------------------------------------------*
*       Module  STATUS_ALV_1  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE status_alv_1 OUTPUT.

* Check ALV 1 in Edit Mode?
  IF gs_layout_1-edit EQ space.
    READ TABLE gt_fieldcat_1 TRANSPORTING NO FIELDS
                             WITH KEY edit = 'X'.
    IF sy-subrc NE 0.
      APPEND gc_save_1 TO gt_excl.
    ENDIF.
  ENDIF.

  SET PF-STATUS '9000' EXCLUDING gt_excl ##STAT_UNDEF.
  SET TITLEBAR  '9000' ##TITL_UNDEF.

ENDMODULE.                 " STATUS_0100_1  OUTPUT

*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV_1  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE display_alv_1 OUTPUT.

* Display ALV Grid
  PERFORM f_alv_display_1.

ENDMODULE.                 " DISPLAY_ALV_1  OUTPUT

*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS_1 INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE exit_commands_1 INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMANDS_1  INPUT

*----------------------------------------------------------------------*
*       Module  USER_COMMAND_ALV_1  INPUT
*----------------------------------------------------------------------*
*       User-Commands from ALV
*----------------------------------------------------------------------*
MODULE user_command_alv_1 INPUT.

* to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.

  gv_save_ok = gv_ok_code_1.
  CLEAR gv_ok_code_1.

  CASE gv_save_ok.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'END'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     Call specific user command handler if it exists
      PERFORM f_user_command_1 IN PROGRAM (sy-cprog) USING gv_save_ok
        IF FOUND.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY_1
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM f_alv_display_1 .

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF gref_grid_1 IS INITIAL.

*   Get First ALV Container
    PERFORM f_get_container_1  USING  'FIRST'
                                      gv_alv_height_1
                             CHANGING gref_container_grid_1.

*   Create First ALV Object
    CREATE OBJECT gref_grid_1
      EXPORTING
        i_parent = gref_container_grid_1.

*   Set Event Handler for First ALV
    CREATE OBJECT gref_event_receiver_1.
    SET HANDLER:
      gref_event_receiver_1->on_print_top_of_page_1  FOR gref_grid_1,
      gref_event_receiver_1->on_hotspot_click_1      FOR gref_grid_1,
      gref_event_receiver_1->on_double_click_1       FOR gref_grid_1,
      gref_event_receiver_1->handle_toolbar_1        FOR gref_grid_1,
      gref_event_receiver_1->handle_user_command_1   FOR gref_grid_1,
      gref_event_receiver_1->on_data_changed_1       FOR gref_grid_1.

*   Show First ALV
    CALL METHOD gref_grid_1->set_table_for_first_display
      EXPORTING
        i_buffer_active               = 'X'
        is_variant                    = gs_variant_1
        is_layout                     = gs_layout_1
        is_print                      = gs_print_1
        i_save                        = gc_save_all
        it_toolbar_excluding          = gt_tool_exc_1
      CHANGING
        it_outtab                     = <gfs_list_1>
        it_fieldcatalog               = gt_fieldcat_1
        it_sort                       = gt_sort_1
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL METHOD gref_grid_1->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
      CALL METHOD gref_grid_1->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.
    ENDIF.
  ELSE.

    IF gv_no_auto_refresh_1 = space.
      IF gv_soft_refresh_1 IS INITIAL.
        CALL METHOD gref_grid_1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = gt_fieldcat_1.
        CALL METHOD gref_grid_1->set_frontend_layout
          EXPORTING
            is_layout = gs_layout_1.
        CALL METHOD gref_grid_1->set_frontend_print
          EXPORTING
            is_print = gs_print_1.
        CALL METHOD gref_grid_1->refresh_table_display
          EXPORTING
            i_soft_refresh = ' '.
      ELSE.
        CALL METHOD gref_grid_1->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF gv_alv_header_1 EQ 'X' AND
     cl_gui_alv_grid=>offline( ) IS INITIAL.

    IF gref_container_html_1 IS INITIAL.

*     Get HTML Header Container
      PERFORM f_get_container_1  USING  'HEADER'
                                        gv_header_hight_1
                               CHANGING gref_container_html_1.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        gref_event_receiver_1->on_top_of_page_1        FOR gref_grid_1.
*     Create TOP-Document
      CREATE OBJECT gref_dyndoc_id_1
        EXPORTING
          style = 'ALV_GRID'.
    ENDIF.

*   Initializing document
    CALL METHOD gref_dyndoc_id_1->initialize_document.
*   Processing events
    CALL METHOD gref_grid_1->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = gref_dyndoc_id_1.
*   Merge all setting
    CALL METHOD gref_dyndoc_id_1->merge_document.
*   Display TOP document
    CALL METHOD gref_dyndoc_id_1->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = gref_container_html_1
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0 ##NEEDED.
      "Do nothing
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create Second ALV Container if flag is marked
* --------------------------------------------------
  IF gv_second_alv_1 EQ 'X'.

    IF gref_container_grid_2 IS INITIAL.
*     Get Second ALV Container
      PERFORM f_get_container_1  USING  'SECOND'
                                        gv_alv_height_2
                               CHANGING gref_container_grid_2.

*     Create First ALV Object
      CREATE OBJECT gref_grid_2
        EXPORTING
          i_parent = gref_container_grid_2.

*     Set Event Handler for First ALV
      CREATE OBJECT gref_event_receiver_1.
      SET HANDLER:
        gref_event_receiver_1->on_print_top_of_page_2  FOR gref_grid_2,
        gref_event_receiver_1->on_hotspot_click_2      FOR gref_grid_2,
        gref_event_receiver_1->on_double_click_2       FOR gref_grid_2,
        gref_event_receiver_1->handle_toolbar_2        FOR gref_grid_2,
        gref_event_receiver_1->handle_user_command_2   FOR gref_grid_2.

*     Show First ALV
      CALL METHOD gref_grid_2->set_table_for_first_display
        EXPORTING
          i_buffer_active               = 'X'
          is_variant                    = gs_variant_2
          is_layout                     = gs_layout_2
          is_print                      = gs_print_2
          i_save                        = gc_save_all
          it_toolbar_excluding          = gt_tool_exc_2
        CHANGING
          it_outtab                     = <gfs_list_2>
          it_fieldcatalog               = gt_fieldcat_2
          it_sort                       = gt_sort_2
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CALL METHOD gref_grid_2->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter.
        CALL METHOD gref_grid_2->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.
      ENDIF.
    ELSE.

      IF gv_no_auto_refresh_2 = space.
        IF gv_soft_refresh_2 IS INITIAL.
          CALL METHOD gref_grid_2->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = gt_fieldcat_2.
          CALL METHOD gref_grid_2->set_frontend_layout
            EXPORTING
              is_layout = gs_layout_2.
          CALL METHOD gref_grid_2->set_frontend_print
            EXPORTING
              is_print = gs_print_2.
          CALL METHOD gref_grid_2->refresh_table_display
            EXPORTING
              i_soft_refresh = ' '.
        ELSE.
          CALL METHOD gref_grid_2->refresh_table_display
            EXPORTING
              i_soft_refresh = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_ALV_DISPLAY_1

*----------------------------------------------------------------------*
*       Form  F_GET_CONTAINER_1
*----------------------------------------------------------------------*
*       Get Container
*----------------------------------------------------------------------*
FORM f_get_container_1  USING  pv_type       TYPE  char10
                               pv_hight      TYPE  i
                      CHANGING pref_container  TYPE  REF TO cl_gui_container.

  STATICS:
    lv_row  TYPE  i.


* Only when container is initial
  IF pref_container IS NOT INITIAL.
    RETURN.
  ENDIF.

* Assign Dock container for background processing
  IF cl_gui_alv_grid=>offline( ) IS NOT INITIAL.
    pref_container = gref_dock_container_1.
    RETURN.
  ENDIF.

  IF gref_custom_container_1 IS INITIAL.
    CREATE OBJECT gref_custom_container_1
      EXPORTING
        container_name = gv_container_1.
  ENDIF.

* Create Splitter if needed
  IF gref_splitter_1 IS INITIAL AND
     ( gv_alv_header_1 EQ 'X' OR
       gv_second_alv_1 EQ 'X' ).

    IF gv_alv_header_1 EQ 'X' AND
       gv_second_alv_1 EQ 'X'.
      lv_row = 3.
    ELSE.
      lv_row = 2.
    ENDIF.

*   Create Splitter for custom_container
    CREATE OBJECT gref_splitter_1
      EXPORTING
        parent  = gref_custom_container_1
        rows    = lv_row
        columns = 1.

  ENDIF.

* Case container type
  CASE pv_type.

    WHEN 'HEADER'.
*     Set height for alv header
      CALL METHOD gref_splitter_1->set_row_height
        EXPORTING
          id     = 1
          height = pv_hight.
*     Get Container
      CALL METHOD gref_splitter_1->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = pref_container.

    WHEN 'FIRST'.
*     Set Container Height
      IF gv_alv_header_1 EQ 'X'.
*       Set height for alv header
        CALL METHOD gref_splitter_1->set_row_height
          EXPORTING
            id     = 2
            height = pv_hight.
*       Get Container
        CALL METHOD gref_splitter_1->get_container
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = pref_container.
      ELSEIF gv_second_alv_1 EQ 'X'.
*       Set height for alv header
        CALL METHOD gref_splitter_1->set_row_height
          EXPORTING
            id     = 1
            height = pv_hight.
*       Get Container
        CALL METHOD gref_splitter_1->get_container
          EXPORTING
            row       = 1
            column    = 1
          RECEIVING
            container = pref_container.
      ELSE.
        pref_container = gref_custom_container_1.
      ENDIF.

    WHEN 'SECOND'.
*     Set height for alv header
      CALL METHOD gref_splitter_1->set_row_height
        EXPORTING
          id     = lv_row
          height = pv_hight.
*     Get Container
      CALL METHOD gref_splitter_1->get_container
        EXPORTING
          row       = lv_row
          column    = 1
        RECEIVING
          container = pref_container.

  ENDCASE.

ENDFORM.                    " F_GET_CONTAINER_1

*----------------------------------------------------------------------*
*  Form f_read_alv_sum_1
*----------------------------------------------------------------------*
*  Read Summarized row from Grid 1
*----------------------------------------------------------------------*
FORM f_read_alv_sum_1  USING  ps_row TYPE lvc_s_row
                     CHANGING ps_data TYPE any  ##CALLED.

  DATA:
    lref_col00 TYPE REF TO data,
    lref_col01 TYPE REF TO data,
    lref_col02 TYPE REF TO data,
    lref_col03 TYPE REF TO data,
    lref_col04 TYPE REF TO data,
    lref_col05 TYPE REF TO data,
    lref_col06 TYPE REF TO data,
    lref_col07 TYPE REF TO data,
    lref_col08 TYPE REF TO data,
    lref_col09 TYPE REF TO data.

  DATA:
    lt_grouplevel   TYPE lvc_t_grpl.

  DATA:
    ls_grouplevel  TYPE lvc_s_grpl.

  DATA:
    lv_index  TYPE  sytabix.

  DATA: BEGIN OF ls_row_data,
          dtype(1)    TYPE c,
          filler1(1)  TYPE c,
          level(2)    TYPE n,
          filler2(19) TYPE c,
          index(10)   TYPE n,
        END OF ls_row_data.

  FIELD-SYMBOLS:
    <lfs_collect> TYPE ANY TABLE.


  ls_row_data = ps_row.
  CALL METHOD gref_grid_1->get_subtotals
    IMPORTING
      ep_collect00   = lref_col00
      ep_collect01   = lref_col01
      ep_collect02   = lref_col02
      ep_collect03   = lref_col03
      ep_collect04   = lref_col04
      ep_collect05   = lref_col05
      ep_collect06   = lref_col06
      ep_collect07   = lref_col07
      ep_collect08   = lref_col08
      ep_collect09   = lref_col09
      et_grouplevels = lt_grouplevel.
  IF ls_row_data-dtype = 'T'.
    ASSIGN lref_col00->* TO <lfs_collect>.
    lv_index = 1.
  ELSE.
    READ TABLE lt_grouplevel INDEX ls_row_data-index
                             INTO ls_grouplevel.
    lv_index = ls_grouplevel-cindx_from.
    CASE ls_grouplevel-collect.
      WHEN '01'.
        ASSIGN lref_col01->* TO <lfs_collect>.
      WHEN '02'.
        ASSIGN lref_col02->* TO <lfs_collect>.
      WHEN '03'.
        ASSIGN lref_col03->* TO <lfs_collect>.
      WHEN '04'.
        ASSIGN lref_col04->* TO <lfs_collect>.
      WHEN '05'.
        ASSIGN lref_col05->* TO <lfs_collect>.
      WHEN '06'.
        ASSIGN lref_col06->* TO <lfs_collect>.
      WHEN '07'.
        ASSIGN lref_col07->* TO <lfs_collect>.
      WHEN '08'.
        ASSIGN lref_col08->* TO <lfs_collect>.
      WHEN '09'.
        ASSIGN lref_col09->* TO <lfs_collect>.
    ENDCASE.
  ENDIF.
  CLEAR ps_data.
  LOOP AT <lfs_collect> ASSIGNING FIELD-SYMBOL(<lfs_data>).
    IF sy-tabix EQ lv_index.
      ps_data = <lfs_data>.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
