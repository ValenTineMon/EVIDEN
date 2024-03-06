*----------------------------------------------------------------------*
* Confidential and Proprietary
* Copyright 2024,
* All Rights Reserved
*----------------------------------------------------------------------*
* Program Name  : <Program Name>
* Program Desc  : <Program Title>
* T-code        : <T-code>
* WRICEF id     : <RICEF ID>
* Start Date    : <DD.MM.YYYY>
* Developer     : <Creator Name>
* Functional    : <Requester Name>
* Tech. Design  : <Technical Design Overview>
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Search term   :
* Description   :
***********************************************************************
REPORT zbcrxxx_program_template.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  sscrfields,
  sflight.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: ts_result  TYPE  zclbc_flight=>ts_sflight.
TYPES: tt_result  TYPE  STANDARD TABLE OF ts_result.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  gc_true  TYPE  char1     VALUE 'X',
  gc_tcode TYPE  sy-tcode  VALUE 'ZBCXXX'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  gt_result        TYPE  tt_result                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  gv_param           TYPE  char10                              ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  gc_structure_1     TYPE  tabname  VALUE 'ZSBCXXX'.

CONSTANTS:
  gc_header_height_1 TYPE  i                VALUE 10,
  gc_alv_height_1    TYPE  i                VALUE 90.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE mc_show_progress.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  SELECT-OPTIONS:
    s_carrid  FOR  sflight-carrid,
    s_connid  FOR  sflight-connid,
    s_fldate  FOR  sflight-fldate.
  PARAMETERS:
    cb_edit  TYPE char1 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.
* Text-s02: File Criteria
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS:
    p_ifile  TYPE  string LOWER CASE,
    p_ofile  TYPE  string LOWER CASE,
    cb_exprt TYPE  flag DEFAULT space.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_authorize_check USING gc_tcode.
  PERFORM f_get_constants.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ifile.
* List input File
  PERFORM f_list_ifile CHANGING p_ifile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ofile.
* List Output File
  PERFORM f_list_ofile CHANGING p_ofile.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'ONLI'.
    PERFORM f_validate_selection_screen.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Example Excel file reading
  IF p_ifile IS NOT INITIAL.
    PERFORM f_read_input_file USING p_ifile.
    RETURN.
  ELSE.
*   Get Data
    PERFORM f_get_data CHANGING gt_result.
    IF gt_result IS INITIAL.
*     Message: No data found.
      MESSAGE s001(zbc).
      RETURN.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF cb_exprt IS INITIAL.
*   Display Processing Result
    PERFORM f_display_result USING gt_result.
  ELSE.
*   Export data to excel
    PERFORM f_generate_excel  USING  gt_result
                                     p_ofile.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE mzbcrxxx_incl_alv ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM f_authorize_check USING pv_tcode  TYPE  sy-tcode.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD pv_tcode.

  IF sy-subrc <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE s172(00) WITH pv_tcode.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM f_get_constants .

  CONSTANTS:
    lc_param  TYPE  zeparam_name VALUE 'TEST1'.

  STATICS:
    lv_read       TYPE  flag.

  DATA:
    lt_genc       TYPE  zclbc_utilities=>tt_gen_c.

  DATA:
    lv_repid   TYPE  programm.


* Check Already Read?
  IF lv_read EQ gc_true.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: gv_param.

* Assign REPID
  lv_repid = sy-repid.

* Read All GenC constants for program
  CALL METHOD zclbc_utilities=>get_gen_c
    EXPORTING
      if_repid = lv_repid
    IMPORTING
      et_gen_c = lt_genc.

* Mark Read Flag
  lv_read = gc_true.

* Assign GenC Constants
  LOOP AT lt_genc ASSIGNING FIELD-SYMBOL(<lfs_genc>).

    CASE <lfs_genc>-param.
*     ------------------------------------
*     Example Parameter
*     ------------------------------------
      WHEN lc_param.
*       <Fill logic to assign value to GenC Constant>
        gv_param = <lfs_genc>-value_low.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM f_list_ifile  CHANGING pv_filename  TYPE  string.

  DATA:
    lt_file     TYPE  filetable.

  DATA:
    lv_rc     TYPE  i,
    lv_action TYPE  i.

  FIELD-SYMBOLS:
    <lfs_file>  TYPE  file_table.


* List Local File
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      file_filter             = 'Excel File(XLSX)|*.XLSX' ##NO_TEXT
      multiselection          = space
    CHANGING
      file_table              = lt_file
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

* Read Selected
  IF NOT ( lv_action IS INITIAL AND
           lv_rc     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE lt_file ASSIGNING <lfs_file>
                     INDEX 1.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

* Assign Output
  pv_filename = <lfs_file>-filename.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LIST_OFILE
*----------------------------------------------------------------------*
*  Popup for Output file selection
*----------------------------------------------------------------------*
FORM f_list_ofile  CHANGING pv_filename  TYPE  string.

  DATA:
    lv_path     TYPE  string,
    lv_filename TYPE  string.


* Initialize Output
  CLEAR pv_filename.

* Get File name dialog
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      file_filter          = 'Excel File(XLSX)|*.XLSX' ##NO_TEXT
      prompt_on_overwrite  = gc_true
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = pv_filename
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM f_validate_selection_screen .

** <Validate Selection Screen Input Here. . .>

* Example Validating Output filename
  IF cb_exprt EQ gc_true.
    IF p_ofile IS INITIAL.
      SET CURSOR FIELD 'P_OFILE'.
*     Error: Please enter a valid filename.
      MESSAGE e002(zbc).
      RETURN.
    ENDIF.
  ENDIF.
  IF p_ofile IS NOT INITIAL.
    PERFORM f_validate_ofile USING p_ofile.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_data
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM f_get_data  CHANGING pt_result TYPE tt_result.

  DATA:
    lt_filter  TYPE  /iwbep/t_mgw_select_option.


* Initialize Output
  CLEAR: pt_result.

* Assign Condition for selection
  APPEND VALUE #( property = 'CARRID'
                  select_options = CORRESPONDING #( s_carrid[] ) )
            TO lt_filter.
  APPEND VALUE #( property = 'CONNID'
                  select_options = CORRESPONDING #( s_connid[] ) )
            TO lt_filter.
  APPEND VALUE #( property = 'FLDATE'
                  select_options = CORRESPONDING #( s_fldate[] ) )
            TO lt_filter.

* Call Class method or Select from CDS View
* --> This is to be able to reuse the logic if it
*     needs to be converted to/ reused for ODATA Service
  CALL METHOD zclbc_flight=>get_data
    EXPORTING
      it_filter = lt_filter
    IMPORTING
      et_sflight = pt_result.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_ofile
*----------------------------------------------------------------------*
*  Validate Output File
*----------------------------------------------------------------------*
FORM f_validate_ofile  USING  pv_ofile TYPE string.

  DATA:
    lt_dummy   TYPE  STANDARD TABLE OF string.

  DATA:
    lv_exist  TYPE  flag.


  IF pv_ofile IS INITIAL.
    SET CURSOR FIELD 'P_OFILE'.
*   Error: Please enter a valid filename.
    MESSAGE e002(zbc).
    RETURN.
  ENDIF.

* Check File Exists?
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = pv_ofile
    RECEIVING
      result               = lv_exist
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    CLEAR lv_exist.
  ENDIF.
* Test Overwrite File
  IF lv_exist EQ gc_true.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = pv_ofile
        filetype                = 'ASC'
        confirm_overwrite       = gc_true
      CHANGING
        data_tab                = lt_dummy
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
*     Error: Cannot modify the specified filename.
      MESSAGE e003(zbc).
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM f_display_result  USING  pt_result TYPE tt_result.

* Show progress
* Text-p99 : Generating ALV Report . . .
  mc_show_progress 99 TEXT-p99.

* Set Container name
  gv_container_1 = 'CTL_ALV_1'.
* Disable Header area
  gv_alv_header_1 = gc_true.
  IF cb_edit EQ space.
*   Enable Soft Refresh only in display mode
    gv_soft_refresh_1 = gc_true.
  ELSE.
*   No auto refresh in edit mode
    gv_no_auto_refresh_1 = gc_true.
  ENDIF.

* ALV Layout
  PERFORM f_alv_layout CHANGING gs_layout_1
                                gs_variant_1
                                gs_print_1.

* Assign Output Data
* Assign Size
  gv_header_hight_1 = gc_header_height_1.
  gv_alv_height_1   = gc_alv_height_1.
  ASSIGN pt_result TO <gfs_list_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM f_alv_build_fieldcat CHANGING gt_fieldcat_1.
* Sort data
  PERFORM f_alv_sort_result CHANGING gt_sort_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM f_alv_layout CHANGING cs_layout  TYPE  lvc_s_layo
                           cs_variant TYPE  disvariant
                           cs_print   TYPE  lvc_s_prnt.

* Initialize Output
  CLEAR:  cs_layout, cs_variant, cs_print.

* determine layout
  cs_layout-sel_mode   = 'B'. "Multiple Selection with Push Box
  cs_layout-cwidth_opt = space.
  cs_layout-zebra      = gc_true.

* For Variant Saving
  cs_variant-report  = sy-repid.

  cs_print-no_colwopt = gc_true.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat CHANGING pt_fieldcat  TYPE lvc_t_fcat.

*  DATA:
*    LV_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <lfs_fieldcat>   TYPE  lvc_s_fcat.


* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure_1
                              CHANGING pt_fieldcat.

  LOOP AT pt_fieldcat ASSIGNING <lfs_fieldcat>.

    CASE <lfs_fieldcat>-fieldname.
      WHEN 'CARRID'.
        <lfs_fieldcat>-key       = gc_true.
      WHEN 'CONNID'.
        <lfs_fieldcat>-key       = gc_true.
      WHEN 'FLDATE'.
        <lfs_fieldcat>-key       = gc_true.
      WHEN 'SEATSOCC_B'.
        <lfs_fieldcat>-edit      = cb_edit.
      WHEN 'SEATSOCC_F'.
        <lfs_fieldcat>-edit      = cb_edit.

**       Text-c01 : Code
*        LV_TEXT                  = TEXT-C01.
*        <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
*        <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
*        <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
*        <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
*        <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
*        <LFS_FIELDCAT>-OUTPUTLEN = 10.
*
*      WHEN OTHERS.
*        <LFS_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM f_alv_sort_result  CHANGING pt_sort TYPE lvc_t_sort.

  CONSTANTS:
    lc_sort1 TYPE  lvc_s_sort-fieldname VALUE 'CARRID',
    lc_sort2 TYPE  lvc_s_sort-fieldname VALUE 'CONNID',
    lc_sort3 TYPE  lvc_s_sort-fieldname VALUE 'FLDATE'.

  DATA:
    ls_sort  TYPE  lvc_s_sort.


* Initialize Output
  CLEAR: pt_sort.

* Sort by CARRID
  CLEAR ls_sort.
  ls_sort-spos      = 1.
  ls_sort-fieldname = lc_sort1.
  ls_sort-up        = gc_true.
  ls_sort-subtot    = space.
  APPEND ls_sort TO pt_sort.

* Sort by CONNID
  CLEAR ls_sort.
  ls_sort-spos      = 2.
  ls_sort-fieldname = lc_sort2.
  ls_sort-up        = gc_true.
  ls_sort-subtot    = space.
  APPEND ls_sort TO pt_sort.

* Sort by FLDATE
  CLEAR ls_sort.
  ls_sort-spos      = 3.
  ls_sort-fieldname = lc_sort3.
  ls_sort-up        = gc_true.
  ls_sort-subtot    = space.
  APPEND ls_sort TO pt_sort.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM f_top_of_page_1 USING pref_dyndoc_id  TYPE  REF TO cl_dd_document. "#EC CALLED

  CONSTANTS:
    lc_size_key TYPE  sdydo_value  VALUE '18%',
    lc_size_val TYPE  sdydo_value  VALUE '82%'.

  DATA:
    lv_text      TYPE  sdydo_text_element,
    lref_table   TYPE  REF TO cl_dd_table_element,
    lref_col_key TYPE  REF TO cl_dd_area,
    lref_col_val TYPE  REF TO cl_dd_area.

* Create table
  CALL METHOD pref_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '50%'
    IMPORTING
      table         = lref_table.
* Get Column Element
  CALL METHOD lref_table->add_column
    EXPORTING
      width  = lc_size_key
    IMPORTING
      column = lref_col_key.
* Get Column Element
  CALL METHOD lref_table->add_column
    EXPORTING
      width  = lc_size_val
    IMPORTING
      column = lref_col_val.
* Set Key column style
  CALL METHOD lref_table->set_column_style
    EXPORTING
      col_no       = 1
      sap_emphasis = cl_dd_area=>strong.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD lref_table->new_row.
* Text-h01 : Report:
  lv_text = TEXT-h01.
  CALL METHOD lref_col_key->add_text
    EXPORTING
      text = lv_text.
  lv_text = sy-title.
  CALL METHOD lref_col_val->add_text
    EXPORTING
      text = lv_text.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM f_print_top_of_page_1.                                 "#EC CALLED

  DATA:
    lv_col01 TYPE  i VALUE 18,
    lv_col02 TYPE  i VALUE 35.


* Text-h01 : Report:
  WRITE AT: /1(lv_col01)  TEXT-h01 INTENSIFIED ON NO-GAP,
            (lv_col02)    sy-title NO-GAP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GENERATE_EXCEL
*----------------------------------------------------------------------*
*  Generate Excel File from internal table
*----------------------------------------------------------------------*
FORM f_generate_excel  USING  pt_result TYPE tt_result
                              pv_ofile TYPE  string.

  DATA:
    lt_bin      TYPE  solix_tab.

  DATA:
    lv_xstring TYPE  xstring,
    lv_size    TYPE  i.


* Show progress
* Text-p98 : Generating Excel File . . .
  mc_show_progress 99 TEXT-p98.

* ALV Layout
  PERFORM f_alv_layout CHANGING gs_layout_1
                                gs_variant_1
                                gs_print_1.

* Build Field catalog
  PERFORM f_alv_build_fieldcat CHANGING gt_fieldcat_1.

* Sort data
  PERFORM f_alv_sort_result CHANGING gt_sort_1.

* Call Method Generate XLSX Xstring
  CLEAR lv_xstring.
  lv_xstring = zclbc_utilities=>create_xlsx_from_itab(
                 EXPORTING
                   is_layout   = gs_layout_1
                   it_fieldcat = gt_fieldcat_1
                   it_sort     = gt_sort_1
                   it_data     = pt_result ).
  IF lv_xstring IS INITIAL.
*   Error occurred during generate Excel file.
    MESSAGE s004(zbc) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Convert XString to Binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xstring
    IMPORTING
      output_length = lv_size
    TABLES
      binary_tab    = lt_bin.

* Download to local file
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_size
      filename                = pv_ofile
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_bin
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
*   Error occurred during generate Excel file.
    MESSAGE s004(zbc) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Show Success Message
* Export file generated successfully.
  MESSAGE s005(zbc).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM f_user_command_1 USING pv_ucomm  TYPE  sy-ucomm ##CALLED.

  DATA:
    lv_valid   TYPE  char01,
    lv_refresh TYPE char01.


  CASE pv_ucomm.
    WHEN 'SAVE_1'.
*     Validate Data
      gref_grid_1->check_changed_data( IMPORTING e_valid = lv_valid
                                       CHANGING c_refresh = lv_refresh ).
*     Continue processing only when valid
      IF lv_valid IS INITIAL.
        RETURN.
      ENDIF.

*     Add processing logic for User-Command here. . .

  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM f_on_data_changed_1 USING pref_data_changed TYPE REF TO  cl_alv_changed_data_protocol ##CALLED
                               pv_onf4 TYPE  char01          ##NEEDED
                               pv_onf4_before TYPE  char01   ##NEEDED
                               pv_onf4_after TYPE  char01    ##NEEDED
                               pv_ucomm TYPE  sy-ucomm       ##NEEDED.

* ****************************
* Validate changed value in here...
* - All invalid input must be rejected here with error message
* ****************************

* For all valid changing cells
  LOOP AT pref_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<lfs_good_cell>).

*   Read Row
    READ TABLE gt_result ASSIGNING FIELD-SYMBOL(<lfs_result>)
                         INDEX <lfs_good_cell>-row_id.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CASE <lfs_good_cell>-fieldname.

*     Example on validation error
      WHEN 'SEATSOCC_B'.
*       Check Value not exceed max
        IF <lfs_good_cell>-value > <lfs_result>-seatsmax_b.
*         Assign error to block data changed
          CALL METHOD pref_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZBC'
              i_msgty     = 'E'
              i_msgno     = '000'
*             Text-e01: Occupied value must not exceed capacity.
              i_msgv1     = TEXT-e01
              i_msgv2     = space
              i_msgv3     = space
              i_msgv4     = space
              i_fieldname = <lfs_good_cell>-fieldname
              i_row_id    = <lfs_good_cell>-row_id
              i_tabix     = <lfs_good_cell>-tabix.

        ENDIF.

*     Example on changing other related field value
      WHEN 'SEATSOCC_F'.
*       Update Capacity
        IF <lfs_good_cell>-value > <lfs_result>-seatsmax_f.
          CALL METHOD pref_data_changed->modify_cell
            EXPORTING
              i_row_id    = <lfs_good_cell>-row_id
              i_tabix     = <lfs_good_cell>-tabix
              i_fieldname = 'SEATSMAX_F'
              i_value     = <lfs_good_cell>-value.
*         Assign Info if needed
          CALL METHOD pref_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZBC'
              i_msgty     = 'I'
              i_msgno     = '000'
*             Text-i01: Capacity has been updated.
              i_msgv1     = TEXT-i01
              i_msgv2     = space
              i_msgv3     = space
              i_msgv4     = space
              i_fieldname = <lfs_good_cell>-fieldname
              i_row_id    = <lfs_good_cell>-row_id
              i_tabix     = <lfs_good_cell>-tabix.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM f_handle_toolbar_1 USING pref_object TYPE REF TO	cl_alv_event_toolbar_set ##CALLED
                              pv_interactive TYPE	char01 ##NEEDED.

* Handle Toolbar as needed
  IF cb_edit EQ gc_true.
    DELETE pref_object->mt_toolbar WHERE function EQ '&CHECK'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&REFRESH'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&&SEP01'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&CUT'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&PASTE'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&UNDO'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&&SEP02'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&APPEND'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&DELETE_ROW'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY_ROW'.
    DELETE pref_object->mt_toolbar WHERE function EQ '&&SEP03'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_READ_INPUT_FILE
*----------------------------------------------------------------------*
*  Read Input File
*----------------------------------------------------------------------*
FORM f_read_input_file  USING  pv_ifile TYPE string.

  DATA:
    lt_data  TYPE  zclbc_utilities=>tt_xlsx_data.

  DATA:
    lref_struc  TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS:
    <lfs_sheet_data> TYPE STANDARD TABLE.


  CALL METHOD zclbc_utilities=>read_xlsx_into_itab
    EXPORTING
      if_filename                 = pv_ifile
      if_read_active_worksheet    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      et_xlsx_data                = lt_data
    EXCEPTIONS
      missing_filename_or_xstring = 1
      error_read_filename         = 2
      no_data_found               = 3
      OTHERS                      = 4.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* -----------
* Worksheets
* -----------
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_sheet>).

    ASSIGN <lfs_sheet>-data->* TO <lfs_sheet_data>.

*   -----------
*   Rows
*   -----------
    LOOP AT <lfs_sheet_data> ASSIGNING FIELD-SYMBOL(<lfs_row>).

      lref_struc ?= cl_abap_structdescr=>describe_by_data( p_data = <lfs_row> ).
*     -----------
*     Columns
*     -----------
      LOOP AT lref_struc->components ASSIGNING FIELD-SYMBOL(<lfs_comp>).

        ASSIGN COMPONENT <lfs_comp>-name OF STRUCTURE <lfs_row> TO FIELD-SYMBOL(<lfs_field>).
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

*       Processing field here...

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

  cl_demo_output=>display_data( <lfs_sheet_data> ).

ENDFORM.
