CLASS zclbc_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  TYPES:
    BEGIN OF ts_xlsx_data,
        worksheet_name TYPE  string,
        data           TYPE  REF TO data,
      END OF ts_xlsx_data .
  TYPES:
    tt_xlsx_data  TYPE  STANDARD TABLE OF ts_xlsx_data .
  TYPES:
    trt_range_param TYPE RANGE OF ztbc_genc-param .
  TYPES:
    trt_range_ext   TYPE RANGE OF ztbc_genc-param_ext .
  TYPES:
    BEGIN OF ts_gen_c,
        repid        TYPE  ztbc_genc-repid,
        param        TYPE  ztbc_genc-param,
        param_ext    TYPE  ztbc_genc-param_ext,
        sequence     TYPE  ztbc_genc-sequence,
        param_sign   TYPE  ztbc_genc-param_sign,
        param_option TYPE  ztbc_genc-param_option,
        value_low    TYPE  ztbc_genc-value_low,
        value_high   TYPE  ztbc_genc-value_high,
        vdesc        TYPE  ztbc_genc-vdesc,
      END OF ts_gen_c .
  TYPES:
    tt_gen_c  TYPE STANDARD TABLE OF ts_gen_c .

  CLASS-METHODS read_file_to_xstring
    IMPORTING
      !if_filename TYPE string
    EXPORTING
      !ef_xstring TYPE xstring
    EXCEPTIONS
      error_read_filename .
  CLASS-METHODS create_xlsx_from_itab
    IMPORTING
      !it_fieldcat TYPE lvc_t_fcat OPTIONAL
      !it_sort TYPE lvc_t_sort OPTIONAL
      !it_filt TYPE lvc_t_filt OPTIONAL
      !is_layout TYPE lvc_s_layo OPTIONAL
      !it_hyperlinks TYPE lvc_t_hype OPTIONAL
      VALUE(it_data) TYPE STANDARD TABLE                 "#EC CI_VALPAR
    RETURNING
      VALUE(rf_xstring) TYPE xstring .
  CLASS-METHODS get_gen_c
    IMPORTING
      !if_repid TYPE programm
      !irt_param TYPE trt_range_param OPTIONAL
      !irt_ext TYPE trt_range_ext OPTIONAL
    EXPORTING
      !et_gen_c TYPE tt_gen_c .
  CLASS-METHODS read_xlsx_into_itab
    IMPORTING
      !if_filename TYPE string
      !if_xstring TYPE xstring OPTIONAL
      !if_read_active_worksheet TYPE xflag DEFAULT ' '
      !it_worksheet TYPE if_fdt_doc_spreadsheet=>t_worksheet_names OPTIONAL
    EXPORTING
      !et_xlsx_data TYPE tt_xlsx_data
    EXCEPTIONS
      missing_filename_or_xstring
      error_read_filename
      no_data_found .
protected section.
private section.
ENDCLASS.



CLASS ZCLBC_UTILITIES IMPLEMENTATION.


METHOD CREATE_XLSX_FROM_ITAB.

  FIELD-SYMBOLS:
    <l_tab>  TYPE STANDARD TABLE.


* Create Data reference
  DATA(lref_data) = REF #( it_data ).

* Generate Field category based on internal table
  IF it_fieldcat IS INITIAL.

    ASSIGN lref_data->* TO <l_tab>.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = DATA(lref_salv_table)
          CHANGING
            t_table      = <l_tab> ).

        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                 r_columns      = lref_salv_table->get_columns( )
                                 r_aggregations = lref_salv_table->get_aggregations( ) ).
      CATCH cx_salv_msg.
        RETURN.

    ENDTRY.

  ELSE.
    lt_fcat = it_fieldcat.
  ENDIF.

* Update Default Long Header text
  LOOP AT lt_fcat TRANSPORTING NO FIELDS
                  WHERE colddictxt IS NOT INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MODIFY lt_fcat FROM VALUE lvc_s_fcat( colddictxt = 'L'  ) TRANSPORTING colddictxt
                   WHERE colddictxt IS INITIAL.
  ENDIF.

* Call Method generate XLSX as XSTRING
  CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
    EXPORTING
      xml_type      = if_salv_bs_xml=>c_type_xlsx
      xml_version   = cl_salv_bs_a_xml_base=>get_version( )
      r_result_data = cl_salv_ex_util=>factory_result_data_table(
                                              r_data                      = lref_data
                                              s_layout                    = is_layout
                                              t_fieldcatalog              = lt_fcat
                                              t_sort                      = it_sort
                                              t_filter                    = it_filt
                                              t_hyperlinks                = it_hyperlinks )
      xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
      gui_type      = if_salv_bs_xml=>c_gui_type_gui
    IMPORTING
      xml           = rf_xstring.

*  TRY.
*      CALL METHOD CL_SALV_BS_LEX=>EXPORT_FROM_RESULT_DATA_TABLE
*        EXPORTING
*          IS_FORMAT            = IF_SALV_BS_LEX_FORMAT=>MC_FORMAT_XLSX
*          IR_RESULT_DATA_TABLE = CL_SALV_EX_UTIL=>FACTORY_RESULT_DATA_TABLE(
*                                         R_DATA                      = LREF_DATA
*                                         S_LAYOUT                    = IS_LAYOUT
*                                         T_FIELDCATALOG              = LT_FCAT
*                                         T_SORT                      = IT_SORT
*                                         T_FILTER                    = IT_FILT
*                                         T_HYPERLINKS                = IT_HYPERLINKS )
*        IMPORTING
*          ER_RESULT_FILE       = RF_XSTRING.
*    CATCH CX_SALV_UNEXPECTED_PARAM_VALUE.
*      RETURN.
*  ENDTRY.

ENDMETHOD.


METHOD GET_GEN_C.

* Initialize Output
  CLEAR et_gen_c.

* Get Data
  SELECT repid,
         param,
         param_ext,
         sequence,
         param_sign,
         param_option,
         value_low,
         value_high,
         vdesc
    INTO TABLE @et_gen_c
    FROM ztbc_genc
   WHERE repid EQ @if_repid
     AND param IN @irt_param
     AND param_ext IN @irt_ext.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD READ_FILE_TO_XSTRING.

 DATA:
    lf_filelength TYPE  i.

  DATA:
    lt_data  TYPE solix_tab.


* Initialize Output
  CLEAR: ef_xstring.

* Read File as Binary
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = if_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = lf_filelength
    CHANGING
      data_tab                = lt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    RAISE error_read_filename.
  ENDIF.
* Convert to XString
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lf_filelength
    IMPORTING
      buffer       = ef_xstring
    TABLES
      binary_tab   = lt_data
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    RAISE error_read_filename.
  ENDIF.

ENDMETHOD.


METHOD READ_XLSX_INTO_ITAB.

  DATA:
    ls_xlsx_data  TYPE  ts_xlsx_data.

  DATA:
    lf_xstring    TYPE  xstring.


* Initialize Output
  CLEAR: et_xlsx_data.

* -------------------------
* Get XLSX Binary data
* -------------------------
  CLEAR lf_xstring.
  IF if_xstring IS NOT INITIAL.
    lf_xstring = if_xstring.

* -------------------------
* Read Data from File if Binary string is not provided
* -------------------------
  ELSE.
    IF if_filename IS INITIAL.
      RAISE missing_filename_or_xstring.
    ENDIF.

*   Read File to XString
    read_file_to_xstring( EXPORTING if_filename = if_filename
                          IMPORTING ef_xstring = lf_xstring ).
  ENDIF.

  IF lf_xstring IS INITIAL.
    RAISE no_data_found.
  ENDIF.

* -------------------------
* Processing Excel Data
* -------------------------
  TRY .
      DATA(lf_filename) = if_filename.
      IF lf_filename IS INITIAL.
        lf_filename = 'EXCEL.XLSX'.
      ENDIF.
      DATA(lref_excel) = NEW cl_fdt_xl_spreadsheet(
                             document_name = lf_filename
                             xdocument     = lf_xstring ) .
    CATCH cx_fdt_excel_core.
      RAISE no_data_found.
  ENDTRY .

* Get List of Worksheets
  lref_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
                            IMPORTING
                              worksheet_names = DATA(lt_worksheets) ).

* Get and filter only Active Worksheet
  IF if_read_active_worksheet IS NOT INITIAL.
    DATA: lf_active_indx TYPE i.
    DATA: lf_activetab TYPE string.
    TRY.
        DATA(lf_workbook) = lref_excel->if_fdt_doc_pkg~get_file_as_xstring( 'xl/workbook.xml' ).
        CALL TRANSFORMATION zbc_fdt_xl_get_activetab
            SOURCE XML lf_workbook
            RESULT activetab = lf_activetab.
        lf_active_indx = lf_activetab + 1.
      CATCH cx_root.
        lf_active_indx = 1.
    ENDTRY.
*   Remove non-active worksheet
    DELETE lt_worksheets WHERE table_line NE lt_worksheets[ lf_active_indx ].
  ENDIF.

* Read Worksheet data
  LOOP AT lt_worksheets ASSIGNING FIELD-SYMBOL(<l_worksheet>).

*   Check Specified Sheet
    IF it_worksheet IS NOT INITIAL AND
       NOT line_exists( it_worksheet[ table_line = <l_worksheet> ] ).
      CONTINUE.
    ENDIF.

*   Read and Assign data
    CLEAR ls_xlsx_data.
    ls_xlsx_data-worksheet_name = <l_worksheet>.
    ls_xlsx_data-data = lref_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                             <l_worksheet> ).
    INSERT ls_xlsx_data INTO TABLE et_xlsx_data.
  ENDLOOP.

  IF et_xlsx_data IS INITIAL.
    RAISE no_data_found.
  ENDIF.

ENDMETHOD.
ENDCLASS.
