class ZCLBC_CDS_FLIGHT definition
  public
  final
  create public .

public section.

  interfaces IF_RAP_QUERY_PROVIDER .
protected section.
private section.
ENDCLASS.



CLASS ZCLBC_CDS_FLIGHT IMPLEMENTATION.


METHOD if_rap_query_provider~select.

  DATA: lt_filter TYPE /iwbep/t_mgw_select_option.
  DATA: lt_data TYPE zclbc_flight=>tt_sflight.

  TRY.

*      Additional check on Paging/Request Fields/Sort Order as needed
      DATA(lv_top)     = io_request->get_paging( )->get_page_size( ).
      DATA(lv_skip)    = io_request->get_paging( )->get_offset( ).
*      DATA(requested_fields)  = io_request->get_requested_elements( ).
*      DATA(sort_order)    = io_request->get_sort_elements( ).

      lt_filter = CORRESPONDING #( io_request->get_filter( )->get_as_ranges( )
                                     MAPPING property       = name
                                             select_options = range ).
      CALL METHOD zclbc_flight=>get_data
        EXPORTING
          it_filter  = lt_filter
        IMPORTING
          et_sflight = lt_data.

      io_response->set_total_number_of_records( lines( lt_data ) ).
      io_response->set_data( lt_data ).

    CATCH cx_root INTO DATA(exception).
      DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

  ENDTRY.

ENDMETHOD.
ENDCLASS.
