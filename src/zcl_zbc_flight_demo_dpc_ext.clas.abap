class ZCL_ZBC_FLIGHT_DEMO_DPC_EXT definition
  public
  inheriting from ZCL_ZBC_FLIGHT_DEMO_DPC
  create public .

public section.
protected section.

  methods FLIGHTSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZBC_FLIGHT_DEMO_DPC_EXT IMPLEMENTATION.


METHOD flightset_get_entityset.

  DATA:
    lt_filter TYPE /iwbep/t_mgw_select_option.

* Map Property name here. . .
  lt_filter = it_filter_select_options.
  LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<lfs_filter>).
    CASE <lfs_filter>-property.
      WHEN 'Airline'.
        <lfs_filter>-property = 'CARRID'.
      WHEN 'FlightNo'.
        <lfs_filter>-property = 'CONNID'.
      WHEN 'FlightDate'.
        <lfs_filter>-property = 'FLDATE'.
      WHEN OTHERS.
        "Error? Ignore?
    ENDCASE.
  ENDLOOP.

  CALL METHOD zclbc_flight=>get_data
    EXPORTING
      it_filter  = lt_filter
    IMPORTING
      et_sflight = et_entityset.

* Process Sort and Paging here. . .

ENDMETHOD.
ENDCLASS.
