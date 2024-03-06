class ZCLBC_FLIGHT definition
  public
  final
  create public .

public section.

  types TS_SFLIGHT type ZSBCXXX .
  types:
    tt_sflight TYPE STANDARD TABLE OF ts_sflight .
  types:
    trt_range_carrid TYPE RANGE OF sflight-carrid .
  types:
    trt_range_connid TYPE RANGE OF sflight-connid .
  types:
    trt_range_fldate TYPE RANGE OF sflight-fldate .

  class-methods GET_DATA
    importing
      !IT_FILTER type /IWBEP/T_MGW_SELECT_OPTION
    exporting
      !ET_SFLIGHT type TT_SFLIGHT .
protected section.
private section.
ENDCLASS.



CLASS ZCLBC_FLIGHT IMPLEMENTATION.


METHOD get_data.

  DATA:
    lt_carrid  TYPE  RANGE OF sflight-carrid,
    lt_connid  TYPE  RANGE OF sflight-connid,
    lt_fldate  TYPE  RANGE OF sflight-fldate.


* Initialize Output
  CLEAR: et_sflight.

* Get GenC Setting for class if needed.

* Get Filter/Condition data
* (Validate / Get List of Condition here. . . )
  lt_carrid = CORRESPONDING #( VALUE #( it_filter[ property = 'CARRID' ]-select_options OPTIONAL ) ).
  lt_connid = CORRESPONDING #( VALUE #( it_filter[ property = 'CONNID' ]-select_options OPTIONAL ) ).
  lt_fldate = CORRESPONDING #( VALUE #( it_filter[ property = 'FLDATE' ]-select_options OPTIONAL ) ).

* Logic to get and Assign Output data
  SELECT carrid,
         connid,
         fldate,
         price,
         currency,
         planetype,
         seatsmax,
         seatsocc,
         paymentsum,
         seatsmax_b,
         seatsocc_b,
         seatsmax_f,
         seatsocc_f
    INTO TABLE @et_sflight
    FROM sflight
   WHERE carrid IN @lt_carrid
     AND connid IN @lt_connid
     AND fldate IN @lt_fldate.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

ENDMETHOD.
ENDCLASS.
