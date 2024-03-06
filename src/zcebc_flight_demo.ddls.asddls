@EndUserText.label: 'Flight Demo'
@ObjectModel:{
    query: {
        implementedBy: 'ABAP:zclbc_cds_flight'
    }
}
define root custom entity ZCEBC_FLIGHT_DEMO
{
  key Carrid     : abap.char( 3 );
  key Connid     : abap.numc( 4 );
  key Fldate     : abap.dats;
      @Semantics.amount.currencyCode: 'Currency'
      Price      : abap.curr( 15, 2 );
      @Semantics.currencyCode: true
      Currency   : abap.cuky( 5 );
      //Planetype  : abap.char( 10 );
      Seatsmax   : abap.int4;
      Seatsocc   : abap.int4;
      @Semantics.amount.currencyCode: 'Currency'
      Paymentsum : abap.curr( 17, 2 );
      SeatsmaxB  : abap.int4;
      SeatsoccB  : abap.int4;
      SeatsmaxF  : abap.int4;
      SeatsoccF  : abap.int4;

}
