@AbapCatalog.viewEnhancementCategory: #NONE
@AbapCatalog.sqlViewName: 'ZVBC_FLIGHT_DEMO'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Demo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@OData.entitySet.name: 'Flight' 
@OData.publish: true
define view ZIBC_FLIGHT_DEMO
  as select from sflight
{
  key carrid,
  key connid,
  key fldate,
  //  @Semantics.amount.currencyCode: 'Currency'
  price,
  // @Semantics.currencyCode: true
  currency,
  planetype,
  seatsmax,
  seatsocc,
  //  @Semantics.amount.currencyCode: 'Currency'
  paymentsum,
  seatsmax_b,
  seatsocc_b,
  seatsmax_f,
  seatsocc_f

}
