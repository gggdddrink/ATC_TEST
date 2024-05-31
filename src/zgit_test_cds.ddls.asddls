@AbapCatalog.sqlViewName: 'ZGIT_TEST_CDSS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZGIT_TEST_CDS'
define view ZGIT_TEST_CDS as select from zls_cds_test
{
    bukrs,
    butxt,
    land1,
    ort01,
    waers,
    spras,
    kkber,
    kokfi
}
