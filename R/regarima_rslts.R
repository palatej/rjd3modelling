#' @include protobuf.R
NULL

p2r_regarima_rslts<-function(p){

  return (structure(list(
    description=p2r_regarima_description(p$description),
    estimation=p2r_regarima_estimation(p$estimation),
    diagnostics=p2r_regarima_diagnostics(p$diagnostics)),
    class="JD3_REGARIMA_RSLTS")
  )
}

p2r_regarima_description<-function(p){
  return (list(
    log=p$log,
    preadjustment = rjd3toolkit:::enum_extract(modelling.LengthOfPeriod, p$preadjustment),
    arima=p2r_spec_sarima(p$arima),
    variables=p2r_variables(p$variables)
  ))
}

p2r_regarima_estimation<-function(p){
  return (list(
    y=p$y,
    X=rjd3toolkit:::p2r_matrix(p$x),
    parameters=rjd3toolkit:::p2r_parameters_estimation(p$parameters),
    b=p$b,
    bvar=rjd3toolkit:::p2r_matrix(p$bcovariance),
    likelihood=rjd3toolkit:::p2r_likelihood(p$likelihood),
    res=p$residuals
  ))
}


p2r_regarima_diagnostics<-function(p){
  tlist<-lapply(p$residuals_tests, function(z){rjd3toolkit:::p2r_test(z$value)})
  tnames<-lapply(p$residuals_tests, function(z){z$key})
  testonresiduals<-`names<-`(tlist, tnames)
}

