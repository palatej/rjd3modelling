#' @include utils.R
NULL

regarima_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(regarima.RegArimaModel, q)
  return (p2r_regarima_rslts(rq))
}

p2r_var<-function(p){
  name<-sapply(p, function(z){z$name})
  type<-sapply(p, function(z){enum_extract(modelling.VariableType, z$var_type)})
  coef<-sapply(p, function(z){z$coefficient})
  stde<-sapply(p, function(z){z$stde})
  pvalue<-sapply(p, function(z){z$pvalue})
  a<-data.frame(coef=coef, stde=stde, pvalue=pvalue, type=type)
  row.names(a)<-name
  return (a)
}

p2r_variables<-function(p){
  return (lapply(p, function(v){p2r_variable(v)}))
}

p2r_variable<-function(p){
  name<-p$name
  type<-enum_extract(modelling.VariableType, p$var_type)
  coeff<-p2r_parameters_rsltx(p$coefficients)
  return (list(name=name, type=type, coeff=coeff))
}


p2r_regarima_rslts<-function(p){

  return (structure(list(
    description=p2r_regarima_description(p$description),
    estimation=p2r_regarima_estimation(p$estimation),
    diagnostics=p2r_regarima_diagnostics(p$diagnostics)),
    class="JD3REGARIMA_RSLTS")
  )
}

p2r_regarima_description<-function(p){
  return (list(
    log=p$log,
    preadjustment = enum_extract(modelling.LengthOfPeriod, p$preadjustment),
    arima=p2r_spec_sarima(p$arima),
    variables=p2r_variables(p$variables)
  ))
}

p2r_regarima_estimation<-function(p){
  return (list(
    y=p$y,
    X=p2r_matrix(p$x),
    parameters=p2r_parameters_estimation(p$parameters),
    b=p$b,
    bvar=p2r_matrix(p$bcovariance),
    likelihood=p2r_likelihood(p$likelihood),
    res=p$residuals
  ))
}


p2r_regarima_diagnostics<-function(p){
  tlist<-lapply(p$residuals_tests, function(z){p2r_test(z$value)})
  tnames<-lapply(p$residuals_tests, function(z){z$key})
  testonresiduals<-`names<-`(tlist, tnames)
}

