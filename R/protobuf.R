#' @import RProtoBuf
#' @import rjd3toolkit
NULL

#' Java Utility Functions
#'
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#'
#' @param p,r,spec,model,jucm,vp,start,end,jrslts parameters.
#'
#' @name jd3_utilities
NULL
#> NULL

p2r_sarima<-function(p){
  return (sarima.model(p$name, p$period, p$phi, p$d, p$theta,
               p$bphi, p$bd, p$btheta))
}

#' @export
#' @rdname jd3_utilities
p2r_arima<-function(p){
  return (arima.model(p$name, p$ar, p$delta, p$ma, p$innovation_variance))
}
#' @export
#' @rdname jd3_utilities
p2r_ucarima<-function(p){
  model<-p2r_arima(p$model)
  return (ucarima.model(model,lapply(p$components, function(z){p2r_arima(z)}), lapply(p$complements, function(z){p2r_arima(z)}), F))
}

# Parameter

# Matrix in the following form:
# row(1): values
# row(2): Parameters type


# Sarima
#' @export
#' @rdname jd3_utilities
p2r_spec_sarima<-function(spec){
  return (structure(
    list(
    period=spec$period,
    d=spec$d,
    bd=spec$bd,
    phi=rjd3toolkit::p2r_parameters(spec$phi),
    theta=rjd3toolkit::p2r_parameters(spec$theta),
    bphi=rjd3toolkit::p2r_parameters(spec$bphi),
    btheta=rjd3toolkit::p2r_parameters(spec$btheta)
  ),
  class="JD3_SARIMA_ESTIMATION"))
}

#' @export
#' @rdname jd3_utilities
r2p_spec_sarima<-function(r){
  p<-regarima.SarimaSpec$new()
  p$period<-r$period
  p$d<-r$d
  p$bd<-r$bd
  p$phi<-rjd3toolkit::r2p_parameters(r$phi)
  p$theta<-rjd3toolkit::r2p_parameters(r$theta)
  p$bphi<-rjd3toolkit::r2p_parameters(r$bphi)
  p$btheta<-rjd3toolkit::r2p_parameters(r$btheta)
  return (p)
}


p2r_outlier<-function(p){
  return (list(
    name=p$name,
    pos=rjd3toolkit::p2r_date(p$position),
    code=p$code,
    coef=rjd3toolkit::p2r_parameter(p$coefficient)
  ))
}

r2p_outlier<-function(r){
  p<-modelling.Outlier$new()
  p$name=r$name
  p$code<-r$code
  p$position<-rjd3toolkit::r2p_date(r$pos)
  p$coefficient<-rjd3toolkit::r2p_parameter(r$coef)
  return (p)
}

#' @export
#' @rdname jd3_utilities
p2r_outliers<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_outlier(z)}))
}

#' @export
#' @rdname jd3_utilities
r2p_outliers<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_outlier(z)}))
}


p2r_ramp<-function(p){
  return (list(
    name=p$name,
    start=rjd3toolkit::p2r_date(p$start),
    end=rjd3toolkit::p2r_date(p$end),
    coef=rjd3toolkit::p2r_parameter(p$coefficient)
  ))
}

r2p_ramp<-function(r){
  p<-modelling.Ramp$new()
  p$name<-r$name
  p$start<-rjd3toolkit::r2p_date(r$start)
  p$end<-rjd3toolkit::r2p_date(r$end)
  p$coefficient<-rjd3toolkit::r2p_parameter(r$coefficient)
  return (p)
}

#' @export
#' @rdname jd3_utilities
p2r_ramps<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_ramp(z)}))
}

#' @export
#' @rdname jd3_utilities
r2p_ramps<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_ramp(z)}))
}

rlags<-function(l0, l1){
  if (l0 == 0 && l1 == 0) {return (NULL)}
  if (l0 == l1){return (l0)}
  else {
    if (l1 < l0) stop("Invalid lags")
    return (c(l0, l1))
  }
}

regeffect<-function(map){
  r<-which(sapply(map, function(z){z$key == "regeffect"}))
  if (length(r) == 0) return ("Undefined")
  return (map[min(r)]$value)
}

p2r_uservar<-function(p){
  l0<-p$first_lag
  l1<-p$last_lag
  lapply
  return (list(
    id=p$id,
    name=p$name,
    lags=rlags(l0, l1),
    coef=rjd3toolkit::p2r_parameter(p$coefficient),
    regeffect=regeffect(p$metadata)
  ))
}

r2p_uservar<-function(r){
  p<-modelling.TsVariable$new()
  p$name<-r$name
  p$id<-r$id
  if (! is.null(r$lags)){
    if (length(r$lags) ==1){
      p$first_lag<-r$lags[1]
      p$last_lag<-r$lags[1]
    }else if (length(r$lags) ==2){
      p$first_lag<-r$lags[1]
      p$last_lag<-r$lags[2]
    }else
      stop("Invalid lags")
  }
  p$coefficient<-rjd3toolkit::r2p_parameters(r$coef)
  p$metadata<-list(list(key="regeffect", value=r$regeffect))
  return (p)
}

p2r_uservars<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_uservar(z)}))
}

r2p_uservars<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_uservar(z)}))
}
#' @export
#' @rdname jd3_utilities
p2r_variables<-function(p){
  return (lapply(p, function(v){p2r_variable(v)}))
}

p2r_variable<-function(p){
  name<-p$name
  type<-rjd3toolkit::enum_extract(modelling.VariableType, p$var_type)
  coeff<-rjd3toolkit::p2r_parameters_rsltx(p$coefficients)
  return (list(name=name, type=type, coeff=coeff))
}



