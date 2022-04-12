#' Utility Functions For Specifications
#'
#' Utility functions to set specifications used in other packages
#'
#' @param start,end,name,coef,code,pos,id,lag0,lag1,regeffect  parameters.
#'
#' @name utility-spec
#' @rdname utility-spec
#' @export
createVariable<-function(id, name = NULL, lag0 = 0, lag1 = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {
    name<-id
  }
  res = list(id=id, name=name, lags=rlags(lag0, lag1), coef = NULL, regeffect=regeffect)
  res["coef"] = fixedParameters(coef)
  return (res)
}

#' @rdname utility-spec
#' @export
createRamp<-function(start, end, name = NULL, coef=NULL){
  res = list(name=name, start=start, end=end, coef = NULL)
  res["coef"] = fixedParameters(coef)
  return (res)
}
#' @rdname utility-spec
#' @export
createOutlier<-function(code, pos, name = NULL, coef=NULL){
  res = list(name=name, pos=pos, code=code, coef = NULL)
  res["coef"] = fixedParameters(coef)
  return (res)
}
#' @rdname utility-spec
#' @export
fixedParameters<-function(coef){
  if (length(coef) == 0)return (NULL)
  if (coef == 0) return (NULL)
  return (lapply(coef, function(z){list(value=z, type="FIXED")}))
}


