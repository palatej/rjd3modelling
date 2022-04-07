#' Utility Functions For Specifications
#'
#' Utility functions to set specifications used in other packages
#' @param start,end,name,coef,code,pos,id,lag0,lag1,coef,regeffect
#'
#' @name utility-spec
#' @rdname utility-spec
#' @export
createVariable<-function(id, name = NULL, lag0 = 0, lag1 = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {name<-id}

  return (list(id=id, name=name, lags=rlags(lag0, lag1), coef=fixedParameters(coef), regeffect=regeffect))
}

#' @rdname utility-spec
#' @export
createRamp<-function(start, end, name = NULL, coef=NULL){
  return (list(name=name, start=start, end=end, coef=fixedParameters(coef) ))
}
#' @rdname utility-spec
#' @export
createOutlier<-function(code, pos, name = NULL, coef=NULL){
  return (list(name=name, pos=pos, code=code, coef=fixedParameters(coef)))
}

