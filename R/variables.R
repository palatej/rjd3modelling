fixedParameters<-function(coef){
  if (length(coef) == 0) return (NULL)
  return (lapply(coef, function(z){list(value=z, type="FIXED")}))
}

#' Title
#'
#' @param name
#' @param id
#' @param lag0
#' @param lag1
#' @param regeffect
#'
#' @return
#' @export
#'
#' @examples
createVariable<-function(id, name = NULL, lag0 = 0, lag1 = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {name<-id}

  return (list(id=id, name=name, lags=rlags(lag0, lag1), coef=fixedParameters(coef), regeffect=regeffect))
}

#' Title
#'
#' @param start
#' @param end
#' @param name
#' @param coef
#'
#' @return
#' @export
#'
#' @examples
createRamp<-function(start, end, name = NULL, coef=0){
  s<-parseDate(start)
  e<-parseDate(end)
  return (list(name=name, start=s, end=e, coef=fixedParameters(coef) ))
}

#' Title
#'
#' @param code
#' @param pos
#' @param name
#' @param coef
#'
#' @return
#' @export
#'
#' @examples
createOutlier<-function(code, pos, name = NULL, coef=0){
  p<-parseDate(po)
  return (list(name=name, code=code, pos=p, coef=fixedParameters(coef)))
}

