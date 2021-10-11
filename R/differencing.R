#' @include utils.R
NULL

p2r_differencing<-function(p){
  if (is.null(p)){
    return (NULL)
  } else{
    del<-sapply(p$differences, function(z){(return (c(z$lag,z$order)))})
    del<-`rownames<-`(del, c("lag", "order"))
    return (list(ddata=p$stationary_series,
                 mean=p$mean_correction,
            differences=del))
  }
}

#' Automatic stationary transformation
#'
#' Stationary transformation of a series by simple differencing of lag 1.
#' Automatic processing (identification of the order of the differencing) based on auto-correlations and on mean correction.
#' The series should not be seasonal.
#' Source: Tramo
#'
#' @param data Series being differenced
#' @param period Period of the series
#'
#' @return
#' Stationary transformation
#' * ddata: data after differencing
#' * mean: mean correction
#' * differences:
#'    * lag: ddata(t)=data(t)-data(t-lag)
#'    * order: order of the differencing
#' @md
#' @export
#'
#' @examples
do.stationary<-function(data, period){
  jst<-.jcall("demetra/modelling/r/Differencing", "Ldemetra/modelling/StationaryTransformation;", "doStationary",
         as.numeric(data), as.integer(period))
  q<-.jcall("demetra/modelling/r/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(modelling.StationaryTransformation, q)
  return (p2r_differencing(p))
}

#' Automatic differencing
#'
#' The series is differentiated till its variance is decreasing
#'
#' @param data Series being differenced
#' @param period Period considered in the automatic differencing
#' @param mad Use of MAD in the computation of the variance (true by default)
#' @param centile Percentage of the data used for computing the variance (90 by default)
#' @param k tolerance in the decrease of the variance. The algorithm stops if the new varance is higher than k*the old variance
#'
#' @return
#' Stationary transformation
#' * ddata: data after differencing
#' * mean: mean correction
#' * differences:
#'    * lag: ddata(t)=data(t)-data(t-lag)
#'    * order: order of the differencing
#' @md
#' @export
#'
#' @examples
#'
differencing.fast<-function(data, period, mad=TRUE, centile=90, k=1.2){
  jst<-.jcall("demetra/modelling/r/Differencing", "Ldemetra/modelling/StationaryTransformation;", "fastDifferencing",
              as.numeric(data), as.integer(period), as.logical(mad), centile, k)
  q<-.jcall("demetra/modelling/r/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(modelling.StationaryTransformation, q)
  return (p2r_differencing(p))

}

#' Differencing of a series
#'
#' @param data The series to be differenced
#' @param lags Lags of the differencing
#' @param mean Mean correction
#'
#' @return The differenced series
#' @export
#'
#' @examples
#' differences(retail$BookStores, c(1,1,12), FALSE)
#'
differences<-function(data, lags=1, mean=TRUE){
  return (.jcall("demetra/modelling/r/Differencing", "[D", "differences",
                 as.numeric(data), .jarray(as.integer(lags)), mean))
}

#' Title
#'
#' @param data
#' @param period
#'
#' @return
#' @export
#'
#' @examples
rangemean.tstat<-function(data, period=0, groupsize = 0, trim = 0){
  return (.jcall("demetra/modelling/r/AutoModelling", "D", "rangeMean",
                 as.numeric(data), as.integer(period), as.integer(groupsize), as.integer(trim)))

}
