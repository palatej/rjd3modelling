#' @include protobuf.R jd3_r.R
NULL

#'
#' Title
#'
#' @param s
#' @param model
#' @param nyears
#'
#' @return
#' @export
#'
#' @examples
td.f<-function(s, model=c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"), nyears=0){
  model<-match.arg(model)
  jts<-rjd3toolkit:::ts_r2jd(s)
  jtest<-.jcall("demetra/modelling/r/TradingDaysTests", "Ldemetra/stats/StatisticalTest;", "fTest",
                jts, model, as.integer(nyears))
  return (rjd3toolkit:::jd2r_test(jtest))
}

#' Title
#'
#' @param s
#' @param differencing
#'
#' @return
#' @export
#'
#' @examples
td.ch<-function(s, differencing){
  jts<-rjd3toolkit:::ts_r2jd(s)
  return (.jcall("demetra/modelling/r/TradingDaysTests", "[D", "chTest",
                jts, .jarray(as.integer(differencing))))
}
