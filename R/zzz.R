#' @importFrom rjd3toolkit .JD3_ENV
NULL

DATE_MIN<-NULL
DATE_MAX<-NULL
enum_extract<-NULL
enum_of<-NULL
jd2r_test<-NULL
matrix_jd2r<-NULL
matrix_r2jd<-NULL
ts_jd2r<-NULL
ts_r2jd<-NULL
tsdomain_r2jd<-NULL

.onLoad <- function(libname, pkgname) {

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

  enum_extract<<-.JD3_ENV$enum_extract
  enum_of<<-.JD3_ENV$enum_of
  jd2r_test<<-.JD3_ENV$jd2r_test
  matrix_jd2r<<-.JD3_ENV$matrix_jd2r
  matrix_r2jd<<-.JD3_ENV$matrix_r2jd
  ts_jd2r<<-.JD3_ENV$ts_jd2r
  ts_r2jd<<-.JD3_ENV$ts_r2jd
  tsdomain_r2jd<<-.JD3_ENV$tsdomain_r2jd

  .JD3_ENV$p2r_matrix<-p2r_matrix
  .JD3_ENV$p2r_ts<-p2r_ts
  .JD3_ENV$p2r_test<-p2r_test
  .JD3_ENV$p2r_regarima_rslts<-p2r_regarima_rslts
  .JD3_ENV$regarima_rslts<-regarima_rslts
  .JD3_ENV$DATE_MIN<-DATE_MIN
  .JD3_ENV$DATE_MAX<-DATE_MAX

}

