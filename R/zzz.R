#' @importFrom rjd3toolkit .JD3_ENV
NULL

DATE_MIN<-NULL
DATE_MAX<-NULL
jd2r_test<-NULL
matrix_jd2r<-NULL
matrix_r2jd<-NULL
ts_jd2r<-NULL
ts_r2jd<-NULL
tsdomain_r2jd<-NULL

.onLoad <- function(libname, pkgname) {

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

  jd2r_test<<-.JD3_ENV$jd2r_test
  matrix_jd2r<<-.JD3_ENV$matrix_jd2r
  matrix_r2jd<<-.JD3_ENV$matrix_r2jd
  ts_jd2r<<-.JD3_ENV$ts_jd2r
  ts_r2jd<<-.JD3_ENV$ts_r2jd
  tsdomain_r2jd<<-.JD3_ENV$tsdomain_r2jd

  .JD3_ENV$DATE_MIN<-DATE_MIN
  .JD3_ENV$DATE_MAX<-DATE_MAX

  .JD3_ENV$p2r_regarima_rslts<-p2r_regarima_rslts
  .JD3_ENV$p2r_date<-p2r_date
  .JD3_ENV$r2p_date<-r2p_date
  .JD3_ENV$p2r_span<-p2r_span
  .JD3_ENV$r2p_span<-r2p_span
  .JD3_ENV$p2r_arima<-p2r_arima
  .JD3_ENV$p2r_sarima<-p2r_sarima
  .JD3_ENV$p2r_spec_sarima<-p2r_spec_sarima
  .JD3_ENV$r2p_spec_sarima<-r2p_spec_sarima
  .JD3_ENV$p2r_ucarima<-p2r_ucarima
  .JD3_ENV$p2r_outlier<-p2r_outlier
  .JD3_ENV$r2p_outlier<-r2p_outlier
  .JD3_ENV$p2r_outliers<-p2r_outliers
  .JD3_ENV$r2p_outliers<-r2p_outliers
  .JD3_ENV$p2r_ramp<-p2r_ramp
  .JD3_ENV$r2p_ramp<-r2p_ramp
  .JD3_ENV$p2r_ramps<-p2r_ramps
  .JD3_ENV$r2p_ramps<-r2p_ramps
  .JD3_ENV$p2r_uservar<-p2r_uservar
  .JD3_ENV$r2p_uservar<-r2p_uservar
  .JD3_ENV$p2r_uservars<-p2r_uservars
  .JD3_ENV$r2p_uservars<-r2p_uservars
  .JD3_ENV$p2r_parameters_estimation<-p2r_parameters_estimation
  .JD3_ENV$p2r_variable<-p2r_variable
  .JD3_ENV$p2r_variables<-p2r_variables

  .JD3_ENV$jd2r_arima<-jd2r_arima
  .JD3_ENV$r2jd_arima<-r2jd_arima
  .JD3_ENV$jd2r_sarima<-jd2r_sarima
  .JD3_ENV$r2jd_sarima<-r2jd_sarima
  .JD3_ENV$jd2r_ucarima<-jd2r_ucarima
  .JD3_ENV$r2jd_ucarima<-r2jd_ucarima
}

