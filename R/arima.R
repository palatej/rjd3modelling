#' @importFrom methods is
#' @include protobuf.R jd3_r.R
NULL

#' Seasonal ARIMA model (Box-Jenkins)
#'
#' @param period Period of the model.
#' @param phi Coefficients of the regular auto-regressive polynomial (1 + phi(1)B + phi(2)B + ...). True signs.
#' @param d Regular differencing order.
#' @param theta Coefficients of the regular moving average polynomial (1 + theta(1)B + theta(2)B + ...). True signs.
#' @param bphi Coefficients of the seasonal auto-regressive polynomial. True signs.
#' @param bd Seasonal differencing order.
#' @param btheta Coefficients of the seasonal moving average polynomial. True signs.
#' @param name Name of the model.
#'
#' @return a `"JD3_SARIMA"` model.
#' @export
sarima.model<-function(name="sarima", period, phi=NULL, d=0, theta=NULL, bphi=NULL, bd=0, btheta=NULL){
  return (structure(
    list(name=name, period=period, phi = phi, d=d, theta=theta,
                         bphi = bphi, bd = bd, btheta = btheta), class="JD3_SARIMA"))
}

#' Simulate Seasonal ARIMA
#'
#' @param model a `"JD3_SARIMA"` model (see [sarima.model()] function).
#' @param length length of the output series.
#' @param stde the standard deviation of the simulated series.
#'
#' @examples
#' # Airline model
#' s_model <- sarima.model(period = 12, d =1, bd = 1, theta = 0.2, btheta = 0.2)
#' x <- sarima.random(s_model, length = 64)
#' plot(x, type = "line")
#' @export
sarima.random<-function(model, length, stde=5){
  if (class(model) != "JD3_SARIMA") stop("Invalid model")
  return (.jcall("demetra/arima/r/SarimaModels", "[D", "random",
         as.integer(length),
         as.integer(model$period),
         .jarray(as.numeric(model$phi)),
         as.integer(model$d),
         .jarray(as.numeric(model$theta)),
         .jarray(as.numeric(model$bphi)),
         as.integer(model$bd),
         .jarray(as.numeric(model$btheta)),
         stde))
}

#' ARIMA Model
#'
#' @param name Name of the model.
#' @param ar Coefficients of the regular auto-regressive polynomial (1 + ar(1)B + ar(2)B + ...). True signs.
#' @param delta The non stationary auto-regressive polynomial.
#' @param ma Coefficients of the regular moving average polynomial (1 + ma(1)B + ma(2)B + ...). True signs.
#' @param variance the innovation variance
#'
#' @return a `"JD3_ARIMA"` model.
#' @export
#'
#' @examples
arima.model<-function(name="arima", ar=1, delta=1, ma=1, variance=1){
  return (structure(list(name=name, ar=ar, delta=delta, ma=ma, var=variance), class="JD3_ARIMA"))
}

jd2r_doubleseq<-function(jobj, jprop){
  jseq<-.jcall(jobj, "Ldemetra/data/DoubleSeq;", jprop)
  return (.jcall(jseq, "[D", "toArray"))
}


jd2r_sarima<-function(jsarima){
  q<-.jcall("demetra/arima/r/SarimaModels", "[B", "toBuffer", jsarima)
  rq<-RProtoBuf::read(modelling.SarimaModel, q)
  return (p2r_sarima(rq))
}

r2jd_sarima<-function(model){
  return (.jcall("demetra/arima/r/SarimaModels", "Ljdplus/sarima/SarimaModel;", "of",
                 as.integer(model$period),
                 .jarray(as.numeric(model$phi)),
                 as.integer(model$d),
                 .jarray(as.numeric(model$theta)),
                 .jarray(as.numeric(model$bphi)),
                 as.integer(model$bd),
                 .jarray(as.numeric(model$btheta))))
}



jd2r_arima<-function(jarima){
  q<-.jcall("demetra/arima/r/ArimaModels", "[B", "toBuffer", jarima)
  rq<-RProtoBuf::read(modelling.ArimaModel, q)
  return (p2r_arima(rq))
}

r2jd_arima<-function(model){
  return (.jcall("demetra/arima/r/ArimaModels", "Ljdplus/arima/ArimaModel;", "of",
                 .jarray(as.numeric(model$ar)),
                 .jarray(as.numeric(model$delta)),
                 .jarray(as.numeric(model$ma)),
                 as.numeric(model$var), F))
}

#' Sum ARIMA Models
#'
#' @param ... list of ARIMA models (created with [arima.model()]).
#'
#' @return a `"JD3_ARIMA"` model.
#'
#'
#' @details
#' Adds several Arima models, considering that their innovations are independent.
#' The sum of two Arima models is computed as follows:
#' the auto-regressive parts (stationary and non stationary of the aggregated
#' model are the smaller common multiple of the corresponding polynomials of
#' the components. The sum of the acf of the modified moving average
#' polynomials is then computed and factorized, to get the moving average
#' polynomial and innovation variance of the sum.
#'
#' @examples
#' mod1 = arima.model(ar = c(0.1, 0.2), delta = 0, ma = 0)
#' mod2 = arima.model(ar = 0, delta = 0, ma = c(0.4))
#' arima.sum(mod1, mod2)
#' @export
arima.sum<-function(...){
  components<-list(...)
  return (arima.lsum(components))
}

arima.lsum<-function(components){
  q<-.jarray(lapply(components, r2jd_arima), "jdplus/arima/ArimaModel")
  jsum<-.jcall("demetra/arima/r/ArimaModels", "Ljdplus/arima/ArimaModel;", "sum", q)
  return (jd2r_arima(jsum))
}

#' ARIMA Properties
#'
#' @param model a `"JD3_ARIMA"` model (created with [arima.model()]).
#' @param nspectrum number of points in \[0, pi\] to calculate the spectrum.
#' @param nacf maximum lag at which to calculate the acf.
#'
#' @examples
#' mod1 = arima.model(ar = c(0.1, 0.2), delta = 0, ma = 0)
#' arima.properties(mod1)
#' @export
arima.properties<-function(model, nspectrum=601, nacf=36){
  jmodel<-r2jd_arima(model)
  spectrum<-.jcall("demetra/arima/r/ArimaModels", "[D", "spectrum", jmodel, as.integer(nspectrum))
  acf<-.jcall("demetra/arima/r/ArimaModels", "[D", "acf", jmodel, as.integer(nacf))
  return (list(acf=acf, spectrum=spectrum))
}

#' Title
#'
#' @param model
#' @param components
#' @param complements Complements of (some) components
#'
#' @return
#' @export
#'
#' @examples
ucarima.model<-function(model=NULL, components, complements=NULL, checkmodel=F){
  if (is.null(model))
    model<-arima.lsum(components)
  else if (! is(model, "JD3_ARIMA") && ! is(model, "JD3_SARIMA")) stop("Invalid model")

  # TODO: checkmodel
  return (structure(list(model=model, components=components, complements=complements), class="JD3_UCARIMA"))
}

r2jd_ucarima<-function(ucm){
  jmodel<-r2jd_arima(ucm$model)
  jcmps<-.jarray(lapply(ucm$components, r2jd_arima), "jdplus/arima/ArimaModel")
  return (.jcall("demetra/arima/r/UcarimaModels", "Ljdplus/ucarima/UcarimaModel;", "of", jmodel, jcmps))
}

jd2r_ucarima<-function(jucm){
#  model<-.jcall(jucm, "Ljdplus/arima/ArimaModel;", "sum")
#  jcmps<-.jcall(jucm, "[Ljdplus/arima/ArimaModel;", "getComponents")
#  return (ucarima.model(jd2r_arima(model), lapply(jcmps, jd2r_arima)))
  q<-.jcall("demetra/arima/r/UcarimaModels", "[B", "toBuffer", jucm)
  rq<-RProtoBuf::read(modelling.UcarimaModel, q)
  return (p2r_ucarima(rq))
}


#' Title
#'
#' @param ucm
#' @param cmp
#' @param signal
#' @param nspectrum
#' @param nwk
#'
#' @return
#' @export
#'
#' @examples
ucarima.wk<-function(ucm, cmp, signal=T, nspectrum=601, nwk=300){
  jucm<-r2jd_ucarima(ucm)
  jwks<-.jcall("demetra/arima/r/UcarimaModels", "Ljdplus/ucarima/WienerKolmogorovEstimators;", "wienerKolmogorovEstimators", jucm)
  jwk<-.jcall("demetra/arima/r/UcarimaModels", "Ljdplus/ucarima/WienerKolmogorovEstimator;", "finalEstimator", jwks, as.integer(cmp-1), signal)

  spectrum<-.jcall("demetra/arima/r/UcarimaModels", "[D", "spectrum", jwk, as.integer(nspectrum))
  wk<-.jcall("demetra/arima/r/UcarimaModels", "[D", "filter", jwk, as.integer(nwk))
  gain<-.jcall("demetra/arima/r/UcarimaModels", "[D", "gain", jwk, as.integer(nspectrum))

  return (structure(list(spectrum=spectrum, filter=wk, gain2=gain*gain), class="JD3_UCARIMA_WK"))
}

#' Title
#'
#' @param ucm
#' @param cmp
#' @param adjust
#'
#' @return
#' @export
#'
#' @examples
ucarima.canonical<-function(ucm, cmp=0, adjust=T){
  jucm<-r2jd_ucarima(ucm)
  jnucm<-.jcall("demetra/arima/r/UcarimaModels", "Ljdplus/ucarima/UcarimaModel;", "doCanonical",
               jucm, as.integer(cmp-1), as.logical(adjust))
  return (jd2r_ucarima(jnucm))
}

#' Title
#'
#' @param ucm
#' @param data
#' @param stdev
#'
#' @return
#' @export
#'
#' @examples
ucarima.estimate<-function(ucm, data, stdev=T){
  jucm<-r2jd_ucarima(ucm)
  jcmps<-.jcall("demetra/arima/r/UcarimaModels", "Ldemetra/math/matrices/Matrix;", "estimate",
                as.numeric(data), jucm, as.logical(stdev))
  return (rjd3toolkit::matrix_jd2r(jcmps))
}


