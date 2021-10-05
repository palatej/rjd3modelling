#' Title
#'
#' @param period
#' @param phi
#' @param d
#' @param theta
#' @param bphi
#' @param bd
#' @param btheta
#' @param name
#'
#' @return
#' @export
#'
#' @examples
sarima.model<-function(name=NULL, period, phi=NULL, d=0, theta=NULL, bphi=NULL, bd=0, btheta=NULL){
  return (structure(
    list(name=name, period=period, phi = phi, d=d, theta=theta,
                         bphi = bphi, bd = bd, btheta = btheta), class="JD3_SARIMA"))
}

#' Title
#'
#' @param model
#' @param length
#' @param stde
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param name
#' @param ar
#' @param delta
#' @param ma
#' @param variance
#'
#' @return
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
  return (sarima.model(.jcall(jsarima, "S", "getName"),
                       .jcall(jarima, "I", "getPeriod"),
                       jd2r_doubleseq(jsarima, "getPhi"),
                      .jcall(jsarima, "D", "getD"),
                      jd2r_doubleseq(jsarima, "getTheta"),
                      jd2r_doubleseq(jsarima, "getBphi"),
                      .jcall(jsarima, "D", "getBd"),
                      jd2r_doubleseq(jsarima, "getBtheta")
  ))
}

r2jd_sarima<-function(model){
  return (.jcall("demetra/arima/r/SarimaModels", "Ldemetra/arima/SarimaModel;", "of",
                 model$name,
                 as.integer(model$period),
                 .jarray(as.numeric(model$phi)),
                 as.integer(model$d),
                 .jarray(as.numeric(model$theta)),
                 .jarray(as.numeric(model$bphi)),
                 as.integer(model$bd),
                 .jarray(as.numeric(model$btheta))))
}



jd2r_arima<-function(jarima){
  return (arima.model(.jcall(jarima, "S", "getName"),
                jd2r_doubleseq(jarima, "getAr"),
                jd2r_doubleseq(jarima, "getDelta"),
                jd2r_doubleseq(jarima, "getMa"),
                .jcall(jarima, "D", "getInnovationVariance")
                ))
}

r2jd_arima<-function(model){
  return (.jcall("demetra/arima/r/ArimaModels", "Ldemetra/arima/ArimaModel;", "of",
                 model$name,
                 .jarray(as.numeric(model$ar)),
                 .jarray(as.numeric(model$delta)),
                 .jarray(as.numeric(model$ma)),
                 as.numeric(model$var)))
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
arima.sum<-function(...){
  components<-list(...)
  return (arima.lsum(components))
}

arima.lsum<-function(components){

  q<-.jarray(lapply(components, r2jd_arima), "demetra/arima/ArimaModel")
  jsum<-.jcall("demetra/arima/r/ArimaModels", "Ldemetra/arima/ArimaModel;", "sum", q)
  return (jd2r_arima(jsum))
}

#' Title
#'
#' @param model
#' @param nspectrum
#' @param nacf
#'
#' @return
#' @export
#'
#' @examples
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
#'
#' @return
#' @export
#'
#' @examples
ucarima.model<-function(model=NULL, components, checkmodel=F){
  if (is.null(model))
    model<-arima.lsum(components)
  else if (! is(model, "JD3_ARIMA") && ! is(model, "JD3_SARIMA")) stop("Invalid model")

  # TODO: checkmodel
  return (structure(list(model=model, components=components), class="JD3_UCARIMA"))
}

r2jd_ucarima<-function(ucm){
  jmodel<-r2jd_arima(ucm$model)
  jcmps<-.jarray(lapply(ucm$components, r2jd_arima), "demetra/arima/ArimaModel")
  return (.jcall("demetra/arima/r/UcarimaModels", "Ldemetra/arima/UcarimaModel;", "of", jmodel, jcmps))
}

jd2r_ucarima<-function(jucm){
  model<-.jcall(jucm, "Ldemetra/arima/ArimaModel;", "getSum")
  jcmps<-.jcall(jucm, "[Ldemetra/arima/ArimaModel;", "getComponents")
  return (ucarima.model(jd2r_arima(model), lapply(jcmps, jd2r_arima)))
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
  jnucm<-.jcall("demetra/arima/r/UcarimaModels", "Ldemetra/arima/UcarimaModel;", "doCanonical",
               jucm, as.integer(cmp-1), as.logical(adjust))
  return (jd2r_ucarima(jnucm))
}

