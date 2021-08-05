
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
arima<-function(name="arima", ar=1, delta=1, ma=1, variance=1){
  return (structure(list(name=name, ar=ar, delta=delta, ma=ma, var=variance), class="JD3_ARIMA"))
}

jd2r_doubleseq<-function(jobj, jprop){
  jseq<-.jcall(jobj, "Ldemetra/data/DoubleSeq;", jprop)
  return (.jcall(jseq, "[D", "toArray"))
}

jd2r_arima<-function(jarima){
  return (arima(.jcall(jarima, "S", "getName"),
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
ucarima<-function(model=NULL, components){
  if (is.null(model))
    model<-arima.lsum(components)
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
  return (ucarima(jd2r_arima(model), lapply(jcmps, jd2r_arima)))
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

