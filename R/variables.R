fixedParameters<-function(coef){
  if (length(coef) == 0)return (NULL)
  if (coef == 0) return (NULL)
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
createRamp<-function(start, end, name = NULL, coef=NULL){
  return (list(name=name, start=start, end=end, coef=fixedParameters(coef) ))
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
createOutlier<-function(code, pos, name = NULL, coef=NULL){
  return (list(name=name, code=code, pos=pos, coef=fixedParameters(coef)))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param duration
#' @param endpos
#' @param correction
#'
#' @return
#' @export
#'
#' @examples
easter.variable<-function(frequency, start, length, duration=6, endpos=-1,
                          correction=c("Simple", "PreComputed", "Theoretical", "None")){
  correction<-match.arg(correction)
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "easter", jdom, as.integer(duration), as.integer(endpos), correction)
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param duration
#'
#' @return
#' @export
#'
#' @examples
julianeaster.variable<-function(frequency, start, length, duration=6){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "julianEaster", jdom, as.integer(duration))
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param type
#'
#' @return
#' @export
#'
#' @examples
lp.variable<-function(frequency, start, length, type=c("LeapYear", "LengthOfPeriod")){
  type=match.arg(type)
  lp<-type == "LeapYear"
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "leapYear", jdom, as.logical(lp))
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param pos
#' @param date
#'
#' @return
#' @export
#'
#' @examples
ao.variable<-function(frequency, start, length, pos, date=NULL){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.integer(pos-1))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.character(date))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param pos
#' @param date
#' @param rate
#'
#' @return
#' @export
#'
#' @examples
tc.variable<-function(frequency, start, length, pos, date=NULL, rate=0.7){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.integer(pos-1), rate)
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.character(date), rate)
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param pos
#' @param date
#' @param zeroended
#'
#' @return
#' @export
#'
#' @examples
ls.variable<-function(frequency, start, length, pos, date=NULL, zeroended=T){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.character(date), as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param period
#' @param pos
#' @param date
#' @param zeroended
#'
#' @return
#' @export
#'
#' @examples
so.variable<-function(frequency, start, length, pos, date=NULL, zeroended=T){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.character(date), as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param ramp.start
#' @param ramp.end
#'
#' @return
#' @export
#'
#' @examples
ramp.variable<-function(frequency, start, length, range){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (length(range) != 2) stop("Invalid range")
  if (is.character(range)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ramp", jdom,
                 as.character(range[1]),
                 as.character(range[2]))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ramp", jdom,
                 as.integer(range[1]-1),
                 as.integer(range[2]-1))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Title
#'
#' @param frequency
#' @param start
#' @param length
#' @param starts
#' @param ends
#' @param delta
#' @param seasonaldelta
#'
#' @return
#' @export
#'
#' @examples
intervention.variable<-function(frequency, start, length, starts, ends, delta=0, seasonaldelta=0){
  if (length(starts) != length(ends)) stop("Invalid spans in intervention variable")

  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.character(starts) && is.character(ends)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "interventionVariable", jdom,
                 delta,
                 seasonaldelta,
                 .jarray(as.character(starts)),
                 .jarray(as.character(ends)))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "interventionVariable", jdom,
                 delta,
                 seasonaldelta,
                 .jarray(as.integer(starts-1)),
                 .jarray(as.integer(ends-1)))
  }
  return (ts(data, frequency = frequency, start= start))
}
