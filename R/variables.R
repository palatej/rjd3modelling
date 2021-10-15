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

#' Easter regressors
#'
#' @inheritParams td
#' @param duration Duration (length in days) of the Easter effect.
#' @param endpos Position of the end of the Easter effect, relatively to Easter.
#' @param correction
#'
#' @export
easter.variable<-function(frequency, start, length, duration=6, endpos=-1,
                          correction=c("Simple", "PreComputed", "Theoretical", "None")){
  correction<-match.arg(correction)
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "easter", jdom, as.integer(duration), as.integer(endpos), correction)
  return (ts(data, frequency = frequency, start= start))
}
#' @export
#' @rdname easter.variable
easter.variable.forTs<-function(s, duration = 6, endpos=-1, correction=c("Simple", "PreComputed", "Theoretical", "None")){
  if (! is.ts(s))
    stop("s should be a time series")
  return (easter.variable(frequency = frequency(s), start = start(s), length = length_ts(s),
                          duration = duration, endpos = endpos, correction = correction))
}



#' @rdname easter.variable
#' @export
julianeaster.variable<-function(frequency, start, length, duration=6){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "julianEaster", jdom, as.integer(duration))
  return (ts(data, frequency = frequency, start= start))
}

#' Leap Year regressor
#'
#' @inheritParams td
#' @param type the modelisation of the leap year effect: as a contrast variable (\code{type = "LeapYear"}, default)
#' or by a length-of-month (or length-of-quarter; \code{type = "LengthOfPeriod"}).
#'
#' @return
#' @export
#'
#' @examples
#' # Leap years occurs on 2000, 2004, 2008 and 2012
#' lp.variable(4, start = c(2000, 1), length = 4*13)
lp.variable<-function(frequency, start, length, type=c("LeapYear", "LengthOfPeriod")){
  type=match.arg(type)
  lp<-type == "LeapYear"
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "leapYear", jdom, as.logical(lp))
  return (ts(data, frequency = frequency, start= start))
}

#' Outliers regressors
#'
#' @inheritParams td
#' @param pos,date the date of the outlier, defined by the position in period compared to the first date (\code{pos} parameter)
#' or by a specific \code{date} defined in the format \code{"YYYY-MM-DD"}.
#' @param rate the rate of the transitory change regressor (see details).
#' @param zeroended boolean indicating if the regressor should end by 0 (\code{zeroended = TRUE}, default) or 1 (\code{zeroended = FALSE}).
#'
#' @details \loadmathjax
#' An additive outlier (AO, \code{ao.variable}) is defined as:
#' \mjsdeqn{AO_t = \begin{cases}1 &\text{if } t=t_0 \newline
#'  0 & \text{if }t\ne t_0\end{cases}}
#'
#' A level shift (LS, \code{ls.variable}) is defined as (if \code{zeroended = FALSE}):
#' \mjsdeqn{LS_t = \begin{cases}-1 &\text{if } t < t_0 \newline
#'  0 & \text{if }t\geq t_0 \end{cases}}
#' A transitory change (TC, \code{tc.variable}) is defined as:
#' \mjsdeqn{TC_t = \begin{cases} 0 &\text{if }t < t_0 \newline
#' \alpha^{t-t_0} & t\geq t_0 \end{cases}}
#' A seasonal outlier (SO, \code{so.variable}) is defined as (if \code{zeroended = FALSE}):
#' \mjsdeqn{SO_t = \begin{cases} 0 &\text{if }t\geq t_0 \newline
#' -1 & \text{if }t < t_0 \text{ and $t$ same periode as }t_0\newline
#'  -\frac{1}{s-1} & \text{otherwise }\end{cases}}
#'
#' @export
#'
#' @examples
#' #Outliers in February 2002
#' ao <- ao.variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' ls <- ls.variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' tc <- tc.variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' so <- so.variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' plot.ts(ts.union(ao, ls, tc, so), plot.type = "single",
#'         col = c("black", "orange", "green", "gray"))
#' @name outliers.variables
#' @rdname outliers.variables
ao.variable<-function(frequency, start, length, pos, date=NULL){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.integer(pos-1))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.character(date))
  }
  return (ts(data, frequency = frequency, start= start))
}
#' @export
#' @rdname outliers.variables
tc.variable<-function(frequency, start, length, pos, date=NULL, rate=0.7){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.integer(pos-1), rate)
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.character(date), rate)
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers.variables
ls.variable<-function(frequency, start, length, pos, date=NULL, zeroended=TRUE){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.character(date), as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers.variables
so.variable<-function(frequency, start, length, pos, date=NULL, zeroended=TRUE){
  jdom<-tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.character(date),
                 0L,
                 as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Ramp regressor
#'
#' @inheritParams outliers.variables
#' @param range the range of the regressor. A vector of length 2 containing the datesin the format \code{"YYYY-MM-DD"}
#' or the position in period compared to the first date.
#'
#' @details \loadmathjax
#' A ramp between two dates \mjseqn{t_0} and \mjseqn{t_1} is defined as:
#' \mjsdeqn{RP_t=
#' \begin{cases}
#' -1 & \text{if }t\geq t_0 \newline
#' \frac{t-t_0}{t_1-t_0}-1 & t_0< t < t_1 \newlin
#' 0 & t \leq t_1
#' \end{cases}
#' }
#'
#' @return
#' @export
#'
#' @examples
#' # Ramp variable from January 2001 to September 2001
#' ramp.variable(12, c(2000,1), length = 12*4, range = c(13, 21))
#' # Or equivalently
#' ramp.variable(12, c(2000,1), length = 12*4, range = c("2001-01-01", "2001-09-02"))
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

#' Intervention variable
#'
#' @inheritParams outliers.variables
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

#' Periodic dummies and contrasts
#'
#' @inheritParams outliers.variables
#'@export
periodic.dummies <-function(frequency, start, length){
  jdom <- tsdomain_r2jd(frequency, start[1], start[2], length)
  r.Variables <- J("demetra/modelling/r/Variables")
  data <- matrix_jd2r(r.Variables$periodicDummies(jdom))
  return (ts(data, frequency = frequency, start= start))
}
#'@export
#'@rdname periodic.dummies
periodic.contrasts <-function(frequency, start, length){
  jdom <- tsdomain_r2jd(frequency, start[1], start[2], length)
  r.Variables <- J("demetra/modelling/r/Variables")
  data <- matrix_jd2r(r.Variables$periodicContrasts(jdom))
  return (ts(data, frequency = frequency, start= start))
}
#
# trigonometric.variables <- function(frequency, start, length, pos, date=NULL){
#   jdom <- tsdomain_r2jd(frequency, start[1], start[2], length)
#   r.Variables <- J("demetra/modelling/r/Variables")
#   if (is.null(date)){
#     data<-r.Variables$trigonometricVariables(jdom, as.integer(c(12,2)), "2020-01-01")
#   }else{
#     data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.character(date), as.logical(zeroended))
#   }
#   data <- matrix_jd2r(r.Variables$periodicContrasts(jdom))
#   return (ts(data, frequency = frequency, start= start))
# }
