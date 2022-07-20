#' @include protobuf.R jd3_r.R
NULL

#' Easter regressors
#'
#' @inheritParams td
#' @param duration Duration (length in days) of the Easter effect.
#' @param endpos Position of the end of the Easter effect, relatively to Easter.
#' @param correction mean correction option.
#'
#' @export
easter.variable<-function(frequency, start, length, s, duration=6, endpos=-1,
                          correction=c("Simple", "PreComputed", "Theoretical", "None")){
  correction<-match.arg(correction)
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "easter", jdom, as.integer(duration), as.integer(endpos), correction)
  return (ts(data, frequency = frequency, start= start))
}

#' @rdname easter.variable
#' @export
julianeaster.variable<-function(frequency, start, length, s, duration=6){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  data<-.jcall("demetra/modelling/r/Variables", "[D", "julianEaster", jdom, as.integer(duration))
  return (ts(data, frequency = frequency, start= start))
}

#' Leap Year regressor
#'
#' @inheritParams td
#' @param type the modelisation of the leap year effect: as a contrast variable (\code{type = "LeapYear"}, default)
#' or by a length-of-month (or length-of-quarter; \code{type = "LengthOfPeriod"}).
#'
#' @export
#'
#' @examples
#' # Leap years occurs on 2000, 2004, 2008 and 2012
#' lp.variable(4, start = c(2000, 1), length = 4*13)
lp.variable<-function(frequency, start, length, s, type=c("LeapYear", "LengthOfPeriod")){
  type=match.arg(type)
  lp<-type == "LeapYear"
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
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
#' @details
#' An additive outlier (AO, \code{ao.variable}) is defined as:
#' \deqn{AO_t = \begin{cases}1 &\text{if } t=t_0 \newline
#'  0 & \text{if }t\ne t_0\end{cases}}
#'
#' A level shift (LS, \code{ls.variable}) is defined as (if \code{zeroended = FALSE}):
#' \deqn{LS_t = \begin{cases}-1 &\text{if } t < t_0 \newline
#'  0 & \text{if }t\geq t_0 \end{cases}}
#' A transitory change (TC, \code{tc.variable}) is defined as:
#' \deqn{TC_t = \begin{cases} 0 &\text{if }t < t_0 \newline
#' \alpha^{t-t_0} & t\geq t_0 \end{cases}}
#' A seasonal outlier (SO, \code{so.variable}) is defined as (if \code{zeroended = FALSE}):
#' \deqn{SO_t = \begin{cases} 0 &\text{if }t\geq t_0 \newline
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
ao.variable<-function(frequency, start, length, s, pos, date=NULL){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.integer(pos-1))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ao", jdom, as.character(date))
  }
  return (ts(data, frequency = frequency, start= start))
}
#' @export
#' @rdname outliers.variables
tc.variable<-function(frequency, start, length, s, pos, date=NULL, rate=0.7){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.integer(pos-1), rate)
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "tc", jdom, as.character(date), rate)
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers.variables
ls.variable<-function(frequency, start, length, s, pos, date=NULL, zeroended=TRUE){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "ls", jdom, as.character(date), as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers.variables
so.variable<-function(frequency, start, length, s, pos, date=NULL, zeroended=TRUE){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("demetra/modelling/r/Variables", "[D", "so", jdom, as.character(date),
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
#' @details
#' A ramp between two dates \eqn{t_0} and \eqn{t_1} is defined as:
#' \deqn{RP_t=
#' \begin{cases}
#' -1 & \text{if }t\geq t_0 \newline
#' \frac{t-t_0}{t_1-t_0}-1 & t_0< t < t_1 \newlin
#' 0 & t \leq t_1
#' \end{cases}
#' }
#'
#' @export
#'
#' @examples
#' # Ramp variable from January 2001 to September 2001
#' ramp.variable(12, c(2000,1), length = 12*4, range = c(13, 21))
#' # Or equivalently
#' ramp.variable(12, c(2000,1), length = 12*4, range = c("2001-01-01", "2001-09-02"))
ramp.variable<-function(frequency, start, length, s, range){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
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
#' @param starts,ends characters specifying sequences of starts/ends dates for the intervention variable.
#' Can be characters or integers.
#' @param delta regular differencing order.
#' @param seasonaldelta segular differencing order.
#' @details
#' Intervention variables are combinations of any possible sequence of ones and zeros
#' (the sequence of ones being defined by  by the parameters `starts` and `ends`)
#' and of \eqn{\frac{1}{(1-B)^d}} and \eqn{\frac{1}{(1-B^s)^D}} where \eqn{B} is the
#' backwar operation, \eqn{s} is the frequency of the time series,
#' \eqn{d} and \eqn{D} are the parameters `delta` and `seasonaldelta`.
#'
#' For example, with `delta = 0` and `seasonaldelta = 0` we get temporary level shifts defined
#' by the parameters `starts` and `ends`. With `delta = 1` and `seasonaldelta = 0` we get
#' the cumulative sum of temporary level shifts.
#'
#' @examples
#' intervention.variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01")
#' intervention.variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01", delta = 1)
#' @export
intervention.variable<-function(frequency, start, length, s, starts, ends, delta=0, seasonaldelta=0){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  if (length(starts) != length(ends)) stop("Invalid spans in intervention variable")

  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
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
periodic.dummies <-function(frequency, start, length, s){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom <- rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;", "periodicDummies", jdom)
  data <- rjd3toolkit::matrix_jd2r(jm)
  return (ts(data, frequency = frequency, start= start))
}
#'@export
#'@rdname periodic.dummies
periodic.contrasts <-function(frequency, start, length, s){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom <- rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;", "periodicContrasts", jdom)
  data <- rjd3toolkit::matrix_jd2r(jm)
  return (ts(data, frequency = frequency, start= start))
}
#' Trigonometric variables
#'
#' Computes trigonometric variables at different frequencies
#'
#' @inheritParams outliers.variables
#' @param seasonal_frequency the seasonal frequencies.
#' By default the fundamental seasonal frequency and all the harmonics are used.
#'
#' @details
#' Denote by \eqn{P} the value of `frequency` (= the period) and
#' \eqn{f_1}, ..., \eqn{f_n} the frequencies provides by \code{seasonal_frequency}
#' (if \code{seasonal_frequency = NULL} then \eqn{n=\lfloor P/2\rfloor} and \eqn{f_i}=i).
#'
#' \code{trigonometric.variables} returns a matrix of size \eqn{length\times(2n)}.
#'
#' For each date \eqn{t} associated to the period \eqn{m} (\eqn{m\in[1,P]}),
#' the columns \eqn{2i} and \eqn{2i-1} are equal to:
#' \deqn{
#' \cos \left(
#' \frac{2 \pi}{P}  \times m \times f_i
#' \right)
#' \text{ and }
#' \sin \left(
#' \frac{2 \pi}{P} \times m \times f_i
#' \right)
#' }
#' Take for example the case when the first date (\code{date}) is a January, \code{frequency = 12}
#' (monthly time series), \code{length = 12} and \code{seasonal_frequency = NULL}.
#' The first frequency, \eqn{\lambda_1 = 2\pi /12} represent the fundamental seasonal frequency and the
#' other frequencies (\eqn{\lambda_2 = 2\pi /12 \times 2}, ..., \eqn{\lambda_6 = 2\pi /12 \times 6})
#' are the five harmonics. The output matrix will be equal to:
#' \deqn{
#' \begin{pmatrix}
#' \cos(\lambda_1) & \sin (\lambda_1) & \cdots &
#' \cos(\lambda_6) & \sin (\lambda_6) \newline
#' \cos(\lambda_1\times 2) & \sin (\lambda_1\times 2) & \cdots &
#' \cos(\lambda_6\times 2) & \sin (\lambda_6\times 2)\newline
#' \vdots & \vdots & \cdots & \vdots & \vdots \newline
#' \cos(\lambda_1\times 12) & \sin (\lambda_1\times 12) & \cdots &
#' \cos(\lambda_6\times 12) & \sin (\lambda_6\times 12)
#' \end{pmatrix}
#' }
#'
#'
#' @export
trigonometric.variables <- function(frequency, start, length, s,
                                    seasonal_frequency = NULL){
  if(!missing(s) && !is.ts(s)) {
    frequency = frequency(s)
    start = start(s)
    length = length_ts(s)
  }
  jdom <- rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)

  if(!is.null(seasonal_frequency))
    seasonal_frequency <- as.integer(seasonal_frequency)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;", "trigonometricVariables",
             jdom, .jarray(seasonal_frequency))
  data <- rjd3toolkit::matrix_jd2r(jm)

  if(ncol(data) %% 2 == 1)
    data <- cbind(data, 0)

  return(ts(data, frequency = frequency, start = start))
}

# Denote by \eqn{l} the value of \code{length},
# \eqn{s} the value of \code{start} and
# \eqn{f_1}, ..., \eqn{f_n} the different frequencies.
# \code{trigonometric.variables} returns a matrix of size \eqn{l\times(2n)}.
#
# For \eqn{i} in \eqn{[1,n]}, the columns \eqn{2*i} and
# \eqn{2*i+1} are equal to
# \deqn{
# \begin{pmatrix}
# \cos(f_i \pi (0 + s)) \newline
# \cos(f_i \pi (1 + s)) \newline \vdots \newline
# \cos(f_i \pi (l-1 + s))
# \end{pmatrix} \text{ and }
# \begin{pmatrix}
# \sin(f_i \pi (0 + s)) \newline
# \sin(f_i \pi (1 + s)) \newline \vdots \newline
# \sin(f_i \pi (l-1 + s))
# \end{pmatrix}
# }
# trigonometric.variables2 <- function(frequencies, length, start){
#   r.Variables <- J("demetra/modelling/r/Variables")
#   data <- r.Variables$trigonometricVariables(.jarray(frequencies),
#                                      as.integer(start),
#                                      as.integer(length))
#   data <- rjd3toolkit::matrix_jd2r(data)
#   if(ncol(data) %% 2 == 1)
#     data <- cbind(data, 0)
#   colnames(data) <- sprintf("%s - frequency %i",
#                             rep(c("cos","sin"), length(freq)),
#                             rep(seq_along(freq), length(freq)))
#   data
# }
