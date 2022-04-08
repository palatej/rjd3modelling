#' @include protobuf.R jd3_r.R
NULL

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
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
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
#' @return
#' @export
#'
#' @examples
#' # Leap years occurs on 2000, 2004, 2008 and 2012
#' lp.variable(4, start = c(2000, 1), length = 4*13)
lp.variable<-function(frequency, start, length, type=c("LeapYear", "LengthOfPeriod")){
  type=match.arg(type)
  lp<-type == "LeapYear"
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
tc.variable<-function(frequency, start, length, pos, date=NULL, rate=0.7){
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
ls.variable<-function(frequency, start, length, pos, date=NULL, zeroended=TRUE){
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
so.variable<-function(frequency, start, length, pos, date=NULL, zeroended=TRUE){
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
periodic.dummies <-function(frequency, start, length){
  jdom <- rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;", "periodicDummies", jdom)
  data <- rjd3toolkit::matrix_jd2r(jm)
  return (ts(data, frequency = frequency, start= start))
}
#'@export
#'@rdname periodic.dummies
periodic.contrasts <-function(frequency, start, length){
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
#' @details \loadmathjax
#' Denote by \mjseqn{P} the value of `frequency` (= the period) and
#' \mjseqn{f_1}, ..., \mjseqn{f_n} the frequencies provides by \code{seasonal_frequency}
#' (if \code{seasonal_frequency = NULL} then \mjseqn{n=\lfloor P/2\rfloor} and \mjseqn{f_i}=i).
#'
#' \code{trigonometric.variables} returns a matrix of size \mjseqn{length\times(2n)}.
#'
#' For each date \mjseqn{t} associated to the period \mjseqn{m} (\mjseqn{m \in \[1,P\]}),
#' the columns \mjseqn{2i} and \mjseqn{2i-1} are equal to:
#' \mjsdeqn{
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
#' The first frequency, \mjseqn{\lambda_1 = 2\pi /12} represent the fundamental seasonal frequency and the
#' other frequencies (\mjseqn{\lambda_2 = 2\pi /12 \times 2}, ..., \mjseqn{\lambda_6 = 2\pi /12 \times 6})
#' are the five harmonics. The output matrix will be equal to:
#' \mjsdeqn{
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
trigonometric.variables <- function(frequency, start, length,
                                    seasonal_frequency = NULL){
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

# Denote by \mjseqn{l} the value of \code{length},
# \mjseqn{s} the value of \code{start} and
# \mjseqn{f_1}, ..., \mjseqn{f_n} the different frequencies.
# \code{trigonometric.variables} returns a matrix of size \mjseqn{l\times(2n)}.
#
# For \mjseqn{i} in \mjseqn{[1,n]}, the columns \mjseqn{2*i} and
# \mjseqn{2*i+1} are equal to
# \mjsdeqn{
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
