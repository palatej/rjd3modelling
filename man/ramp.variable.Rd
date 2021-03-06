% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables.R
\name{ramp.variable}
\alias{ramp.variable}
\title{Ramp regressor}
\usage{
ramp.variable(frequency, start, length, range)
}
\arguments{
\item{frequency}{Annual frequency. Should be a divisor of 12.}

\item{start}{Array with the first year and the first period (for instance \code{c(1980, 1)}).}

\item{length}{Length of the variables}

\item{range}{the range of the regressor. A vector of length 2 containing the datesin the format \code{"YYYY-MM-DD"}
or the position in period compared to the first date.}
}
\description{
Ramp regressor
}
\details{
\loadmathjax
A ramp between two dates \mjseqn{t_0} and \mjseqn{t_1} is defined as:
\mjsdeqn{RP_t=
\begin{cases}
-1 & \text{if }t\geq t_0 \newline
\frac{t-t_0}{t_1-t_0}-1 & t_0< t < t_1 \newlin
0 & t \leq t_1
\end{cases}
}
}
\examples{
# Ramp variable from January 2001 to September 2001
ramp.variable(12, c(2000,1), length = 12*4, range = c(13, 21))
# Or equivalently
ramp.variable(12, c(2000,1), length = 12*4, range = c("2001-01-01", "2001-09-02"))
}
