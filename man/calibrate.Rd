% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate.R
\name{calibrate}
\alias{calibrate}
\alias{print.calibrate}
\alias{plot.calibrate}
\title{External probability calibration}
\usage{
calibrate(
  prob,
  y,
  method = c("pratt", "iso", "ns", "bins"),
  pos.class = NULL,
  probs = c(0.05, 0.35, 0.65, 0.95),
  nbins = 10
)

\method{print}{calibrate}(x, ...)

\method{plot}{calibrate}(
  x,
  refline = TRUE,
  refline.col = 2,
  refline.lty = "dashed",
  refline.lwd = 1,
  ...
)
}
\arguments{
\item{prob}{Vector of predicted probabilities.}

\item{y}{Vector of binary (i.e., 0/1) outcomes. If \code{y} is coded as
anything other than 0/1, then you must specify which of the two categories
represents the "positive" class (i.e., the class for which the probabilities
specified in \code{prob} correspond to) via the \code{pos.class} argument.}

\item{method}{Character string specifying which calibration method to use.
Current options include:
\describe{

  \item{\code{"pratt"}}{Pratt scaling.}

  \item{\code{"iso"}}{Isotonic (i.e., monotonic) calibration.}

  \item{\code{"ns"}}{Natural (i.e., restricted) cubic splines; essentially,
  a spline-based nonparametric version of Pratt scaling.}
  
  \item{\code{"binned"}}{Use binning to discretize the probabilities into 
  bins (i.e., no model).}

}}

\item{pos.class}{Numeric/character string specifying which values in \code{y}
correspond to the "positive" class. Default is \code{NULL}. (Must be
specified whenever \code{y} is not coded as 0/1., where 1 is assumed to
represent the "positive" class.)}

\item{probs}{Numeric vector specifying the probabilities for generating the
quantiles of \code{prob} on the logit scale; these are used for the knot
locations defining the spline whenever \code{method = "ns"}. The default
corresponds to a good choice based on four knots; see
Harrel (2015, pp. 26-28) for details.}

\item{nbins}{Integer specifying the number of bins to use for grouping the
probabilities; only used if \code{method = "binned"}.}

\item{x}{An object of class \code{"calibrate"}.}

\item{...}{Additional optional argument to be passed on to other methods.}

\item{refline}{Logical indicating whether or not to include a reference line.}

\item{refline.col}{The color to use for the reference line. Default is
\code{"red"}.}

\item{refline.lty}{The type of line to use for the reference line. Default is
\code{"dashed"}.}

\item{refline.lwd}{The width of the reference line. Default is 1.}
}
\value{
A \code{"calibrate"} object, which is essentially a list with the
following components:
\describe{

  \item{\code{"probs"}}{A data frame containing two columns: \code{original}
  (the original probability estimates) and \code{calibrated} (the calibrated
  probability estimates).}

  \item{\code{"calibrater"}}{The calibration function (essentially a fitted
  model object) which can be used to calibrate new probabilities.}

  \item{\code{"bs"}}{The Brier score between \code{prob} and \code{y}.}

}
}
\description{
Validates predicted probabilities against a set of observed (binary)
outcomes.
}
\references{
Harrell, Frank. (2015). Regression Modeling Strategies. Springer Series in
Statistics. Springer International Publishing.
}
