% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cummean.R
\name{cummean}
\alias{cummean}
\title{Cumulative means}
\usage{
cummean(x)
}
\arguments{
\item{x}{A numeric object.}
}
\value{
A vector of the same length and type as \code{x} (after coercion). 
Names are preserved.

An \code{NA} value in \code{x} causes the corresponding and following 
elements of the return value to be \code{NA}, as does integer overflow (with 
a warning).
}
\description{
Returns a vector whose elements are the cumulative means of the argument.
}
\examples{
x <- 1:10
cummean(x)
cumsum(x) / seq_along(x)  # equivalent using cumulative sums
}
