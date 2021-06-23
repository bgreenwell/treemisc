#' Cumulative means
#' 
#' Returns a vector whose elements are the cumulative means of the argument.
#' 
#' @param x A numeric object.
#' 
#' @return A vector of the same length and type as \code{x} (after coercion). 
#' Names are preserved.
#' 
#' An \code{NA} value in \code{x} causes the corresponding and following 
#' elements of the return value to be \code{NA}, as does integer overflow (with 
#' a warning).
#' 
#' @export
#' 
#' @examples 
#' x <- 1:10
#' cummean(x)
#' cumsum(x) / seq_along(x)  # equivalent using cumulative sums
cummean <- function(x) {
  cumsum(x) / seq_along(x)
}
