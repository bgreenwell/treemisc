#' Modified Wilson-Hilferty approximation
#' 
#' Implements the modified Wilson-Hilferty (1931) approximation used by GUIDE to 
#' convert a chi-square random variable to an approximate chi-square random
#' variable with one degree of freedom.
#' 
#' @param x Numeric value of the observed chi-square statistic.
#' 
#' @param df Integer specifying the degrees of freedom associated with \code{x}.
#' 
#' @returns An approximate chi-square statistic with one degree of freedom. 
#' 
#' @export
#' 
#' @examples 
#' wilson_hilferty(2056, df = 4)
#' wilson_hilferty(1831, df = 20)
#' 
#' set.seed(1144)  # for reproducibility
#' x <- rchisq(1000, df = 10)
#' w <- sapply(x, FUN = function(x) wilson_hilferty(x, df = 10))
#' px <- pchisq(x, df = 10)
#' pw <- pchisq(round(w), df = 1)
#' plot(px, pw)
#' abline(0, 1, lty = 2, col = 2)
#' cor(px, pw)
wilson_hilferty <- function(x, df) {
  W1 <- (sqrt(2 * x) - sqrt(2 * df - 1) + 1) ^ 2 / 2
  temp <- 7 / 9 + sqrt(df) * ((x / df) ^ (1 / 3) - 1 + 2 / (9 * df))
  W2 <- max(c(0, temp ^ 3))
  W <- if (x < df + 10 * sqrt(2 * df)) {
    W2
  } else if (x >= df + 10 * sqrt(2 * df) && W2 < x) {
    (W1 + W2) / 2
  } else {
    W1
  }
  W  # round(W, digits = 0)
}
