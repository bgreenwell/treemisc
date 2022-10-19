#' Friedman benchmark data
#'
#' Simulate data from the Friedman 1 benchmark problem. See
#' \code{\link[mlbench]{mlbench.friedman1}} for details and references.
#'
#' @param n Integer specifying the number of samples (i.e., rows) to
#' generate. Default is 100.
#'
#' @param nx Integer specifying the number of predictor variables to generate.
#' Default is 10. Note that \code{nx >= 5}.
#'
#' @param sigma Numeric specifying the standard deviation of the standard 
#' Gaussian noise.
#' 
#' @returns A data frame with \code{n} rows and \code{nx} + 1 columns (for
#' \code{nx} features and the response).
#'
#' @export
#'
#' @examples
#' set.seed(2319)  # for reproducibility
#' friedman1 <- gen_friedman1(nx = 5)
#' pairs(friedman1, col = "purple2")
gen_friedman1 <- function(n = 100, nx = 10, sigma = 0.1) {
  if (nx < 5) {
    stop("`nsim` must be >= 5.", call. = FALSE)
  }
  x <- matrix(stats::runif(n * nx), ncol = nx)
  colnames(x) <- paste0("x", seq_len(nx))
  y = 10 * sin(pi * x[, 1L] * x[, 2L]) + 20 * (x[, 3L] - 0.5) ^ 2 +
    10 * x[, 4L] + 5 * x[, 5L] + stats::rnorm(n, sd = sigma)
  as.data.frame(cbind(y = y, x))
}