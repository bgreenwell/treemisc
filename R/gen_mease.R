#' Generate data from the Mease model
#' 
#' Generate binary classification data from the Mease model Mease et al. (2007).
#' 
#' @param n Integer specifying the number of observations. Default is 
#' \code{1000}.
#' 
#' @param nsim Integer specifying the number of binary repsonses to generate.
#' Default is \code{1}.
#' 
#' @returns A data frame with \code{3 + nsim} columns. The first two columns 
#' give the values of the numeric features \code{x1} and \code{x2}. The third
#' column (\code{yprob}) gives the true probabilities (i.e., Pr{Y = 1 | X = x}). 
#' The remaining \code{nsim} columns (\code{yclass<i>}, 
#' \code{i = 1, 2, ..., nsim}) give the simulated binary outcomes corresponding 
#' to \code{yprob}.
#' 
#' @references 
#' Mease D, Wyner AJ, Buja A. Boosted classification trees and class probability
#' quantile estimation. Journal of Machine Learning Research. 2007; 8:409–439.
#' 
#' @importFrom stats rbinom runif
#' 
#' @export
#' 
#' @examples
#' # Generate N = 1000 observations from the Mease model
#' set.seed(2254)  # for reproducibility 
#' mease <- gen_mease(1000, nsim = 1)
#' 
#' # Plot predictor values colored by binary outcome
#' cols <- palette.colors(2, palette = "Okabe-Ito", alpha = 0.3)
#' plot(x2 ~ x1, data = mease, col = cols[mease$yclass1 + 1], pch = 19)
gen_mease <- function(n = 1000, nsim = 1) {
  X <- matrix(runif(2 * n, min = 0, max = 50), ncol = 2)
  x <- t(c(25, 25))
  rx <- apply(X, MARGIN = 1, function(y) {
    sqrt(sum((y - x) ^ 2))
  })
  yprob <- ifelse(rx < 8, 1, ifelse(rx > 28, 0, (28 - rx) / 20))
  yclass <- sapply(seq_len(nsim), FUN = function(x) {
    rbinom(n, size = 1, prob = yprob)
  })
  colnames(yclass) <- paste0("yclass", seq_len(nsim))
  d <- cbind(X, yprob, yclass)
  colnames(d) <- c("x1", "x2", "yprob", colnames(yclass))
  as.data.frame(d)
} 
