#' Gain and lift charts
#'
#' Validates predicted probabilities against a set of observed (binary)
#' outcomes.
#'
#' @param prob Vector of predicted probabilities.
#'
#' @param y Vector of binary (i.e., 0/1) outcomes. If \code{y} is coded as
#' anything other than 0/1, then you must specify which of the two categories
#' represents the "positive" class (i.e., the class for which the probabilities
#' specified in \code{prob} correspond to) via the \code{pos.class} argument.
#'
#' @param pos.class Numeric/character string specifying which values in \code{y}
#' correspond to the "positive" class. Default is \code{NULL}. (Must be
#' specified whenever \code{y} is not coded as 0/1, where 1 is assumed to
#' represent the "positive" class.)
#'
#' @param cumulative Logical indicating whether or not to compute cumulative
#' lift (i.e., gain). Default is \code{TRUE}.
#'
#' @param nbins Integer specifying the number of bins to use when computing
#' lift. Default is 0, which corresponds to no binning. For example, setting
#' \code{nbins = 10} will result in computing lift within each decile of the
#' sorted probabilities.
#'
#' @param refline.col The color to use for the reference line. Default is
#' \code{"red"}.
#'
#' @param refline.lty The type of line to use for the reference line. Default is
#' \code{"dashed"}.
#'
#' @param refline.lwd The width of the reference line. Default is 1.
#'
#' @param x An object of class \code{"lift"}.
#'
#' @param ... Additional optional argument to be passed on to other methods.
#'
#' @return A \code{"lift"} object, which is essentially a list with the
#' following components:
#' \describe{
#'
#'   \item{\code{"lift"}}{A numeric vector containing the computed lift values.}
#'
#'   \item{\code{"prop"}}{The corresponding proportion of cases associated with
#'   each lift value.}
#'
#'   \item{\code{"cumulative"}}{Same value as that supplied via the
#'   \code{cumulative} argument. (Used by the \code{plot.lift()} method.)}
#'
#' }
#' 
#' @importFrom graphics abline legend
#'
#' @rdname lift
#'
#' @export
lift <- function(prob, y, pos.class = NULL, cumulative = TRUE, nbins = 0) {
  if (!all(sort(unique(y)) == c(0, 1))) {
    if (is.null(pos.class)) {
      stop("A value for `pos.class` is required whenever `y` is not a 0/1 ",
           "outcome.", call. = FALSE)
    }
    y <- ifelse(y == pos.class, 1, 0)
  }
  ord <- order(prob, decreasing = TRUE)
  prob <- prob[ord]
  y <- y[ord]
  prop <- seq_along(y) / length(y)
  if (nbins > 0) {
    bins <- cut(prop, breaks = nbins)
    y <- tapply(y, INDEX = bins, FUN = sum)
    prop <- seq_len(nbins) / nbins
    y <- c(0, y)
    prop <- c(0, prop)
  }
  lift <- if (isTRUE(cumulative)) {
    cumsum(y)
  } else {
    (cumsum(y) / seq_along(y)) / mean(y)
  }
  structure(list("lift" = lift, "prop" = prop, "cumulative" = cumulative,
                 "y" = y), class = "lift")
}


#' @rdname lift
#'
#' @export
plot.lift <- function(x, refline = TRUE, refline.col = "red", 
                      refline.lty = "dashed", refline.lwd = 1, ...) {
  if (isTRUE(x[["cumulative"]])) {
    plot(x[["prop"]], x[["lift"]], type = "l", xlab = "Proportion of cases",
         ylab = "Cumulative lift", ...)
    if (isTRUE(refline)) {
      maxy <- sum(x$y)
      polygon(c(0, mean(x$y), 1), c(0, maxy, maxy), col = "grey")
      #abline(0, 1, col = refline.col, lty = refline.lty,
      #       lwd = refline.lwd)
      legend("bottomright", legend = "Baseline", lty = 2, col = "red",
             bty = "n")
    }
  } else {
    plot(x[["prop"]], x[["lift"]], type = "l", xlab = "Proportion of cases",
         ylab = "Lift", ...)
    abline(h = 1, col = refline.col, lty = refline.lty, lwd = refline.lwd)
    legend("topright", legend = "Baseline", lty = 2, col = "red",
           bty = "n")
  }
  invisible()
}
