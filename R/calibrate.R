#' External probability calibration
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
#' @param method Character string specifying which calibration method to use.
#' Current options include:
#' \describe{
#'
#'   \item{\code{"pratt"}}{Pratt scaling.}
#'
#'   \item{\code{"iso"}}{Isotonic (i.e., monotonic) calibration.}
#'
#'   \item{\code{"ns"}}{Natural (i.e., restricted) cubic splines; essentially,
#'   a spline-based nonparametric version of Pratt scaling.}
#'   
#'   \item{\code{"binned"}}{Use binning to discretize the probabilities into 
#'   bins (i.e., no model).}
#'
#' }
#'
#' @param pos.class Numeric/character string specifying which values in \code{y}
#' correspond to the "positive" class. Default is \code{NULL}. (Must be
#' specified whenever \code{y} is not coded as 0/1., where 1 is assumed to
#' represent the "positive" class.)
#'
#' @param probs Numeric vector specifying the probabilities for generating the
#' quantiles of \code{prob} on the logit scale; these are used for the knot
#' locations defining the spline whenever \code{method = "ns"}. The default
#' corresponds to a good choice based on four knots; see
#' Harrel (2015, pp. 26-28) for details.
#' 
#' @param nbins Integer specifying the number of bins to use for grouping the
#' probabilities; only used if \code{method = "binned"}.
#' 
#' @param refline Logical indicating whether or not to include a reference line.
#'
#' @param refline.col The color to use for the reference line. Default is
#' \code{"red"}.
#'
#' @param refline.lty The type of line to use for the reference line. Default is
#' \code{"dashed"}.
#'
#' @param refline.lwd The width of the reference line. Default is 1.
#'
#' @param x An object of class \code{"calibrate"}.
#'
#' @param ... Additional optional argument to be passed on to other methods.
#'
#' @return A \code{"calibrate"} object, which is essentially a list with the
#' following components:
#' \describe{
#'
#'   \item{\code{"probs"}}{A data frame containing two columns: \code{original}
#'   (the original probability estimates) and \code{calibrated} (the calibrated
#'   probability estimates).}
#'
#'   \item{\code{"calibrater"}}{The calibration function (essentially a fitted
#'   model object) which can be used to calibrate new probabilities.}
#'
#'   \item{\code{"bs"}}{The Brier score between \code{prob} and \code{y}.}
#'
#' }
#'
#' @references
#' Harrell, Frank. (2015). Regression Modeling Strategies. Springer Series in
#' Statistics. Springer International Publishing.
#'
#' @importFrom graphics abline legend
#' @importFrom stats binomial glm isoreg qlogis quantile
#' @importFrom splines ns
#' @importFrom utils head
#'
#' @rdname calibrate
#'
#' @export
calibrate <- function(prob, y, method = c("pratt", "iso", "ns", "bins"), 
                      pos.class = NULL, probs = c(0.05, 0.35, 0.65, 0.95),
                      nbins = 10) {
  if (!all(sort(unique(y)) == c(0, 1))) {
    if (is.null(pos.class)) {
      stop("A value for `pos.class` is required whenever `y` is not a 0/1 ",
           "outcome.", call. = FALSE)
    }
    y <- ifelse(y == pos.class, 1, 0)
  }
  ord <- order(prob)
  prob <- prob[ord]
  y <- y[ord]
  bs <- mean((prob - y) ^ 2, na.rm = TRUE)
  method <- match.arg(method)
  prob.cal <- if (method %in% c("pratt", "ns")) {
    prob[prob == 0] <- 0.0001  # avoid -Inf
    prob[prob == 1] <- 0.9999  # avoid +Inf
    logit <- qlogis(prob)
    ind <- !is.infinite(logit)
    cal <- if (method == "ns") {
      knots <- quantile(logit[ind], probs = probs)
      d <- data.frame(y = y[ind], "x" = logit[ind])
      #glm(y ~ splines::ns(x, knots = knots), data = d
      #    family = binomial(link = "logit"))
      glm(y[ind] ~ splines::ns(logit[ind], knots = knots),
          family = binomial(link = "logit"))
    } else {
      glm(y[ind] ~ logit[ind], family = binomial(link = "logit"))
    }
    prob <- prob[ind]
    prob.cal <- cal[["fitted.values"]]
  } else if (method == "iso") {
    cal <- isoreg(prob, y)
    prob.cal <- cal$yf
  } else {
    # FIXME: How to return output?
    prob.cut <- cut(prob, breaks = seq(from = 0, to = 1, length = nbins + 1), 
                    include.lowest = TRUE)
    prob.cal <- tapply(y, INDEX = prob.cut, FUN = mean)
  }
  probs <- data.frame("original" = prob, "calibrated" = prob.cal)
  structure(list("probs" = probs, "calibrater" = cal, "bs" = bs),
            class = "calibrate")
}


#' @rdname calibrate
#'
#' @export
print.calibrate <- function(x, ...) {
  cat("\nBrier score:", x$bs, "\n")
  cat("\nOriginal vs. calibrated probabilties:\n")
  print(head(x$probs, n = 5))
  cat("Omitting remaining", nrow(x$probs) - 5, "rows...")
  invisible(x)
}


#' @rdname calibrate
#'
#' @export
plot.calibrate <- function(x, refline = TRUE, refline.col = 2, 
                           refline.lty = "dashed", refline.lwd = 1, ...) {
  plot(x$probs[["original"]], x$probs[["calibrated"]], type = "l", 
       xlab = "Original probabilities", ylab = "Calibrated probabilities", ...)
  if (isTRUE(refline)) {
    abline(0, 1, col = refline.col, lty = refline.lty, lwd = refline.lwd)
    legend("topleft", legend = "Perfectly calibrated", lty = 2, col = refline.col,
           bty = "n")
  }  
  invisible()
}
