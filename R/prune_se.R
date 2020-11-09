#' Prune an \code{rpart} object
#' 
#' Prune an \code{rpart} object using the standard error (SE) of the 
#' cross-validation results. 
#' 
#' @param object An object that inherits from class \code{"rpart"}.
#' 
#' @param prune Logical indicating whether or not to return the pruned decision
#' tree. Default is \code{TRUE}. If \code{FALSE}, the optimal value of the 
#' cost-complexity parameter is returned instead.
#' 
#' @param se Numeric specifying the number of standard errors to use when 
#' pruning the tree. Default is \code{1}, which corresponds to the 1-SE rule
#' described in Breiman et al. (1984).
#' 
#' @return Either an object that inherits from class \code{"rpart"} (ideally, 
#' one that's been simplified using cost-complexity pruning with the 1-SE rule)
#' or a numeric value representing the cost-complexity parameter to use for 
#' pruning.
#' 
#' @seealso \code{\link[rpart]{prune}}
#' 
#' @references 
#' Breiman, L., Friedman, J., and Charles J. Stone, R. A. O. (1984). 
#' Classification and Regression Trees. The Wadsworth and Brooks-Cole 
#' statistics-probability series. Taylor & Francis.
#' 
#' @export
prune_se <- function(object, prune = TRUE, se = 1) {
  if (!inherits(object, what = "rpart")) {
    stop("Not a legitimate \"rpart\" object.")
  }
  p.rpart <- object$cptable
  if (ncol(p.rpart) < 5L) {
    stop("The fitted tree object does not contain cross-validation results.", 
         call. = FALSE)
  }
  xstd <- p.rpart[, 5L]
  xerror <- p.rpart[, 4L]
  cp0 <- p.rpart[, 1L]
  minpos <- min(seq_along(xerror)[xerror == min(xerror)])
  if (se == 0) {
    cp <- cp0[minpos][1L]
  } else {
    min.xerror.se <- (xerror + se * xstd)[minpos]
    cp <- cp0[xerror < min.xerror.se][1L]
  }
  if (isTRUE(prune)) {
    rpart::prune(object, cp = cp)
  } else {
    cp
  }
}
