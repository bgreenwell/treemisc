#' Two-way interactions
#' 
#' Computes Friedman's H-statistic (Friedman & Popescu, 2008) for all pairwise 
#' interaction effects in a \code{\link[gbm]{gbm}} model.
#' 
#' @param object A \code{\link[gbm]{gbm}} object.
#' 
#' @param data Data frame containing the original training data (or 
#' representative sample thereof).
#' 
#' @param var.names Character string specifying the predictor names to consider.
#' 
#' @param n.trees Integer specifying the number of trees to use.
#' 
#' @references 
#' Friedman, J. H., & Popescu, B. E. (2008). Predictive Learning via Rule 
#' Ensembles. The Annals of Applied Statistics, 2(3), 916–954. 
#' http://www.jstor.org/stable/30245114
#' 
#' @importFrom utils combn
#' 
#' @export
gbm_2way <- function(object, data, var.names = object$var.names, 
                     n.trees = object$n.trees) {
  # Check for gbm package
  if (!requireNamespace("gbm", quietly = TRUE)) {
    stop("Package \"gbm\" needed for this function to work. ",
         "Please install it.", call. = FALSE)
  }
  var.pairs <- combn(var.names, m = 2, simplify = TRUE)
  h <- combn(var.names, m = 2, simplify = TRUE, FUN = function(x) {
    gbm::interact.gbm(object, data = data, i.var = x, n.trees = n.trees)
  })
  res <- as.data.frame(t(var.pairs))
  res$h <- h
  names(res) <- c("var1", "var2", "h")
  res[order(h, decreasing = TRUE), ]
}
