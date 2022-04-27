#' Importance sampled learning ensemble
#' 
#' Uses \code{\link[glmnet]{glmnet}} or \code{\link[glmnet]{cv.glmnet}} to fit 
#' the entire LASSO path for post-processing the individual trees of a 
#' tree-based ensemble (e.g., a random forest).
#' 
#' @param X A matrix of training predictions, one column for each tree in the
#' ensemble.
#' 
#' @param y Vector of training response values. See \code{\link[glmnet]{glmnet}} 
#' for acceptable values (e.g., numeric for \code{family = "gaussian"}).
#' 
#' @param newX Same as argument \code{X}, but should correspond to an 
#' independent test set. (Required whenever \code{cv = FALSE}.)
#' 
#' @param newy Same as argument \code{y}, but should correspond to an 
#' independent test set. (Required whenever \code{cv = FALSE}.)
#' 
#' @param cv Logical indicating whether or not to use n-fold cross-validation.
#' Default is \code{FALSE} (Must be \code{TRUE} whenever \code{newX = NULL} and 
#' \code{newy = NULL}.)
#' 
#' @param nfolds Integer specifying the number of folds to use for 
#' cross-validation (i.e., whenever \code{cv = TRUE}). Default is \code{FALSE}.
#' 
#' @param family The model fitting family (e.g., \code{family = "binomial"} for
#' binary outcomes); see \code{\link[glmnet]{glmnet}} for details on acceptable 
#' values.
#' 
#' @param loss Optional character string specifying the loss to use for 
#' n-fold cross-validation. Default is \code{"default"}; see
#' \code{\link[glmnet]{cv.glmnet}} for details. (Only used when 
#' \code{cv = TRUE}.)
#' 
#' @param offset Optional value for the offset. Default is \code{NULL}, which
#' corresponds to no offset.
#' 
#' @param ... Additional (optional) arguments to be passed on to 
#' \code{\link[glmnet]{glmnet}} (e.g., \code{intercept = FALSE}).
#' 
#' @return A list with two components:
#' \describe{
#'   \item{\code{results}}{A data frame with one row for each value of lambda in
#'   the coefficient path and columns giving the corresponding number of 
#'   trees/non-zero coefficients, error metric(s), and the corresponding value 
#'   of lambda.}
#'   \item{lasso.fit}{The fitted \code{\link[glmnet]{glmnet}} or 
#'   \code{\link[glmnet]{cv.glmnet}} object.}
#' }
#' 
#' @export
isle_post <- function(X, y, newX = NULL, newy = NULL, cv = FALSE, nfolds = 5, 
                      family = NULL, loss = "default", offset = NULL, ...) {
  
  # TODO: Add better checks (e.g., user needs to supply test set or use CV).

  # Check for dependencies
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package \"glmnet\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  
  # Check `family` argument
  if (is.null(family)) {
    stop("Please specify a value for the `family` argument; see ",
         "`?glmnet::glmnet` for details.", call. = FALSE)
  }
  
  # Fit the entire LASSO path and collect test set or CV info
  if (isTRUE(cv)) {  # use n-fold cross-validation
    fit <- glmnet::cv.glmnet(X, y = y, lower.limits = 0, standardize = FALSE, 
                             offset = offset, nfolds = nfolds, 
                             type.measure = loss, family = family, ...)
    # Use the mean cross-validated error - a vector of length `length(lambda)`
    res <- data.frame("ntree" = fit$nzero, "error" = fit$cvm, 
                      "lambda" = fit$lambda)
    names(res)[names(res) == "error"] <- names(fit$name)
  } else {  # use an independent test set
    fit <- glmnet::glmnet(X, y = y, lower.limits = 0, standardize = FALSE, 
                          offset = offset, family = family, ...)
    # Assess performance of fit using an independent test set
    perf <- glmnet::assess.glmnet(fit, newx = newX, newy = newy, 
                                 family = family, newoffset = offset)
    perf.mat <- do.call(cbind, args = perf)
    res <- as.data.frame(cbind("ntree" = fit$df, perf.mat, 
                               "lambda" = fit$lambda))
  }

  # Return results ordered by number of trees/non-zero coefficients
  list("results" = res[order(res[["ntree"]], decreasing = FALSE), ],
       "lasso.fit" = fit)
  
}