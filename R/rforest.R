#' Suppress randomForest() warning message
#' 
#' Suppresses a specific \code{\link[randomForest]{randomForest}} warning 
#' message regarding the response having "...five or fewer unique values."
#' 
#' @param expr Expression to evaluate.
#' 
#' @keywords internal
suppressRegressionWarning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if(any(grepl("The response has five or fewer unique values", w))) {
      invokeRestart("muffleWarning")
    }
  })
}


#' Random forest tree
#' 
#' Fits a single \code{\link[randomForest]{randomForest}} tree.
#' 
#' @param X A data frame or a matrix of predictors.
#' 
#' @param y Numeric vector of response value. For binary outcomes, \code{y} 
#' should be mapped to \{0, 1\}. Note that multiclass outcomes are not supported.
#' 
#' @param rotate Logical indicating whether or not to randomly rotate the 
#' feature values prior to fitting the tree. Default is \code{FALSE} which
#' results in a traditional random forest tree.
#' 
#' @param ... Optional arguments to be passed on to 
#' \code{\link[randomForest]{randomForest}}.
#' 
#' @keywords internal
rftree <- function(X, y, rotate = FALSE, ...) {
  if (isTRUE(rotate)) {
    numeric.cols <- if (is.data.frame(X)) {
      sapply(X, FUN = is.numeric)
    } else {
      seq_len(ncol(X))
    }
    nX <- data.matrix(X)[, numeric.cols]  # numeric cols of X only
    R <- rrm(ncol(nX))  # random rotation matrix
    X[, numeric.cols] <- nX %*% R
  }
  # nodesize <- if (is.factor(y)) 10 else 5
  suppressRegressionWarning(
    tree <- randomForest::randomForest(X, y = y, ntree = 1, #nodesize = nodesize, 
                                       ...)
  )
  if (isTRUE(rotate)) {
    attr(tree, which = "rotation.matrix") <- R
  }
  res <- list("description" = "A single `randomForest()` tree", "rfo" = tree)
  class(res) <- c("rftree", class(tree))
  res
}


#' Model predictions
#' 
#' Compute predictions from an \code{"rftree"} object.
#' 
#' @param object An object of class \code{"rftree"}.
#' 
#' @param newX Data frame or matrix of new feature values.
#' 
#' @returns Has the same return type as 
#' \code{\link[randomForest]{predict.randomForest}}.
#' 
#' @keywords internal
predict.rftree <- function(object, newX) {
  if (!is.null(R <- attr(object, which = "rotation.matrix"))) {
    numeric.cols <- if (is.data.frame(newX)) {
      sapply(newX, FUN = is.numeric)
    } else {
      seq_len(ncol(newX))
    }
    nX <- data.matrix(newX)[, numeric.cols]  # numeric cols of X only
    newX[, numeric.cols] <- nX %*% R
  }
  rfo <- object[["rfo"]]  # grab `randomForest` object
  if (rfo[["type"]] == "classification") {
    # randomForest:::predict.randomForest(object, newdata = newX, type = "prob")
    predict(rfo, newdata = newX, type = "prob")
  } else {
    # randomForest:::predict.randomForest(object, newdata = newX, type = "response")
    predict(rfo, newdata = newX, type = "response")
  }
}


#' Random forest
#' 
#' A poor man's implementation of random forest (Breiman, 2001) with the option
#' to incorporate random rotations as described in Blaser and Fryzlewicz (2016).
#' 
#' @param X A data frame or a matrix of predictors.
#' 
#' @param y Numeric vector of response value. For binary outcomes, \code{y} 
#' should be mapped to \{0, 1\}. Note that multiclass outcomes are not supported.
#' 
#' @param mtry Integer specifying the number of variables randomly sampled as 
#' candidates splitters at each node in a tree. Note that the default values are 
#' different for classification (\code{floor(sqrt(p))} where \code{p} is number 
#' of columns of \code{X}) and regression \code{floor(p/3)}).
#' 
#' @param ntree Integer specifying the number of trees to grow. This should not 
#' be set to too small a number, to ensure that every input row gets predicted 
#' at least a few times. Default is \code{500}.
#' 
#' @param rotate Logical indicating whether or not to randomly rotate the 
#' feature values prior to fitting each tree. Default is \code{FALSE} which
#' results in a traditional random forest.
#' 
#' @param ... Optional arguments to be passed on to 
#' \code{\link[randomForest]{randomForest}} (e.g., \code{nodesize = 10}).
#' 
#' @returns An object of class \code{"rforest"}, which is essentially a list of
#' \link{rftree} objects.
#' 
#' @export
#' 
#' @references 
#' Breiman, Leo. (2001), Random Forests, Machine Learning 45(1), 5-32.
#' 
#' Rico Blaser and Piotr Fryzlewicz. Random rotation ensembles. Journal of 
#' Machine Learning Research, 17:1–26, 2016.
rforest <- function(X, y, mtry = NULL, ntree = 500, rotate = FALSE, ...) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package \"randomForest\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  if (is.null(mtry)) {
    mtry <- if (is.factor(y)) {
      floor(sqrt(ncol(X)))
    } else {
      max(floor(ncol(X) / 3), 1)
    }
  }
  trees <- lapply(seq_len(ntree), FUN = function(i) {
    rftree(X, y = y, mtry = mtry, rotate = rotate, ...)
  })
  class(trees) <- "rforest"
  trees
}

# list2array <- function(x) {
#   array(unlist(x), dim = c(nrow(x[[1L]]), ncol(x[[1L]]), length(x)))
# }


#' Random forest predictions
#' 
#' Compute predictions from an \code{"rftree"} object.
#' 
#' @param object An object of class \code{"rftree"}.
#' 
#' @param newX Data frame or matrix of new feature values.
#' 
#' @param predict.all Logical indicating whether or not to return predictions
#' for each individual tree. Default is \code{FALSE}, which only returns the 
#' aggregated predictions.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#' 
#' @return A vector of predictions. For binary outcomes coded as 0/1, the 
#' predictions represent Pr(Y = 1).
#' 
#' @export
predict.rforest <- function(object, newX, predict.all = FALSE, ...) {
  p <- lapply(object, FUN = function(tree) {
    # predict.rftree(tree, newX = newX)
    predict.rftree(tree, newX = newX)
  })
  p <- do.call(cbind, args = p)
  if (isTRUE(predict.all)) {
    p
  } else {
    rowMeans(p)
  }
}
