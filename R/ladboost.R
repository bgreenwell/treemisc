#' Gradient tree boosting with least absolute deviation (LAD) loss
#' 
#' A poor-man's implementation of stochastic gradient tree boosting with LAD 
#' loss.
#' 
#' @param X A data frame of only predictors.
#' 
#' @param y A vector of response values
#' 
#' @param ntree Integer specifying the number of trees to use in the ensemble.
#' 
#' @param shrinkage Numeric specifying the shrinkage factor.
#' 
#' @param depth Integer specifying the depth of each tree.
#' 
#' @param subsample Numeric specifying the proportion of the training data to
#' randomly sample before building each tree. Default is \code{0.5}.
#' 
#' @param init Numeric specifying the initial value to boost from. Defaults to
#' the median response (i.e., \code{median(y)}).
#' 
#' @return An object of class \code{"ladboost"} which is just a list with the 
#' following components:
#' \itemize{
#'   \item \code{trees} A list of length \code{ntree} containing the individual 
#'   \code{\link[rpart]{rpart}} tree fits.
#'   \item \code{shrinkage} The corresponding shrinkage parameter.
#'   \item \code{depth} The maximum depth of each tree.
#'   \item \code{subsample} The (row) subsampling rate.
#'   \item \code{init} The initial constant fit.
#' }
#' 
#' @note 
#' By design, the final model does not include the predictions from the initial
#' (constant) fit. So the constant is stored in the \code{init} component of the
#' returned output to be used later by \code{predict.ladboost()}.
#' 
#' @importFrom stats median
#'
#' @rdname ladboost
#' 
#' @export
#' 
#' @examples 
#' # Simulate data from the Friedman 1 benchmark problem
#' set.seed(1025)  # for reproducibility
#' trn <- gen_friedman1(500)  # training data
#' tst <- gen_friedman1(500)  # test data
#' 
#' # Gradient boosted decision trees
#' set.seed(1027)  # for reproducibility
#' bst <- ladboost(subset(trn, select = -y), y = trn$y, depth = 2)
#' pred <- predict(bst, newdata = tst)
#' mean((pred - tst$y) ^ 2)
ladboost <- function(X, y, ntree = 100, shrinkage = 0.1, depth = 6, 
                     subsample = 0.5, init = median(y)) {
  yhat <- rep(init, times = nrow(X))  # initialize fit
  trees <- vector("list", length = ntree)  # to store each tree
  ctrl <- rpart::rpart.control(cp = 0, maxdepth = depth, minbucket = 10)
  for (tree in seq_len(ntree)) {
    id <- sample.int(nrow(X), size = floor(subsample * nrow(X)))
    samp <- X[id, ]
    samp$pr <- sign(y[id] - yhat[id])  # use signed residual
    trees[[tree]] <- 
      rpart::rpart(pr ~ ., data = samp, control = ctrl)
    #---------------------------------------------------------------------------
    # Line search; update terminal node estimates using median 
    where <- trees[[tree]]$where  # terminal node assignments
    map <- tapply(samp$pr, INDEX = where, FUN = median)  # terminal node medians
    trees[[tree]]$frame$yval[where] <- map[as.character(where)]
    # trees[[tree]] <- partykit::as.party(trees[[tree]])
    # med <- function(y, w) median(y)
    # yhat <- yhat + shrinkage * partykit::predict.party(trees[[tree]], newdata = X, FUN = med)
    #---------------------------------------------------------------------------
    yhat <- yhat + shrinkage * predict(trees[[tree]], newdata = X)
  }
  res <- list("trees" = trees, "shrinkage" = shrinkage, "depth" = depth,
              "subsample" = subsample, "init" = init)
  class(res) <- "ladboost"
  res
}


#' Print method for \code{ladboost} objects
#' 
#' Print basic information about a fitted \code{"ladboost"} object.
#' 
#' @param x An object of class \code{"ladboost"}.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @rdname ladboost
#' 
#' @export
print.ladboost <- function(x, ...) {
  cat("Gradient tree boosting with least absolute deviation (LAD) loss", "\n\n")
  cat("            Initial fit:", x$init[1L], "\n")
  cat("        Number of trees:", length(x$trees), "\n")
  cat("Shrinkage/learning rate:", x$shrinkage, "\n")
  cat("     Maximum tree depth:", x$depth, "\n")
  cat("     Row subsample rate:", x$subsample, "\n")
}


#' Predict method for \code{ladboost} objects
#' 
#' Compute predictions from an \code{"ladboost"} object using new data.
#' 
#' @param object An object of class \code{"ladboost"}.
#' 
#' @param newdata Data frame of new observations for making predictions.
#' 
#' @param ntree Integer specifying the number of trees in the ensemble to use.
#' Defaults to using all the trees in the ensemble.
#'
#' @param individual Logical indicating whether or not to return the (shrunken)
#' predictions from each tree individually (\code{TRUE}) or the overall ensemble
#' prediction (\code{FALSE}). Default is \code{FALSE}.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#' 
#' @return A vector (\code{individual = TRUE}) or matrix 
#' (\code{individual = FALSE}) of predictions.
#'
#' @rdname ladboost
#' 
#' @export
predict.ladboost <- function(object, newdata, ntree = NULL, 
                             individual = FALSE, ...) {
  if (is.null(ntree)) {
    ntree <- length(object[["trees"]])
  }
  shrinkage <- object[["shrinkage"]]
  trees <- object[["trees"]][seq_len(ntree)]
  pmat <- sapply(trees, FUN = function(tree) {
    shrinkage * predict(tree, newdata = newdata)
    # med <- function(y, w) median(y)
    # shrinkage * partykit::predict.party(tree, newdata = newdata, FUN = med)
  })
  if (isTRUE(individual)) {
    pmat
  } else {
    rowSums(pmat) + object$init
  }
}