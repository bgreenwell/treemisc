#' Add decision boundary to a scatterplot.
#' 
#' Adds the decision boundary from a classification model (binary or multiclass)
#' to an existing scatterplot.
#' 
#' @param model The associated model object.
#' 
#' @param train Data frame of training observations.
#' 
#' @param y Character string giving the name of the outcome variable in 
#' \code{train}.
#' 
#' @param x1 Character string giving the name of the predictor in \code{train} 
#' that corresponds to the x-axis.
#' 
#' @param x2 Character string giving the name of the predictor in \code{train} 
#' that corresponds to the y-axis.
#' 
#' @param pfun Optional prediction wrapper that returns a vector of predicted
#' predicted class labels. It must have exactly two arguments: \code{object} and
#' \code{newdata}.
#' 
#' @param grid.resolution Integer specifying the resolution of the contour plot.
#' Default is \code{100}.
#' 
#' @param ... Additional optional arguments to be passed on to 
#' \code{\link[graphics]{contour}}.
#' 
#' @note Based on a function written by Michael Hahsler; see
#' https://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html.
#' 
#' @importFrom graphics contour
#' 
#' @importFrom grDevices extendrange
#' 
#' @importFrom stats predict
#' 
#' @rdname decision_boundary
#' 
#' @export
decision_boundary <- function(model, train, y, x1, x2, pfun, 
                              grid.resolution = 100, ...) {
  UseMethod("decision_boundary")
}


#' @rdname decision_boundary
#' 
#' @export
decision_boundary.default <- function(model, train, y, x1, x2, pfun = NULL, 
                                      grid.resolution = 100, ...) {
  k <- length(unique(train[[y]]))
  train <- train[, c(x1, x2)]
  r <- sapply(train, extendrange, f = 0.1)
  xs <- seq(r[1L, 1L], r[2L, 1L], length.out = grid.resolution)
  ys <- seq(r[1L, 2L], r[2L, 2L], length.out = grid.resolution)
  g <- cbind(rep(xs, each = grid.resolution), rep(ys, times = grid.resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  if (is.null(pfun)) {
    p <- get_class_labels(model, newdata = g)
  } else {
    p <- pfun(model, newdata = g)
  }
  z <- matrix(as.integer(as.factor(p)), nrow = grid.resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE, 
          levels = (1L:(k - 1)) + 0.5, ...)
  invisible(z)
}


#' @keywords internal
get_class_labels <- function(object, newdata) {
  UseMethod("get_class_labels")
}


#' @keywords internal
get_class_labels.default <- function(object, newdata) {
  stop("Cannot obtain predicted class labels from objects of class ",
       "\"", class(object)[[1L]], "\"")
}


#' @keywords internal
get_class_labels.constparty <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}


#' @keywords internal
get_class_labels.lda <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")$class
}


#' @keywords internal
get_class_labels.qda <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")$class
}


#' @keywords internal
get_class_labels.rpart <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")
}
