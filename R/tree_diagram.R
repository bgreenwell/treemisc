#' Tree diagram
#' 
#' Draw a decision tree diagram from a fitted \code{\link[rpart]{rpart}} model.
#' 
#' @param object An \code{\link[rpart]{rpart}} object.
#' 
#' @param prob Logical indicating whether or not to display leaf node probability
#' estimates for classification trees; default is \code{TRUE}.
#' 
#' @param box.palette Chjaracter string specifying the palette to use for 
#' coloring the nodes; see \code{\link[rpart.plot]{rpart.plot}} for details.
#' 
#' @param ... Additional optional argumebts to be passed onto 
#' \code{\link[rpart.plot]{rpart.plot}}.
#' 
#' @returns No return value, only called for side effects; in this case, a 
#' decision tree diagram constructed by a simple call to 
#' \code{\link[rpart.plot]{rpart.plot}}.
#' 
#' @note This function is just a light wrapper around 
#' \code{\link[rpart.plot]{rpart.plot}} and was used to produce several of the
#' tree diagrams in the accompanying book.
#' 
#' @export
tree_diagram <- function(object, prob = TRUE, box.palette = "BuOr", ...) {
  # Check for dependency
  if (!requireNamespace("rpart.plot", quietly = TRUE)) {
    stop("The \"rpart.plot\" package is required for this function to work. ",
         "Please install it from CRAN.", call. = FALSE)
  }
  extra <- if (object$method == "class") {
    if (isTRUE(prob)) 104 else 101
  } else {
    "auto"
  }
  rpart.plot::rpart.plot(object, type = 2, extra = extra, nn = TRUE,
                         left = FALSE, box.palette = box.palette, ...)
}