#' Is the tree just a root node?
#'
#' Tests if an \code{"rpart"} or \code{"BinaryTree"} object contains only a root
#' node (i.e., no splits).
#'
#' @param object An object that inherits from class \code{"rpart"} or
#'   \code{"BinaryTree"}.
#'   
#' @param ... Additional optional arguments. Currently ignored.
#'
#' @return Returns \code{TRUE} if \code{object} does not contain any splits and
#'   \code{FALSE} otherwise.
#'
#' @rdname is_root
#'
#' @export
is_root <- function(object, ...) {
  UseMethod("is_root")
}


#' @rdname is_root
#'
#' @export
is_root.rpart <- function(object, ...) {
  # length(unique(object$where)) == 1
  is.null(object$splits) || nrow(object$splits) == 0  # no splits implies root
}


#' @rdname is_root
#'
#' @export
is_root.BinaryTree <- function(object, ...) {
  length(unique(party::where(object))) == 1
}


#' @rdname is_root
#'
#' @export
is_root.train <- function(object, ...) {
  is_root(object$finalModel)
}
