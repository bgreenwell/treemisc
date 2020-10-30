#' Number of leaves/terminal nodes
#'
#' Compute the number of leaves (i.e., the number of temrinal nodes) of a tree.
#'
#' @param object An object of class \code{"rpart"} or \code{"BinaryTree"}.
#'
#' @return Returns \code{TRUE} if \code{object} does not contain any splits and
#' \code{FALSE} otherwise.
#'
#' @rdname nleaves
#' 
#' @export
nleaves <- function(object) {
  UseMethod("nleaves")
}


#' @rdname nleaves
#' 
#' @export
nleaves.rpart <- function(object) {
  length(unique(object$where))
}


#' @rdname nleaves
#' 
#' @export
nleaves.BinaryTree <- function(object) {
  length(unique(party::where(object)))
}


#' @rdname nleaves
#' 
#' @export
nleaves.train <- function(object) {
  nleaves(object$finalModel)
}
