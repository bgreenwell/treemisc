#' Number of splits
#'
#' Compute the number of splits of a tree.
#'
#' @param object An object of class \code{"rpart"} or \code{"BinaryTree"}.
#'
#' @return The number of splits contained in \code{object}.
#'
#' @export
nsplits <- function(object) {
  num.leaves <- nleaves(object)
  if (num.leaves == 1) {
    0
  } else if (num.leaves == 2) {
    1
  } else {
    num.leaves - 1
  }
}
