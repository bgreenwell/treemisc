#' Random rotation matrix
#' 
#' Generates a random rotation matrix as described in Blaser and Fryzlewicz 
#' (2016).
#' 
#' @param n Integer specifying the dimension of the resulting (square) random 
#' rotation matrix.
#' 
#' @return An \code{n}-by-\code{n} matrix (i.e., an object of class 
#' \code{c("matrix" "array")}.
#' 
#' @references 
#' Rico Blaser and Piotr Fryzlewicz. Random rotation ensembles. Journal of 
#' Machine Learning Research, 17:1–26, 2016.
#' 
#' @source https://www.jmlr.org/papers/volume17/blaser16a/blaser16a.pdf.
#' 
#' @importFrom stats rnorm
#' 
#' @export
#' 
#' @examples 
#' (R <- rrm(3))
#' det(R)  # determinant should always be +1
#' solve(R)  # R^{-1} = R'
#' t(R)  # R^{-1} = R'
rrm <- function(n) {
  QR <- qr(matrix(rnorm(n ^ 2), ncol = n))  # A = QR
  M <- qr.Q(QR) %*% diag(sign(diag(qr.R(QR))))
  if (det(M) < 0) M[, 1L] <- -M[, 1L]  # det(M) = +1
  M
}

# rescale <- function(X) {  # rescale each column to be in [0, 1]
#   apply(X, MARGIN = 2, FUN = function(x) (x - min(x)) / diff(range(x)))
# }