#' Gaussian mixture data
#' 
#' Load the Gaussian mixture data from Hastie et al. (2009, Sec. 2.3.3).
#' 
#' @return A list with the following components:
#' \describe{ 
#'   \item{x}{200 x 2 matrix of training predictors.}
#'   \item{y}{class variable; logical vector of 0s and 1s - 100 of each.}
#'   \item{xnew}{matrix 6831 x 2 of lattice points in predictor space.}
#'   \item{prob}{vector of 6831 probabilities (of class TRUE) at each lattice point.}
#'   \item{marginal}{marginal probability at each lattice point.}
#'   \item{px1}{69 lattice coordinates for \code{x1}.}
#'   \item{px2}{99 lattice values for \code{x2}  (69*99=6831).}
#'   \item{means}{20 x 2 matrix of the mixture centers, first ten for one class, next ten for the other.}
#' }
#' 
#' @references 
#' Trevor Hastie, Robert. Tibshirani, and Jerome Friedman. The Elements of 
#' Statistical Learning: Data Mining, Inference, and Prediction, Second Edition. 
#' Springer Series in Statistics. Springer-Verlag, 2009.
#' 
#' @source 
#' https://web.stanford.edu/~hastie/ElemStatLearn/datasets/mixture.example.info.txt
#' 
#' @export
#' 
#' @examples 
#' eslmix <- load_eslmix()
#' names(eslmix)
load_eslmix <- function() {
  readRDS(system.file("extdata", "eslmix.rds", package = "treemisc"))
}
