#' Swiss banknote data
#'
#' Measurements from 200 Swiss 1000-franc banknotes: 100 genuine and 100 counterfeit.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @format A data frame with 200 rows and 7 columns.
#' 
#' @details 
#' 
#' \describe{
#' 
#' \item{length}{The length of the bill in mm.}
#' \item{left}{The length of the left edge in mm.}
#' \item{right}{The length of the right edge in mm.}
#' \item{bottom}{The length of the botttom edge in mm.}
#' \item{top}{The length of the top edge in mm.}
#' \item{diagonal}{The length of the diagonal in mm.}
#' \item{class}{Integer specifying whether or not the bill was genuine
#' (\code{class = 0}) or counterfeit (\code{class = 1}).}
#' 
#' }
#'
#' @name banknote
#'
#' @source
#' Flury, B. and Riedwyl, H. (1988). \emph{Multivariate Statistics: A practical 
#' approach}. London: Chapman & Hall.
NULL
