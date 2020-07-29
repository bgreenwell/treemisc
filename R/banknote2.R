#' Swiss banknote data (UCI version)
#'
#' Data were extracted from images that were taken from genuine \code{class = 1} 
#' and forged \code{class = 0} banknote-like specimens. For digitization, an 
#' industrial camera usually used for print inspection was used. The final 
#' images contained 400 x 400 pixels. Due to the object lens and distance to the 
#' investigated object, gray-scale pictures with a resolution of about 660 dpi 
#' were gained. Wavelet transformation tools were used to extract features from 
#' the images.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @format A data frame with 1372 rows and 5 variables.
#' 
#' @details 
#' 
#' \describe{
#' 
#' \item{vow}{Variance of the wavelet transformed image (continuous)}
#' \item{sow}{Skewness of the wavelet transformed image (continuous)}
#' \item{kow}{Kurtosis of the wavelet transformed image (continuous)}
#' \item{eoi}{Entropy of the image (continuous)}
#' \item{class}{Integer specifying whether or not the specimen was genuine
#' (\code{class = 1}) or forged (\code{class = 0}).}
#' 
#' }
#'
#' @name banknote2
#'
#' @source
#' Dua, D. and Graff, C. (2019). \emph{UCI Machine Learning Repository}
#' [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, 
#' School of Information and Computer Science.
NULL
