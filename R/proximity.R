#' Proximity matrix
#' 
#' Compute proximity matrix from a random forest or matrix of terminal node
#' assignments (one row for each observation and one column for each tree in the 
#' forest).
#' 
#' @param x Either a \code{\link[ranger]{ranger}} object or a matrix of matrix 
#' of terminal node assignments (one row for each observation and one column for 
#' each tree in the forest).
#' 
#' @param data Optional data frame passed on to 
#' \code{\link[ranger]{predict.ranger}}. It's a good idea to pass the data via 
#' this argument whenever \code{x} is a \code{\link[ranger]{ranger}} object. If
#' \code{NULL} (the default) it will be looked for recursively.
#' 
#' @param sparse Logical or \code{NULL} indicating whether or not the resulting 
#' matrix should be sparse. If \code{NULL} (the default) it is made sparse when 
#' more than half the entries are 0.
#' 
#' @param upper Logical indicating whether or not to return the proximities in
#' upper triangular form (\code{TRUE}) or as a symmetric matrix (\code{FALSE}).
#' Default is \code{TRUE}.
#' 
#' @param ... Additional optional argument. (Currently ignored.)
#' 
#' @useDynLib treemisc, .registration = TRUE
#' 
#' @rdname proximity
#' 
#' @export
proximity <- function(x, ...) {
  UseMethod("proximity")
}


#' @rdname proximity
#' 
#' @export
proximity.default <- function(x, sparse = NULL, upper = TRUE, ...) {
  stop("x should be a \"ranger\" object or a matrix.")
}


#' @rdname proximity
#' 
#' @export
proximity.matrix <- function(x, sparse = NULL, upper = TRUE, ...) {
  prox <- proximity_cpp(x)
  diag(prox) <- 1
  if (isFALSE(upper)) {
    prox <- t(prox) + prox
    diag(prox) <- 1
  } 
  if (!isFALSE(sparse)) {
    prox <- Matrix::Matrix(prox, sparse = sparse)
  }
  prox
}


#' @rdname proximity
#' 
#' @export
proximity.ranger <- function(x, data = NULL, sparse = NULL, upper = TRUE, ...) {
  
  # Error message to display when training data cannot be extracted form x
  msg <- paste0(
    "The training data could not be extracted from object. Please supply the ",
    "raw training data using the `train` argument in the call to `partial()`."
  )
  
  # Try to extract training data if not provided
  if (is.null(data)) {
    
    # Grab the call
    mcall <- tryCatch(stats::getCall(x), error = function(e) {
      stop(msg, call. = FALSE)
    })
    
    # Make sure all of the arguments are specified by their full names
    f <- eval(mcall[[1L]], envir = env)
    if (!is.primitive(f)) {
      mcall <- match.call(f, call = mcall)
    }
    
    # Grab the data component
    n <- 1
    while(length(env) != 0) {
      data <- tryCatch(eval(mcall$data, envir = env), error = function(e) {
        NULL
      })
      if (!is.null(data) || identical(env, globalenv())) {
        break
      }
      env <- parent.frame(n)  # inspect calling environment
      n <- n + 1
    }
    if (is.null(data)) {
      stop(msg, call. = FALSE)
    } else {
      if (!(is.data.frame(data))) {
        if (is.matrix(data) || is.list(data)) {
          data <- as.data.frame(data)
        } else {
          stop(msg, call. = FALSE)
        }
      }
    }
  }
  
  # Compute terminal node assignment
  p <- stats::predict(x, data = data, predict.all = TRUE, 
                      type = "terminalNodes")
  
  # Compute proximity matrix
  proximity.matrix(p$predictions, sparse = sparse)

}
