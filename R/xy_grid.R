#' Create a Cartesian product from evenly spaced values of two variables
#'
#' Create a Cartesian product from evenly spaced values of two variables.
#'
#' @param x Either a numeric vector (if argument \code{y} is also specified),
#' a matrix-like object (e.g., a data frame), or two-variable formula of the
#' form \code{y ~ x}.
#'
#' @param y A numeric vector representing the second variable (only required if
#' \code{x} is a numeric vector).
#'
#' @param formula A two-variable formula of the form \code{y ~ x}. The response
#' (i.e., the variable on the left side of the formula) corresponds to the
#' second column of the output.
#'
#' @param data A data frame containing the variables specified in
#' \code{x} if \code{x} is a formula.
#'
#' @param grid.resolution Integer specifying the number of equally-spaced values
#' to use for each numeric variable. For example, if \code{grid.resolution = k}, 
#' then the final data frame will have \code{k^2} rows (formed by a Cartesian
#' product).
#'
#' @param col.names Optional vector of column names to use for the output
#' whenever both \code{x} and \code{y} are supplied.
#' 
#' @param ... Additional (optional) arguments. (Currently ignored.)
#'
#' @return A data frame representing the Cartesian product between equally
#' spaced values from each variable.
#' 
#' @rdname xy_grid
#' 
#' @export
#'
#' @examples
#' x1 <- 1:3
#' x2 <- letters[1L:3L]
#' xy_grid(x1, x2, gr = 3, col.names = c("x1", "x2"))  # will have 3^2=9 rows
#' xy_grid(m <- cbind(x1, x2), gr = 3)     # equivalent
#' xy_grid(d <- as.data.frame(m), gr = 3)  # equivalent
#' xy_grid(x2 ~ x1, data = d, gr = 3)      # equivalent
xy_grid <- function(x, ...) {
  UseMethod("xy_grid")
}


#' @rdname xy_grid
#' 
#' @export
xy_grid.default <- function(x, y, grid.resolution = 51, col.names = NULL, ...) {
  # TODO: Let x and/or y be non-numeric
  x.seq <- if (is.numeric(x)) {
    seq(from = min(x), to = max(x), length = grid.resolution)
  } else {
    sort(unique(x))
  }
  y.seq <- if (is.numeric(y)) {
    seq(from = min(y), to = max(y), length = grid.resolution)
  } else {
    sort(unique(y))
  }  
  cp <- expand.grid(x.seq, y.seq)  # Cartesian product
  names(cp) <- if (is.null(col.names)) c("x", "y") else col.names
  cp
}


#' @rdname xy_grid
#' 
#' @export
xy_grid.formula <- function(x, data, grid.resolution = 51, ...) {
  m <- match.call(expand.dots = FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, envir = eframe)
  if (is.matrix(md)) {
    md <- as.data.frame(data)
  }
  var.names <- all.vars(x)
  xy_grid.default(md[[var.names[2L]]], y = md[[var.names[1L]]],
                  grid.resolution = grid.resolution,
                  col.names = var.names[2L:1L])
}


#' @rdname xy_grid
#' 
#' @export
xy_grid.matrix <- function(x, grid.resolution = 51, ...) {
  xy_grid.default(x[, 1L], y = x[, 2L], grid.resolution = grid.resolution,
                  col.names = colnames(x)[1L:2L])
}


#' @rdname xy_grid
#' 
#' @export
xy_grid.data.frame <- function(x, grid.resolution = 51, ...) {
  xy_grid.default(x[[1L]], y = x[[2L]], grid.resolution = grid.resolution,
                  col.names = names(x)[1L:2L])
}
