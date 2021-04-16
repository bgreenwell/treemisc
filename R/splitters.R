#' Split variables
#'
#' Extract the variables used as splitters in a tree.
#'
#' @param object An object of class \code{"rpart"} or \code{"BinaryTree"}.
#'
#' @return The number of splits contained in \code{object}.
#'
#' @rdname splitters
#' 
#' @export
splitters <- function(object) {
  UseMethod("splitters")
}


#' @rdname splitters
#' 
#' @export
splitters.rpart <- function(object) {
  if (is_root(object)) {
    "<root>"
  } else {
    levs <- unique(as.character(object$frame$var))
    levs[levs != "<leaf>"]
  }
}


#' @rdname splitters
#' 
#' @export
splitters.BinaryTree <- function(object) {

  if (is_root(object)) {
    "<root>"
  } else {
  
    # Capture printed tree in a character string
    tree_path <- utils::capture.output(object@tree)

    # Pattern we are looking for
    pattern <- "^ *([0-9]*\\) )(.*)( [<>]).*$"

    # Extract the names of the splitting variables
    split.vars <- unique(gsub(pattern, "\\2", tree_path[grep(pattern, tree_path)]))
    x.vars <- all.vars(object@data@formula$input)

    # Sanity check
    if (all(split.vars %in% x.vars)) {
      split.vars
    } else {
      stop(paste0("Unexpected split variable(s) found: ",
                  paste(split.vars[!(split.vars %in% x.vars)], collapse = ", "),
                  "."))
    }
    
  }

}


#' @rdname splitters
#' 
#' @export
splitters.train <- function(object) {
  splitters(object$finalModel)
}
