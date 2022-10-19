#' Generate GUIDE input files
#' 
#' Just a simple helper function I found useful while using the GUIDE terminal 
#' application (http://pages.stat.wisc.edu/~loh/guide.html). It creates two 
#' input text files required by GUIDE: a data file and description file.
#'   
#' @param data A data frame containing the training data.
#' 
#' @param path Character string specifying the full path to where the GUIDE 
#' input files will be written to. If the given path does not exist, it will be
#' created automatically using \code{dir.create()}.
#' 
#' @param dv Character string specifying which column represents the target/
#' dependent variable.
#' 
#' @param var.roles A named character vector specifying the role of each column.
#' 
#' @param na Character string specifying the missing value indicator.
#' 
#' @param file.name Character string giving the file name (or prefix) to use for
#' the generated input files. If \code{NULL}, the default, it will be parsed 
#' from the \code{data} argument.
#' 
#' @param data.loc Character string specifying the the full path to the data  
#' input file, which is used for the first line of the generated description 
#' file. If \code{NULL}, the default, it will be determined automatically by 
#' \code{path} and \code{file.name}. This is useful if the data input file does 
#' not reside in the same directory as the GUIDE executable.
#' 
#' @param verbose Logical indicating whether or not to print progress 
#' information.
#' 
#' @returns No return value, only called for side effects; in this case, two
#' text file are created for consumption by the GUIDE terminal application
#' 
#' @note 
#' This function assumes that the GUIDE executable is located in the same 
#' directory specified by the `path` argument. For details, see the official 
#' software manual for GUIDE: 
#' \url{http://pages.stat.wisc.edu/~loh/treeprogs/guide/guideman.pdf}. This 
#' function has only been tested on GUIDE v38.0.
#' 
#' @importFrom utils write.csv
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # New York air quality measurements
#' aq <- airquality
#' aq <- aq[!is.na(aq$Ozone), ]  # remove rows with missing response values
#' 
#' # Default variable roles
#' guide_setup(aq, path = "some/path/aq", dv = "Ozone")
#' 
#' # User specified variable roles
#' var.roles <- c("Ozone" = "d", "Solar.R" = "n", "Wind" = "n", "Temp" = "c",
#'                "Month" = "p", "Day" = "p")
#' guide_setup(aq, path = "some/path/aq", var.roles = var.roles)
#' }
guide_setup <- function(data, path, dv = NULL, var.roles = NULL, na = "NA", 
                        file.name = NULL, data.loc = NULL, verbose = FALSE) {
  
  # FIXME: What about file names and NA flags with non-alphanumeric characters?
  # They will need to be quoted.
  
  # Check directory
  if (!dir.exists(path)) {
    dir.create(path)
  }
  
  # Inititialize data and description files
  if (is.null(file.name)) {
    file.name <- deparse(substitute(data))
  }
  if (isTRUE(verbose)) {
    message("Writing data file to ", 
            paste0(path, "/", paste0(file.name, ".txt")), "...")
  }
  write.csv(data, file = paste0(path, "/", paste0(file.name, ".txt")), na = na, 
            row.names = FALSE)
  
  if (isTRUE(verbose)) {
    message("Writing description file to ", 
            paste0(path, "/", paste0(file.name, "_desc.txt")), "...")
  }
  file.conn <- file(paste0(path, "/", paste0(file.name, "_desc.txt")), 
                    open = "w")
  
  # Write out relative path to training data, NA indicator, and starting line
  if (is.null(data.loc)) {
    writeLines(paste0(file.name, ".txt"), con = file.conn)
  } else {
    writeLines(sprintf('"%s"', data.loc), con = file.conn)
  }
  writeLines(na, con = file.conn)
  writeLines("2", con = file.conn)
  
  # Write out variable roles
  if (is.null(var.roles)) {  # default variable roles
    var.names <- names(data)
    var.roles <- sapply(data, FUN = class)
    var.roles <- ifelse(var.roles %in% c("character", "factor"), "c", "n")
    names(var.roles) <- var.names
    if (is.null(dv)) {
      stop("The `dv` argument must be specified whenever ",
           "`var.roles` is `NULL`.", call. = FALSE)
    }
    var.roles[dv] <- "d"
  } else {  # make sure they're in the right order
    var.roles <- var.roles[names(data)]
  }
  for (i in seq_along(var.roles)) {
    writeLines(paste(i, names(var.roles)[i], var.roles[i]), con = file.conn)
  }
  close(file.conn)
}

# gen.tree.diagram <- function(x, path) {
#   cd <- getwd()
#   setwd(path)
#   input <- deparse(substitute(x))
#   system(paste0("latex ", input))
#   system(paste0("dvips ", input))
#   system(paste0("ps2pdf ", input, ".ps"))
#   setwd(cd)
# }