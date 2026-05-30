#' create a directory if it does not exist
#'
#' This function checks whether a directory exists at the specified path. If the directory does not exist, it is created.
#'
#' @param dir A character string specifying the path of the directory to be checked and created if necessary.
#'
#' @return The specified directory path, with the directory created if it did not exist.
#' @export
#'
#' @examples
#' mkdir("~/test/")
mkdir = function(dir){
  if (! dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }else{
    message(paste0(dir, " exists! Skip creating..."))
  }
  return(invisible(dir))
}

#' Find a project root by walking upward
#'
#' Walk upward from an anchor path until a project marker file or directory is
#' found.
#'
#' @param anchor A character string specifying the path to start from. If
#'   `anchor` is a file path, the search starts from its parent directory.
#' @param marker A character string specifying the project marker to search for.
#'
#' @return The absolute path to the directory containing `marker`.
#' @export
#'
#' @examples
#' \dontrun{
#' find_project_root(marker = "DESCRIPTION")
#' }
find_project_root <- function(anchor = ".", marker = "DESCRIPTION") {
  stopifnot(is.character(anchor), length(anchor) == 1)
  stopifnot(is.character(marker), length(marker) == 1)

  anchor <- normalizePath(anchor, winslash = "/", mustWork = FALSE)
  start <- if (dir.exists(anchor)) anchor else dirname(anchor)
  start <- normalizePath(start, winslash = "/", mustWork = FALSE)

  path <- start
  repeat {
    if (file.exists(file.path(path, marker))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      break
    }
    path <- parent
  }

  stop(sprintf("Could not find %s above %s", sQuote(marker), start), call. = FALSE)
}
