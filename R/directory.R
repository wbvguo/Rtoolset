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

#' Search for a file in a directory and its subdirectories
#'
#' Recursively search a directory tree for files matching a given name,
#' optionally skipping selected subdirectories and/or hidden folders.
#'
#' @param name A character string specifying the file name to search for.
#' @param path A character string specifying the directory to start the search
#'   from. Defaults to the current working directory (`"."`).
#' @param exclude_dir A character vector of directory names to skip during the
#'   search. Matching is done on the directory's base name. Defaults to `NULL`.
#' @param hidden Logical; whether to descend into hidden folders (those whose
#'   name starts with a dot). Defaults to `FALSE`, i.e. hidden folders are
#'   skipped.
#'
#' @return A character vector of paths (relative to `path`) for every match
#'   found. Returns `character(0)` if no match is found.
#' @export
#'
#' @examples
#' \dontrun{
#' find_file("DESCRIPTION")
#' find_file("test.R", path = "~/project", exclude_dir = c("renv", "node_modules"))
#' }
find_file <- function(name, path = ".", exclude_dir = NULL, hidden = FALSE) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(is.logical(hidden), length(hidden) == 1)
  if (!dir.exists(path)) {
    stop(sprintf("Directory does not exist: %s", path), call. = FALSE)
  }

  matches <- character(0)

  walk <- function(dir) {
    entries <- list.files(dir, all.files = hidden, no.. = TRUE, full.names = TRUE)
    for (entry in entries) {
      base <- basename(entry)
      if (dir.exists(entry)) {
        if (base %in% exclude_dir) next
        if (!hidden && startsWith(base, ".")) next
        walk(entry)
      } else if (base == name) {
        matches[[length(matches) + 1L]] <<- entry
      }
    }
  }

  walk(path)
  matches
}
