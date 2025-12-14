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
}

