# make directory
#' Title: make_dir
#'
#' @param dir a character string of directory path
#'
#' @return a directory created if it does not exist
#' @export
#'
#' @examples
#' make_dir("~/test/")
make_dir = function(dir){
  if (! dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }else{
    print(paste0(dir, " exists! Skip creating..."))
  }
}
