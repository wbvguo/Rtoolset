#' Create a named list
#'
#' Given a character vector, this function creates an empty list with names
#' corresponding to the elements of the vector.
#'
#' @param vec A character vector specifying names of each element in the list.
#'
#' @return A named list with the same length as \code{vec}.
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' createNamedList(c("x", "y"))
createNamedList <- function(vec) {
  if (length(vec) > 0) {
    setNames(vector("list", length(vec)), vec)
  } else {
    list()
  }
}
