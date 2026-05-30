#' Filter a data frame using a tidy-eval expression
#'
#' @param df A data frame or tibble
#' @param var A filtering expression (unquoted)
#'
#' @return A filtered data frame
#' @export
#'
#' @examples
#' filter_rows(mtcars, mpg > 20)
filter_rows <- function(df, var) {
  dplyr::filter(df, {{ var }})
}
