#' Convert beta values to M values
#'
#' @param x scalar, matrix, or data frame of beta values
#' @param alpha a small number to avoid log(0), default is 1e-6
#'
#' @return M values
#' @export
#'
#' @examples
#' beta2M(meth_df)
beta2M = function(x, alpha = 1e-6){
  return(log2((x+alpha)/(1-x+alpha)))
}
