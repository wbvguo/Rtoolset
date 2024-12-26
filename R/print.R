#' `vec2print`
#'
#' print a vector so that the output can be copy/paste to the code chunk
#'
#' @param vec the vector to be printed
#' @param lang the language ('R', 'shell', 'python') used, default is 'R'
#'
#' @return a string, which can be copy and paste to the code chunk
#' @export
#'
#' @examples vec2print(c("Apple", "Orange"))
vec2print = function(vec, lang="R"){
  # print a vector so that the output can be copy/paste to the code chunk
  # :param vec: vector
  #
  # :return: a string

  if(lang=="R"){
    cat("c(", paste("\"", vec, "\"", sep="", collapse=", "), ")", sep="")
  }else if(lang=="shell"){
    cat("(", paste("\"", vec, "\"", sep="", collapse=" "), ")", sep="")
  }else if(lang=="python"){
    cat("[", paste("\"", vec, "\"", sep="", collapse=", "), "]", sep="")
  }else{
    stop("language not supported yet...")
  }

  #################### USAGE ####################
  # vec2print(c("Apple", "Orange"))
  # R > c("Apple", "Orange")
  # sh> ("Apple" "Orange")
  # py> ["Apple", "Orange"]
}


#' `print_pval`
#'
#' print p-values in a more readable format
#'
#' @param pvalues a numeric vector of p-values
#' @param threshold threshold below which p-values are printed in scientific notation, default is 1e-4
#' @param rm_tailing0 remove tailing 0 in the decimal, default is TRUE
#'
#' @return a string of p-values
#' @export
#'
#' @examples print_pval(0.0101)
print_pval <- function(pvalues, threshold = 1e-4, rm_tailing0 = TRUE) {
  p_str <- sapply(pvalues, function(pvalue) {
    if(pvalue < 2.2e-16){
      return(" < 2.2e-16")
    } else if(pvalue < threshold) {
      # Print in scientific notation
      return(sprintf("%.2e", pvalue))
    } else {
      # Print in standard decimal format
      p_str_temp <- sprintf("%.4f", pvalue)

      if(rm_tailing0){
        p_str_temp <- gsub("0+$", "", p_str_temp)
        p_str_temp <- gsub("\\.$", "", p_str_temp)
        ## another implementation detect the number of decimals
        # n_decimal = match(TRUE, round(pvalue, 1:4) == pvalue)
        #if(is.na(n_decimal)){n_decimal = 4}
        #sprintf("%.4f", pvalue)
        #sprintf(paste0("%.", n_decimal, "f"), pvalue)
      }
      return(p_str_temp)
    }
  })

  return(p_str)


  #################### USAGE ####################
  # print_pval(0.0101)
}


