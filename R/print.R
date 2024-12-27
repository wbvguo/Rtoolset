#' print a vector in a copy-paste friendly format
#'
#' Formats a vector into a string representation suitable for direct copy-pasting
#' into code chunks in R, shell scripts, or Python.
#'
#' @param vec A vector to be printed.
#' @param lang The language to format the vector for. Options are 'R', 'shell', or 'python'. Default is 'R'.
#'
#' @return A formatted string representation of the vector.
#' @export
#'
#' @examples
#' # Example usage of vec2print
#' vec2print(c("Apple", "Orange"))  # For R: c("Apple", "Orange")
#' vec2print(c("Apple", "Orange"), lang = "shell")  # For Shell: ("Apple" "Orange")
#' vec2print(c("Apple", "Orange"), lang = "python")  # For Python: ["Apple", "Orange"]
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


#' print p-values in a more readable format
#'
#' Formats a numeric vector of p-values into a more readable string. P-values below a
#' threshold are displayed in scientific notation. Optionally, trailing zeros in
#' the decimal representation can be removed.
#'
#' @param pvalues A numeric vector of p-values to be formatted.
#' @param threshold The threshold below which p-values are printed in scientific notation. Default is 1e-4.
#' @param rm_tailing0 Logical. If TRUE, trailing zeros in the decimal representation are removed. Default is TRUE.
#'
#' @return A character vector of formatted p-values.
#' @export
#'
#' @examples
#' # Example usage of print_pval
#' print_pval(c(0.0101, 0.00003, 0.00000001))
#' print_pval(c(0.0101, 0.00003, 0.00000001), threshold = 1e-3, rm_tailing0 = FALSE)
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


