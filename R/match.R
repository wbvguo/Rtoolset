#' find the closest match of a string in a vector of strings
#'
#' This function searches the closest match of a string in a vector of strings using the levenshtein distance, sometimes useful for matching gene names.
#'
#' @param string A string
#' @param stringVector A vector of strings to be compared with
#'
#' @return a list of the closest match and its index in the vector
#' @export
#'
#' @examples
#' closestMatch("DFNB31", c("DNMT1", "DTNBP1", "IFNB1"))
#' #[[1]] [1] "DTNBP1" "IFNB1"
#' #[[2]] [1] 2 3
closestMatch = function(string, stringVector){
  distance = RecordLinkage::levenshteinSim(string, stringVector);
  return(list(stringVector[distance==max(distance)], which(distance==max(distance))))
  #################### USAGE ####################
  # closestMatch("DFNB31", c("DNMT1", "DTNBP1", "IFNB1"))
}
