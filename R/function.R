#' extract and assign default function arguments to the global environment
#'
#' This function extracts the default arguments of a given function and assigns  # Description
#' them to variables in the global environment. It is particularly useful for
#' debugging when you need quick access to the default values.
#'
#' @param func A function
#'
#' @return assign the default values of the function to the global environment
#' @export
#'
#' @examples
#' rejection_sampler <- function(D, n_trails = 10000, seed = 2024) {}
#' extract_params(rejection_sampler)
#' print(n_trails)  # Output: 10000
extract_params = function(func) {
  # Get the formal arguments of the function
  formals_list = formals(func)

  for (arg_name in names(formals_list)) {
    # If the default value is missing, assign NA
    if (formals_list[[arg_name]] == "") {
      assign(arg_name, NA, envir = .GlobalEnv)
    } else {
      # Evaluate the default value and assign it
      assign(arg_name, eval(formals_list[[arg_name]], envir = .GlobalEnv), envir = .GlobalEnv)
    }
  }

  #################### USAGE ####################
  # rejection_sampler <- function(D, n_trails = 10000, seed = 2024) {}
  #
  # # Extract and assign the variables
  # extract_params(rejection_sampler)
  #
  # # Check the assigned variables
  # print(D)         # Output: NA
  # print(n_trails)  # Output: 10000
  # print(seed)      # Output: 2024
}
