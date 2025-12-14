#' extract and assign default function arguments to the global environment
#'
#' This function extracts the default arguments of a given function and assigns
#' them to variables in the global environment. It is particularly useful for
#' debugging when you need quick access to the default values.
#'
#' @param func A function
#' @param verbose A logical value. If TRUE, the function will print the default
#'   parameter values to the console.
#'
#' @return Invisibly returns NULL. Assigns the default values of the function
#'   to the global environment.
#' @export
#'
#' @examples
#' rejection_sampler <- function(D, n_trails = 10000, seed = 2024) {}
#' extract_params(rejection_sampler)
#' print(n_trails)
#' # 10000
extract_params = function(func, verbose=FALSE) {
  # Get the formal arguments of the function
  formals_list = formals(func)
  msg_flag = TRUE

  for (arg_name in names(formals_list)) {
    # If the default value is missing, assign NA
    if (formals_list[[arg_name]] == "") {
      arg_value = NA
    } else {
      # Evaluate the default value and assign it
      arg_value = eval(formals_list[[arg_name]], envir = .GlobalEnv)
    }

    assign(arg_name, arg_value, envir = .GlobalEnv)
    if(verbose){
      if(msg_flag){message("default params:")}
      cat(paste0("  ", arg_name, " = ", arg_value, "\n"))
    }
    msg_flag = FALSE
  }
}
