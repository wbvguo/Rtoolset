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
#' 
#' @note This function modifies the global environment by assigning variables.
#'   This is intentional for debugging purposes but should be used with caution
#'   in production code.
#' 
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
    # Get the default value for this argument
    default_val = formals_list[[arg_name]]
    
    # Check if argument has no default (missing argument)
    # Missing arguments are represented as empty symbols
    if (missing(default_val) || 
        (is.symbol(default_val) && nchar(as.character(default_val)) == 0)) {
      # No default provided
      arg_value = NA
    } else {
      # Has a default value, evaluate it
      tryCatch({
        arg_value = eval(default_val, envir = .GlobalEnv)
      }, error = function(e) {
        arg_value <<- NA
      })
    }

    # Intentional assignment to global environment for debugging purposes
    # This is documented in the function's @note section
    assign(arg_name, arg_value, envir = .GlobalEnv)  # nolint: object_usage_linter
    if(verbose){
      if(msg_flag){message("default params:")}
      cat(paste0("  ", arg_name, " = ", arg_value, "\n"))
    }
    msg_flag = FALSE
  }
}
