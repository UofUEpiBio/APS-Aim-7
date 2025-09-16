#' Check for missing required variables in the data
#' 
#' `check_missing_variables` checks which variables listed in
#' `required_vars` are missing from the dataframe `data`.
#' 
#' @inheritParams validate_required_variables
#' 
#' @returns A character vector of the names of the missing variables.
#' If all required variables are present, the vector is empty.
check_missing_variables <- function(data, required_vars) {
    missing_vars <- c()

    for (v in required_vars) {
        if (is.null(data[[v]])) {
            missing_vars <- c(missing_vars, v)
        }
    }

    missing_vars
}

#' Validate that required variables are present in the data
#' 
#' `validate_required_variables` checks that all variables listed in
#' `required_vars` are present in the dataframe `data`. If any are missing,
#' an error is raised with a message listing the missing variables.
#' 
#' @param data Dataframe.
#' @param required_vars Character vector of variable names that must be present
#'   in `data`.
#' @param function_name Name of the function performing the check.
#' 
#' @returns NULL. Raises an error if any required variables are missing.
#' @export
validate_required_variables <- function(data, required_vars, function_name) {
  missing_vars <- check_missing_variables(
    data = data,
    required_vars = required_vars
  )

  if (length(missing_vars) > 0) {
    stop(
        "`",
        function_name,
        "()` failed. Missing required variables: \n\t- ",
        paste(missing_vars, collapse = ",\n\t- ")
    )
  }
}
