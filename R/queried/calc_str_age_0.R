## -----------------------------------------------------------------------------
## Age (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for age on Day 0
#'
#' `calc_age_0` calculates the streamlined DAG variable for age from the data.
#'
#' @param age Numeric vector. The `age` column from the data.
#'
#' @returns A numeric vector representing age on day 0.
#' @export
calc_str_age_0 <- function(
  age
) {
  dplyr::case_when(
    !is.na(age) & age >= 0 ~ age
  )
}
