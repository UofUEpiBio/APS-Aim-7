## -----------------------------------------------------------------------------
## Dementia (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for dementia history
#'
#' `calc_str_dementia_0` calculates the streamlined DAG variable for
#' dementia history based on neurologic conditions assessment.
#'
#' @param m_neurologic_conditions___1 Character vector. The `m_neurologic_conditions___1`
#' column from the data. Possible values: "Checked", "Unchecked", or NA.
#' @param m_neurologic Character vector. The `m_neurologic` column from the data.
#' Used for branching logic. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = No dementia history
#' - 1 = Dementia history
#' - 99 = Unknown
#' @export
calc_str_dementia_0 <- function(
  m_neurologic_conditions___1,
  m_neurologic
  ) {

  dplyr::case_when(
    is_checked(m_neurologic_conditions___1) ~ 1,

    m_neurologic == "Yes" & is_unchecked(m_neurologic_conditions___1) ~ 0,
    m_neurologic == "No" ~ 0,

    is_unknown(m_neurologic) ~ 99
  )
}
