## -----------------------------------------------------------------------------
## Chronic Neuromuscular Disease (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for chronic neuromuscular disease
#'
#' `calc_str_neuromuscular_disease_0` calculates the streamlined DAG variable for
#' chronic neuromuscular disease history based on neurologic conditions assessment.
#'
#' @param m_neurologic_conditions___6 Character vector. The `m_neurologic_conditions___6`
#' column from the data. Checkbox value for neuromuscular disorder. Possible values:
#' "Checked", "Unchecked", or NA.
#' @param m_neurologic Character vector. The `m_neurologic` column from the data.
#' Used for branching logic. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = No chronic neuromuscular disease history
#' - 1 = Chronic neuromuscular disease history
#' - 99 = Unknown
#' @export
calc_str_neuromuscular_disease_0 <- function(
  m_neurologic_conditions___6,
  m_neurologic
  ) {

  dplyr::case_when(
    is_checked(m_neurologic_conditions___6) ~ 1,
    m_neurologic == "Yes" & is_unchecked(m_neurologic_conditions___6) ~ 0,
    m_neurologic == "No" ~ 0,
    is_unknown(m_neurologic) ~ 99
  )
}
