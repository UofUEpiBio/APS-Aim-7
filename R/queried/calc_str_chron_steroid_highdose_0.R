## -----------------------------------------------------------------------------
## Chronic Steroids High Dose (Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for chronic high-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_highdose_0` calculates the streamlined DAG variable for
#' chronic high-dose steroid use from the data.
#'
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#'
#' @returns A vector with values:
#' - 0 = No chronic high-dose steroid use
#' - 1 = Chronic high-dose steroid use
#' - 99 = Unknown
#' @export
calc_str_chron_steroid_highdose_0 <- function(
  m_immunosuppression,
  m_immunosup_conditions___1
) {
  dplyr::case_when(
    m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 0,
    m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1) ~ 1,
    is_unknown(m_immunosuppression) ~ 99
  )
}
