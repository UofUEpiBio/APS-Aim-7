## -----------------------------------------------------------------------------
## Chronic Immunocompromise (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate the streamlined DAG variable for chronic immunocompromise on Day 0
#'
#' `calc_sys_chron_immunocomp_0` calculates the streamlined DAG variable for
#' chronic immunocompromise from the data.
#'
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#' @param m_immunosup_conditions___2 Character vector. The `m_immunosup_conditions___2` column
#' from the data.
#' @param m_immunosup_conditions___3 Character vector. The `m_immunosup_conditions___3` column
#' from the data.
#' @param m_immunosup_conditions___4 Character vector. The `m_immunosup_conditions___4` column
#' from the data.
#' @param m_immunosup_conditions___5 Character vector. The `m_immunosup_conditions___5` column
#' from the data.
#' @param m_immunosup_conditions___6 Character vector. The `m_immunosup_conditions___6` column
#' from the data.
#' @param m_immunosup_conditions___7 Character vector. The `m_immunosup_conditions___7` column
#' from the data.
#' @param m_immunosup_conditions___88 Character vector. The `m_immunosup_conditions___88` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No chronic immunocompromise
#' - 1 = Chronic immunocompromise present
#' - 99 = Unknown
#' @export
calc_sys_chron_immunocomp_0 <- function(
  m_immunosuppression,
  m_immunosup_conditions___2,
  m_immunosup_conditions___3,
  m_immunosup_conditions___4,
  m_immunosup_conditions___5,
  m_immunosup_conditions___6,
  m_immunosup_conditions___7,
  m_immunosup_conditions___88
) {
  dplyr::case_when(
    m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___2) &
      is_unchecked(m_immunosup_conditions___3) &
      is_unchecked(m_immunosup_conditions___4) &
      is_unchecked(m_immunosup_conditions___5) &
      is_unchecked(m_immunosup_conditions___6) &
      is_unchecked(m_immunosup_conditions___7) &
      is_unchecked(m_immunosup_conditions___88) ~ 0,

    m_immunosuppression == "Yes" & (
      is_checked(m_immunosup_conditions___2) |
      is_checked(m_immunosup_conditions___3) |
      is_checked(m_immunosup_conditions___4) |
      is_checked(m_immunosup_conditions___5) |
      is_checked(m_immunosup_conditions___6) |
      is_checked(m_immunosup_conditions___7) |
      is_checked(m_immunosup_conditions___88)
      ) ~ 1,

    m_immunosuppression == "Unknown" ~ 99
  )
}
