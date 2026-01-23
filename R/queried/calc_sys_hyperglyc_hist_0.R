## -----------------------------------------------------------------------------
## Hyperglycemia (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable 1 for hyperglycemia on Day 0
#'
#' `calc_sys_hyperglyc_hist_0` calculates the systematic DAG variable 1 for
#' hyperglycemia from the data.
#'
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_endo_conditions___1 Character vector. The `m_endo_conditions___1` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No diabetes medical history
#' - 1 = Diabetes medical history present
#' - 99 = Unknown
#' @export
calc_sys_hyperglyc_hist_0 <- function(
  m_endocrine,
  m_endo_conditions___1
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___1) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___1) ~ 1,
    m_endocrine == "Unknown" ~ 99
  )
}


#' Calculate the systematic DAG variable 2 for hyperglycemia on Day 0
#'
#' `calc_sys_hyperglyc_gluc_0` calculates the systematic DAG variable 2 for
#' hyperglycemia from the data.
#'
#' @param daily_gluc_8a_0 Numeric vector. The `daily_gluc_8a_0` column from the data.
#' @param daily_gluc_nc_0 Character vector. The `daily_gluc_nc_0` column from the data.
#'
#' @returns A numeric vector representing the glucose level on day 0 based on
#' available data:
#' - Glucose value if measured
#' - -1 if glucose not collected
#' @export
calc_sys_hyperglyc_gluc_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    !is.na(daily_gluc_8a_0) ~ daily_gluc_8a_0,
    daily_gluc_nc_0 == 'Not Collected' ~ -1
  )
}

#' Calculate the streamlined DAG variable for hyperglycemia on Day 0
#'
#' `calc_str_hyperglyc_hist_0` calculates the streamlined DAG variable for
#' hyperglycemia from the data.
#'
#' @inheritParams calc_sys_hyperglyc_gluc_0
#'
#' @returns A vector with values:
#' - 0 = No hyperglycemia on day 0 (glucose ≤ 250 or not collected)
#' - 1 = Hyperglycemia present (glucose > 250)
calc_str_hyperglyc_hist_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    daily_gluc_8a_0 > 250 ~ 1,
    is.na(daily_gluc_8a_0) | (daily_gluc_8a_0 <= 250) ~ 0,
    daily_gluc_nc_0 == 'Not Collected' ~ 0
  )
}
