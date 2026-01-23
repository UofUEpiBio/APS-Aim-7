## -----------------------------------------------------------------------------
## Definite Adrenal Insufficiency (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the systematic DAG variable for adrenal insufficiency on Day 0
#'
#' `calc_sys_def_adrenal_insufficiency_0` calculates the systematic DAG variable for
#' adrenal insufficiency from the data.
#'
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_endo_conditions___2 Character vector. The `m_endo_conditions___2` column
#' from the data.
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#'
#' @returns A vector with values:
#' - 0 = No definite adrenal insufficiency
#' - 1 = Definite adrenal insufficiency
#' - 99 = Unknown
#' @export
calc_sys_def_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2,
  m_immunosuppression,
  m_immunosup_conditions___1
  ) {

  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) & m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 0,

    (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
    (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 1,

    is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}

#' Calculate the streamlined DAG variable for adrenal insufficiency on Day 0
#'
#' `calc_sys_def_adrenal_insufficiency_0` calculates the streamlined DAG variable for
#' adrenal insufficiency from the data.
#'
#' @inheritParams calc_sys_def_adrenal_insufficiency_0
#'
#' @inherits calc_sys_def_adrenal_insufficiency_0 returns
#' @export
calc_str_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___2) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}
