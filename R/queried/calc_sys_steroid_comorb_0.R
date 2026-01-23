## -----------------------------------------------------------------------------
## Steroid Responsive Comorbidity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for steroid responsive comorbidity on Day 0
#'
#' `calc_sys_steroid_comorb_0` calculates the systematic DAG variable for
#' steroid responsive comorbidity from the data.
#'
#' @param mhrheumd Character vector. The `mhrheumd` column from the data.
#' @param m_rheum_conditions___1 Character vector. The `m_rheum_conditions___1` column
#' from the data.
#' @param m_rheum_conditions___2 Character vector. The `m_rheum_conditions___2` column
#' from the data.
#' @param m_rheum_conditions___3 Character vector. The `m_rheum_conditions___3` column
#' from the data.
#' @param m_rheum_conditions___4 Character vector. The `m_rheum_conditions___4` column
#' from the data.
#' @param m_rheum_conditions___5 Character vector. The `m_rhe um_conditions___5` column
#' from the data.
#' @param m_rheum_conditions___6 Character vector. The `m_rheum_conditions___6` column
#' from the data.
#' @param m_rheum_conditions___7 Character vector. The `m_rheum_conditions___7` column
#' from the data.
#' @param m_rheum_conditions___8 Character vector. The `m_rheum_conditions___8` column
#' from the data.
#' @param m_rheum_conditions___88 Character vector. The `m_rheum_conditions___88` column
#' from the data.
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___6 Character vector. The `m_pulm_conditions___6` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No steroid responsive comorbidity
#' - 1 = Steroid responsive comorbidity present
#' - 99 = Unknown
#' @export
calc_sys_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___8,
  m_rheum_conditions___88,
  m_pulmonary,
  m_pulm_conditions___6
  ) {
    dplyr::case_when(
      mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___8) &
      is_unchecked(m_rheum_conditions___88) &
      m_pulmonary %in% c("Yes", "No") &
      is_unchecked(m_pulm_conditions___6) ~ 0,

      (mhrheumd == "Yes" & (
        is_checked(m_rheum_conditions___1) |
        is_checked(m_rheum_conditions___2) |
        is_checked(m_rheum_conditions___3) |
        is_checked(m_rheum_conditions___4) |
        is_checked(m_rheum_conditions___5) |
        is_checked(m_rheum_conditions___6) |
        is_checked(m_rheum_conditions___7) |
        is_checked(m_rheum_conditions___8) |
        is_checked(m_rheum_conditions___88)
        )
      ) |
      (m_pulmonary == "Yes" &
      is_checked(m_pulm_conditions___6)) ~ 1,

      mhrheumd == "Unknown" | m_pulmonary == "Unknown" ~ 99
  )
}


#' Calculate the streamlined DAG variable for steroid responsive comorbidity on Day 0
#'
#' `calc_str_steroid_comorb_0` calculates the streamlined DAG variable for
#' steroid responsive comorbidity from the data.
#'
#' @inheritParams calc_sys_steroid_comorb_0
#'
#' @inherits calc_sys_steroid_comorb_0 returns
#' @export
calc_str_steroid_comorb_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___88,
  m_immunosuppression,
  m_immunosup_conditions___4,
  mhccster
) {
  dplyr::case_when(
    (mhrheumd == "No" &
      is_unchecked(m_rheum_conditions___1) &
      is_unchecked(m_rheum_conditions___2) &
      is_unchecked(m_rheum_conditions___3) &
      is_unchecked(m_rheum_conditions___4) &
      is_unchecked(m_rheum_conditions___5) &
      is_unchecked(m_rheum_conditions___6) &
      is_unchecked(m_rheum_conditions___7) &
      is_unchecked(m_rheum_conditions___88)) |
    (m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___4) &
      mhccster == "No") ~ 0,

    (mhrheumd == "Yes" & (
      is_checked(m_rheum_conditions___1) |
      is_checked(m_rheum_conditions___2) |
      is_checked(m_rheum_conditions___3) |
      is_checked(m_rheum_conditions___4) |
      is_checked(m_rheum_conditions___5) |
      is_checked(m_rheum_conditions___6) |
      is_checked(m_rheum_conditions___7) |
      is_checked(m_rheum_conditions___88))
      ) &
    ((m_immunosuppression == "Yes" &
      is_checked(m_immunosup_conditions___4)) |
      mhccster == "Yes") ~ 1,

    is_unknown(mhrheumd) | is_unknown(m_immunosuppression) | is_unknown(mhccster) ~ 99
  )
}
