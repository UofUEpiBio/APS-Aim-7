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


#' Calculate the systematic DAG variable for rheumatologic disease on Day 0
#'
#' `calc_sys_rheumdis_0` calculates the systematic DAG variable for
#' rheumatologic disease from the data.
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
#' - 0 = No rheumatologic disease
#' - 1 = Rheumatologic disease present
#' - 99 = Unknown
#' @export
calc_sys_rheumdis_0 <- function(
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


#' Calculate the streamlined DAG variable for rheumatologic disease on Day 0
#'
#' `calc_str_rheumdis_0` calculates the streamlined DAG variable for
#' rheumatologic disease from the data.
#'
#' @inheritParams calc_sys_rheumdis_0
#'
#' @inherits calc_sys_rheumdis_0 returns
#' @export
calc_str_rheumdis_0 <- function(
  mhrheumd,
  m_rheum_conditions___1,
  m_rheum_conditions___2,
  m_rheum_conditions___3,
  m_rheum_conditions___4,
  m_rheum_conditions___5,
  m_rheum_conditions___6,
  m_rheum_conditions___7,
  m_rheum_conditions___8,
  m_rheum_conditions___88
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
      is_unchecked(m_rheum_conditions___88) ~ 0,

    mhrheumd == "Yes" & (
      is_checked(m_rheum_conditions___1) |
      is_checked(m_rheum_conditions___2) |
      is_checked(m_rheum_conditions___3) |
      is_checked(m_rheum_conditions___4) |
      is_checked(m_rheum_conditions___5) |
      is_checked(m_rheum_conditions___6) |
      is_checked(m_rheum_conditions___7) |
      is_checked(m_rheum_conditions___8) |
      is_checked(m_rheum_conditions___88)
      ) ~ 1,
    mhrheumd == "Unknown" ~ 99
  )
}


#' Calculate the streamlined DAG variable for ILD on Day 0
#'
#' `calc_str_ild_0` calculates the streamlined DAG variable for
#' interstitial lung disease from the data.
#'
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___6 Character vector. The `m_pulm_conditions___6` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No ILD
#' - 1 = ILD present
#' - 99 = Unknown
#' @export
calc_str_ild_0 <- function(
  m_pulmonary,
  m_pulm_conditions___6
) {
  dplyr::case_when(
    m_pulmonary == "Yes" & is_checked(m_pulm_conditions___6) ~ 1,
    m_pulmonary %in% c("Yes", "No") & is_unchecked(m_pulm_conditions___6) ~ 0,
    m_pulmonary == "Unknown" ~ 99
  )
}


#' Calculate the streamlined DAG variable for obstructive lung disease on Day 0
#'
#' `calc_sys_obstruct_lung_0` calculates the streamlined DAG variable for
#' obstructive lung disease from the data.
#'
#' @param m_pulmonary Character vector. The `m_pulmonary` column from the data.
#' @param m_pulm_conditions___1 Character vector. The `m_pulm_conditions___1` column
#' from the data.
#' @param m_pulm_conditions___3 Character vector. The `m_pulm_conditions___3` column
#' from the data.
#'
#' @returns A vector with values:
#' - 0 = No obstructive lung disease
#' - 1 = Obstructive lung disease present
#' - 99 = Unknown
#' @export
calc_sys_obstruct_lung_0 <- function(
  m_pulmonary,
  m_pulm_conditions___1,
  m_pulm_conditions___3
) {
  dplyr::case_when(
    m_pulmonary == "Yes" &
      (is_checked(m_pulm_conditions___1) | is_checked(m_pulm_conditions___3)) ~ 1,
    m_pulmonary %in% c("Yes", "No") &
      (is_unchecked(m_pulm_conditions___1) & is_unchecked(m_pulm_conditions___3)) ~ 0,
    m_pulmonary == "Unknown" ~ 99
  )
}


#' WIP
calc_sys_active_covid19_0 <- function(
  pathogen_date,
  resp_pathogen_code,
  cxpos_code
) {
  dplyr::case_when(
    !is.na(pathogen_date) &
      resp_pathogen_code == 6 &
      cxpos_code == 3 ~ 1,

    is.na(pathogen_date) |
      resp_pathogen_code != 6 |
      cxpos_code != 3 ~ 0
  )
}


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


#' Calculate the streamlined DAG variable for age on Day 0
#'
#' `calc_age_0` calculates the streamlined DAG variable for age from the data.
#'
#' @param age Numeric vector. The `age` column from the data.
#'
#' @returns A numeric vector representing age on day 0.
#' @export
calc_age_0 <- function(
  age
) {
  dplyr::case_when(
    !is.na(age) & age >= 0 ~ age
  )
}
