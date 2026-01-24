## -----------------------------------------------------------------------------
## Chronic Moderate-Dose Steroid Use (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for chronic moderate-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_moddose_0` calculates the systematic DAG variable for
#' chronic moderate-dose steroid use. This variable represents chronic
#' moderate-dose steroid use leading to possible adrenal insufficinecy (but not
#' definite adrenal insufficiency).
#'
#' @param mhccster Character vector. The `mhccster` column from the data.
#' @param m_endo_conditions___2 Character vector. The `m_endo_conditions___2` column
#' from the data.
#' @param m_immunosup_conditions___1 Character vector. The `m_immunosup_conditions___1`
#' column from the data.
#' @param m_endocrine Character vector. The `m_endocrine` column from the data.
#' @param m_immunosuppression Character vector. The `m_immunosuppression` column from the data.
#'
#' @returns A vector with values:
#' - 0 = No possible adrenal insufficiency
#' - 1 = Possible adrenal insufficiency
#' - 99 = Unknown
#' @export
calc_sys_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_immunosup_conditions___1,
  m_endocrine,
  m_immunosuppression
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
        (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) &
        m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}


#' Calculate the streamlined DAG variable for chronic moderate-dose steroid use on Day 0
#'
#' `calc_str_chron_steroid_moddose_0` calculates the streamlined DAG variable for
#' chronic moderate-dose steroid use. This variable represents chronic
#' moderate-dose steroid use leading to possible adrenal insufficinecy (but not
#' definite adrenal insufficiency).
#'
#' @inheritParams calc_sys_chron_steroid_moddose_0
#'
#' @inherits calc_sys_chron_steroid_moddose_0 returns
#' @export
calc_str_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_endocrine
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) ~ 99
  )
}
