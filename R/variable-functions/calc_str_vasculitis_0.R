## -----------------------------------------------------------------------------
## Active Vasculitis (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for active vasculitis
#'
#' `calc_str_vasculitis_0` calculates the streamlined DAG variable for active
#' vasculitis based on rheumatologic conditions, immunosuppression status, and
#' chronic steroid use.
#'
#' @param m_rheum_conditions___8 Character vector. The `m_rheum_conditions___8`
#' column from the data. Checkbox value for vasculitis. Possible values:
#' "Checked", "Unchecked", or NA.
#' @param m_immunosup_conditions___4 Character vector. The `m_immunosup_conditions___4`
#' column from the data. Checkbox value for immunosuppression for nontransplant/
#' non-cancer condition. Possible values: "Checked", "Unchecked", or NA.
#' @param mhccster Character vector. The `mhccster` column from the data.
#' Chronic corticosteroid use. Possible values: "Yes", "No", "Unknown", or NA.
#'
#' @returns A numeric vector with values:
#' - 0 = No active vasculitis
#' - 1 = Active vasculitis
#' - 99 = Unknown
#' @export
calc_str_vasculitis_0 <- function(
  mhrheumd,
  m_rheum_conditions___8,
  m_immunosuppression,
  m_immunosup_conditions___4,
  mhccster
  ) {


  dplyr::case_when(
    ## Active vasculitis: has vasculitis AND (immunosuppression OR steroids)
    is_checked(m_rheum_conditions___8) &
      (is_checked(m_immunosup_conditions___4) | mhccster == "Yes") ~ 1,

    ## No active vasculitis: no vasculitis OR (no immunosuppression AND no steroids)
    (mhrheumd %in% c("No", "Yes") & is_unchecked(m_rheum_conditions___8)) |
      (
        m_immunosuppression %in% c("No", "Yes") &
        is_unchecked(m_immunosup_conditions___4) &
        mhccster == "No"
      ) ~ 0,

    ## Unknown status
    is_unknown(mhccster) | is_unknown(m_immunosuppression) | is_unknown(mhrheumd) ~ 99
  )
}
