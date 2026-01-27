## -----------------------------------------------------------------------------
## Active Vasculitis (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Active Vasculitis'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
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
