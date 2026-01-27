## -----------------------------------------------------------------------------
## Dementia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Dementia'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_dementia_0 <- function(
  m_neurologic_conditions___1,
  m_neurologic
  ) {

  dplyr::case_when(
    is_checked(m_neurologic_conditions___1) ~ 1,

    m_neurologic == "Yes" & is_unchecked(m_neurologic_conditions___1) ~ 0,
    m_neurologic == "No" ~ 0,

    is_unknown(m_neurologic) ~ 99
  )
}
