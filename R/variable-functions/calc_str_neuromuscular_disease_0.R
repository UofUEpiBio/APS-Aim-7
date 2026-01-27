## -----------------------------------------------------------------------------
## Chronic Neuromuscular Disease (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Chronic Neuromuscular Disease'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_neuromuscular_disease_0 <- function(
  m_neurologic_conditions___6,
  m_neurologic
  ) {

  dplyr::case_when(
    is_checked(m_neurologic_conditions___6) ~ 1,
    m_neurologic == "Yes" & is_unchecked(m_neurologic_conditions___6) ~ 0,
    m_neurologic == "No" ~ 0,
    is_unknown(m_neurologic) ~ 99
  )
}
