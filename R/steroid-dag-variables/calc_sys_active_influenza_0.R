## -----------------------------------------------------------------------------
## Active Influenza Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Active Influenza Infection' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
calc_sys_active_influenza_0 <- function(has_pos_influenza_0) {
  dplyr::case_when(
    has_pos_influenza_0 == 1 ~ 1,
    is.na(has_pos_influenza_0) ~ 0
  )
}
