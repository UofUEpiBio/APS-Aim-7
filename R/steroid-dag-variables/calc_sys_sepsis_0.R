## -----------------------------------------------------------------------------
## Presence of Sepsis Syndrome (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of Sepsis Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_sepsis_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement
  ) {

  ## Check if sepsis is present based on either documented presence or clinical judgement
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" ~ 0,
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" ~ 1,
    is_unknown(sepsis_clinical_judgement) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_sepsis_0 columns (one row per record_id)
wrapper_calc_sys_sepsis_0 <- function(data) {
  data_with_sepsis <- data |>
    filter(event_label == 'Syndrome Adjudication') |>
    mutate(
      sys_sepsis_0 = calc_sys_sepsis_0(
        sepsis_present,
        sepsis_clinical_judgement
      )
    ) |>
    select(record_id, sys_sepsis_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_sepsis, by = 'record_id')
}
