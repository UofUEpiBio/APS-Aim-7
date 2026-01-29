## -----------------------------------------------------------------------------
## Septic Shock (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Septic Shock'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_septic_shock_0 <- function(
  sepsis_present,
  sepsis_clinical_judgement,
  daily_vasopressors_0___0
) {
  # TODO: If after querying, it is determined that 'Unknown' cannot be fixed, set those to 0 as well
  dplyr::case_when(
    sepsis_present == "No" | sepsis_clinical_judgement == "No" | is_checked(daily_vasopressors_0___0) ~ 0,
    sepsis_present == "Yes" & sepsis_clinical_judgement == "Yes" & is_unchecked(daily_vasopressors_0___0) ~ 1,
    is_unknown(sepsis_clinical_judgement) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_septic_shock_0 columns (one row per record_id)
wrapper_calc_str_septic_shock_0 <- function(data) {
  data_with_vasopressors <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    select(record_id, daily_vasopressors_0___0)

  # Get sepsis data
  data_with_sepsis <- data |>
    filter(event_label == 'Syndrome Adjudication') |>
    select(record_id, sepsis_present, sepsis_clinical_judgement)

  data_with_str_septic_shock <- data_with_vasopressors |>
    left_join(data_with_sepsis, by = 'record_id') |>
    mutate(
      str_septic_shock_0 = calc_str_septic_shock_0(
        sepsis_present = sepsis_present,
        sepsis_clinical_judgement = sepsis_clinical_judgement,
        daily_vasopressors_0___0 = daily_vasopressors_0___0
      )
    ) |>
    select(record_id, str_septic_shock_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_str_septic_shock, by = 'record_id')
}
