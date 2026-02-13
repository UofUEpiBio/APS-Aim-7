## -----------------------------------------------------------------------------
## Presence of ARDS Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of ARDS Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_ards_0 <- function(
  ards_present,
  ards_clinical_judgement
  ) {

  ## Check if ARDS is present based on either documented presence or clinical judgement
  dplyr::case_when(
    ards_present == "No" | ards_clinical_judgement == "No" ~ 0,
    ards_present == "Yes" & ards_clinical_judgement == "Yes" ~ 1,
    is_unknown(ards_clinical_judgement) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_ards_0 columns (one row per record_id)
wrapper_calc_sys_ards_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_ards_0 and join back to record_id
      data |>
        filter(event_label == 'Syndrome Adjudication') |>
        mutate(sys_ards_0 = calc_sys_ards_0(
          ards_present = ards_present,
          ards_clinical_judgement = ards_clinical_judgement
        )) |>
        select(record_id, sys_ards_0),
      by = 'record_id'
    )
}


# Check for missing input parameters
check_missing_sys_ards_0 <- function(data, record_ids) {
  data |>
    filter(record_id %in% record_ids, event_label == 'Syndrome Adjudication') |>
    select(record_id, ards_present, ards_clinical_judgement) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(ards_present)) missing <- c(missing, "ards_present")
      if (is.na(ards_clinical_judgement)) missing <- c(missing, "ards_clinical_judgement")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)
}
