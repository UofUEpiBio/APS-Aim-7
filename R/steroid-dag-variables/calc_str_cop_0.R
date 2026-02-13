## -----------------------------------------------------------------------------
## Acute Cryptogenic Organizing Pneumonia (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Acute Cryptogenic Organizing Pneumonia'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_cop_0 <- function(
  organ_dysfnx_cause_code
  ) {

  dplyr::case_when(
    organ_dysfnx_cause_code %in% c(1, 3, 99) ~ 0,
    organ_dysfnx_cause_code == 2 ~ 1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_cop_0 columns (one row per record_id)
wrapper_calc_str_cop_0 <- function(data, dictionary) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_cop_0 and join back to record_id
      data |>
        filter(event_label == 'Syndrome Adjudication') |>
        left_join(
          get_code_label_map('organ_dysfnx_cause', dictionary),
          by = 'organ_dysfnx_cause'
        ) |>
        mutate(str_cop_0 = calc_str_cop_0(
          organ_dysfnx_cause_code = organ_dysfnx_cause_code
        )) |>
        select(record_id, str_cop_0),
      by = 'record_id'
    )
}


# Check for missing input parameters
check_missing_str_cop_0 <- function(data, record_ids) {
  data |>
    filter(record_id %in% record_ids, event_label == 'Syndrome Adjudication') |>
    select(record_id, organ_dysfnx_cause) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(organ_dysfnx_cause)) missing <- c(missing, "organ_dysfnx_cause")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)
}
