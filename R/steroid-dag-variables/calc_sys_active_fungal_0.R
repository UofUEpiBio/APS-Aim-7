## -----------------------------------------------------------------------------
## Active Fungal Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Active Fungal Infection' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_active_fungal_0 <- function(
  mhantifungals,
  daily_antifungal_0,
  trx_0
) {
  dplyr::case_when(
    mhantifungals == "No" & (trx_0 == "Not Available" | daily_antifungal_0 == "Not administered") ~ 0,

    mhantifungals == "Yes" | (trx_0 == "Available" & daily_antifungal_0 == "Administered") ~ 1,

    is_unknown(mhantifungals) | daily_antifungal_0 == "UNK" ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_active_fungal_0 columns (one row per record_id)
wrapper_calc_sys_active_fungal_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_active_fungal_0 and join back to record_id
      # This requires joining from multiple event labels
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        select(record_id, daily_antifungal_0, trx_0) |>
        left_join(
          data |>
            filter(event_label == 'Day 0') |>
            select(record_id, mhantifungals),
          by = 'record_id'
        ) |>
        mutate(sys_active_fungal_0 = calc_sys_active_fungal_0(
          mhantifungals = mhantifungals,
          daily_antifungal_0 = daily_antifungal_0,
          trx_0 = trx_0
        )) |>
        select(record_id, sys_active_fungal_0),
      by = 'record_id'
    )
}


# Check for missing input parameters
check_missing_sys_active_fungal_0 <- function(data, record_ids) {
  # Check Day 0 parameters
  day0_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Day 0') |>
    select(record_id, mhantifungals) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(mhantifungals)) missing <- c(missing, "mhantifungals")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)

  # Check Daily In-Hospital Forms parameters
  daily_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Daily In-Hospital Forms') |>
    select(record_id, daily_antifungal_0, trx_0) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(daily_antifungal_0)) missing <- c(missing, "daily_antifungal_0")
      if (is.na(trx_0)) missing <- c(missing, "trx_0")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)

  # Combine both
  bind_rows(day0_missing, daily_missing) |>
    group_by(record_id) |>
    summarize(missing_params = paste(unique(unlist(strsplit(missing_params, "; "))), collapse = "; "), .groups = "drop")
}
