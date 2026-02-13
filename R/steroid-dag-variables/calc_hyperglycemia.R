## -----------------------------------------------------------------------------
## Hyperglycemia (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Hyperglycemia: History' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_hyperglyc_hist_0 <- function(
  m_endocrine,
  m_endo_conditions___1
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___1) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___1) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}


# Calculate SYSTEMATIC DAG 'Hyperglycemia: Glucose Level' for Day 0
#
# Value: Glucose level in mg/dL (-1 = 'Not Collected')
calc_sys_hyperglyc_gluc_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    !is.na(daily_gluc_8a_0) ~ daily_gluc_8a_0,
    daily_gluc_nc_0 == 'Not Collected' ~ -1
  )
}

# Calculate STREAMLINED DAG 'Hyperglycemia' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_hyperglyc_hist_0 <- function(
  daily_gluc_8a_0,
  daily_gluc_nc_0
) {
  dplyr::case_when(
    daily_gluc_8a_0 > 250 ~ 1,
    is.na(daily_gluc_8a_0) | (daily_gluc_8a_0 <= 250) ~ 0,
    daily_gluc_nc_0 == 'Not Collected' ~ 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id, sys_hyperglyc_hist_0, and sys_hyperglyc_gluc_0 columns (one row per record_id)
wrapper_calc_sys_hyperglycemia_0 <- function(data) {
  # Get history data from Day 0
  data_with_hist <- data |>
    filter(event_label == 'Day 0') |>
    mutate(sys_hyperglyc_hist_0 = calc_sys_hyperglyc_hist_0(
      m_endocrine = m_endocrine,
      m_endo_conditions___1 = m_endo_conditions___1
    )) |>
    select(record_id, sys_hyperglyc_hist_0)

  # Get glucose data from Daily In-Hospital Forms
  data_with_gluc <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    mutate(sys_hyperglyc_gluc_0 = calc_sys_hyperglyc_gluc_0(
      daily_gluc_8a_0 = daily_gluc_8a_0,
      daily_gluc_nc_0 = daily_gluc_nc_0
    )) |>
    select(record_id, sys_hyperglyc_gluc_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_hist, by = 'record_id') |>
    left_join(data_with_gluc, by = 'record_id')
}


# Convenience wrapper function
# Returns a data frame with record_id and str_hyperglyc_hist_0 columns (one row per record_id)
wrapper_calc_str_hyperglyc_hist_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_hyperglyc_hist_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_hyperglyc_hist_0 = calc_str_hyperglyc_hist_0(
          daily_gluc_8a_0 = daily_gluc_8a_0,
          daily_gluc_nc_0 = daily_gluc_nc_0
        )) |>
        select(record_id, str_hyperglyc_hist_0),
      by = 'record_id'
    )
}


# Check for missing input parameters (SYS)
check_missing_sys_hyperglycemia_0 <- function(data, record_ids) {
  # Check Day 0 parameters
  day0_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Day 0') |>
    select(record_id, m_endocrine, m_endo_conditions___1) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(m_endocrine)) missing <- c(missing, "m_endocrine")
      if (is.na(m_endo_conditions___1)) missing <- c(missing, "m_endo_conditions___1")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)

  # Check Daily In-Hospital Forms parameters
  daily_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Daily In-Hospital Forms') |>
    select(record_id, daily_gluc_8a_0, daily_gluc_nc_0) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(daily_gluc_8a_0)) missing <- c(missing, "daily_gluc_8a_0")
      if (is.na(daily_gluc_nc_0)) missing <- c(missing, "daily_gluc_nc_0")
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

# Check for missing input parameters (STR)
check_missing_str_hyperglyc_hist_0 <- function(data, record_ids) {
  data |>
    filter(record_id %in% record_ids, event_label == 'Daily In-Hospital Forms') |>
    select(record_id, daily_gluc_8a_0, daily_gluc_nc_0) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(daily_gluc_8a_0)) missing <- c(missing, "daily_gluc_8a_0")
      if (is.na(daily_gluc_nc_0)) missing <- c(missing, "daily_gluc_nc_0")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)
}
