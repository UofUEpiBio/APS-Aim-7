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
  daily_antibiotics_0,
  daily_antiviral_0,
  daily_antifungal_0,
  antifungal_agents_0___1,
  antifungal_agents_0___2,
  antifungal_agents_0___3,
  antifungal_agents_0___6,
  antifungal_agents_0___7,
  antifungal_agents_0___9,
  antifungal_agents_0___13,
  has_qualifying_pathogen
) {

  # Check if qualifying antifungals were administered
  qualifying_antifungal <- daily_antifungal_0 == 'Administered' & (
    is_checked(antifungal_agents_0___1) |
    is_checked(antifungal_agents_0___2) |
    is_checked(antifungal_agents_0___3) |
    is_checked(antifungal_agents_0___6) |
    is_checked(antifungal_agents_0___7) |
    is_checked(antifungal_agents_0___9) |
    is_checked(antifungal_agents_0___13)
  )

  # Sepsis is present if any of the criteria are met
  dplyr::case_when(
    daily_antibiotics_0 == 'Administered' |
      daily_antiviral_0 == 'Administered' |
      has_qualifying_pathogen |
      qualifying_antifungal ~ 1,
    .default = 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_sepsis_0 columns (one row per record_id)
wrapper_calc_sys_sepsis_0 <- function(data, dictionary) {
  # Get enrollment times (Day 0 date)
  data_enrollment <- data |>
    filter(event_label == 'Day 0') |>
    select(record_id, enrollment_time)

  # Get qualifying respiratory pathogens identified on or before Day 0
  data_qualifying_pathogen <- data |>
    filter(event_label == 'Hospital Discharge and Summary') |>
    left_join(
      get_code_label_map('cxpos', dictionary),
      by = 'cxpos'
    ) |>
    left_join(
      get_code_label_map('resp_pathogen', dictionary),
      by = 'resp_pathogen'
    ) |>
    filter(
      cxpos_code == 3 &
      resp_pathogen_code %in% c(1, 2, 3, 4, 5, 7, 8, 17, 18, 19, 20, 21)
    ) |>
    select(record_id, pathogen_date) |>
    left_join(data_enrollment, by = 'record_id') |>
    # Keep only pathogens identified on or before Day 0
    filter(pathogen_date <= enrollment_time) |>
    # Group by patient and mark if ANY qualifying pathogen was found
    group_by(record_id) |>
    summarise(has_qualifying_pathogen = TRUE, .groups = 'drop')

  # Calculate sepsis classification
  data_sepsis <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    select(
      record_id,
      daily_antibiotics_0,
      daily_antiviral_0,
      daily_antifungal_0,
      antifungal_agents_0___1,
      antifungal_agents_0___2,
      antifungal_agents_0___3,
      antifungal_agents_0___6,
      antifungal_agents_0___7,
      antifungal_agents_0___9,
      antifungal_agents_0___13
    ) |>
    left_join(data_qualifying_pathogen, by = 'record_id') |>
    # If no qualifying pathogen found, set to FALSE
    mutate(has_qualifying_pathogen = coalesce(has_qualifying_pathogen, FALSE)) |>
    mutate(
      sys_sepsis_0 = calc_sys_sepsis_0(
        daily_antibiotics_0 = daily_antibiotics_0,
        daily_antiviral_0 = daily_antiviral_0,
        daily_antifungal_0 = daily_antifungal_0,
        antifungal_agents_0___1 = antifungal_agents_0___1,
        antifungal_agents_0___2 = antifungal_agents_0___2,
        antifungal_agents_0___3 = antifungal_agents_0___3,
        antifungal_agents_0___6 = antifungal_agents_0___6,
        antifungal_agents_0___7 = antifungal_agents_0___7,
        antifungal_agents_0___9 = antifungal_agents_0___9,
        antifungal_agents_0___13 = antifungal_agents_0___13,
        has_qualifying_pathogen = has_qualifying_pathogen
      )
    ) |>
    select(record_id, sys_sepsis_0)

  data |>
    distinct(record_id) |>
    left_join(data_sepsis, by = 'record_id')
}


# Check for missing input parameters
check_missing_sys_sepsis_0 <- function(data, record_ids, dictionary) {
  # Check Daily In-Hospital Forms parameters
  daily_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Daily In-Hospital Forms') |>
    select(record_id, daily_antibiotics_0, daily_antiviral_0, daily_antifungal_0,
           antifungal_agents_0___1, antifungal_agents_0___2, antifungal_agents_0___3,
           antifungal_agents_0___6, antifungal_agents_0___7, antifungal_agents_0___9,
           antifungal_agents_0___13) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(daily_antibiotics_0)) missing <- c(missing, "daily_antibiotics_0")
      if (is.na(daily_antiviral_0)) missing <- c(missing, "daily_antiviral_0")
      if (is.na(daily_antifungal_0)) missing <- c(missing, "daily_antifungal_0")
      if (is.na(antifungal_agents_0___1)) missing <- c(missing, "antifungal_agents_0___1")
      if (is.na(antifungal_agents_0___2)) missing <- c(missing, "antifungal_agents_0___2")
      if (is.na(antifungal_agents_0___3)) missing <- c(missing, "antifungal_agents_0___3")
      if (is.na(antifungal_agents_0___6)) missing <- c(missing, "antifungal_agents_0___6")
      if (is.na(antifungal_agents_0___7)) missing <- c(missing, "antifungal_agents_0___7")
      if (is.na(antifungal_agents_0___9)) missing <- c(missing, "antifungal_agents_0___9")
      if (is.na(antifungal_agents_0___13)) missing <- c(missing, "antifungal_agents_0___13")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)

  # Check Hospital Discharge and Summary parameters (pathogen data)
  discharge_missing <- data |>
    filter(record_id %in% record_ids, event_label == 'Hospital Discharge and Summary') |>
    select(record_id, cxpos, resp_pathogen, pathogen_date) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(cxpos)) missing <- c(missing, "cxpos")
      if (is.na(resp_pathogen)) missing <- c(missing, "resp_pathogen")
      if (is.na(pathogen_date)) missing <- c(missing, "pathogen_date")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)

  # Combine both
  bind_rows(daily_missing, discharge_missing) |>
    group_by(record_id) |>
    summarize(missing_params = paste(unique(unlist(strsplit(missing_params, "; "))), collapse = "; "), .groups = "drop")
}
