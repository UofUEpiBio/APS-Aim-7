## -----------------------------------------------------------------------------
## Definite Adrenal Insufficiency (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Definite Adrenal Insufficiency' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_def_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2,
  m_immunosuppression,
  m_immunosup_conditions___1
  ) {

  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) & m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 0,

    (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
    (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 1,

    is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}

# Calculate STREAMLINED DAG 'Definite Adrenal Insufficiency' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___2) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_def_adrenal_insufficiency_0 columns (one row per record_id)
wrapper_calc_sys_def_adrenal_insufficiency_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_def_adrenal_insufficiency_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(sys_def_adrenal_insufficiency_0 = calc_sys_def_adrenal_insufficiency_0(
          m_endocrine = m_endocrine,
          m_endo_conditions___2 = m_endo_conditions___2,
          m_immunosuppression = m_immunosuppression,
          m_immunosup_conditions___1 = m_immunosup_conditions___1
        )) |>
        select(record_id, sys_def_adrenal_insufficiency_0),
      by = 'record_id'
    )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_adrenal_insufficiency_0 columns (one row per record_id)
wrapper_calc_str_adrenal_insufficiency_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_adrenal_insufficiency_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_adrenal_insufficiency_0 = calc_str_adrenal_insufficiency_0(
          m_endocrine = m_endocrine,
          m_endo_conditions___2 = m_endo_conditions___2
        )) |>
        select(record_id, str_adrenal_insufficiency_0),
      by = 'record_id'
    )
}


# Check for missing input parameters (SYS)
check_missing_sys_def_adrenal_insufficiency_0 <- function(data, record_ids) {
  data |>
    filter(record_id %in% record_ids, event_label == 'Day 0') |>
    select(record_id, m_endocrine, m_endo_conditions___2, m_immunosuppression, m_immunosup_conditions___1) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(m_endocrine)) missing <- c(missing, "m_endocrine")
      if (is.na(m_endo_conditions___2)) missing <- c(missing, "m_endo_conditions___2")
      if (is.na(m_immunosuppression)) missing <- c(missing, "m_immunosuppression")
      if (is.na(m_immunosup_conditions___1)) missing <- c(missing, "m_immunosup_conditions___1")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)
}

# Check for missing input parameters (STR)
check_missing_str_adrenal_insufficiency_0 <- function(data, record_ids) {
  data |>
    filter(record_id %in% record_ids, event_label == 'Day 0') |>
    select(record_id, m_endocrine, m_endo_conditions___2) |>
    distinct() |>
    rowwise() |>
    mutate(missing_params = {
      missing <- c()
      if (is.na(m_endocrine)) missing <- c(missing, "m_endocrine")
      if (is.na(m_endo_conditions___2)) missing <- c(missing, "m_endo_conditions___2")
      if (length(missing) > 0) paste(missing, collapse = "; ") else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(missing_params)) |>
    select(record_id, missing_params)
}
