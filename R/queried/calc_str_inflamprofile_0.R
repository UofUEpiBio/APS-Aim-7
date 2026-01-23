## -----------------------------------------------------------------------------
## Inflammatory Profile (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for inflammatory profile on Day 0
#'
#' `calc_str_inflamprofile_0` calculates the streamlined DAG variable for
#' inflammatory profile from the data. This variable represents
#' CRP level on admission day. If the Day 0 value is missing, it looks back up to
#' 2 days to find a valid value.
#'
#' @param daily_crp_8a_0 Numeric vector. The `daily_crp_8a_0` column from the data.
#' @param daily_crp_nc_0 Character vector. The `daily_crp_nc_0` column from the data.
#'
#' @returns A vector with values:
#' - 0 = Day 0 CRP not checked
#' - 1 = Day 0 CRP checked and < 15
#' - 2 = Day 0 CRP checked and ≥ 15
#' @export
calc_str_inflamprofile_0 <- function(
  daily_crp_8a_0,
  daily_crp_nc_0,
  daily_crp_8a_m1,
  daily_crp_nc_m1,
  daily_crp_8a_m2,
  daily_crp_nc_m2
  ) {
  calc_crp_0(
    daily_crp_8a_0,
    daily_crp_nc_0,
    daily_crp_8a_m1,
    daily_crp_nc_m1,
    daily_crp_8a_m2,
    daily_crp_nc_m2
  )
}


# Calculate the CRP component of the inflammatory profile variable
calc_crp_0 <- function(
  daily_crp_8a_0,
  daily_crp_nc_0,
  daily_crp_8a_m1,
  daily_crp_nc_m1,
  daily_crp_8a_m2,
  daily_crp_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 CRP >= 15 (if missing, look back up to 2 days)
    daily_crp_8a_0 >= 15 |
      (is.na(daily_crp_8a_0) & daily_crp_8a_m1 >= 15) |
      (is.na(daily_crp_8a_0) & is.na(daily_crp_8a_m1) & daily_crp_8a_m2 >= 15) ~ 2,

    # Day 0 CRP >= 15 (if missing, look back up to 2 days)
    daily_crp_8a_0 < 15 |
      (is.na(daily_crp_8a_0) & daily_crp_8a_m1 < 15) |
      (is.na(daily_crp_8a_0) & is.na(daily_crp_8a_m1) & daily_crp_8a_m2 < 15) ~ 1,

    # Day 0 CRP not collected (if missing, look back up to 2 days)
    daily_crp_nc_0 == 'Not Collected' |
      daily_crp_nc_m1 == 'Not Collected' |
      daily_crp_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate the Ferritin component of the inflammatory profile variable
calc_ferritin_0 <- function(
  daily_ferritin_8a_0,
  daily_ferritin_nc_0,
  daily_ferritin_8a_m1,
  daily_ferritin_nc_m1,
  daily_ferritin_8a_m2,
  daily_ferritin_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 Ferritin >= 700 (if missing, look back up to 2 days)
    daily_ferritin_8a_0 >= 700 |
      (is.na(daily_ferritin_8a_0) & daily_ferritin_8a_m1 >= 700) |
      (is.na(daily_ferritin_8a_0) & is.na(daily_ferritin_8a_m1) & daily_ferritin_8a_m2 >= 700) ~ 2,

    # Day 0 Ferritin < 700 (if missing, look back up to 2 days)
    daily_ferritin_8a_0 < 700 |
      (is.na(daily_ferritin_8a_0) & daily_ferritin_8a_m1 < 700) |
      (is.na(daily_ferritin_8a_0) & is.na(daily_ferritin_8a_m1) & daily_ferritin_8a_m2 < 700) ~ 1,

    # Day 0 Ferritin not collected (if missing, look back up to 2 days)
    daily_ferritin_nc_0 == 'Not Collected' |
      daily_ferritin_nc_m1 == 'Not Collected' |
      daily_ferritin_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate the Fibrinogen component of the inflammatory profile variable
calc_fibrinogen_0 <- function(
  daily_fibrinogen_8a_0,
  daily_fibrinogen_nc_0,
  daily_fibrinogen_8a_m1,
  daily_fibrinogen_nc_m1,
  daily_fibrinogen_8a_m2,
  daily_fibrinogen_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 Fibrinogen >= 150 (if missing, look back up to 2 days)
    daily_fibrinogen_8a_0 >= 150 |
      (is.na(daily_fibrinogen_8a_0) & daily_fibrinogen_8a_m1 >= 150) |
      (is.na(daily_fibrinogen_8a_0) & is.na(daily_fibrinogen_8a_m1) & daily_fibrinogen_8a_m2 >= 150) ~ 2,

    # Day 0 Fibrinogen < 150 (if missing, look back up to 2 days)
    daily_fibrinogen_8a_0 < 150 |
      (is.na(daily_fibrinogen_8a_0) & daily_fibrinogen_8a_m1 < 150) |
      (is.na(daily_fibrinogen_8a_0) & is.na(daily_fibrinogen_8a_m1) & daily_fibrinogen_8a_m2 < 150) ~ 1,

    # Day 0 Fibrinogen not collected (if missing, look back up to 2 days)
    daily_fibrinogen_nc_0 == 'Not Collected' |
      daily_fibrinogen_nc_m1 == 'Not Collected' |
      daily_fibrinogen_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate the D-dimer component of the inflammatory profile variable
calc_ddimer_0 <- function(
  daily_ddimer_8a_0,
  daily_ddimer_nc_0,
  daily_ddimer_8a_m1,
  daily_ddimer_nc_m1,
  daily_ddimer_8a_m2,
  daily_ddimer_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 D-dimer >= 1500 (if missing, look back up to 2 days)
    daily_ddimer_8a_0 >= 1500 |
      (is.na(daily_ddimer_8a_0) & daily_ddimer_8a_m1 >= 1500) |
      (is.na(daily_ddimer_8a_0) & is.na(daily_ddimer_8a_m1) & daily_ddimer_8a_m2 >= 1500) ~ 2,

    # Day 0 D-dimer < 1500 (if missing, look back up to 2 days)
    daily_ddimer_8a_0 < 1500 |
      (is.na(daily_ddimer_8a_0) & daily_ddimer_8a_m1 < 1500) |
      (is.na(daily_ddimer_8a_0) & is.na(daily_ddimer_8a_m1) & daily_ddimer_8a_m2 < 1500) ~ 1,

    # Day 0 D-dimer not collected (if missing, look back up to 2 days)
    daily_ddimer_nc_0 == 'Not Collected' |
      daily_ddimer_nc_m1 == 'Not Collected' |
      daily_ddimer_nc_m2 == 'Not Collected' ~ 0
  )
}
