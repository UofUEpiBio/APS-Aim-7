## -----------------------------------------------------------------------------
## Inflammatory Profile (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Inflammatory Profile'
#
# Values:
# - 0 = CRP not checked
# - 1 = CRP checked and < 15 mg/L
# - 2 = CRP checked and >= 15 mg/L
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


# Calculate SYSTEMATIC DAG 'Inflammatory Profile: CRP Component'
#
# Values:
# - 0 = CRP not checked
# - 1 = CRP checked and < 15 mg/L
# - 2 = CRP checked and >= 15 mg/L
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

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: Ferritin Component'
#
# Values:
# - 0 = Ferritin not checked
# - 1 = Ferritin checked and < 700 ng/mL
# - 2 = Ferritin checked and >= 700 ng/mL
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

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: Fibrinogen Component'
#
# Values:
# - 0 = Fibrinogen not checked
# - 1 = Fibrinogen checked and < 150 mg/dL
# - 2 = Fibrinogen checked and >= 150 mg/dL
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

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: D-dimer Component'
#
# Values:
# - 0 = D-dimer not checked
# - 1 = D-dimer checked and < 1500 ng/mL
# - 2 = D-dimer checked and >= 1500 ng/mL
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
