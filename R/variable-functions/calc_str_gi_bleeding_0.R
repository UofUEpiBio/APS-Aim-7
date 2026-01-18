## -----------------------------------------------------------------------------
## Gastrointestinal Bleeding (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for GI bleeding on Day 0
#'
#' `calc_str_gi_bleeding_0` calculates the streamlined DAG variable for
#' gastrointestinal bleeding by summing packed red blood cell units transfused
#' from Day -2 to Day 0. Only counts units when branching logic is satisfied.
#'
#' @param blood_prbc_units_m2 Numeric vector. PRBC units on Day -2.
#' @param blood_prbc_units_m1 Numeric vector. PRBC units on Day -1.
#' @param blood_prbc_units_0 Numeric vector. PRBC units on Day 0.
#' @param daily_blood_product_m2 Character vector. Blood product indicator on Day -2.
#' @param daily_blood_product_m1 Character vector. Blood product indicator on Day -1.
#' @param daily_blood_product_0 Character vector. Blood product indicator on Day 0.
#' @param blood_product_spec_m2___1 Character vector. PRBC checkbox on Day -2.
#' @param blood_product_spec_m1___1 Character vector. PRBC checkbox on Day -1.
#' @param blood_product_spec_0___1 Character vector. PRBC checkbox on Day 0.
#'
#' @returns A numeric vector representing total PRBC units transfused (Days -2 to 0).
#'   Returns NA if branching logic configuration is invalid.
#' @export
calc_str_gi_bleeding_0 <- function(
  blood_prbc_units_m2,
  blood_prbc_units_m1,
  blood_prbc_units_0,
  daily_blood_product_m2,
  daily_blood_product_m1,
  daily_blood_product_0,
  blood_product_spec_m2___1,
  blood_product_spec_m1___1,
  blood_product_spec_0___1
) {

  # Day -2:
  prbc_m2 <- dplyr::case_when(
    (daily_blood_product_m2 == "Administered" &
      is_checked(blood_product_spec_m2___1)) ~ blood_prbc_units_m2,

    (daily_blood_product_m2 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_m2___1)) ~ 0
  )

  # Day -1:
  prbc_m1 <- dplyr::case_when(
    (daily_blood_product_m1 == "Administered" &
      is_checked(blood_product_spec_m1___1)) ~ blood_prbc_units_m1,

    (daily_blood_product_m1 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_m1___1)) ~ 0
  )

  # Day 0:
  prbc_0 <- dplyr::case_when(
    (daily_blood_product_0 == "Administered" &
      is_checked(blood_product_spec_0___1)) ~ blood_prbc_units_0,

    (daily_blood_product_0 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_0___1)) ~ 0
  )

  # Sum PRBC units across all three days
  # - If any day has invalid configuration, return NA
  # - Otherwise sum all days (values and 0s)
  # QUESTION: Should we treat NA for a single day as 0 instead of NA overall?
  # - I think YES, because there are 317 NA right now, mostly due to NA for Day -2
  total_prbc <- rowSums(
    cbind(prbc_m2, prbc_m1, prbc_0),
    na.rm = FALSE
  )

  return(total_prbc)
}
