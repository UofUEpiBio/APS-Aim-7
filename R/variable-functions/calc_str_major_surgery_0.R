## -----------------------------------------------------------------------------
## Major Recent Surgery (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for major recent surgery on Day 0
#'
#' `calc_str_major_surgery_0` calculates the streamlined DAG variable for
#' major recent surgery by checking if any surgery occurred in the OR from
#' Day -2 to Day 0.
#'
#' @param daily_surgery_m2 Character vector. Surgery indicator on Day -2.
#' @param daily_surgery_m1 Character vector. Surgery indicator on Day -1.
#' @param daily_surgery_0 Character vector. Surgery indicator on Day 0.
#' ## QUESTION: Do we need description variables? If so, what are they?
#' ## - `daily_surgery_description_0` doesn't exist in the data codebook
#'
#' @returns A numeric vector with values:
#' - 0 = No surgery on Day -2, Day -1, AND Day 0
#' - 1 = Surgery on Day -2, Day -1, OR Day 0
#' - 99 = Unknown
#' @export
calc_str_major_surgery_0 <- function(
  daily_surgery_m2,
  daily_surgery_m1,
  daily_surgery_0
) {

  ## Check if surgery occurred on any of the three days
  had_surgery <- (
    daily_surgery_m2 == "Administered" |
    daily_surgery_m1 == "Administered" |
    daily_surgery_0 == "Administered"
  )

  # QUESTION: Should this be AND instead of OR?
  # - Many records (~295) have "Not administered" on one day and "UNK"/NA on the others
  no_surgery <- (
    daily_surgery_m2 == "Not administered" |
    daily_surgery_m1 == "Not administered" |
    daily_surgery_0 == "Not administered"
  )

  unk_surgery <- (
    daily_surgery_m2 == "UNK" |
    daily_surgery_m1 == "UNK" |
    daily_surgery_0 == "UNK"
  )

  ## Return binary indicator
  dplyr::case_when(
    had_surgery ~ 1,
    no_surgery ~ 0,
    unk_surgery ~ 99
  )
}
