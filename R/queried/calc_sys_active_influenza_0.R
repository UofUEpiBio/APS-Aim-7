## -----------------------------------------------------------------------------
## Active Influenza Infection (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for active influenza infection on Day 0
#'
#' `calc_sys_active_influenza_0` calculates the systematic DAG variable for
#' active influenza infection from the data.
#'
#' @param has_pos_influenza_0 Numeric vector. Indicator (1) if patient had a positive
#' influenza test on or before enrollment, NA otherwise.
#'
#' @returns A vector with values:
#' - 0 = No active influenza (not positive on or before Day 0, or not tested)
#' - 1 = Active influenza (tested positive on or before Day 0)
#' @export
calc_sys_active_influenza_0 <- function(has_pos_influenza_0) {
  dplyr::case_when(
    has_pos_influenza_0 == 1 ~ 1,
    is.na(has_pos_influenza_0) ~ 0
  )
}
