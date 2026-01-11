## -----------------------------------------------------------------------------
## Baseline Performance Status (Systematic DAG)
## -----------------------------------------------------------------------------


#' Calculate the systematic DAG variable for frailty status on Day 0
#'
#' `calc_sys_frailty_status_0` calculates the first variable component of
#' the systematic DAG variable for baseline performance status from the data.
#'
#' @param frailty_base_code Integer vector. The `frailty_base_code` code map from the
#' `frailty_base` column from the data.
#' @param frailty_perf Character vector. The `frailty_perf` column from the data.
#'
#' @returns A vector with values:
#' - 0 = Not performed
#' - 1 = Very Fit
#' - 2 = Fit
#' - 3 = Managing Well
#' - 4 = Living with very mild frailty
#' - 5 = Living with mild frailty
#' - 6 = Living with moderate frailty
#' - 7 = Living with severe frailty
#' - 8 = Living with very severe frailty
#' - 9 = Terminally ill
#' - 99 = Unknown
#' @export
calc_sys_frailty_status_0 <- function(
  frailty_base_code,
  frailty_perf
) {
  dplyr::case_when(
    frailty_perf == "No" ~ 0,
    frailty_perf == "Yes" & !is.na(frailty_base_code) ~ as.numeric(frailty_base_code),
    frailty_perf == "Yes" & is.na(frailty_base_code) ~ 99
  )
}
