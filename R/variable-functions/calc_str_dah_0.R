## -----------------------------------------------------------------------------
## Diffuse Alveolar Hemorrhage (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for diffuse alveolar hemorrhage
#'
#' `calc_str_dah_0` calculates the streamlined DAG variable for suspected
#' diffuse alveolar hemorrhage (DAH) based on organ dysfunction cause and
#' inflammatory condition description.
#'
#' @param organ_dysfnx_cause_code Numeric vector. The `organ_dysfnx_cause` column.
#'
#' @returns A numeric vector with values:
#' - 0 = No suspected DAH
#' - 1 = Suspected DAH
#' @export
calc_str_dah_0 <- function(
  organ_dysfnx_cause_code
  ) {

  dplyr::case_when(
    organ_dysfnx_cause_code %in% c(1, 3, 99) ~ 0,
    organ_dysfnx_cause_code == 2 ~ 1
  )
}
