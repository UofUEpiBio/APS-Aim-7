## -----------------------------------------------------------------------------
## Acute Inflammatory Drug Toxicity (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for acute inflammatory drug toxicity
#'
#' `calc_str_drug_toxicity_0` calculates the streamlined DAG variable for
#' suspected acute inflammatory drug reaction based on organ dysfunction cause
#' and inflammatory condition description.
#'
#' @param organ_dysfnx_cause_code Numeric vector. The `organ_dysfnx_cause` column.
#'
#' @returns A numeric vector with values:
#' - 0 = No suspected drug toxicity
#' - 1 = Suspected drug toxicity
#' @export
calc_str_drug_toxicity_0 <- function(
  organ_dysfnx_cause_code
  ) {

  dplyr::case_when(
    organ_dysfnx_cause_code %in% c(1, 3, 99) ~ 0,
    organ_dysfnx_cause_code == 2 ~ 1
  )
}
