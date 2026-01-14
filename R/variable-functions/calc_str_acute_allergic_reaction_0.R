## -----------------------------------------------------------------------------
## Acute Allergic Reaction (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for acute allergic reaction
#'
#' `calc_str_acute_allergic_reaction_0` calculates the streamlined DAG variable
#' for suspected anaphylaxis based on organ dysfunction cause and inflammatory
#' condition description.
#'
#' @param organ_dysfnx_cause_code Numeric vector. The `organ_dysfnx_cause` column.
#'
#' @returns A numeric vector with values:
#' - 0 = No suspected anaphylaxis
#' - 1 = Suspected anaphylaxis
#' @export
calc_str_acute_allergic_reaction_0 <- function(
  organ_dysfnx_cause_code
  ) {

  # QUESTION: The original variable definition uses
  # - 0 = OR `organ_dysnfx_inflam` is not related to allergic reaction or anaphylaxis
  # - 1 = AND `organ_dysnfx_inflam` is related to allergic reaction or anaphylaxis
  # - But `organ_dysnfx_inflam` isn't in the data. How should we handle this?
  # For now, using only `organ_dysfnx_cause_code`

  dplyr::case_when(
    organ_dysfnx_cause_code == 2 ~ 1,
    organ_dysfnx_cause_code %in% c(1, 3, 99) ~ 0
  )
}
