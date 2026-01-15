## -----------------------------------------------------------------------------
## SCAP Criterion: Bilateral Opacities (Streamlined DAG)
## -----------------------------------------------------------------------------

#' Calculate the streamlined DAG variable for SCAP bilateral opacities criterion
#'
#' `calc_str_scap_bilateral_opacities_0` calculates the SCAP (Severe Community-Acquired
#' Pneumonia) bilateral opacities criterion by checking imaging from Days -2, -1, and 0.
#'
#' @param cxr_dm2_available Character vector. CXR availability on Day -2.
#' @param cxr_dm2_opacity Character vector. CXR opacity on Day -2.
#' @param ct_dm2_available Character vector. CT availability on Day -2.
#' @param ct_dm2_opacity Character vector. CT opacity on Day -2.
#' @param cxr_dm1_available Character vector. CXR availability on Day -1.
#' @param cxr_dm1_opacity Character vector. CXR opacity on Day -1.
#' @param ct_dm1_available Character vector. CT availability on Day -1.
#' @param ct_dm1_opacity Character vector. CT opacity on Day -1.
#' @param cxr_d0_available Character vector. CXR availability on Day 0.
#' @param cxr_d0_opacity Character vector. CXR opacity on Day 0.
#' @param ct_d0_available Character vector. CT availability on Day 0.
#' @param ct_d0_opacity Character vector. CT opacity on Day 0.
#'
#' @returns A numeric vector with values:
#' - 0 = No bilateral opacities (none are "2")
#' - 1 = Bilateral opacities present (any are "2")
#' @export
calc_str_scap_bilateral_opacities_0 <- function(
  cxr_dm2_available,
  cxr_dm2_opacity_code,
  ct_dm2_available,
  ct_dm2_opacity_code,

  cxr_dm1_available,
  cxr_dm1_opacity_code,
  ct_dm1_available,
  ct_dm1_opacity_code,

  cxr_d0_available,
  cxr_d0_opacity_code,
  ct_d0_available,
  ct_d0_opacity_code
) {

  ## Check if any opacity value is "2" (bilateral)
  has_opacity <- (
    cxr_dm2_opacity_code == "2" |
    ct_dm2_opacity_code == "2" |

    cxr_dm1_opacity_code == "2" |
    ct_dm1_opacity_code == "2" |

    cxr_d0_opacity_code == "2" |
    ct_d0_opacity_code == "2"
  )

  # QUESTION: How to handle missing imaging data?
  # - Here I assume that all will have valid entries, but the vast majority are NA
  # - for at least one of these fields
  # - Should we just assign 0 if the first condition isn't met?
  has_no_opacity <- (
    (cxr_dm2_available == "No" |
    (cxr_dm2_available == "Yes" & cxr_dm2_opacity_code %in% c("00", "0", "1", "99"))) &
    (ct_dm2_available == "No" |
    (ct_dm2_available == "Yes" & ct_dm2_opacity_code %in% c("00", "0", "1", "99"))) &

    (cxr_dm1_available == "No" |
    (cxr_dm1_available == "Yes" & cxr_dm1_opacity_code %in% c("00", "0", "1", "99"))) &
    (ct_dm1_available == "No" |
    (ct_dm1_available == "Yes" & ct_dm1_opacity_code %in% c("00", "0", "1", "99"))) &

    (cxr_d0_available == "No" |
    (cxr_d0_available == "Yes" & cxr_d0_opacity_code %in% c("00", "0", "1", "99"))) &
    (ct_d0_available == "No" |
    (ct_d0_available == "Yes" & ct_d0_opacity_code %in% c("00", "0", "1", "99")))
  )

  dplyr::case_when(
    has_opacity ~ 1,
    has_no_opacity ~ 0
  )
}
