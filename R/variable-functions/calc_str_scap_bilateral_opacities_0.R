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

  # Two methods were used in assembling these variables:
  # Method 1 - cxr/ct_available indicates if imaging was done, with the result stored in cxr/ct_opacity (if present)
  # Method 2 - cxr_opacity set to "00" or "0" indicates nothing was found for BOTH cxr and ct
  # Thus we check both options for each day and if any day has invalid data, it should be queried
  has_no_opacity <- (

    # Day -2
    (
      (cxr_dm2_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_dm2_available %in% c("No", "Yes") & ct_dm2_available %in% c("No", "Yes"))
    ) &

    # Day -1
    (
      (cxr_dm1_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_dm1_available %in% c("No", "Yes") & ct_dm1_available %in% c("No", "Yes"))
    ) &

    # Day 0
    (
      (cxr_d0_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_d0_available %in% c("No", "Yes") & ct_d0_available %in% c("No", "Yes"))
    )
  )

  dplyr::case_when(
    has_opacity ~ 1,
    has_no_opacity ~ 0
  )
}
