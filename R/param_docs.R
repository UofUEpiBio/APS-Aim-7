## -----------------------------------------------------------------------------
## Master Parameter Documentation
## -----------------------------------------------------------------------------
##
## This file provides centralized parameter documentation for all derived
## variable functions in the APS Aim 7 project. Functions inherit parameter
## documentation using @inheritParams to avoid redundancy and ensure consistency.
##
## Parameters are organized by REDCap event and category for logical grouping.


# =============================================================================
# Day 0 - Baseline Medical History
# =============================================================================

#' Parameters taken from `event_label == 'Day 0'`
#'
#' @param m_endocrine Character vector. Indicator of whether patient has
#'   chronic endocrine conditions (including diabetes mellitus).
#'   Value: "Yes"/"No"/"Unknown".
#' @param m_endo_conditions___1 Character vector. Checkbox for diabetes mellitus.
#'   Value: "Checked"/"Unchecked".
#' @param m_endo_conditions___2 Character vector. Checkbox for adrenal
#'   insufficiency. Value: "Checked"/"Unchecked".
#' @param m_endo_conditions___3 Character vector. Checkbox for osteoporosis.
#'   Value: "Checked"/"Unchecked".
#' @param m_endo_conditions___4 Character vector. Checkbox for hypothyroidism.
#'   Value: "Checked"/"Unchecked".
#' @param m_endo_conditions___5 Character vector. Checkbox for hyperthyroidism.
#'   Value: "Checked"/"Unchecked".
#' @param m_endo_conditions___88 Character vector. Checkbox for other endocrine
#'   condition not listed above. Value: "Checked"/"Unchecked".
#' @param m_immunosuppression Character vector. Indicator of whether patient has
#'   immunocompromising conditions (including medication-induced immunosuppression).
#'   Value: "Yes"/"No"/"Unknown".
#' @param m_immunosup_conditions___1 Character vector. Checkbox for receipt of
#'   systemic corticosteroids at a dose of ≥20 mg prednisone or equivalent (on average)
#'   per day for ≥2 weeks leading up to hospital arrival. Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___2 Character vector. Checkbox for receipt of
#'   immunosuppressive medication in the prior 90 days due to an organ transplant.
#'   Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___3 Character vector. Checkbox for receipt of cancer
#'   treatment that suppresses immune function ("chemotherapy") in the prior 90 days.
#'   Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___4 Character vector. Checkbox for receipt of an
#'   immunosuppressive medication other than corticosteroids for a reason other than
#'   organ transplant or cancer in the prior 90 days. Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___5 Character vector. Checkbox for AIDS (HIV
#'   infection with at least one of the following: CD4 count < 200 cells/mcl; AIDS
#'   defining illness without immune reconstitution; clinical manifestations of
#'   symptomatic HIV). Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___6 Character vector. Checkbox for hematopoietic
#'   ("stem cell," "bone marrow") transplant in the prior 2 years.
#'   Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___7 Character vector. Checkbox for moderate or severe
#'   primary immunodeficiency (e.g., severe combined immunodeficiency, DiGeorge syndrome,
#'   Wiskott-Aldrich syndrome, common variable immunodeficiency disease).
#'   Value: "Checked"/"Unchecked".
#' @param m_immunosup_conditions___88 Character vector. Checkbox for other
#'   immunocompromising condition not listed above. Value: "Checked"/"Unchecked".
#' @param m_pulmonary Character vector. Indicator of whether patient has chronic
#'   pulmonary conditions. Value: "Yes"/"No"/"Unknown".
#' @param m_pulm_conditions___1 Character vector. Checkbox for asthma.
#'   Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___2 Character vector. Checkbox for bronchiectasis.
#'   Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___3 Character vector. Checkbox for chronic obstructive
#'   pulmonary disease (COPD). Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___4 Character vector. Checkbox for home oxygen use.
#'   Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___5 Character vector. Checkbox for pulmonary hypertension.
#'   Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___6 Character vector. Checkbox for restrictive or
#'   interstitial lung disease. Value: "Checked"/"Unchecked".
#' @param m_pulm_conditions___88 Character vector. Checkbox for other pulmonary
#'   condition not listed above. Value: "Checked"/"Unchecked".
#' @param mhrheumd Character vector. Indicator of whether patient has chronic
#'   rheumatologic condition. Value: "Yes"/"No"/"Unknown".
#' @param m_rheum_conditions___1 Character vector. Checkbox for ankylosing
#'   spondylitis. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___2 Character vector. Checkbox for dermatomyositis
#'   and/or polymyositis. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___3 Character vector. Checkbox for polymyalgia
#'   rheumatica. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___4 Character vector. Checkbox for psoriatic
#'   arthritis. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___5 Character vector. Checkbox for rheumatoid
#'   arthritis. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___6 Character vector. Checkbox for scleroderma or
#'   systemic sclerosis. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___7 Character vector. Checkbox for systemic lupus
#'   erythematosus. Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___8 Character vector. Checkbox for vasculitis.
#'   Value: "Checked"/"Unchecked".
#' @param m_rheum_conditions___88 Character vector. Checkbox for other
#'   rheumatologic condition not listed above. Value: "Checked"/"Unchecked".
#'
#' @name day0_baseline_medical_history_params
#' @keywords internal
NULL


# =============================================================================
# Day 0 - Medications Prior to Hospital Arrival
# =============================================================================

#' Parameters taken from `event_label == 'Day 0'`
#'
#' @param mhccster Character vector. Indicator of whether patient was taking
#'   systemic corticosteroids prior to hospital arrival.
#'   Value: "Yes"/"No"/"Unknown".
#' @param mhantifungals Character vector. Indicator of whether patient was taking
#'   systemic anti-fungals prior to hospital arrival.
#'   Value: "Yes"/"No"/"Unknown".
#'
#' @name day0_medications_params
#' @keywords internal
NULL


# =============================================================================
# Day 0 - Enrollment Form & Demographics
# =============================================================================

#' Parameters taken from `event_label == 'Day 0'`
#'
#' @param age Numeric vector. Calculated age at enrollment in years.
#'
#' @name enrollment_demographics_params
#' @keywords internal
NULL


# =============================================================================
# Daily In-Hospital Forms - In-Hospital Treatments
# =============================================================================

#' Parameters taken from `event_label == 'Daily In-Hospital Forms'`
#'
#' @param trx_0 Character vector. Indicator of whether daily treatment data is
#'   available for Day 0. Value: "Available"/"Not Available".
#' @param daily_paralysis_0 Character vector. Indicator of whether chemical
#'   paralysis (other than during intubation) was administered on Day 0.
#'   Value: "Administered"/"Not administered"/"UNK".
#' @param daily_antifungal_0 Character vector. Indicator of whether systemic
#'   antifungal agent(s) were administered on Day 0.
#'   Value: "Administered"/"Not administered"/"UNK".
#'
#' @name daily_treatments_params
#' @keywords internal
NULL


# =============================================================================
# Daily In-Hospital Forms - Daily Assessment/SOFA Scoring
# =============================================================================

#' Parameters taken from `event_label == 'Daily In-Hospital Forms'`
#'
#' @param dailysofa_perf_0 Character vector. Indicator of whether daily assessment
#'   data is available for Day 0. Value: "Available"/"Not Available".
#' @param daily_spo2_8a_0 Numeric vector. SpO2 measurement (%) closest to 8am on
#'   Day 0, representing a stable SpO2 value.
#' @param daily_resp_8a_0 Character vector. Respiratory support at time of SpO2
#'   measurement closest to 8am on Day 0. Categorical field with values for ECMO,
#'   invasive mechanical ventilation, non-invasive ventilation, high-flow nasal
#'   oxygen, standard flow supplemental oxygen, or no respiratory support.
#' @param daily_imv_fio2_8a_0 Numeric vector. FiO2 during invasive mechanical
#'   ventilation at time closest to 8am on Day 0.
#' @param daily_epap_8a_0 Numeric vector. PEEP (positive end-expiratory pressure,
#'   also known as EPAP) during invasive mechanical ventilation at time closest
#'   to 8am on Day 0 (cm H2O).
#' @param daily_niv_fi02_8a_0 Numeric vector. FiO2 during non-invasive ventilation
#'   at time closest to 8am on Day 0.
#' @param daily_hfnc_fi02_8a_0 Numeric vector. FiO2 during high-flow nasal cannula
#'   at time closest to 8am on Day 0.
#' @param daily_standard_flow_8a_0 Numeric vector. Oxygen flow rate (L/min)
#'   during standard flow oxygen delivery (nasal cannula or mask) at time closest
#'   to 8am on Day 0.
#' @param daily_crp_8a_0 Numeric vector. C-reactive protein (CRP) level (mg/L)
#'   closest to 8am on Day 0.
#' @param daily_crp_8a_m1 Numeric vector. C-reactive protein (CRP) level (mg/L)
#'   closest to 8am on Day -1.
#' @param daily_crp_8a_m2 Numeric vector. C-reactive protein (CRP) level (mg/L)
#'   closest to 8am on Day -2.
#' @param daily_gluc_8a_0 Numeric vector. Glucose concentration (mg/dL) closest
#'   to 8am on Day 0.
#' @param daily_ferritin_8a_0 Numeric vector. Ferritin level (ng/mL) closest
#'   to 8am on Day 0.
#' @param daily_ferritin_8a_m1 Numeric vector. Ferritin level (ng/mL) closest
#'   to 8am on Day -1.
#' @param daily_ferritin_8a_m2 Numeric vector. Ferritin level (ng/mL) closest
#'   to 8am on Day -2.
#' @param daily_fibrinogen_8a_0 Numeric vector. Fibrinogen level (mg/dL) closest
#'   to 8am on Day 0.
#' @param daily_fibrinogen_8a_m1 Numeric vector. Fibrinogen level (mg/dL) closest
#'   to 8am on Day -1.
#' @param daily_fibrinogen_8a_m2 Numeric vector. Fibrinogen level (mg/dL) closest
#'   to 8am on Day -2.
#' @param daily_ddimer_8a_0 Numeric vector. D-dimer level (ng/mL) closest
#'   to 8am on Day 0.
#' @param daily_ddimer_8a_m1 Numeric vector. D-dimer level (ng/mL) closest
#'   to 8am on Day -1.
#' @param daily_ddimer_8a_m2 Numeric vector. D-dimer level (ng/mL) closest
#'   to 8am on Day -2.
#' @param daily_crp_nc_0 Character vector. Indicator that CRP was not collected
#'   on Day 0. Value: "Not Collected".
#' @param daily_crp_nc_m1 Character vector. Indicator that CRP was not collected
#'   on Day -1. Value: "Not Collected".
#' @param daily_crp_nc_m2 Character vector. Indicator that CRP was not collected
#'   on Day -2. Value: "Not Collected".
#' @param daily_gluc_nc_0 Character vector. Indicator that glucose was not
#'   collected on Day 0. Value: "Not Collected".
#' @param daily_ferritin_nc_0 Character vector. Indicator that ferritin was not
#'   collected on Day 0. Value: "Not Collected".
#' @param daily_ferritin_nc_m1 Character vector. Indicator that ferritin was not
#'   collected on Day -1. Value: "Not Collected".
#' @param daily_ferritin_nc_m2 Character vector. Indicator that ferritin was not
#'   collected on Day -2. Value: "Not Collected".
#' @param daily_fibrinogen_nc_0 Character vector. Indicator that fibrinogen was not
#'   collected on Day 0. Value: "Not Collected".
#' @param daily_fibrinogen_nc_m1 Character vector. Indicator that fibrinogen was not
#'   collected on Day -1. Value: "Not Collected".
#' @param daily_fibrinogen_nc_m2 Character vector. Indicator that fibrinogen was not
#'   collected on Day -2. Value: "Not Collected".
#' @param daily_ddimer_nc_0 Character vector. Indicator that D-dimer was not
#'   collected on Day 0. Value: "Not Collected".
#' @param daily_ddimer_nc_m1 Character vector. Indicator that D-dimer was not
#'   collected on Day -1. Value: "Not Collected".
#' @param daily_ddimer_nc_m2 Character vector. Indicator that D-dimer was not
#'   collected on Day -2. Value: "Not Collected".
#'
#' @name daily_assessment_params
#' @keywords internal
NULL


# =============================================================================
# Code Map Derived Variables
# =============================================================================

#' Derived variables created using `get_code_label_map()` helper function
#'
#' These variables are derived by converting REDCap label values to numeric
#' codes using the get_code_label_map() helper function. This function creates
#' a mapping between categorical labels and integer codes for easier analysis.
#'
#' Example usage:
#' ```
#' # Get code mapping for respiratory support field
#' resp_code_map <- get_code_label_map('daily_resp_8a_0', dictionary)
#'
#' # Apply mapping to create daily_resp_8a_0_code variable via left join
#' data <- data |>
#'   left_join(resp_code_map, by = 'daily_resp_8a_0')
#' ```
#'
#' @param daily_resp_8a_0_code Integer vector. Numeric code mapping for
#'   `daily_resp_8a_0` field, where 1=ECMO with IMV, 2=ECMO without IMV, 3=IMV
#'   without ECMO, 4=NIV, 5=HFNC, 6=Standard flow, 7=No support, 99=Unknown.
#'   Derived from `daily_resp_8a_0` via code map (not directly in REDCap data).
#'
#' @name code_map_derived_params
#' @keywords internal
NULL


# =============================================================================
# Derived Variables - Respiratory Support Type (Used by Other Functions)
# =============================================================================

#' Parameters for derived respiratory support variables
#'
#' These are derived/calculated variables representing respiratory support
#' characteristics on Day 0. They are created by processing daily assessment data
#' and are themselves used as inputs to other derived variable calculations.
#'
#' @param resp_support_type_0 Integer vector. Categorized respiratory support type
#'   on Day 0, where 1=No respiratory support, 2=Standard flow, 3=HFNC, 4=NIV,
#'   5=IMV with PEEP<12, 6=IMV with PEEP>=12 or ECMO, 99=Unknown. This is a derived
#'   variable calculated by `calc_resp_support_type_0()` and used as an input to
#'   the S/F ratio calculation.
#'
#' @name derived_respiratory_params
#' @keywords internal
NULL
