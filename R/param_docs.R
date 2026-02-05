## -----------------------------------------------------------------------------
## Master Parameter Documentation
## -----------------------------------------------------------------------------
##
## This file provides centralized parameter documentation for all derived
## variable functions in the APS Aim 7 project.
##
## Parameters are organized by REDCap event (`event_label`).
##
## Though the functions are not documented using `roxygen`, we use the `@param`
## tags here because this file will render nicely in RStudio and can be searched
## easily.


# =============================================================================
# Parameters from `event_label == 'Day 0'`
# =============================================================================

#' Demographics
#' @param age Numeric. Age at enrollment in years.
#' @param sex Biological sex assigned at birth: "Male"/"Female"/"Unknown".
#' @param prssrc Residence prior to hospital admission. Examples: "Hospice (including home hospice)"/"Homeless shelter"/"Unknown".
#'
#' Baseline Clinical Measurements
#' @param m_weight_kg Numeric. Patient weight in kilograms.
#' @param lowtemp_vsorres,hightemp_vsorres Numeric. Lowest/highest temperature (°C).
#' @param lowhr_vsorres,highhr_vsorres Numeric. Lowest/highest heart rate (beats/min).
#' @param lowrr_vsorres,highrr_vsorres Numeric. Lowest/highest respiratory rate (breaths/min).
#' @param lowsysbp_vsorres Numeric. Lowest systolic blood pressure (mmHg).
#' @param lowdbp_vsorres Numeric. Lowest diastolic blood pressure (mmHg).
#'
#' Medications
#' @param mhccster Taking systemic corticosteroids prior to arrival: "Yes"/"No"/"Unknown".
#' @param mhantifungals Taking systemic anti-fungals prior to hospital arrival: "Yes"/"No"/"Unknown".
#'
#' Enrollment and Data Availability Indicators
#' @param enrollment_time Date/time of patient enrollment.
#' @param vs_perf Vital signs performed: "Yes"/"No".
#' @param sofa_unk SOFA baseline information unknown: indicator value.
#'
#'
#' Baseline Chronic Conditions ------------------------------------------------
#'
#' Endocrine Conditions
#' @param m_endocrine Has chronic endocrine condition (including diabetes mellitus): "Yes"/"No"/"Unknown".
#' - Specific condition checkboxes (values: "Checked"/"Unchecked"):
#' @param m_endo_conditions___1 Diabetes mellitus.
#' @param m_endo_conditions___2 Adrenal.
#' @param m_endo_conditions___3 Osteoporosis.
#' @param m_endo_conditions___4 Hypothyroidism.
#' @param m_endo_conditions___5 Hyperthyroidism.
#' @param m_endo_conditions___88 Other condition not listed above.
#'
#' Immunocompromising Conditions
#' @param m_immunosuppression Has immunocompromising condition (including medication-induced): "Yes"/"No"/"Unknown".
#' - Specific condition checkboxes (values: "Checked"/"Unchecked"):
#' @param m_immunosup_conditions___1 Systemic corticosteroids ≥20 mg prednisone equivalent per day for ≥2 weeks prior to arrival.
#' @param m_immunosup_conditions___2 Immunosuppressive medication in prior 90 days due to organ transplant.
#' @param m_immunosup_conditions___3 Cancer treatment suppressing immune function ("chemotherapy") in prior 90 days.
#' @param m_immunosup_conditions___4 Immunosuppressive medication (non-corticosteroid) in prior 90 days for reason other than transplant or cancer.
#' @param m_immunosup_conditions___5 AIDS (HIV with CD4 <200 cells/mcl, AIDS-defining illness, or symptomatic HIV).
#' @param m_immunosup_conditions___6 Hematopoietic ("stem cell," "bone marrow") transplant in prior 2 years.
#' @param m_immunosup_conditions___7 Moderate or severe primary immunodeficiency (e.g., SCID, DiGeorge, Wiskott-Aldrich, CVID).
#' @param m_immunosup_conditions___88 Other condition not listed above.
#'
#' Pulmonary Conditions
#' @param m_pulmonary Has chronic pulmonary condition: "Yes"/"No"/"Unknown".
#' - Specific condition checkboxes (values: "Checked"/"Unchecked"):
#' @param m_pulm_conditions___1 Asthma.
#' @param m_pulm_conditions___2 Bronchiectasis.
#' @param m_pulm_conditions___3 Chronic obstructive pulmonary disease (COPD).
#' @param m_pulm_conditions___4 Home oxygen use.
#' @param m_pulm_conditions___5 Pulmonary hypertension.
#' @param m_pulm_conditions___6 Restrictive or interstitial lung disease.
#' @param m_pulm_conditions___88 Other condition not listed above.
#'
#' Rheumatological Conditions
#' @param mhrheumd Has chronic rheumatologic condition: "Yes"/"No"/"Unknown".
#' - Specific condition checkboxes (values: "Checked"/"Unchecked"):
#' @param m_rheum_conditions___1 Ankylosing spondylitis.
#' @param m_rheum_conditions___2 Dermatomyositis and/or polymyositis.
#' @param m_rheum_conditions___3 Polymyalgia rheumatica.
#' @param m_rheum_conditions___4 Psoriatic arthritis.
#' @param m_rheum_conditions___5 Rheumatoid arthritis.
#' @param m_rheum_conditions___6 Scleroderma or systemic sclerosis.
#' @param m_rheum_conditions___7 Systemic lupus erythematosus.
#' @param m_rheum_conditions___8 Vasculitis.
#' @param m_rheum_conditions___88 Other condition not listed above.
#'
#' Neurological Conditions
#' @param m_neurologic Has chronic neurologic condition: "Yes"/"No"/"Unknown".
#' - Specific condition checkboxes (values: "Checked"/"Unchecked"):
#' @param m_neurologic_conditions___1 Dementia.
#' @param m_neurologic_conditions___2 Prior stroke.
#' @param m_neurologic_conditions___3 Prior transient ischemic attack (TIA).
#' @param m_neurologic_conditions___4 Hemiplegia.
#' @param m_neurologic_conditions___5 Paraplegia.
#' @param m_neurologic_conditions___6 Neuromuscular disease (e.g., myasthenia gravis, Guillain-Barré, ALS).
#'
#' Cardiovascular Conditions
#' @param m_cardiovascular Has chronic cardiovascular condition: 'Yes'/'No'/'Unknown'.
#' - Specific condition checkboxes (values: 'Checked'/'Unchecked'):
#' @param m_cv_conditions___1 Hypertension.
#' @param m_cv_conditions___2 Heart failure.
#' @param m_cv_conditions___3 Coronary artery disease (CAD).
#' @param m_cv_conditions___4 Prior myocardial infarction.
#' @param m_cv_conditions___5 Atrial fibrillation or atrial flutter.
#' @param m_cv_conditions___6 Peripheral vascular disease (symptomatic or peripheral artery bypass/revascularization).
#' @param m_cv_conditions___7 Aortic aneurysm >6cm.
#' @param m_cv_conditions___88 Other.
#'
#' Cancer Conditions
#' @param m_cancer Has active cancer: 'Yes'/'No'/'Unknown'.
#' - Specific condition checkboxes (values: 'Checked'/'Unchecked'):
#' @param m_cancer_conditions___1 Leukemia.
#' @param m_cancer_conditions___2 Lymphoma.
#' @param m_cancer_conditions___3 Multiple myeloma.
#' @param m_cancer_conditions___4 Solid organ cancer.
#' @param m_cancer_conditions___88 Other.
#'
#' Kidney and Liver Conditions
#' @param m_kid_liver Has chronic kidney or liver condition: 'Yes'/'No'/'Unknown'.
#' - Specific condition checkboxes (values: 'Checked'/'Unchecked'):
#' @param m_kid_liver_conditions___1 Chronic kidney disease.
#' @param m_kid_liver_conditions___2 Cirrhosis.
#' @param m_kid_liver_conditions___88 Other.
#' - Severity parameters:
#' @param mhckdsev Chronic kidney disease severity. Examples: "Baseline (before acute illness) creatinine  ≤3.0 mg/dl"/"Unknown severity"
#' @param mhhepsev Liver disease severity. Examples: "Mild (chronic hepatitis w/o portal HTN)"/"Unknown cirrhosis severity"
#' @param mhhepfail Hepatic failure present: "Yes"/"No"/"Unknown".
#'
#' Esophageal, Stomach, and Intestinal Conditions
#' - Specific condition checkboxes (values: 'Checked'/'Unchecked'):
#' @param m_eso_stom_int_conditions___1 Peptic ulcer disease.
#' @param m_eso_stom_int_conditions___2 Inflammatory bowel disease (including Crohn's Disease and Ulcerative Colitis).
#' @param m_eso_stom_int_conditions___3 Chronic enteral tube for feeding.
#' @param m_eso_stom_int_conditions___88 Other.
#'
#' Diabetes Severity
#' @param mhdmsev Diabetes severity. Examples: "Diet controlled"/"Unknown severity"
#'
#' Cancer Metastasis
#' @param mhsolidmeta Solid tumor metastasis: "Yes"/"No"/"Unknown".
#'
#' SOFA Baseline Renal Parameters
#' @param sofa_base_renal_dysnfx Baseline renal dysfunction present: "Yes"/"No".
#' @param sofa_base_renal_chronic Chronic renal severity. Examples: "Creatinine 1.2 - 1.9 mg/dL"/"Creatinine 2.0 - 3.4 mg/dL"
#'
#' Psychiatric Conditions
#' @param m_psych Has chronic psychiatric condition: 'Yes'/'No'/'Unknown'.
#' - Specific condition checkboxes (values: 'Checked'/'Unchecked'):
#' @param m_psych_conditions___1 Depression.
#' @param m_psych_conditions___2 Anxiety (including panic disorder).
#' @param m_psych_conditions___3 Post-traumatic stress disorder (PTSD).
#' @param m_psych_conditions___4 Psychotic disorder (including schizophrenia).
#' @param m_psych_conditions___88 Other.
#'
#' @name day0_params
#' @keywords internal
NULL

# =============================================================================
# Parameters from `event_label == 'Patient/Surrogate Interview'`
# =============================================================================

#' @param frailty_perf Was Clinical Frailty Scale performed: "Yes"/"No".
#' @param frailty_base Clinical Frailty Scale score. Examples: "Very Fit"/"Terminally ill".
#'
#' Recreational Substance Use (other than tobacco or alcohol)
#' - Specific substance checkboxes (values: "Checked"/"Unchecked"):
#' @param susubcat___0 No other substance use.
#' @param susubcat___1 Cocaine.
#' @param susubcat___2 Oral opioid.
#' @param susubcat___3 Injection opioid.
#' @param susubcat___4 Methamphetamine.
#' @param susubcat___5 Oral stimulant.
#' @param susubcat___6 Oral sedative.
#' @param susubcat___7 Marijuana/cannabis.
#' @param susubcat___8 Hallucinogen.
#' @param susubcat___9 Inhalant.
#' @param susubcat___88 Other substance.
#' @param susubcat___99 Unknown substance.
#'
#' @name patient_surrogate_interview_params
#' @keywords internal
NULL


# =============================================================================
# Parameters from `event_label == 'Daily In-Hospital Forms'`
# =============================================================================

#' Data Availability Indicators
#' @param trx_0,trx_m1,trx_m2 Daily treatment data available: "Available"/"Not Available".
#' @param dailysofa_perf_0,dailysofa_perf_m1,dailysofa_perf_m2 Daily assessment data available: "Available"/"Not Available".
#'
#' Treatments and Procedures
#' @param daily_paralysis_0,daily_paralysis_m1,daily_paralysis_m2 Chemical paralysis (other than during intubation) administered: "Administered"/"Not administered"/"UNK".
#' @param daily_antifungal_0,daily_antifungal_m1,daily_antifungal_m2 Systemic antifungal agent(s) administered: "Administered"/"Not administered"/"UNK".
#' @param daily_surgery_0,daily_surgery_m1,daily_surgery_m2 Surgery in operating room occurred: "Administered"/"Not administered"/"UNK".
#'
#' Blood Product Transfusions
#' @param blood_prbc_units_0,blood_prbc_units_m1,blood_prbc_units_m2 Numeric. Number of packed red blood cell units transfused.
#' @param daily_blood_product_0,daily_blood_product_m1,daily_blood_product_m2 Blood product transfusion: "Administered"/"Not administered"/"UNK".
#' - Specific blood product checkboxes (values: "Checked"/"Unchecked"):
#' @param blood_product_spec_0___1,blood_product_spec_m1___1,blood_product_spec_m2___1 Packed red blood cells (pRBCs).
#' @param blood_product_spec_0___2,blood_product_spec_m1___2,blood_product_spec_m2___2 Platelets.
#' @param blood_product_spec_0___3,blood_product_spec_m1___3,blood_product_spec_m2___3 Fresh frozen plasma (FFP).
#' @param blood_product_spec_0___4,blood_product_spec_m1___4,blood_product_spec_m2___4 Cryoprecipitate.
#' @param blood_product_spec_0___5,blood_product_spec_m1___5,blood_product_spec_m2___5 Whole blood.
#' @param blood_product_spec_0___88,blood_product_spec_m1___88,blood_product_spec_m2___88 Other.
#' @param blood_product_spec_0___99,blood_product_spec_m1___99,blood_product_spec_m2___99 Unknown.
#'
#' Respiratory Support and Oxygenation
#' @param daily_resp_8a_0,daily_resp_8a_m1,daily_resp_8a_m2 Respiratory support at time of SpO2 measurement closest to 8am. Examples: "ECMO and invasive mechanical ventilation"/"Standard flow supplemental oxygen"
#' @param daily_imv_mode_8a_0,daily_imv_mode_8a_m1,daily_imv_mode_8a_m2 Invasive mechanical ventilation mode at time closest to 8am.
#' @param daily_spo2_8a_0,daily_spo2_8a_m1,daily_spo2_8a_m2 Numeric. SpO2 measurement (%) closest to 8am, representing stable SpO2 value.
#' @param daily_imv_fio2_8a_0,daily_imv_fio2_8a_m1,daily_imv_fio2_8a_m2 Numeric. FiO2 during invasive mechanical ventilation at time closest to 8am.
#' @param daily_epap_8a_0,daily_epap_8a_m1,daily_epap_8a_m2 Numeric. PEEP (positive end-expiratory pressure, also known as EPAP) during invasive mechanical ventilation at time closest to 8am (cm H2O).
#' @param daily_niv_fi02_8a_0,daily_niv_fi02_8a_m1,daily_niv_fi02_8a_m2 Numeric. FiO2 during non-invasive ventilation at time closest to 8am.
#' @param daily_hfnc_fi02_8a_0,daily_hfnc_fi02_8a_m1,daily_hfnc_fi02_8a_m2 Numeric. FiO2 during high-flow nasal cannula at time closest to 8am.
#' @param daily_standard_flow_8a_0,daily_standard_flow_8a_m1,daily_standard_flow_8a_m2 Numeric. Oxygen flow rate (L/min) during standard flow oxygen delivery (nasal cannula or mask) at time closest to 8am.
#' @param daily_resp_rate_8a_0,daily_resp_rate_8a_m1,daily_resp_rate_8a_m2 Numeric. Set respiratory rate (breaths/min) during IMV at time closest to 8am.
#' @param daily_hfnc_rr_8a_0,daily_hfnc_rr_8a_m1,daily_hfnc_rr_8a_m2 Numeric. Respiratory rate (breaths/min) during HFNC at time closest to 8am.
#' @param daily_niv_rr_8a_0,daily_niv_rr_8a_m1,daily_niv_rr_8a_m2 Numeric. Respiratory rate (breaths/min) during NIV at time closest to 8am.
#' @param daily_standard_rr_8a_0,daily_standard_rr_8a_m1,daily_standard_rr_8a_m2 Numeric. Respiratory rate (breaths/min) during standard flow oxygen at time closest to 8am.
#' @param daily_spo2_lowest_0,daily_spo2_lowest_m1,daily_spo2_lowest_m2 Numeric. Lowest SpO2 (%).
#' @param daily_resp_lowest_0,daily_resp_lowest_m1,daily_resp_lowest_m2 Respiratory support type at time of lowest SpO2.
#' @param daily_fio2_lowest_0,daily_fio2_lowest_m1,daily_fio2_lowest_m2 Numeric. FiO2 at time of lowest SpO2.
#' @param daily_o2_lowest_0,daily_o2_lowest_m1,daily_o2_lowest_m2 Numeric. Oxygen flow rate (L/min) at time of lowest SpO2.
#' @param daily_pao2_occur_0,daily_pao2_occur_m1,daily_pao2_occur_m2 PaO2 measurement occurred: indicator for whether arterial blood gas was obtained.
#' @param daily_pa02_lowest_0,daily_pa02_lowest_m1,daily_pa02_lowest_m2 Numeric. Lowest arterial PaO2 (mmHg).
#' @param daily_resp_lowest_pao2_0,daily_resp_lowest_pao2_m1,daily_resp_lowest_pao2_m2 Respiratory support type at time of lowest PaO2.
#' @param daily_fio2_lowest_pao2_0,daily_fio2_lowest_pao2_m1,daily_fio2_lowest_pao2_m2 Numeric. FiO2 at time of lowest PaO2.
#' @param daily_o2_lowest_pao2_0,daily_o2_lowest_pao2_m1,daily_o2_lowest_pao2_m2 Numeric. Oxygen flow rate (L/min) at time of lowest PaO2.
#'
#' Cardiovascular Support
#' - Vasopressor/inotrope checkboxes (values: "Checked"/"Unchecked"):
#' @param daily_vasopressors_0___0,daily_vasopressors_m1___0,daily_vasopressors_m2___0 No vasopressors or inotropes.
#' @param daily_vasopressors_0___1,daily_vasopressors_m1___1,daily_vasopressors_m2___1 Norepinephrine.
#' @param daily_vasopressors_0___2,daily_vasopressors_m1___2,daily_vasopressors_m2___2 Epinephrine.
#' @param daily_vasopressors_0___3,daily_vasopressors_m1___3,daily_vasopressors_m2___3 Phenylephrine.
#' @param daily_vasopressors_0___4,daily_vasopressors_m1___4,daily_vasopressors_m2___4 Vasopressin.
#' @param daily_vasopressors_0___5,daily_vasopressors_m1___5,daily_vasopressors_m2___5 Dopamine.
#' @param daily_vasopressors_0___6,daily_vasopressors_m1___6,daily_vasopressors_m2___6 Dobutamine.
#' @param daily_vasopressors_0___7,daily_vasopressors_m1___7,daily_vasopressors_m2___7 Angiotensin II.
#' @param daily_vasopressors_0___8,daily_vasopressors_m1___8,daily_vasopressors_m2___8 Milrinone.
#' @param daily_vasopressors_0___88,daily_vasopressors_m1___88,daily_vasopressors_m2___88 Other.
#' - Laboratory Measurements for Vasopressor Dosing
#' @param daily_sbp_8a_0,daily_sbp_8a_m1,daily_sbp_8a_m2 Numeric. Systolic blood pressure (mmHg) closest to 8am.
#' @param daily_dbp_8a_0,daily_dbp_8a_m1,daily_dbp_8a_m2 Numeric. Diastolic blood pressure (mmHg) closest to 8am.
#' - Vasopressor dose units (units of measurement for dosing):
#' @param daily_ne_units_8a_0,daily_ne_units_8a_m1,daily_ne_units_8a_m2 Norepinephrine dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_epi_units_8a_0,daily_epi_units_8a_m1,daily_epi_units_8a_m2 Epinephrine dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_phen_units_8a_0,daily_phen_units_8a_m1,daily_phen_units_8a_m2 Phenylephrine dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_dopa_units_8a_0,daily_dopa_units_8a_m1,daily_dopa_units_8a_m2 Dopamine dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_dobuta_units_8a_0,daily_dobuta_units_8a_m1,daily_dobuta_units_8a_m2 Dobutamine dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_ang2_units_8a_0,daily_ang2_units_8a_m1,daily_ang2_units_8a_m2 Angiotensin II dose units: "mcg/min"/"mcg/kg/min".
#' @param daily_milr_units_8a_0,daily_milr_units_8a_m1,daily_milr_units_8a_m2 Milrinone dose units: "mcg/min"/"mcg/kg/min".
#' - Vasopressor doses (calculated from units and raw doses):
#' @param daily_ne_dose_8a_0_mcg,daily_ne_dose_8a_m1_mcg,daily_ne_dose_8a_m2_mcg Numeric. Norepinephrine dose (mcg/min) closest to 8am.
#' @param daily_ne_dose_8a_0_mcgkg,daily_ne_dose_8a_m1_mcgkg,daily_ne_dose_8a_m2_mcgkg Numeric. Norepinephrine dose (mcg/min/kg) closest to 8am.
#' @param daily_epi_dose_8a_0_mcg,daily_epi_dose_8a_m1_mcg,daily_epi_dose_8a_m2_mcg Numeric. Epinephrine dose (mcg/min) closest to 8am.
#' @param daily_epi_dose_8a_0_mcgkg,daily_epi_dose_8a_m1_mcgkg,daily_epi_dose_8a_m2_mcgkg Numeric. Epinephrine dose (mcg/min/kg) closest to 8am.
#' @param daily_phen_dose_8a_0_mcg,daily_phen_dose_8a_m1_mcg,daily_phen_dose_8a_m2_mcg Numeric. Phenylephrine dose (mcg/min) closest to 8am.
#' @param daily_phen_dos_8a_0_mcgkg,daily_phen_dos_8a_m1_mcgkg,daily_phen_dos_8a_m2_mcgkg Numeric. Phenylephrine dose (mcg/min/kg) closest to 8am.
#' @param daily_vaso_dose_8a_0,daily_vaso_dose_8a_m1,daily_vaso_dose_8a_m2 Numeric. Vasopressin dose (units/min) closest to 8am.
#' @param daily_dopa_dose_8a_0_mcg,daily_dopa_dose_8a_m1_mcg,daily_dopa_dose_8a_m2_mcg Numeric. Dopamine dose (mcg/min) closest to 8am.
#' @param daily_dopa_dos_8a_0_mcgkg,daily_dopa_dos_8a_m1_mcgkg,daily_dopa_dos_8a_m2_mcgkg Numeric. Dopamine dose (mcg/min/kg) closest to 8am.
#' @param daily_dobuta_8a_0_mcg,daily_dobuta_8a_m1_mcg,daily_dobuta_8a_m2_mcg Numeric. Dobutamine dose (mcg/min) closest to 8am.
#' @param daily_dobuta_8a_0_mcgkg,daily_dobuta_8a_m1_mcgkg,daily_dobuta_8a_m2_mcgkg Numeric. Dobutamine dose (mcg/min/kg) closest to 8am.
#' @param daily_ang2_8a_0_mcg,daily_ang2_8a_m1_mcg,daily_ang2_8a_m2_mcg Numeric. Angiotensin II dose (mcg/min) closest to 8am.
#' @param daily_ang2_8a_0_mcgkg,daily_ang2_8a_m1_mcgkg,daily_ang2_8a_m2_mcgkg Numeric. Angiotensin II dose (mcg/min/kg) closest to 8am.
#' @param daily_milr_8a_0_mcg,daily_milr_8a_m1_mcg,daily_milr_8a_m2_mcg Numeric. Milrinone dose (mcg/min) closest to 8am.
#' @param daily_milr_8a_0_mcgkg,daily_milr_8a_m1_mcgkg,daily_milr_8a_m2_mcgkg Numeric. Milrinone dose (mcg/min/kg) closest to 8am.
#'
#' Laboratory Measurements
#' @param daily_crp_8a_0,daily_crp_8a_m1,daily_crp_8a_m2 Numeric. C-reactive protein (CRP) level (mg/L) closest to 8am.
#' @param daily_ferritin_8a_0,daily_ferritin_8a_m1,daily_ferritin_8a_m2 Numeric. Ferritin level (ng/mL) closest to 8am.
#' @param daily_fibrinogen_8a_0,daily_fibrinogen_8a_m1,daily_fibrinogen_8a_m2 Numeric. Fibrinogen level (mg/dL) closest to 8am.
#' @param daily_ddimer_8a_0,daily_ddimer_8a_m1,daily_ddimer_8a_m2 Numeric. D-dimer level (ng/mL) closest to 8am.
#' @param daily_gluc_8a_0,daily_gluc_8a_m1,daily_gluc_8a_m2 Numeric. Glucose concentration (mg/dL) closest to 8am.
#' @param daily_wbc_8a_0,daily_wbc_8a_m1,daily_wbc_8a_m2 Numeric. White blood cell count (K/mcL) closest to 8am.
#' @param daily_platelet_8a_0,daily_platelet_8a_m1,daily_platelet_8a_m2 Numeric. Platelet count (K/mcL) closest to 8am.
#' @param daily_hct_8a_0,daily_hct_8a_m1,daily_hct_8a_m2 Numeric. Hematocrit (%) closest to 8am.
#' @param daily_bun_8a_0,daily_bun_8a_m1,daily_bun_8a_m2 Numeric. Blood urea nitrogen (mg/dL) closest to 8am.
#' @param daily_na_8a_0,daily_na_8a_m1,daily_na_8a_m2 Numeric. Sodium (mEq/L) closest to 8am.
#' @param daily_k_8a_0,daily_k_8a_m1,daily_k_8a_m2 Numeric. Potassium (mEq/L) closest to 8am.
#' @param daily_ph_lowest_0,daily_ph_lowest_m1,daily_ph_lowest_m2 Numeric. Lowest arterial pH.
#' @param daily_paco2_lowest_0,daily_paco2_lowest_m1,daily_paco2_lowest_m2 Numeric. Lowest arterial PaCO2 (mmHg).
#' @param daily_tbili_8a_0,daily_tbili_8a_m1,daily_tbili_8a_m2 Numeric. Total bilirubin (mg/dL) closest to 8am.
#' @param daily_cr_8a_0,daily_cr_8a_m1,daily_cr_8a_m2 Numeric. Creatinine (mg/dL) closest to 8am.
#' - Not collected indicators (values: "Not Collected"):
#' @param daily_crp_nc_0,daily_crp_nc_m1,daily_crp_nc_m2 CRP not collected.
#' @param daily_ferritin_nc_0,daily_ferritin_nc_m1,daily_ferritin_nc_m2 Ferritin not collected.
#' @param daily_fibrinogen_nc_0,daily_fibrinogen_nc_m1,daily_fibrinogen_nc_m2 Fibrinogen not collected.
#' @param daily_ddimer_nc_0,daily_ddimer_nc_m1,daily_ddimer_nc_m2 D-dimer not collected.
#' @param daily_gluc_nc_0,daily_gluc_nc_m1,daily_gluc_nc_m2 Glucose not collected.
#' @param daily_wbc_nc_0,daily_wbc_nc_m1,daily_wbc_nc_m2 WBC not collected.
#' @param daily_platelet_nc_0,daily_platelet_nc_m1,daily_platelet_nc_m2 Platelet not collected.
#' @param daily_hct_nc_0,daily_hct_nc_m1,daily_hct_nc_m2 Hematocrit not collected.
#' @param daily_bun_nc_0,daily_bun_nc_m1,daily_bun_nc_m2 BUN not collected.
#' @param daily_na_nc_0,daily_na_nc_m1,daily_na_nc_m2 Sodium not collected.
#' @param daily_k_nc_0,daily_k_nc_m1,daily_k_nc_m2 Potassium not collected.
#' @param daily_tbili_nc_0,daily_tbili_nc_m1,daily_tbili_nc_m2 Total bilirubin not collected.
#' @param daily_cr_nc_0,daily_cr_nc_m1,daily_cr_nc_m2 Creatinine not collected.
#'
#' Neurological Assessment
#' @param daily_gcs_8a_0,daily_gcs_8a_m1,daily_gcs_8a_m2 Glasgow Coma Scale score closest to 8am.
#' @param cam_0,cam_m1,cam_m2 Confusion Assessment Method (CAM) result: "Positive for delirium at least once on this day"/"Negative for delirium on all assessments on this day"/"Not done".
#' @param lowrass_orres_0,lowrass_orres_m1,lowrass_orres_m2 Numeric. Lowest RASS score.
#' @param highestrass_orres_0,highestrass_orres_m1,highestrass_orres_m2 Numeric. Highest RASS score.
#'
#' @name daily_hospital_forms_params
#' @keywords internal
NULL

# =============================================================================
# Parameters from `event_label == 'Syndrome Adjudication'`
# =============================================================================

#' Syndrome Clincial Judgement and Documentation
#' @param organ_dysfnx_cause Primary cause of acute cardiovascular and/or pulmonary organ dysfunction leading to eligibility for this study: "1, Infection"/"2, Inflammatory condition other than infection"/"3, A non-inflammatory condition"/"99, Unknown".
#' @param sepsis_present Documentation that sepsis syndrome was present on or before Day 0: "Yes"/"No".
#' @param sepsis_clinical_judgement Clinical judgement of whether sepsis syndrome was present on or before Day 0: "Yes"/"No"/"Unknown".
#' @param ards_present Documentation that ARDS syndrome was present on or before Day 0: "Yes"/"No".
#' @param ards_clinical_judgement Clinical judgement of whether ARDS syndrome was present on or before Day 0: "Yes"/"No"/"Unknown".
#' @param pna_clinical_judgement Clinical judgement of whether pneumonia syndrome was present on or before Day 0: "Yes"/"No"/"Unknown".
#' @param pna_effusion Numeric. Pleural effusion associated with pneumonia: 1/"Yes"/2/"No"/99/"Unknown".
#'
#' Imaging Data Availability
#' @param cxr_d0_available,cxr_dm1_available,cxr_dm2_available Chest X-ray availability: "Yes"/"No".
#' @param ct_d0_available,ct_dm1_available,ct_dm2_available CT scan availability: "Yes"/"No".
#' @param cxr_d0_opacity,cxr_dm1_opacity,cxr_dm2_opacity New or worsening pulmonary opacities on CXR compared to pre-illness status: "00, No relevant imaging completed on this day"/"0, None"/"1, Unilateral"/"2, Bilateral"/"99, Unknown/equivocal".
#' @param ct_d0_opacity,ct_dm1_opacity,ct_dm2_opacity New or worsening pulmonary opacities on CT compared to pre-illness status: "0, None"/"1, Unilateral"/"2, Bilateral"/"99, Unknown/equivocal".
#'
#' @name syndrome_adjudication_params
#' @keywords internal
NULL


# =============================================================================
# Parameters from `event_label == 'Hospital Discharge and Summary'`
# =============================================================================

#' Pathogen Testing and Culture Results
#' @param cxpos Culture/test result positivity status. Examples: "Positive blood culture"/"Positive respiratory culture"
#' @param resp_pathogen Respiratory pathogen identified. Examples: "Coronavirus 229E"/"Influenza A virus"
#' @param pathogen_date Date of positive pathogen test result.
#'
#' @name hospital_discharge_summary_params
#' @keywords internal
NULL


# =============================================================================
# Code Map Derived Variables
# =============================================================================

#' Derived variables created using `get_code_label_map()` helper function
#'
#' These variables are derived by converting REDCap label values to numeric
#' codes using the `get_code_label_map()` helper function. This function creates
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
#' The following variables are derived using this approach:
#' @param daily_resp_8a_0_code Derived from `daily_resp_8a_0`.
#' @param daily_resp_8a_m1_code Derived from `daily_resp_8a_m1`.
#' @param daily_resp_8a_m2_code Derived from `daily_resp_8a_m2`.
#' @param frailty_base_code Derived from `frailty_base`.
#' @param organ_dysfnx_cause_code Derived from `organ_dysfnx_cause`.
#' @param cxr_dm2_opacity_code Derived from `cxr_dm2_opacity`.
#' @param ct_dm2_opacity_code Derived from `ct_dm2_opacity`.
#' @param cxr_dm1_opacity_code Derived from `cxr_dm1_opacity`.
#' @param ct_dm1_opacity_code Derived from `ct_dm1_opacity`.
#' @param cxr_d0_opacity_code Derived from `cxr_d0_opacity`.
#' @param ct_d0_opacity_code Derived from `ct_d0_opacity`.
#' @param cxpos_code Derived from `cxpos`.
#' @param resp_pathogen_code Derived from `resp_pathogen`.
#'
#' @name code_map_derived_params
#' @keywords internal
NULL
