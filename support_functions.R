require('dplyr')

## Data dictionary functions

## parse the "select_choices_or_calculations" field in the data dictionary
## for checkbox and multiple choice fields
## choices - a string vector of length 1
parse_choices <-  function(choices) {
  if(length(choices) != 1)
    stop("'choices' must have length 1")
  keyvals <- strsplit(choices, split = ' ?\\| ?')[[1]]
  keys <- sub('^([^,]+),.+$', '\\1', keyvals)
  values <- sub('^[^,]+, (.+)$', '\\1', keyvals)
  data.frame(key=keys, value=values)
}

## create a label-to-code mapping table for a multiple choice field
## field_name - name of field corresponding to 'field_name' in dictionary
## dictionary - data dictionary table
get_code_label_map <- function(field_name, dictionary) {
  field_name_orig <- field_name
  field_name_code <- paste0(field_name, '_code')
  dictionary %>%
    filter(field_name == field_name_orig) %>%
    pull(select_choices_or_calculations) %>%
    parse_choices() %>%
    rename(!!field_name_orig := value,
           !!field_name_code := key)
}


## View common dictionary fields for a given field name
## field - name of field corresponding to 'field_name' in dictionary
## dictionary - data dictionary table
view_dict <- function(field, dictionary) {
  dictionary %>%
    filter(field_name == field) %>%
    select('field_name', 'field_type', 'form_label', 'field_label',
           'select_choices_or_calculations', 'branching_logic') %>%
    unclass()
}


#' Helper function to identify "Checked" values
is_checked <- function(x) {
  x == "Checked"
}

#' Helper function to identify "Unchecked" values
is_unchecked <- function(x) {
  x == "Unchecked"
}

#' Helper function to identify "Unknown" values
is_unknown <- function(x) {
  x == "Unknown"
}

# Helper function: Get value with lookback (Day 0 -> Day -1 -> Day -2)
get_value_with_lookback <- function(val_0, val_m1, val_m2) {
  dplyr::coalesce(val_0, val_m1, val_m2)
}

# Helper function: Display count table with grand total row
# Usage: data |> display_count_with_total(column_name)
display_grand_total <- function(data, col) {
  data |>
    count({{ col }}) |>
    gt() |>
    grand_summary_rows(
      columns = n,
      fns = list(label = md("**TOTAL**"), id = "totals") ~ sum(.)
    )
}


## SOFA score functions

## calculate p/f ratio for respiratory SOFA score
## low_pao2 - lowest PaO2 (e.g., daily_pa02_lowest_m2)
## resp_low_pao2 - respiratory support at lowest PaO2 (e.g., daily_resp_lowest_pao2_m2)
## fio2_low_pao2 - FiO2 at lowest PaO2 (e.g., daily_fio2_lowest_pao2_m2)
## o2_low_pao2 - Std. Oxygen L/min at lowest PaO2 (e.g., daily_o2_lowest_pao2_m2)
calc_pf_ratio <- function(low_pao2, resp_low_pao2, fio2_low_pao2, o2_low_pao2) {
  case_when(

    ## patient on ECMO without invasive mechanical ventilation (FiO2 is nonsense)
    resp_low_pao2 == 'ECMO without invasive mechanical ventilation' ~ NA,

    ## lowest PaO2 available and patient on IMV, NIV, or HFNC
    !is.na(low_pao2) & !is.na(fio2_low_pao2) ~ low_pao2/fio2_low_pao2,

    ## lowest PaO2 available and patient on supplemental oxygen (0.21 + 0.03 * L/min)
    !is.na(low_pao2) & !is.na(o2_low_pao2) ~ low_pao2/pmin(0.21 + 0.03*o2_low_pao2, 1),

    ## lowest PaO2 available and patient on room air
    !is.na(low_pao2) & grepl('^No respiratory support', resp_low_pao2) ~ low_pao2/0.21,

    TRUE ~ NA
  )
}

## calculate s/f ratio for respiratory SOFA score
## low_spo2 - lowest SpO2 (e.g., daily_spo2_lowest_m2)
## resp_low_spo2 - respiratory support at lowest SpO2 (e.g., daily_resp_lowest_m2)
## fio2_low_spo2 - FiO2 at lowest SpO2 (e.g., daily_fio2_lowest_m2)
## o2_low_spo2 - Std. Oxygen L/min at lowest SpO2 (e.g., daily_o2_lowest_m2)
calc_sf_ratio <- function(low_spo2, resp_low_spo2, fio2_low_spo2, o2_low_spo2) {
  case_when(

    ## patient on ECMO without invasive mechanical ventilation (FiO2 is nonsense)
    resp_low_spo2 == 'ECMO without invasive mechanical ventilation' ~ NA,

    ## lowest SaO2 available and patient on IMV, NIV, or HFNC
    !is.na(low_spo2) & !is.na(fio2_low_spo2) ~ low_spo2/fio2_low_spo2,

    ## lowest SaO2 available and patient on supplemental oxygen (0.21 + 0.03 * L/min)
    !is.na(low_spo2) & !is.na(o2_low_spo2) ~ low_spo2/pmin(0.21 + 0.03*o2_low_spo2, 1),

    ## lowest SaO2 available and patient on room air
    !is.na(low_spo2) & grepl('^No respiratory support', resp_low_spo2) ~ low_spo2/0.21,

    TRUE ~ NA
  )
}

## calculate respiratory SOFA score
## pf_ratio - worst PaO2/FiO2 ratio
## sf_ratio - worst SpO2/FiO2 ratio
calc_sofa_resp <- function(pf_ratio, sf_ratio) {
  case_when(
    pf_ratio  < 100 | sf_ratio < 67 ~ 4,
    pf_ratio  < 200 | sf_ratio < 142 ~ 3,
    pf_ratio  < 300 | sf_ratio < 221 ~ 2,
    pf_ratio  < 400 | sf_ratio < 302 ~ 1,
    pf_ratio >= 400 | sf_ratio >= 302 ~ 0,
    TRUE ~ NA
  )
}

## calculate coagulation SOFA score
## platelets - platelets (10^3/mm^3)
calc_sofa_coag <- function(platelets) {
  case_when(
    platelets  < 20 ~ 4,
    platelets  < 50 ~ 3,
    platelets  < 100 ~ 2,
    platelets  < 150 ~ 1,
    platelets >= 150 ~ 0,
    TRUE ~ NA
  )
}

## calculate liver SOFA score
## bilirubin - bilirubin (mg/dL)
calc_sofa_livr <- function(bilirubin) {
  case_when(
    bilirubin >= 12 ~ 4,
    bilirubin >= 6 ~ 3,
    bilirubin >= 2 ~ 2,
    bilirubin >= 1.2 ~ 1,
    bilirubin  < 1.2 ~ 0,
    TRUE ~ NA
  )
}

## calculate cardiovascular SOFA score
## sbp - SBP (mmHg)
## dbp - DBP (mmHg)
## dopa_mcg - dopamine dose (mcg/min)
## dopa_mcgkg - dopamine dose (mcg/min/kg)
## dobu_mcg - dobutamine dose (mcg/min)
## dobu_mcgkg - dobutamine dose (mcg/min/kg)
## epin_mcg - epinephrine dose (mcg/min)
## epin_mcgkg - epinephrine dose (mcg/min/kg)
## nore_mcg - norepinephrine dose (mcg/min)
## nore_mcgkg - norepinephrine dose (mcg/min/kg)
## weight_kg - weight (kg)
calc_sofa_card <- function(sbp, dbp, dopa_mcg, dopa_mcgkg, dobu_mcg, dobu_mcgkg,
                           epin_mcg, epin_mcgkg, nore_mcg, nore_mcgkg, weight_kg) {
  map <- 1/3*sbp + 2/3*dbp
  case_when(
    dopa_mcgkg > 15  | dopa_mcg/weight_kg > 15  |
      epin_mcgkg > 0.1 | epin_mcg/weight_kg > 0.1 |
      nore_mcgkg > 0.1 | nore_mcg/weight_kg > 0.1  ~ 4,
    dopa_mcgkg > 5.1 | dopa_mcg/weight_kg > 5.1 |
      epin_mcgkg > 0.0 | epin_mcg/weight_kg > 0.0 |
      nore_mcgkg > 0.0 | nore_mcg/weight_kg > 0.0  ~ 3,
    dopa_mcgkg > 0.0 | dopa_mcg/weight_kg > 0.0 |
      dobu_mcgkg > 0.0 | dobu_mcg/weight_kg > 0.0  ~ 2,
    map < 70                                       ~ 1,
    map >= 70                                      ~ 0,
    TRUE ~ NA
  )
}

## calculate CNS SOFA score
## gcs - Glasgow Coma Score
calc_sofa_cns <- function(gcs) {

  ## treat 'not documented' as NA
  gcs <- ifelse(gcs == 'not documented', NA, gcs)
  ## GCS contains T/patient was intubated/no verbal score
  gcs_intub <- grepl('T$', gcs)
  ## extract quantitative component of GCS
  gcs_quant <- as.numeric(sub('T', '', gcs))

  ## imputed GCS
  gcs_imput <- case_when(
    gcs_intub ~ gcs_quant + 5.0, ## impute normal verbal score
    TRUE ~ gcs_quant
  )

  ## convert to SOFA CNS score
  case_when(
    gcs_imput <  6  ~ 4,
    gcs_imput <= 9  ~ 3,
    gcs_imput <= 12 ~ 2,
    gcs_imput <= 14 ~ 1,
    gcs_imput == 15 ~ 0,
    TRUE ~ NA
  )
}

## calculate renal SOFA score
## cr - creatinine concentration (mg/dL)
calc_sofa_rena <- function(cr) {
  case_when(
    cr > 5.0 ~ 4,
    cr > 3.5 ~ 3,
    cr > 2.0 ~ 2,
    cr > 1.2 ~ 1,
    cr > 0.0 ~ 0,
    TRUE ~ NA
  )
}

## calculate norepinephrine equivalents (NEE) for vasopressor dosing
## Converts all vasopressor and inotrope doses to norepinephrine-equivalent mcg/min
## Used in SOFA cardiovascular scoring and septic shock definitions
##
## Parameters: doses in mcg/min or mcg/min/kg for various agents
## daily_ne_dose_8a_0_mcg - norepinephrine dose (mcg/min)
## daily_ne_dose_8a_0_mcgkg - norepinephrine dose (mcg/min/kg)
## daily_epi_dose_8a_0_mcg - epinephrine dose (mcg/min)
## daily_epi_dose_8a_0_mcgkg - epinephrine dose (mcg/min/kg)
## daily_phen_dose_8a_0_mcg - phenylephrine dose (mcg/min)
## daily_phen_dos_8a_0_mcgkg - phenylephrine dose (mcg/min/kg)
## daily_vaso_dose_8a_0 - vasopressin dose (units/min)
## daily_dopa_dose_8a_0_mcg - dopamine dose (mcg/min)
## daily_dopa_dos_8a_0_mcgkg - dopamine dose (mcg/min/kg)
## daily_ang2_8a_0_mcg - angiotensin II dose (mcg/min)
## daily_ang2_8a_0_mcgkg - angiotensin II dose (mcg/min/kg)
## m_weight_kg - patient weight (kg)
##
## Returns: norepinephrine equivalent dose (mcg/min)
## Conversion factors: NE = 1, Epi = 1, Phenylephrine / 10, Vasopressin * 2.5,
##                     Dopamine / 100, Angiotensin II * 10
calc_norepi_equivalents <- function(
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  m_weight_kg
) {

  ## Convert all doses to mcg/min using weight if needed
  ne_dose <- dplyr::coalesce(daily_ne_dose_8a_0_mcg, daily_ne_dose_8a_0_mcgkg * m_weight_kg, 0)
  epi_dose <- dplyr::coalesce(daily_epi_dose_8a_0_mcg, daily_epi_dose_8a_0_mcgkg * m_weight_kg, 0)
  phen_dose <- dplyr::coalesce(daily_phen_dose_8a_0_mcg, daily_phen_dos_8a_0_mcgkg * m_weight_kg, 0)
  dopa_dose <- dplyr::coalesce(daily_dopa_dose_8a_0_mcg, daily_dopa_dos_8a_0_mcgkg * m_weight_kg, 0)
  ang2_dose <- dplyr::coalesce(daily_ang2_8a_0_mcg, daily_ang2_8a_0_mcgkg * m_weight_kg, 0)
  vaso_dose <- dplyr::coalesce(daily_vaso_dose_8a_0, 0)

  ## Calculate norepinephrine equivalents
  ## NE = 1, Epi = 1, Phenylephrine = 10, Vasopressin = 0.04 units = 1 mcg NE equivalent
  ## Dopamine at high doses ~ 0.01, Angiotensin II ~ 1
  norepi_equiv <- ne_dose +
                  epi_dose +
                  (phen_dose / 10) +
                  (vaso_dose * 2.5) +

                  (dopa_dose / 100) +
                  (ang2_dose * 10)

  return(norepi_equiv)
}
