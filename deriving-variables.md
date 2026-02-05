# Instructions for Deriving Variables

This document details the general steps followed in order to derive variables for the APS Aim 7 project. Following this process will ensure consistency with the data dictionary and the variable definition.

## Steps
1. Identify the required columns from the dataset based on the variable definition.
    - For the Steroid DAG, these were typically listed in the DAG sheet.
    - You will often identify additional columns needed as your write the derivation function.
2. For each input column, verify spelling and data format with `summary(data$<column_name>)`.
    - If the spelling is incorrect, you'll see `Warning message: Unknown or uninitialised column`. Otherwise, the function will show you how the data is formatted in that column.
    - For example, `daily_vasopressors_0___0` must have 3 underscores `_` before the final `0` and has values of `Checked`, `Unchecked`, or `NA` as shown here:
    ```r
    > summary(data$daily_vasopressors_0___0)
    Unchecked   Checked      NA's
          299       200      7686
    ```
3. Examine the data dictionary entry for each column using `view_dict("<column_name>", dictionary)` from `support_functions.R`.
    - This will show you the field name, description, possible values, and branching logic.
    - For example, here is the dictionary entry for `daily_vasopressors_0___0`:
    ```r
    > view_dict("daily_vasopressors_0___0", dictionary)
    $field_name
    [1] "daily_vasopressors_0___0"

    $field_type
    [1] "checkbox"

    $form_label
    [1] "Daily Assessment/SOFA Scoring [day 0]"

    $field_label
    [1] "What intravenous vasopressor/inotrope was infusing at the time the blood pressure values closest to 8am were measured on this day? [check all that apply]"

    $select_choices_or_calculations
    [1] "0, none (no vasopressors) | 1, norepinephrine | 2, epinephrine | 3, phenylephrine | 4, vasopressin | 5, dopamine | 6, dobutamine | 7, angiotensin II | 8, milrinone | 88, other: {daily_other_vp_8a_0:icons}"

    $branching_logic
    [1] "[dailysofa_perf_0] = '1'"
    ```
    - This tells us that `daily_vasopressors_0___0` is a checkbox field representing whether "none (no vasopressors)", while `daily_vasopressors_0___1` would represent "norepinephrine", and so on.
    - Further, it shows that this checkbox value is only meaningful if the column `dailysofa_perf_0` is '1'.
        - WARNING: Be sure to check the dictionary and summary for upstream branching logic functions. The branching logic can have multiple layers. Also, the values shown in the branching logic section aren't always a 1:1 match for how the data of that branching logic column is formatted. For example, consider `dailysofa_perf_0`:
        ```r
        > view_dict("dailysofa_perf_0", dictionary)
        $field_name
        [1] "dailysofa_perf_0"

        $field_type
        [1] "radio"

        $form_label
        [1] "Daily Assessment/SOFA Scoring [day 0]"

        $field_label
        [1] "<div class=\"rich-text-field-label\"><p> </p> <p>Is daily assessment data for the participant available for Day 0 ([day_0_arm_1][day0])?</p> <p> </p></div>"

        $select_choices_or_calculations
        [1] "1, Available | 0, Not Available"

        $branching_logic
        [1] NA
        > summary(data$dailysofa_perf_0)
        Available Not Available          NA's
              498             1          7686
        ```
        - In this case, the '1' in the `branching_logic` section for `daily_vasopressors_0___0` corresponds to the value "Available" in `dailysofa_perf_0`.
    - You will often need upstream columns to correctly handle different edge cases in your variable definition, especially for handling `NA` values that arise from the branching logic (i.e., if the branching logic condition is not met, the column will be `NA`).
4. For each input column, determine which `event_label` the column belongs to with the `view_label()` function from `support_functions.R`.
    - Columns are associated with a single event label, so if you use the wrong one it will return only `NA` values.
    - Most variable derivations start with `data |> filter(event_label == "<event_label>")` to ensure only the relevant rows are used.
    - For some variables, you will need to combine rows from multiple event labels. This is typically done using `left_join()` by `record_id`.
    - For example, here is the event label for `daily_vasopressors_0___0`:
    ```r
    > view_label(data, daily_vasopressors_0___0)
    [1] "Daily In-Hospital Forms"
    ```
5. Write the scaffolding for deriving the variable. We recommend using ddplyr functions for data manipulation. The general structure is as follows:
    - Select the relevant event label(s) using `filter(event_label == "<event_label>")` (and possibly `left_join()` if multiple event labels are needed)
    - Create code label maps as needed (described below) and use `left_join()` to merge them into the dataset
    - Create a new column for the derived variable using `mutate()`
6. Write the R function to derive the variable using the columns as function inputs. This will be used by the `mutate()` step, so stick to ddplyr conventions. For example, use `if_else()` and `case_when()`.
    - During this step, you may realize you need additional columns or that you want to include upstream branching logic columns as inputs to account for edge cases. Anytime you add a new column, ensure you thoroughly understand it using Steps 2-4 above and double check that your scaffolding from Step 5 is up to date.
    - Often it is more convenient to use a code map rather than raw column values. For example, the `daily_resp_8a_0` variable has extremely long strings as possible values:
    ```r
    > summary(data$daily_resp_8a_0)
           ECMO and invasive mechanical ventilation         ECMO without invasive mechanical ventilation
                                                  4                                                    0
       Invasive mechanical ventilation without ECMO                             Non-invasive ventilation
                                                195                                                   31
    High-flow nasal oxygen (high flow nasal oxygen)                    Standard flow supplemental oxygen
                                                103                                                   98
      No respiratory support or supplemental oxygen
                 (spontaneously breathing room air)                                              Unknown
                                                 64                                                    3
                                               NA's
                                               7687
    ```
    - Instead of checking `daily_resp_8a_0 == 'Invasive mechanical ventilation without ECMO'`, it is easier to create a code map that assigns a short code to each possible values. This can be done using the `get_code_label_map()` function from `support_functions.R`:
    ```r
    > resp_map <- get_code_label_map("daily_resp_8a_0", dictionary)
    > print(resp_map)
    daily_resp_8a_0_code                                                                  daily_resp_8a_0
    1                    1                                         ECMO and invasive mechanical ventilation
    2                    2                                     ECMO without invasive mechanical ventilation
    3                    3                                     Invasive mechanical ventilation without ECMO
    4                    4                                                         Non-invasive ventilation
    5                    5                                  High-flow nasal oxygen (high flow nasal oxygen)
    6                    6                                                Standard flow supplemental oxygen
    7                    7 No respiratory support or supplemental oxygen (spontaneously breathing room air)
    8                   99                                                                          Unknown
    ```
    - You can then `left_join()` this map to your dataset using
    ```r
    data <- data |> left_join(resp_map, by = "daily_resp_8a_0")
    ```
    - This will add a new column `daily_resp_8a_0_code` that contains those short numeric codes. Use this column in your derivation function instead of the long strings to get `daily_resp_8a_0_code == 3`, for example.
7. After writing your variable function, test it on the dataset and display a summary to verify it works as expected. For categorical variables, you can use the `display_grand_total()` function from `support_functions.R` to see the counts for each category using a `gt()` table. For continues variables, you can use `summary()` or `ggplot2` to visualize the distribution.
    - If the results are not as expected, revisit Steps 2-6 to identify any issues. Most problems arise from misunderstanding the branching logic and not accounting for edge cases properly. This can be fixed by adding an upstream branching logic column as an input to your derivation function and handling the `NA` values appropriately.
8. Once you are satisfied with the derived variable, repeat this process as needed for any additional variables.
