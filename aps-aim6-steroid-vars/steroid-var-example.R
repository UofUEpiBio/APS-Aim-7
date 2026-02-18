# Example R script for deriving and analyzing steroid DAG variables
# for APS Aim 6.

# This script should be run from the root folder of the APS-Code-Sharing repo
# to properly source the DAG variables in `R/steroid-dag-variables.R`.

# Libraries
library('ggplot2')
library('dplyr')
library('tidyr')
library('knitr')
library('gt')

## Get support functions
source('support_functions.R')

## Load data from N=500 APS cohort
## provides "data", "dictionary" and "codebook" in the working environment
load(file = '../aps-data/20250731_data.RData')
## Filter the excluded record to not muddy the variable summaries
data <- data |> filter(record_id != '52-0023')

# Source variable derivation code
source("R/steroid-dag-variables.R")

# Calculate steroid DAG variables
steroid_dag_data <- calc_all_steroid_dag_variables(data, dictionary)

# Example: Analyze distribution of sys_hypotension_sev_0 variable
hypotension_summary <- steroid_dag_data |>
  group_by(sys_hypotension_sev_0) |>
  summarise(count = n()) |>
  mutate(percent = count / sum(count) * 100)

print(hypotension_summary)
