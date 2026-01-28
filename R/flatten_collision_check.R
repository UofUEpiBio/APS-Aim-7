# Check for column collisions between event labels
# This identifies columns that have data in multiple event_labels

library(dplyr)
library(tidyr)

# Get the event labels we care about based on your Rmd files
event_labels_of_interest <- c(
  'Daily In-Hospital Forms',
  'Day 0',
  'Syndrome Adjudication',
  'Patient/Surrogate Interview'
)

# Filter to relevant event labels
relevant_data <- data |>
  filter(event_label %in% event_labels_of_interest)

# For each column (except record_id and event_label), check which event_labels have non-NA data
collision_check <- relevant_data |>
  select(-record_id) |>
  pivot_longer(-event_label, names_to = "column_name", values_to = "value") |>
  filter(!is.na(value)) |>
  distinct(column_name, event_label) |>
  group_by(column_name) |>
  summarise(
    event_labels = paste(event_label, collapse = " | "),
    n_event_labels = n(),
    .groups = 'drop'
  ) |>
  arrange(desc(n_event_labels), column_name)

# Show columns that appear in multiple event labels (POTENTIAL COLLISIONS)
collisions <- collision_check |>
  filter(n_event_labels > 1)

cat("Total columns checked:", nrow(collision_check), "\n")
cat("Columns with collisions:", nrow(collisions), "\n\n")

if (nrow(collisions) > 0) {
  cat("COLUMNS WITH POTENTIAL COLLISIONS:\n")
  print(collisions, n = Inf)
} else {
  cat("No collisions detected! Flattening should be safe.\n")
}

# Also check: same record_id appearing in same event_label multiple times
duplicate_events <- data |>
  filter(event_label %in% event_labels_of_interest) |>
  count(record_id, event_label) |>
  filter(n > 1)

cat("\n\nRecords appearing multiple times in same event_label:", nrow(duplicate_events), "\n")
if (nrow(duplicate_events) > 0) {
  print(duplicate_events)
}
