library(redcapcustodian)
library(DBI)
library(tidyverse)
library(lubridate)
library(dotenv)

set_script_run_time()

rc_conn <- connect_to_redcap_db()

randomization_source <- tbl(rc_conn, "redcap_randomization")
allocation_source <- tbl(rc_conn, "redcap_randomization_allocation")
project_id_source <- 18

randomization_data <- randomization_source %>%
  filter(project_id == !!project_id_source) %>%
  collect()

rid_source <- randomization_source %>%
  filter(project_id == !!project_id_source) %>%
  collect() %>%
  pull(rid)

allocation_data <- allocation_source %>%
  filter(rid == rid_source) %>%
  collect()

# export allocation tables from source
# Get column names from randomization_source
# target_field and target_event describe the randomization group
# source_fieldN and source_eventN describe the randomization variables
# Pivot the data longer to prep it for renaming the strata fields in Allocations
column_names_in_source <- randomization_source %>%
  filter(project_id == project_id_source) %>%
  collect() %>%
  select(target_field, starts_with("source_field")) %>%
  pivot_longer(
    cols = contains("field"),
    names_to = "strata",
    values_to = "redcap_field_name"
  ) %>%
  filter(!is.na(redcap_field_name))

# Allocation data is in allocation_source
allocations <-
  allocation_source %>%
  filter(rid == rid_source) %>%
  collect() %>%
  select(aid, project_status, target_field, starts_with("source_field")) %>%
  # Pivot longer to facilitate renaming the abstract field names to redcap field names
  pivot_longer(
    cols = contains("field"),
    names_to = "strata",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  # Rename the *field* columns
  inner_join(column_names_in_source, by = "strata") %>%
  select(-strata) %>%
  pivot_wider(
    id_cols = c("aid", "project_status"),
    names_from = "redcap_field_name",
    values_from = "value"
  )

# Write the allocation tables
write_allocations <- function(allocations, project_status_to_write) {
  base_name = "RandomizationAllocation"
  date_time_stamp = format(get_script_run_time(), "%Y%m%d%H%M%S")
  project_statuses <- setNames(c(0,1), c("development", "production"))

  allocations %>%
    filter(project_status == project_status_to_write) %>%
    select(-aid, -project_status) %>%
    write_csv(here::here("output", paste(base_name, names(project_statuses)[project_status_to_write + 1], date_time_stamp, sep = "_")))
}

write_allocations(allocations, 0)
write_allocations(allocations, 1)

# TODO: Add code to isolate allocation data, transform it for write, and write it

# junk below
randomization_target <- tbl(rc_conn, "redcap_randomization")
allocation_target <- tbl(rc_conn, "redcap_randomization_allocation")
project_id_target <- 19
rid_target <-
  randomization_target %>%
  filter(project_id == !!project_id_target) %>%
  collect() %>%
  pull(rid)

max_aid_target <- allocation_target %>%
  summarise(max_aid = max(aid)) %>%
  collect() %>%
  pull(max_aid)

aid_updates_target
