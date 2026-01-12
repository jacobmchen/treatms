# Combine the computed data for censoring time, event time, 
# and baseline covariates into a single data table.

# package for operations on manipulating data
library(tidyverse)

# read the censoring times data
censoring_times <- readRDS("censoring_times.RDS")
censoring_times <- censoring_times %>% select(PatientName, censor)

# read the event times data
event_times <- readRDS("event_times.RDS")
event_times <- event_times %>% select(PatientName, event_time)

# read the baseline covariate data
covariate_data <- readRDS("baseline_data.RDS")

# combine the data
full_data <- left_join(covariate_data, censoring_times, by="PatientName")
full_data <- full_join(full_data, event_times, by="PatientName")

# add a column of 0s and 1s generated randomly to represent treatment
# group
full_data <- full_data %>%
    mutate(treatment_group = rbinom(nrow(full_data), 1, 0.5))

# rename the column PatientName as id and change it to be a vector of
# 1:n to conform with the estimating functions later on
full_data <- full_data %>%
    rename(id = PatientName) %>%
    mutate(id = 1:nrow(full_data))

# save the full data as an RDS file
saveRDS(full_data, "full_data.RDS")
