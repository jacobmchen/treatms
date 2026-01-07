# Combine the computed data for censoring time, event time, 
# and baseline covariates into a single data table.

# package for operations on manipulating data
library(tidyverse)

# read the censoring times data
censoring_times <- readRDS("censoring_times.RDS")
censoring_times <- censoring_times %>% select(PatientName, censor)
print(censoring_times)

# read the event times data
event_times <- readRDS("event_times.RDS")
event_times <- event_times %>% select(PatientName, event_time)
print(event_times)

full_data <- full_join(censoring_times, event_times, by="PatientName")
print(full_data)
