# Combine the computed data for censoring time, event time, 
# and baseline covariates into a single data table.

# package for operations on manipulating data
library(tidyverse)

# set seed for generating treatment groups
set.seed(0)

# read the censoring times data
censoring_times <- readRDS("censoring_times.RDS")
censoring_times <- censoring_times %>% select(PatientName, censor)

# read the event times data
event_times <- readRDS("event_times.RDS")
event_times <- event_times %>% select(PatientName, event_time)

# define a function that combines the previously computed covariate data,
# data on censoring times, and data on event times
combine_data <- function(covariate_data, censoring_times, event_times, treatment_assignment) {
    # combine the data
    full_data <- left_join(covariate_data, censoring_times, by="PatientName")
    full_data <- full_join(full_data, event_times, by="PatientName")

    # add a column of 0s and 1s generated randomly to represent treatment
    # group
    full_data <- full_data %>%
        mutate(treatment_group = treatment_assignment)

    # rename the column PatientName as id and change it to be a vector of
    # 1:n to conform with the estimating functions later on
    full_data <- full_data %>%
        rename(id = PatientName) %>%
        mutate(id = 1:nrow(full_data))

    return(full_data)
}

# read the baseline covariate data
covariate_data <- readRDS("baseline_data.RDS")

# create one treatment assignment to use for all datasets
treatment_assignment <- rbinom(nrow(covariate_data), 1, 0.5)

# combine the data with all clusters
full_data <- combine_data(covariate_data, censoring_times, event_times, treatment_assignment)

# save the full data as an RDS file
saveRDS(full_data, "full_data_all_clusters.RDS")

# combine the data with no clusters
covariate_data <- readRDS("baseline_data_no_clusters.RDS")
full_data <- combine_data(covariate_data, censoring_times, event_times, treatment_assignment)

# save the full data as an RDS file
saveRDS(full_data, "full_data_no_clusters.RDS")

# combine the data where rare clusters are merged
covariate_data <- readRDS("baseline_data_merge_rare_clusters.RDS")
full_data <- combine_data(covariate_data, censoring_times, event_times, treatment_assignment)

# save the full data as an RDS file
saveRDS(full_data, "full_data_merge_rare_clusters.RDS")

# combine the data where states are merged
covariate_data <- readRDS("baseline_data_merge_states.RDS")
full_data <- combine_data(covariate_data, censoring_times, event_times, treatment_assignment)

# save the full data as an RDS file
saveRDS(full_data, "full_data_merge_states.RDS")
