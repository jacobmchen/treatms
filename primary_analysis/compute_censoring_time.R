# Compute the censoring time for each individual, which will
# be the maximum of the censoring times for EDSS, T25FW, 9HPT

# read global variables
source("global_variables.R")

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

# read the imputed data for EDSS
edss_data <- readRDS("imputed_edss_data.RDS")

# read the event times data, the patients in this dataframe are the only
# patients that we need now
event_times <- readRDS("event_times.RDS")

# get the list of patients that we could calculate an event time for
patients <- event_times %>%
    select(PatientName)
print(patients)

# get the censoring time for edss, based on the imputed edss data and the
# computed event time
edss_censoring_time <- edss_data %>%
    # keep only the relevant columns
    select(PatientName, visit_date, total_edss_score) %>%
    # group by the patient name
    group_by(PatientName) %>%
    # merge the event time data
    inner_join(event_times, by="PatientName") %>%
    # get rid of columns we don't need
    select(-c(t25fw_event, nhpt_event, event_time)) %>%
    summarize(edss_censor = compute_censoring_time(visit_date, total_edss_score, edss_event))

edss_censoring_time %>% slice_head(n=10) %>% print()

# read the imputed data for t25fw
t25fw_data <- readRDS("t25fw_data.RDS")

# get the censoring time for t25fw
t25fw_censoring_time <- t25fw_data %>%
    # keep only the relevant columns
    select(PatientName, visit_date, trial_average_seconds) %>%
    # group by the patient name
    group_by(PatientName) %>%
    # merge the event time data
    inner_join(event_times, by="PatientName") %>%
    # get rid of columns we don't need
    select(-c(edss_event, nhpt_event, event_time)) %>%
    summarize(t25fw_censor = compute_censoring_time(visit_date, trial_average_seconds, t25fw_event, value_type="MSFC"))

t25fw_censoring_time %>% slice_head(n=10) %>% print()

# read the imputed data for nhpt
nhpt_data <- readRDS("nhpt_data.RDS")

# get the censoring time for t25fw
nhpt_censoring_time <- nhpt_data %>%
    # keep only the relevant columns
    select(PatientName, visit_date, hand_average_seconds) %>%
    # group by the patient name
    group_by(PatientName) %>%
    # merge the event time data
    inner_join(event_times, by="PatientName") %>%
    # get rid of columns we don't need
    select(-c(edss_event, t25fw_event, event_time)) %>%
    summarize(nhpt_censor = compute_censoring_time(visit_date, hand_average_seconds, nhpt_event, value_type="MSFC"))

nhpt_censoring_time %>% slice_head(n=10) %>% print()

# merge all of the computed censoring times together;
# use a left join to make sure that all patients are kept
# even if their censoring time could not be calculated for
# one of the outcomes; and we only keep patients that have
# a computed event time
censoring_times <- patients %>%
    left_join(edss_censoring_time, by="PatientName") %>%
    left_join(t25fw_censoring_time, by="PatientName") %>%
    left_join(nhpt_censoring_time, by="PatientName")

censoring_times %>% slice_head(n=10) %>% print()

# the censoring time is the max of each of the censoring times
censor <- censoring_times %>%
    mutate(censor = pmax(edss_censor, t25fw_censor, nhpt_censor,
                         na.rm=TRUE))

censor %>% slice_head(n=10) %>% print()

# save the censoring times as a file
saveRDS(censor, file="censoring_times.RDS")

