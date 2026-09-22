# Compute the censoring time for each individual, which will
# be the maximum of the censoring times for EDSS, T25FW, 9HPT

# read global variables
source("global_variables.R")

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))

# keep a copy of all of the patients from the baseline chars sheet
patients <- data.frame(read_excel(data_file_name, sheet="baseline chars")) %>% select(PatientName) %>% distinct(PatientName)

# get the censoring time for edss
edss_censoring_time <- edss_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, total_edss_score) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # remove all rows where the edss score is a missing value
    filter(!is.na(total_edss_score)) %>%
    # keep only the maximum observed month grouped by the patient name
    slice_max(month) %>%
    # remove the edss score column
    select(-total_edss_score) %>%
    # rename the column
    rename(edss_censor=month)

# read the data for T25FW and 9HPT
msfc_data <- data.frame(read_excel(data_file_name, sheet="msfc"))
# compute the averages for the three relevant metrics; the compute_average_msfc
# function is defined as a global variable
msfc_data <- compute_average_msfc(msfc_data)

# censoring time for t25fw
t25fw_censoring_time <- msfc_data %>%
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    select(PatientName, month, trial_average_seconds) %>%
    group_by(PatientName) %>%
    filter(!is.na(trial_average_seconds)) %>%
    slice_max(month) %>%
    select(-trial_average_seconds) %>%
    # rename the column
    rename(t25fw_censor=month)

# censoring time for 9HPT
hpt_censoring_time <- msfc_data %>%
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    select(PatientName, month, hand_average_seconds) %>%
    group_by(PatientName) %>%
    filter(!is.na(hand_average_seconds)) %>%
    slice_max(month) %>%
    select(-hand_average_seconds) %>%
    # rename the column
    rename(hpt_censor=month)

# merge all of the computed censoring times together;
# use an inner join to make sure that all patients are kept
# even if their censoring time could not be calculated for
# one of the outcomes
censoring_times <- patients %>%
    full_join(edss_censoring_time, by="PatientName") %>%
    full_join(t25fw_censoring_time, by="PatientName") %>%
    full_join(hpt_censoring_time, by="PatientName")

# the censoring time is the max of each of the censoring times
censor <- censoring_times %>%
    mutate(censor = pmax(edss_censor, t25fw_censor, hpt_censor,
                         na.rm=TRUE))

censor %>% slice_head(n=10) %>% print()

# save the censoring times as a file
saveRDS(censor, file="censoring_times.RDS")

# now compute the censoring times if we exclude all scores recorded with
# an exception finding for EDSS
edss_censoring_time <- edss_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    # and exception finding columns
    select(PatientName, month, total_edss_score, fs_finding) %>%
    # keep only observations that were not exception findings
    filter(fs_finding == "No") %>%
    # remove the exception finding column
    select(-fs_finding) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # remove all rows where the edss score is a missing value
    filter(!is.na(total_edss_score)) %>%
    # keep only the maximum observed month grouped by the patient name
    slice_max(month) %>%
    # remove the edss score column
    select(-total_edss_score) %>%
    # rename the column
    rename(edss_censor=month)

# merge all of the computed censoring times together;
# use an inner join to make sure that all patients are kept
# even if their censoring time could not be calculated for
# one of the outcomes
censoring_times <- patients %>%
    full_join(edss_censoring_time, by="PatientName") %>%
    full_join(t25fw_censoring_time, by="PatientName") %>%
    full_join(hpt_censoring_time, by="PatientName")

# the censoring time is the max of each of the censoring times
censor <- censoring_times %>%
    mutate(censor = pmax(edss_censor, t25fw_censor, hpt_censor,
                         na.rm=TRUE))

censor %>% slice_head(n=10) %>% print()

# save the censoring times as a file
saveRDS(censor, file="censoring_times_no_exceptions.RDS")

