# Compute the event time for EDSS, T25FW, 9HPT

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)

source("global_variables.R")

# NOTE: the function for deciding event time has been moved to global_variables.R

# read the imputed and processed for EDSS
edss_data <- readRDS("imputed_edss_data.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# read the treatment data
treatment_data <- readRDS("treatment_data.RDS")

# keep a copy of all of the patients in the baseline data
patients <- baseline_data %>% select(PatientName) %>% distinct(PatientName)

# compute the event time from the (possibly) imputed data
# Note that we don't have to specify the method for the sensitivity analysis
# because the code for imputation already adds an extra row for each patient
# with the best/worst values
# Note that we don't need to worry about exceptions because that's handled in
# the impute edss file already
edss_event_time <- edss_data %>%
    select(PatientName, visit_date, total_edss_score) %>%
    # group by patients
    group_by(PatientName) %>%
    # compute the event time
    summarize(edss_event = compute_event_time(visit_date, total_edss_score))

print(edss_event_time)

# read the imputed t25fw data
t25fw_data <- readRDS("t25fw_data.RDS")

# compute the event time after filling in missing values with MICE
t25fw_event_time <- t25fw_data %>%
    select(PatientName, visit_date, trial_average_seconds) %>%
    # group by patients
    group_by(PatientName) %>%
    # compute the event time
    summarize(t25fw_event = compute_event_time(visit_date, trial_average_seconds, value_type="MSFC"))

print(t25fw_event_time)

# read the nhpt data
nhpt_data <- readRDS("nhpt_data.RDS")

# compute the event time after filling in missing values with MICE
nhpt_event_time <- nhpt_data %>%
    select(PatientName, visit_date, hand_average_seconds) %>%
    # group by patients
    group_by(PatientName) %>%
    # compute the event time
    summarize(nhpt_event = compute_event_time(visit_date, hand_average_seconds, value_type="MSFC"))

print(nhpt_event_time)

# merge all of the event times together
event_times <- patients %>%
    full_join(edss_event_time, by="PatientName") %>%
    full_join(t25fw_event_time, by="PatientName") %>%
    full_join(nhpt_event_time, by="PatientName")

print("event times")
print(head(event_times))

#' Select the minimum event time out of the three computed event times.
#' @param edss The computed event time for EDSS
#' @param t25fw The computed event time for T25FW
#' @param nhpt The computed event time for NHPT
#' @return The minimum of the computed event times. If all three values are missing
#' return missing value.
#' @details Compute the minimum event time from the four computed metrics.
select_event_time <- function(edss, t25fw, nhpt) {
    # combine the three values into a vector
    vec <- c(edss, t25fw, nhpt)

    # if all values are missing, return a missing value
    if (sum(is.na(vec)) == length(vec)) return(NA)

    # otherwise, return the minimum value based on the observed values
    return(min(vec, na.rm=TRUE))
}

# choose the event time corresponding to the event time that happened
# first of edss, t25fw, or nhpt
event_times <- event_times %>%
    group_by(PatientName) %>%
    mutate(event_time = select_event_time(edss_event, t25fw_event, nhpt_event))

print("final event time")
print(head(event_times))

# get the list patients for whom we could not calculate an event time
no_event_time <- event_times %>% 
    filter(is.na(event_time))

# save the list of patients with no event time
saveRDS(no_event_time$PatientName, "no_event_time_patients.RDS")

# add to the consort the patients that do not have enough data
add_to_consort(no_event_time$PatientName,
                "one measurement or less for all of EDSS, T25FW, and NHPT", "event_time.R")

# actually drop the patients who have no event time
event_times <- event_times %>%
    filter(!is.na(event_time))

# save the event times as a file
saveRDS(event_times, file="event_times.RDS")

# also save the event time as a csv file
write.csv(event_times, "event_times.csv", row.names=FALSE)
