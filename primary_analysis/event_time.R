# Compute the event time for EDSS, T25FW, 9HPT

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)

source("global_variables.R")

# define a function for computing the event time, if it happens
#' Compute Event Time
#' @param months A vector containing the months of the measurements
#' @param values A vector containing the observed values
#' @param value_type A string specifying whether the value is an EDSS value
#' or an MSFC value since event time is defined differently for these
#' types of values.
#' @param imputation_method
#' @return A single number signifying the month of the event time. If the
#' event did not occur, returns Inf. If it is not possible to compute the
#' event time, returns NA. 
#' @details Compute the event time for a single individual. It is not
#' possible to compute the event time if there is no baseline value
#' or if there are less than 2 observed values after baseline.
compute_event_time <- function(months, values, value_type="EDSS", imputation_method="interpolate") {
    # if there is only one value or 0 values in the vector, we cannot
    # compute the event time
    if (length(months) <= 1) {
        return(NA)
    }
    # if every observed value is missing, then we cannot compute the 
    # event time
    if (sum(is.na(values)) == length(values)) {
        return(NA)
    }

    # second, check if it is possible to compute an event time
    if (is.na(values[1])) {
        # if there is no baseline value, we cannot compute sustained disease progression
        return(NA)
    } else if (length(values) < 3) {
        # if there is only one observed value after the baseline, then we cannot
        # compute sustained disease progression
        # we are going to impute missing values, so checking the length is sufficient
        return(NA)
    }

    # second, impute missing values in the vector of values based on the specified
    # imputation method
    if (imputation_method == "interpolate") {
        # linearly interpolate the missing values based on the surrounding
        # observed values, ex. 1, NA, NA, 4 -> 1, 2, 3, 4
        values <- approx(values, n=length(values))$y
    }

    # declare the threshold that needs to be reached for two consecutive
    # months
    threshold <- 0
    if (value_type == "EDSS") {
        # first round all values to the nearest 0.5 since EDSS scores
        # are always in increments of 0.5
        values <- round(values / 0.5) * 0.5

        # determine the threshold value
        if (values[1] <= 5.5) {
            threshold <- values[1] + 1
        } else {
            threshold <- values[1] + 0.5
        }
    } else if (value_type == "MSFC") {
        # no need to round values if the value type is MSFC, which
        # includes T25FW and NHPT

        # the threshold value is 1.2 times the baseline value
        threshold <- values[1] * 1.2
    }

    # check if there was sustained disability progression
    # iterate from the first non-baseline value to the second to last value
    for (i in 2:(length(values)-1)) {
        # check if two consecutive values are greater than or equal to the threshold
        if (values[i] >= threshold & values[i+1] >= threshold) {
            return(months[i])
        }
    }

    return(Inf)
}

# test the compute_event_time function
# months <- c(0, 6, 24)
# values <- c(1, 2, 1.5)
#
# print("original")
# print(months)
# print(values)
# compute_event_time(months, values)

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))

# read the data for censoring times, which was computed separately 
censoring_times <- readRDS("censoring_times.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# keep a copy of all of the patients
patients <- edss_data %>% select(PatientName) %>% distinct(PatientName)

# get the censoring times for EDSS, t25fw, nhpt
edss_censoring_time <- censoring_times %>% select(PatientName, edss_censor)
t25fw_censoring_time <- censoring_times %>% select(PatientName, t25fw_censor)
hpt_censoring_time <- censoring_times %>% select(PatientName, hpt_censor)

# read the imputed edss and pdds data from a saved file
imputed_data <- readRDS("imputed_edss_pdds_data.RDS")

# compute the event time after filling in missing values with MICE
edss_event_time <- imputed_data %>%
    select(PatientName, month, total_edss_score) %>%
    group_by(PatientName) %>%
    # the imputation method will be none because we already used MICE to impute
    # missing values
    summarize(edss_event = compute_event_time(month, total_edss_score, imputation_method="none"))

print(edss_event_time)

msfc_data <- data.frame(read_excel(data_file_name, sheet="msfc"))
msfc_data <- compute_average_msfc(msfc_data)

# compute the event time for T25FW data
t25fw_event_time <- msfc_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # after the above operation, get rid of all rows with a missing value
    filter(!is.na(month)) %>%
    # keep only patient name, month, and t25fw score columns
    select(PatientName, month, trial_average_seconds) %>%
    # remove problematic patient for whom we have no data
    filter(PatientName != "0256-013") %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # some patients may not have an entry for every 6-month interval;
    # this makes sure that every month at 6-month intervals are in the 
    # data; new inserted months have a missing value for the edss score
    complete(month=full_seq(month, 6)) %>%
    # # this sorts the patient names and months
    # arrange(PatientName, month) %>%
    # make a new column with the censoring times
    left_join(t25fw_censoring_time, by="PatientName") %>%
    # remove all rows of data there are after the censoring times for each
    # individual
    filter(month <= t25fw_censor) %>%
    # remove the censoring times for each individual
    select(-t25fw_censor) %>%
    # add the baseline covariate data for each individual to allow for
    # missing data imputation
    left_join(baseline_data, by="PatientName")

# make a copy of the original dataset that just keeps track of which
# values will be imputed
t25fw_copy <- t25fw_event_time %>%
    ungroup() %>%
    select(c(PatientName, month, trial_average_seconds)) %>%
    mutate(t25fw_missing=ifelse(is.na(trial_average_seconds), 1, 0)) %>%
    select(-trial_average_seconds)

# use MICE to impute missing values for msfc data in between visits
imp <- mice(t25fw_event_time, m=1, maxit=20, seed=0)

# save as RDS file the t25fw data for use as a secondary outcome
saveRDS(complete(imp, action=1), file="t25fw_data.RDS")

# join back whether values were imputed to the imputed dataset
annotated_t25fw_data <- complete(imp, action=1) %>%
    inner_join(t25fw_copy, by=c("PatientName", "month"))
# save annotated data as RDS
saveRDS(annotated_t25fw_data, file="annotated_t25fw_data.RDS")

# compute the event time after filling in missing values with MICE
t25fw_event_time <- complete(imp, action=1) %>%
    select(PatientName, month, trial_average_seconds) %>%
    group_by(PatientName) %>%
    # the imputation method will be none because we already used MICE to impute
    # missing values
    summarize(t25fw_event = compute_event_time(month, trial_average_seconds, value_type="MSFC", imputation_method="none"))

print(t25fw_event_time)

# compute the event time for NHPT data
nhpt_event_time <- msfc_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # after the above operation, get rid of all rows with a missing value
    filter(!is.na(month)) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, hand_average_seconds) %>%
    # remove problematic patient for whom we have no data
    filter(PatientName != "0256-013") %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # some patients may not have an entry for every 6-month interval;
    # this makes sure that every month at 6-month intervals are in the 
    # data; new inserted months have a missing value for the edss score
    complete(month=full_seq(month, 6)) %>%
    # # this sorts the patient names and months
    # arrange(PatientName, month) %>%
    # make a new column with the censoring times
    left_join(hpt_censoring_time, by="PatientName") %>%
    # remove all rows of data there are after the censoring times for each
    # individual
    filter(month <= hpt_censor) %>%
    # remove the censoring times for each individual
    select(-hpt_censor) %>%
    # add the baseline covariate data for each individual to allow for
    # missing data imputation
    left_join(baseline_data, by="PatientName")

nhpt_event_time %>% slice_head(n=10) %>% print()

# make a copy of the original dataset that just keeps track of which
# values will be imputed
nhpt_copy <- nhpt_event_time %>%
    ungroup() %>%
    select(c(PatientName, month, hand_average_seconds)) %>%
    mutate(nhpt_missing=ifelse(is.na(hand_average_seconds), 1, 0)) %>%
    select(-hand_average_seconds)

# use MICE to impute missing values for msfc data in between visits
imp <- mice(nhpt_event_time, m=1, maxit=20, seed=0)

# save as RDS file the nhpt data for use as a secondary outcome
saveRDS(complete(imp, action=1), file="nhpt_data.RDS")

# join back whether values were imputed to the imputed dataset
annotated_nhpt_data <- complete(imp, action=1) %>%
    inner_join(nhpt_copy, by=c("PatientName", "month"))
# save annotated data as RDS
saveRDS(annotated_nhpt_data, file="annotated_nhpt_data.RDS")

# compute the event time after filling in missing values with MICE
nhpt_event_time <- complete(imp, action=1) %>%
    select(PatientName, month, hand_average_seconds) %>%
    group_by(PatientName) %>%
    # the imputation method will be none because we already used MICE to impute
    # missing values
    summarize(nhpt_event = compute_event_time(month, hand_average_seconds, value_type="MSFC", imputation_method="none"))

print(nhpt_event_time)

# merge all of the event times together
event_times <- full_join(patients, edss_event_time, by="PatientName")
event_times <- full_join(event_times, t25fw_event_time, by="PatientName")
event_times <- full_join(event_times, nhpt_event_time, by="PatientName")

print(head(event_times))

#' Select the minimum event time out of the four computed event times.
#' @param edss The computed event time for EDSS
#' @param t25fw The computed event time for T25FW
#' @param nhpt The computed event time for NHPT
#' @return The minimum of the computed event times. If all four values are missing
#' return missing value.
#' @details Compute the minimum event time from the four computed metrics.
select_event_time <- function(edss, t25fw, nhpt) {
    # combine the three values into a vector
    vec <- c(edss, t25fw, nhpt)

    # if all values are missing, return a missing value
    if (sum(is.na(vec)) == length(vec)) return(NA)
    # otherwise, return the minimum value
    else return(min(vec, na.rm=TRUE))
}

event_times <- event_times %>%
    group_by(PatientName) %>%
    mutate(event_time = select_event_time(edss_event, t25fw_event, nhpt_event))

print(head(event_times))

# save the censoring times as a file
saveRDS(event_times, file="event_times.RDS")
