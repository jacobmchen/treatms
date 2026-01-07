# Compute the event time for EDSS, T25FW, 9HPT dominant,
# and 9HPT non-dominant hand.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

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

    # get the month at which we have the final observed value
    # this takes the index of months at which the final index
    # was observed by taking the maximum of the vector containing
    # which indices had observed values in the vector values
    final_observed_month <- months[max(which(!is.na(values)))]

    # first, the vector months may not contain every single 6 month
    # interval, so we have to add them back in and insert missing values
    # for the months that were not originally in the interval
    months_filled <- seq(0, final_observed_month, by=6)
    values_filled <- c()

    pos <- 1
    cur_month <- 0
    while (cur_month <= final_observed_month) {
        if (months[pos] == cur_month) {
            values_filled <- c(values_filled, values[pos])
            pos <- pos + 1
        } else {
            values_filled <- c(values_filled, NA)
        }
        cur_month <- cur_month + 6
    }

    # second, check if it is possible to compute an event time
    if (is.na(values_filled[1])) {
        # if there is no baseline value, we cannot compute sustained disease progression
        return(NA)
    } else if (length(values_filled) < 3) {
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
        values_filled <- approx(values_filled, n=length(values_filled))$y
    }

    # declare the threshold that needs to be reached for two consecutive
    # months
    threshold <- 0
    if (value_type == "EDSS") {
        # first round all values to the nearest 0.5 since EDSS scores
        # are always in increments of 0.5
        values_filled <- round(values_filled / 0.5) * 0.5

        # determine the threshold value
        if (values_filled[1] <= 5.5) {
            threshold <- values_filled[1] + 1
        } else {
            threshold <- values_filled[1] + 0.5
        }
    } else if (value_type == "MSFC") {
        # no need to round values if the value type is MSFC, which
        # includes T25FW and NHPT

        # the threshold value is 1.2 times the baseline value
        threshold <- values_filled[1] * 1.2
    }

    # check if there was sustained disability progression
    # iterate from the first non-baseline value to the second to last value
    for (i in 2:(length(values_filled)-1)) {
        # check if two consecutive values are greater than or equal to the threshold
        if (values_filled[i] >= threshold & values_filled[i+1] >= threshold) {
            return(months_filled[i])
        }
    }

    return(Inf)
}

# months <- c(0, 6, 24)
# values <- c(1, 2, 1.5)
#
# print("original")
# print(months)
# print(values)
# compute_event_time(months, values)

# read the data for EDSS
edss_data <- data.frame(read_excel("../preliminary_longitudinal_data.xlsx", sheet="edss"))

# keep a copy of all of the patients
patients <- edss_data %>% select(PatientName) %>% distinct(PatientName)

# compute the event time for EDSS data
edss_event_time <- edss_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, total_edss_score) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # compute the event time using interpolation imputation
    summarize(edss_event = compute_event_time(month, total_edss_score))

print(edss_event_time)

msfc_data <- data.frame(read_excel("../preliminary_longitudinal_data.xlsx", sheet="msfc"))

# compute the event time for T25FW data
t25fw_event_time <- msfc_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, trial_average_seconds) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # compute the event time using interpolation imputation
    summarize(t25fw_event = compute_event_time(month, trial_average_seconds, value_type="MSFC"))

print(t25fw_event_time)

# compute the event time for NHPT dominant hand data
nhpt_dominant_event_time <- msfc_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, dominant_hand_average_seconds) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # compute the event time using interpolation imputation
    summarize(nhpt_dominant_event = compute_event_time(month, dominant_hand_average_seconds, value_type="MSFC"))

print(nhpt_dominant_event_time)

# compute the event time for NHPT non-dominant hand data
nhpt_non_dominant_event_time <- msfc_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, non_dominant_hand_average_seconds) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # compute the event time using interpolation imputation
    summarize(nhpt_non_dominant_event = compute_event_time(month, non_dominant_hand_average_seconds, value_type="MSFC"))

print(nhpt_non_dominant_event_time)

# merge all of the event times together
event_times <- full_join(patients, edss_event_time, by="PatientName")
event_times <- full_join(event_times, t25fw_event_time, by="PatientName")
event_times <- full_join(event_times, nhpt_dominant_event_time, by="PatientName")
event_times <- full_join(event_times, nhpt_non_dominant_event_time, by="PatientName")

print(event_times)

#' Select the minimum event time out of the four computed event times.
#' @param edss The computed event time for EDSS
#' @param t25fw The computed event time for T25FW
#' @param nhpt_dom The computed event time for NHPT on the dominant hand.
#' @param nhpt_non_dom The computed event time for NHPT on the non-dominant hand.
#' @return The minimum of the computed event times. If all four values are missing
#' return missing value.
#' @details Compute the minimum event time from the four computed metrics.
select_event_time <- function(edss, t25fw, nhpt_dom, nhpt_non_dom) {
    # combine the four values into a vector
    vec <- c(edss, t25fw, nhpt_dom, nhpt_non_dom)

    # if all values are missing, return a missing value
    if (sum(is.na(vec)) == length(vec)) return(NA)
    # otherwise, return the minimum value
    else return(min(vec, na.rm=TRUE))
}

event_times <- event_times %>%
    group_by(PatientName) %>%
    mutate(event_time = select_event_time(edss_event, t25fw_event, nhpt_dominant_event, nhpt_non_dominant_event))

print(event_times)

# save the censoring times as a file
saveRDS(event_times, file="event_times.RDS")
