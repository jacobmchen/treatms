# Compute the event time for EDSS, T25FW, 9HPT

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)

source("global_variables.R")

# read the user input data that specifies how we are computing event time
args <- commandArgs(trailingOnly = TRUE)

# first check that the user actually entered an argument, if not then
# exit the program
if (length(args) < 1) {
    stop(paste(c("User did not supply argument for how to process event time.",
         "Usage is: Rscript event_time.R \"primary\"",
         "Possible options are \"primary\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

analysis_options <- c("primary", "A best, B worst", "A worst, B best")
analysis_method <- args[1]

# make sure that the user entered a valid imputation option
if (!(analysis_method %in% analysis_options)) {
    stop(paste(c("User did not input a valid option for computing event time.",
                 "Possible options are \"primary\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

# define a function for computing the event time, if it happens
#' Compute Event Time
#' @param months A vector containing the months of the measurements
#' @param values A vector containing the observed values
#' @param value_type A string specifying whether the value is an EDSS value
#' or an MSFC value since event time is defined differently for these
#' types of values.
#' @return A single number signifying the month of the event time. If the
#' event did not occur, returns Inf. If it is not possible to compute the
#' event time, returns NA. 
#' @details Compute the event time for a single individual given their observations. It is not
#' possible to compute the event time if there is no baseline value
#' or if there are less than 2 observed values after baseline.
compute_event_time <- function(months, values, treatment, value_type="EDSS") {
    # the vector treatment will just be a vector with n copies of
    # the individual's treatment based on where n is the number of observations
    treatment <- treatment[1]

    # get the version of months and values with only the observed values
    observed <- observed_months_values(months, values)

    # update the months and values to just observed values
    months <- observed[[1]]
    values <- observed[[2]]

    # if there is only one value or zero values in the vector, we cannot
    # compute the event time
    if (length(months) <= 1) {
        return(NA)
    }

    # first, if every observed value is missing, then we cannot compute the 
    # event time
    if (sum(is.na(values)) > 0) {
        # if we enter this if statement, that means that
        # we encounter missing values in the vector of values, which should
        # not have any missing values because we removed all missing values

        # thus, stop the program and return a fatal error
        stop("Fatal error: For some patient, missing values were not removed.")

        # return NA as a failsafe
        return(NA)
    }

    # declare the threshold that needs to be reached for two consecutive
    # months
    threshold <- 0
    if (value_type == "EDSS") {
        # first round all values to the nearest 0.5 since EDSS scores
        # are always in increments of 0.5
        values <- round(values / 0.5) * 0.5

        # determine the threshold value, treating the first observed value
        # as the baseline value
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

    # second, check if we have exactly 2 measurements (this if
    # statement captures those with only 2 measurements because
    # an if statement above captures those with less than or equal
    # to 1 measurements)
    if (length(values) < 3) {
        # check that the length of the values is exactly 2 before proceeding
        if (length(values) != 2) stop("Fatal error: expected 2 measurements but did not get that")

        # check if second measurement progressed
        if (values[2] >= threshold) {
            if (analysis_method == "primary") {
                # analysis method is primary, so just proceed as normal
                # if second measurement progressed, return -1 because we aren't able
                # to confirm whether they progressed, and this patient should be
                # dropped from the analysis;
                # -1 here is a flag to indicate that this patient should be dropped;
                # we need to use a -1 flag because a patient needs to be dropped if
                # AT LEAST one of EDSS, T25FW, or NHPT satisfies this criterion
                return(-1)
            } else if (analysis_method == "A best, B worst") {
                # if the treatment is A (0), then return the best case scenario, which is that
                # they did not experience disease progression
                if (treatment == 0) return(Inf)
                # otherwise, the treatment is B (1), so return that they progressed at
                # the second measurement
                else return(months[2])
            } else if (analysis_method == "A worst, B best") {
                # if the treatment is A (0), then return the worst case scenario, which is that
                # they experienced disease progression
                if (treatment == 0) return(months[2])
                # otherwise, the treatment is B (1), so return that they did not progress
                else return(Inf)
            } else {
                # if none of the above strings are given as the analysis_method, stop
                # the program and throw an error
                stop("Fatal error: user did not input a valid analysis method")
            }
        } else {
            # otherwise, return Inf, because they never experienced the event, so
            # we can consider them censored at their second measurement
            return(Inf)
        }
    }

    # check if there was sustained disability progression as we have enough
    # measurements to evaluate this
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
# months <- c(0, 6, 12, 24, 30, 36)
# values <- c(1, 1, NA, 2, NA, 2)
# #
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

# read the treatment data
treatment_data <- readRDS("treatment_data.RDS")

# keep a copy of all of the patients
patients <- edss_data %>% select(PatientName) %>% distinct(PatientName)

# get the censoring times for t25fw, nhpt
# these censoring times are needed for imputation of MSFC values
t25fw_censoring_time <- censoring_times %>% select(PatientName, t25fw_censor)
hpt_censoring_time <- censoring_times %>% select(PatientName, hpt_censor)

# read the imputed edss and pdds data from a saved file
imputed_data <- readRDS("imputed_edss_pdds_data.RDS")

# compute the event time after filling in missing values with MICE
edss_event_time <- imputed_data %>%
    select(PatientName, month, total_edss_score) %>%
    # join the patient name data
    left_join(treatment_data, by="PatientName") %>%
    group_by(PatientName) %>%
    # compute the event time
    summarize(edss_event = compute_event_time(month, total_edss_score, treatment_group))

print(edss_event_time)

# read the imputed edss and pdds data with no exceptions from a saved file
imputed_data_no_exceptions <- readRDS("imputed_edss_pdds_data_no_exceptions.RDS")

# compute the event time with no exceptions after filling in missing values with MICE
edss_event_time_no_exceptions <- imputed_data_no_exceptions %>%
    select(PatientName, month, total_edss_score) %>%
    left_join(treatment_data, by="PatientName") %>%
    group_by(PatientName) %>%
    # compute the event time
    summarize(edss_event = compute_event_time(month, total_edss_score, treatment_group))

print(edss_event_time_no_exceptions)

##################
# The following code block is for an exploratory 
# analysis only. May be commented out, or left in without harm.
#
# # read the imputed edss and pdds data with annotations on which
# # values were imputed by MICE
# annotated_edss_pdds <- readRDS("annotated_imputed_edss_pdds_data.RDS") %>%
#     # keep only rows of data where EDSS was observed
#     filter(edss_missing == 0)
#
# # compute the event time using only observed values
# edss_event_time_observed_vals <- annotated_edss_pdds %>%
#     select(PatientName, month, total_edss_score) %>%
#     group_by(PatientName) %>%
#     # compute the event time using only observed values
#     summarize(edss_event_observed = compute_event_time(month, total_edss_score))
#
# print(edss_event_time_observed_vals)
#
# # create a dataframe for comparing EDSS event time based on whether
# # we use imputed MICE values or not
# compare_event_times <- edss_event_time %>%
#     # first, combine the computed event times with and without imputation
#     inner_join(edss_event_time_observed_vals, by="PatientName") %>%
#     # check if both event times are NA
#     mutate(both_na = ifelse(is.na(edss_event) & is.na(edss_event_observed), 1, 0)) %>%
#     # check if both event times have the same value
#     mutate(same_val = ifelse(edss_event == edss_event_observed, 1, 0)) %>%
#     # if same_val returns NA as a result of one of the event times being NA,
#     # then change it to a value of 0, meaning event times do not have the same value
#     mutate(same_val = ifelse(is.na(same_val), 0, same_val)) %>%
#     # if at least one of both_na or same_val is 1, then event time with and
#     # without imputed values have the same value, otherwise they do not
#     mutate(diff_vals = ifelse(both_na == 1 | same_val == 1, 0, 1)) %>%
#     # rename the column edss_event
#     rename(edss_event_imputed = edss_event) %>%
#     # subset to just rows where computed event times are different
#     filter(diff_vals == 1) %>%
#     select(-c(both_na, same_val, diff_vals)) 
#
# print(compare_event_times)
#
# # write this data to a csv file
# write.csv(compare_event_times, "compare_event_times_edss.csv", row.names=FALSE)
#
##########################

# read the imputed t25fw data
t25fw_data <- readRDS("t25fw_data.RDS")

# compute the event time after filling in missing values with MICE
t25fw_event_time <- t25fw_data %>%
    select(PatientName, month, trial_average_seconds) %>%
    left_join(treatment_data, by="PatientName") %>%
    group_by(PatientName) %>%
    summarize(t25fw_event = compute_event_time(month, trial_average_seconds, treatment_group, value_type="MSFC"))

print(t25fw_event_time)

# read the nhpt data
nhpt_data <- readRDS("nhpt_data.RDS")

# compute the event time after filling in missing values with MICE
nhpt_event_time <- nhpt_data %>%
    select(PatientName, month, hand_average_seconds) %>%
    left_join(treatment_data, by="PatientName") %>%
    group_by(PatientName) %>%
    # the imputation method will be none because we already used MICE to impute
    # missing values
    summarize(nhpt_event = compute_event_time(month, hand_average_seconds, treatment_group, value_type="MSFC"))

print(nhpt_event_time)

# merge all of the event times together
event_times <- patients %>%
    full_join(edss_event_time, by="PatientName") %>%
    full_join(t25fw_event_time, by="PatientName") %>%
    full_join(nhpt_event_time, by="PatientName")

print("event times")
print(head(event_times))

# merge all of the event times together with no exceptions
event_times_no_exceptions <- patients %>%
    full_join(edss_event_time_no_exceptions, by="PatientName") %>%
    full_join(t25fw_event_time, by="PatientName") %>%
    full_join(nhpt_event_time, by="PatientName")

print("event times, no exceptions")
print(head(event_times_no_exceptions))

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

    # check if we have the -1 flag, which indicates that an individual
    # had only two measurements and progressed at the second measurement;
    # this means we can't confirm their disease progression
    if (min(vec, na.rm=TRUE) == -1) return(NA)

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

# when adding the patients with no event time to the consort, distinguish between
# having an unconfirmed progression and not having enough measurements

# add a new column to the no event time patients indicating whether they had
# an unconfirmed progression and only two measurements; 1 means yes and 0 means no
no_event_time <- no_event_time %>% 
    # check if -1 appears for each of the event times; if all 3 values are NA, then the
    # min function returns Inf and gives a warning message, but we can safely ignore
    # the warning message
    mutate(unconfirmed_progression = ifelse(min(c(edss_event, t25fw_event, nhpt_event), na.rm=TRUE) == -1, 1, 0)) 

# get the subset of patients who had only two measurements and progressed at the
# second measurement for at least one of EDSS, T25FW, or NHPT
unconfirmed_progression <- no_event_time %>%
    filter(unconfirmed_progression == 1)

# add to the consort the patients who had an unconfirmed progression
add_to_consort(unconfirmed_progression$PatientName,
                "experienced unconfirmed progression and only has two measurements for at least one of EDSS, T25FW, or NHPT", "event_time.R")

# get the subset of patients who had 1 or less measurements for all of EDSS, T25FW,
# and NHPT
not_enough_data <- no_event_time %>%
    filter(unconfirmed_progression == 0)

# add to the consort the patients that do not have enough data
add_to_consort(not_enough_data$PatientName,
                "one measurement or less for all of EDSS, T25FW, and NHPT", "event_time.R")

# actually drop the patients who have no censoring time
event_times <- event_times %>%
    filter(!is.na(event_time))

# save the censoring times as a file
saveRDS(event_times, file="event_times.RDS")

# choose the event time corresponding to the event time that happened
# first of edss, t25fw, or nhpt when ignoring exceptions
event_times_no_exceptions <- event_times_no_exceptions %>%
    group_by(PatientName) %>%
    mutate(event_time = select_event_time(edss_event, t25fw_event, nhpt_event))

print("final event time, no exceptions")
print(head(event_times_no_exceptions))

# get the list patients for whom we could not calculate an event time
no_event_time <- event_times_no_exceptions %>% 
    filter(is.na(event_time))

# save the list of patients with no event time
saveRDS(no_event_time$PatientName, "no_event_time_no_exceptions_patients.RDS")

# add a new column to the no event time patients indicating whether they had
# an unconfirmed progression and only two measurements; 1 means yes and 0 means no
no_event_time <- no_event_time %>% 
    # check if -1 appears for each of the event times; if all 3 values are NA, then the
    # min function returns Inf and gives a warning message, but we can safely ignore
    # the warning message
    mutate(unconfirmed_progression = ifelse(min(c(edss_event, t25fw_event, nhpt_event), na.rm=TRUE) == -1, 1, 0)) 

# get the subset of patients who had only two measurements and progressed at the
# second measurement for at least one of EDSS, T25FW, or NHPT
unconfirmed_progression <- no_event_time %>%
    filter(unconfirmed_progression == 1)

# add to the consort the patients who had an unconfirmed progression
add_to_consort(unconfirmed_progression$PatientName,
                "experienced unconfirmed progression and only has two measurements for at least one of EDSS, T25FW, or NHPT", "event_time.R",
                no_exceptions=TRUE)

# get the subset of patients who had 1 or less measurements for all of EDSS, T25FW,
# and NHPT
not_enough_data <- no_event_time %>%
    filter(unconfirmed_progression == 0)

# add to the consort the patients that do not have enough data
add_to_consort(not_enough_data$PatientName,
                "one measurement or less for all of EDSS, T25FW, and NHPT", "event_time.R",
                no_exceptions=TRUE)

# actually drop the patients who have no censoring time
event_times_no_exceptions <- event_times_no_exceptions %>%
    filter(!is.na(event_time))

# save the event times as a file
saveRDS(event_times_no_exceptions, file="event_times_no_exceptions.RDS")

# check if there are differences if you use exceptions or not;
# there are 28 patients for whom event time is different when
# you don't use exception findings
event_times %>% full_join(event_times_no_exceptions, by="PatientName") %>% 
    select(c(PatientName, edss_event.x, edss_event.y)) %>% filter(edss_event.x != edss_event.y) %>% print()
