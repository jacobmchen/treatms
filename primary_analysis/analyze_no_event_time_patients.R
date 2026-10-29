# analyze the characteristics of the patients who have less than 3 measurements
# of all of EDSS, T25FW, and NHPT

# package for operations on manipulating data
library(tidyverse)

source("global_variables.R")

# read the list of patients that have no event time due to not enough measurements
no_event_time_patients <- readRDS("no_event_time_patients.RDS")

print(no_event_time_patients)

# create a function that takes as input a months vector and a values vector
# and returns a list containing the two vectors where missing values in the
# values vector are removed and the corresponding months are also removed
observed_months_values <- function(months, values) {
    # create two new vectors that are copies of the original months
    # and values except they only keep the months with observed values
    observed_months <- c()
    observed_values <- c()
    # iterate through all the values
    for (i in 1:length(values)) {
        # save the value if it is observed, otherwise, skip it
        if (!is.na(values[i])) {
            observed_months <- c(observed_months, months[i])
            observed_values <- c(observed_values, values[i])
        }
    }
    # update the months and values to just observed values
    months <- observed_months
    values <- observed_values

    return(list(months=months, values=values))
}

# declare a function that returns 1 if there is a month 0 baseline
# measurement and 0 otherwise
has_baseline <- function(months, values) {
    # get the versions of months and values with only the observed values
    observed <- observed_months_values(months, values)
    # update the months and values to just observed values
    months <- observed[[1]]
    values <- observed[[2]]

    if (length(months) == 0) return(0)

    if (months[1] == 0) {
        return(1)
    } else {
        return(0)
    }
}

# declare a function that returns 1 if there are 2 observed measurements
# of values and 0 otherwise
two_measurements <- function(months, values) {
    # get the versions of months and values with only the observed values
    observed <- observed_months_values(months, values)
    # update the months and values to just observed values
    months <- observed[[1]]
    values <- observed[[2]]

    if (length(values) == 2) {
        return(1)
    } else {
        return(0)
    }
}

# declare a function that looks at the months of a patient and returns
# whether we observed month 0 and 6 or not
month_0_6 <- function(months, values) {
    # get the versions of months and values with only the observed values
    observed <- observed_months_values(months, values)
    # update the months and values to just observed values
    months <- observed[[1]]
    values <- observed[[2]]

    # if the length of months is not exactly 2, then they cannot
    # satisfy our desired condition
    if (length(months) != 2) {
        return(0)
    }

    if (months[1] == 0 & months[2] == 6) {
        # satisfy condition
        return(1)
    } else {
        # does not satisfy condition
        return(0)
    }
}

# declare a function that determines whether there is disease progression
# at the second measurement, returns 1 if yes and 0 otherwise
# if there are not exactly two measurements, then return 0
disease_progression <- function(months, values, value_type="EDSS") {
    # get the versions of months and values with only the observed values
    observed <- observed_months_values(months, values)
    # update the months and values to just observed values
    months <- observed[[1]]
    values <- observed[[2]]

    # check if there are actually two observations
    if (length(values) != 2) {
        return(0)
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

    # determine if there is disease progression at the second
    # measurement
    if (values[2] >= threshold) {
        return(1)
    } else {
        return(0)
    }
}

# read the imputed edss and pdds data from a saved file
imputed_data <- readRDS("imputed_edss_pdds_data.RDS")

# read the imputed t25fw data
t25fw_data <- readRDS("t25fw_data.RDS")

# read the nhpt data
nhpt_data <- readRDS("nhpt_data.RDS")

# create a dataframe that contains the following columns:
# 1. whether for EDSS there is a baseline month 0 measurement
# 2. whether for EDSS there are two measurements
# 3. whether for EDSS the measurements are at month 0 and month 6
# 4. whether for EDSS there is disease progression at the second measurement
# 5. whether for T25FW there is a baseline month 0 measurement
# 6. whether for T25FW there are two measurements
# 7. whether for T25FW the measurements are at month 0 and month 6
# 8. whether for T25FW there is disease progression at the second measurement
# 9. whether for NHPT there is a baseline month 0 measurement
# 10. whether for NHPT there are two measurements
# 11. whether for NHPT the measurements are at month 0 and month 6
# 12. whether for NHPT there is disease progression at the second measurement
# the dataframe will be named summary_data

# create columns 1 through 4 above
summary_data <- imputed_data %>%
    select(PatientName, month, total_edss_score) %>%
    filter(PatientName %in% no_event_time_patients) %>%
    group_by(PatientName) %>%
    summarize(edss_has_baseline = has_baseline(month, total_edss_score),
              edss_two_measures = two_measurements(month, total_edss_score),
              edss_month_0_6 = month_0_6(month, total_edss_score),
              edss_second_measure_progress = disease_progression(month, total_edss_score))

# create columns 5 through 8 above
t25fw_summary <- t25fw_data %>%
    select(PatientName, month, trial_average_seconds) %>%
    filter(PatientName %in% no_event_time_patients) %>%
    group_by(PatientName) %>%
    summarize(t25fw_has_baseline = has_baseline(month, trial_average_seconds),
              t25fw_two_measures = two_measurements(month, trial_average_seconds),
              t25fw_month_0_6 = month_0_6(month, trial_average_seconds),
              t25fw_second_measure_progress = disease_progression(month, trial_average_seconds, value_type="MSFC"))

# join the t25fw data to the summary data
summary_data <- summary_data %>%
    full_join(t25fw_summary, by="PatientName")

# create columns 9 through 12 above
nhpt_summary <- nhpt_data %>%
    select(PatientName, month, hand_average_seconds) %>%
    filter(PatientName %in% no_event_time_patients) %>%
    group_by(PatientName) %>%
    summarize(nhpt_has_baseline = has_baseline(month, hand_average_seconds),
              nhpt_two_measures = two_measurements(month, hand_average_seconds),
              nhpt_month_0_6 = month_0_6(month, hand_average_seconds),
              nhpt_second_measure_progress = disease_progression(month, hand_average_seconds, value_type="MSFC"))

# join the nhpt data to the summary data
summary_data <- summary_data %>%
    full_join(nhpt_summary, by="PatientName")
summary_data %>% slice_head(n=10) %>% print(width=Inf)

# write to csv file
write.csv(summary_data, "no_event_time_summary.csv", row.names=FALSE)

# how many are observed at month 0 and 6 for all of EDSS and MSFC?
summary_data %>% filter(edss_month_0_6 == 1 & t25fw_month_0_6 == 1 & nhpt_month_0_6 == 1) %>% print()

# how many have disease progression that at the second measurement for at least one of EDSS or MSFC?
summary_data %>% filter(edss_second_measure_progress == 1 | t25fw_second_measure_progress == 1 | nhpt_second_measure_progress == 1) %>% print()

# how many have only less than 2 visits recorded?
summary_data %>% select(c(PatientName, edss_two_measures, t25fw_two_measures, nhpt_two_measures)) %>% filter(edss_two_measures != 1 & t25fw_two_measures != 1 & nhpt_two_measures != 1) %>% print()
