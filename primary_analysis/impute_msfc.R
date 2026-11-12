# Impute missing values for MSFC, then save
# to an RDS file for use in computation of other outcomes.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)

source("global_variables.R")

# read the user input data that specifies how we are imputing EDSS
args <- commandArgs(trailingOnly = TRUE)

# first check that the user actually entered an argument, if not then
# exit the program
if (length(args) < 1) {
    stop(paste(c("User did not supply argument for how to impute MSFC values.",
         "Usage is: Rscript impute_msfc.R \"none\"",
         "Possible imputation options are \"none\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

imputation_options <- c("none", "A best, B worst", "A worst, B best")
impute_method <- args[1]

# make sure that the user entered a valid imputation option
if (!(impute_method %in% imputation_options)) {
    stop(paste(c("User did not input a valid imputation option for EDSS.",
                 "Possible imputation options are \"none\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

# read the treatment assignemnts
treatment_data <- readRDS("treatment_data.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# read the msfc data
msfc_data <- data.frame(read_excel(data_file_name, sheet="msfc"))
msfc_data <- compute_average_msfc(msfc_data)

# note that below we have to impute t25fw and nhpt data separately
# because sometimes one can be measured without the other being measured

# get the T25FW data
t25fw_data <- msfc_data %>%
    # keep only patient name, visit_date, form group, and t25fw score columns
    select(PatientName, visit_date, FormGroup, trial_average_seconds) %>%
    # keep only observed values of T25FW
    filter(!is.na(trial_average_seconds)) %>%
    # remove a patient that does not have an entry in baseline chars
    filter(PatientName != "0256-013") %>%
    # change Interim Information value to Z so that it will appear after the month measurements;
    # this is so that when we drop duplicate visits, the month visits are kept
    mutate(FormGroup = if_else(FormGroup == "Interim Information", "Z", FormGroup)) %>%
    # sort by patient name and FormGroup
    arrange(PatientName, FormGroup) %>%
    # keep only measurements on different visit dates because there are duplicates
    # technically, we are supposed to keep the month with the correct window, but this
    # heuristic works for now (I manually checked)
    distinct(PatientName, visit_date, .keep_all=TRUE) %>%
    # change Z values back to Interim Information in FormGroup
    mutate(FormGroup = if_else(FormGroup == "Z", "Interim Information", FormGroup)) %>%
    # sort by the visit date
    arrange(PatientName, visit_date)

# check for duplicate visits
t25fw_data %>%
    group_by(PatientName) %>%
    summarize(duplicates = if_else(length(visit_date) > length(unique(visit_date)), 1, 0)) %>%
    filter(duplicates == 1) %>%
    print()

# get the NHPT data
nhpt_data <- msfc_data %>%
    # keep only patient name, visit_date, form group, and nhpt score columns
    select(PatientName, visit_date, FormGroup, hand_average_seconds) %>%
    # keep only observed values of NHPT
    filter(!is.na(hand_average_seconds)) %>%
    # remove a patient that does not have an entry in baseline chars
    filter(PatientName != "0256-013") %>%
    # change Interim Information value to Z so that it will appear after the month measurements;
    # this is so that when we drop duplicate visits, the month visits are kept
    mutate(FormGroup = if_else(FormGroup == "Interim Information", "Z", FormGroup)) %>%
    # sort by patient name and FormGroup
    arrange(PatientName, FormGroup) %>%
    # keep only measurements on different visit dates because there are duplicates
    # technically, we are supposed to keep the month with the correct window, but this
    # heuristic works for now (I manually checked)
    distinct(PatientName, visit_date, .keep_all=TRUE) %>%
    # change Z values back to Interim Information in FormGroup
    mutate(FormGroup = if_else(FormGroup == "Z", "Interim Information", FormGroup)) %>%
    # sort by the visit date
    arrange(PatientName, visit_date)

# check for repeated visits of nhpt
nhpt_data %>%
    group_by(PatientName) %>%
    summarize(duplicates = if_else(length(visit_date) > length(unique(visit_date)), 1, 0)) %>%
    filter(duplicates == 1) %>%
    print()

if (impute_method == "none") {
    t25fw_data <- t25fw_data %>%
        # get rid of the column form group
        select(-FormGroup)

    # save the data to RDS
    saveRDS(t25fw_data, file="t25fw_data.RDS")

    nhpt_data <- nhpt_data %>%
        # get rid of the column form group
        select(-FormGroup)

    # save the data to RDS
    saveRDS(nhpt_data, file="nhpt_data.RDS")
} else {
    # impute the visit dates for the t25fw data
    t25fw_data <- impute_visit_dates(t25fw_data)

    # join the treatment data to the t25fw data
    t25fw_data <- t25fw_data %>%
        left_join(treatment_data, by="PatientName")

    # save the min and max values for t25fw
    t25fw_min <- 0
    t25fw_max <- 80

    # impute the visit dates for the nhpt data
    nhpt_data <- impute_visit_dates(nhpt_data)

    # join the treatment data to the nhpt data
    nhpt_data <- nhpt_data %>%
        left_join(treatment_data, by="PatientName")

    # save the min and max values for nhpt
    nhpt_min <- 0
    nhpt_max <- 200

    if (impute_method == "A best, B worst") {
        # impute missing values of t25fw as the minimum possible t25fw value if treatment is 0
        # and the maximum possible value of t25fw otherwise
        t25fw_data <- t25fw_data %>%
            mutate(trial_average_seconds = if_else(is.na(trial_average_seconds), if_else(treatment_group == 0, t25fw_min, t25fw_max), trial_average_seconds))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit_t25fw <- t25fw_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the t25fw to the best/worst value depending on the treatment
                # group
                trial_average_seconds = if_else(treatment_group == 0, t25fw_min, t25fw_max),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )

        # impute missing values of nhpt as the minimum possible nhpt value if treatment is 0
        # and the maximum possible value of nhpt otherwise
        nhpt_data <- nhpt_data %>%
            mutate(hand_average_seconds = if_else(is.na(hand_average_seconds), if_else(treatment_group == 0, nhpt_min, nhpt_max), hand_average_seconds))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit_nhpt <- nhpt_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the nhpt to the best/worst value depending on the treatment
                # group
                hand_average_seconds = if_else(treatment_group == 0, nhpt_min, nhpt_max),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )
    } else if (impute_method == "A worst, B best") {
        # impute missing values of t25fw as the maximum possible t25fw value if treatment is 0
        # and the minimum possible value of t25fw otherwise
        t25fw_data <- t25fw_data %>%
            mutate(trial_average_seconds = if_else(is.na(trial_average_seconds), if_else(treatment_group == 0, t25fw_max, t25fw_min), trial_average_seconds))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit_t25fw <- t25fw_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the t25fw to the best/worst value depending on the treatment
                # group
                trial_average_seconds = if_else(treatment_group == 0, t25fw_max, t25fw_min),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )

        # impute missing values of nhpt as the maximum possible nhpt value if treatment is 0
        # and the minimum possible value of nhpt otherwise
        nhpt_data <- nhpt_data %>%
            mutate(hand_average_seconds = if_else(is.na(hand_average_seconds), if_else(treatment_group == 0, nhpt_max, nhpt_min), hand_average_seconds))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit_nhpt <- nhpt_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the nhpt to the best/worst value depending on the treatment
                # group
                hand_average_seconds = if_else(treatment_group == 0, nhpt_max, nhpt_min),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )
    }

    # combine the rows of the t25fw data with the extra visits
    t25fw_data <- t25fw_data %>%
        bind_rows(extra_visit_t25fw) 

    # combine the rows of the nhpt data with the extra visits
    nhpt_data <- nhpt_data %>%
        bind_rows(extra_visit_nhpt) 

    # save the data to RDS
    saveRDS(t25fw_data, file="t25fw_data.RDS")

    # save the data to RDS
    saveRDS(nhpt_data, file="nhpt_data.RDs")
}

