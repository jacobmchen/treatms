# Impute missing values for EDSS and PDDS, then save
# to an RDS file for use in computation of other outcomes.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

source("global_variables.R")

# read the user input data that specifies how we are imputing EDSS
args <- commandArgs(trailingOnly = TRUE)

# first check that the user actually entered an argument, if not then
# exit the program
if (length(args) < 1) {
    stop(paste(c("User did not supply argument for how to impute values and whether to keep values of EDSS with exception finding.",
         "Usage is: Rscript impute_edss_pdds.R \"none\" \"exclude exceptions\"",
         "Possible imputation options are \"none\", \"A best, B worst\", \"A worst, B best\"",
         "Possible options for handling EDSS excpetion findings are \"exclude exceptions\", \"include exceptions\""), collapse="\n"))
}

imputation_options <- c("none", "A best, B worst", "A worst, B best")
impute_method <- args[1]
exception_options <- c("exclude exceptions", "include exceptions")
exception_flag <- args[2]

# make sure that the user entered a valid imputation option
if (!(impute_method %in% imputation_options)) {
    stop(paste(c("User did not input a valid imputation option.",
                 "Possible imputation options are \"none\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}
# make sure that the user entered a valid option for how to handle EDSS exceptions
if (!(exception_flag %in% exception_options)) {
    stop(paste(c("User did not specify a valid option for dealing with EDSS exceptions.",
                 "Possible options for handling EDSS exceptions are \"exclude exceptions\", \"include exceptions\""), collapse="\n"))
}

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# read the treatment assignemnts
treatment_data <- readRDS("treatment_data.RDS")

# check if we are ignoring values of EDSS with an exception finding, if so
# keep only rows that do NOT have an exception finding
if (exception_flag == "exclude exceptions") {
    edss_data <- edss_data %>%
        filter(fs_finding == "No")
}

# prepare the EDSS data
edss_data <- edss_data %>%
    # keep only patient name, visit_date, and edss score columns (including
    # sub-functional system scores)
    select(PatientName, visit_date, FormGroup, total_edss_score,
           fs_cfss_total, fsvs_on_total, fs_bfss_total, total_pyramidal_score,
           sensory_system_score_total, cerebellar_system_score_total, bowel_bladder_sys_score_total) %>%
    # remove a patient that does not have an entry in baseline chars
    filter(PatientName != "0256-013") %>%
    # change every instance of Not Obtained to a missing value in the data
    mutate(across(where(is.character), ~ na_if(.x, "Not Obtained"))) %>%
    # this column is being read as a string because some rows in the column
    # are strings, so cast it as a numeric
    mutate(fsvs_on_total = as.numeric(fsvs_on_total)) %>%
    # also cast this column as a numeric for the same reason as above
    mutate(bowel_bladder_sys_score_total = as.numeric(bowel_bladder_sys_score_total)) %>%
    # change Interim Information value to Z so that it will appear after the month measurements;
    # this is so that when we drop duplicate visits, the month visits are kept
    mutate(FormGroup = ifelse(FormGroup == "Interim Information", "Z", FormGroup)) %>%
    # sort by FormGroup
    arrange(PatientName, FormGroup) %>%
    # keep only measurements on different visit dates because there are duplicates
    distinct(PatientName, visit_date, .keep_all=TRUE) %>%
    # change Z values back to Interim Information in FormGroup
    mutate(FormGroup = ifelse(FormGroup == "Z", "Interim Information", FormGroup)) %>%
    # sort by the visit date
    arrange(visit_date)

# add the list of patients we drop to the consort
add_to_consort(c("0256-013"), 
                "recorded in EDSS and PDDS, but not in baseline chars", "impute_edss_pdds.R")

# if we are not imputing any values, then just perform a couple more pre-processing steps
# and save the edss data
if (impute_method == "none") {
    edss_data <- edss_data %>%
        # remove the form group column
        select(-FormGroup)

    saveRDS(edss_data, file="imputed_edss_data.RDS")
} else {
    # get the interim visits in the edss data
    interim_edss <- edss_data %>%
        # keep only visits that are labeled interim information
        filter(FormGroup == "Interim Information") %>%
        # remove the form group column
        select(-FormGroup)

    # get the visits with month labels in the edss data and
    # complete the sequence
    regular_edss <- edss_data %>%
        filter(FormGroup != "Interim Information") %>%
        # convert the formgroup data to numbers
        mutate(FormGroup = as.integer(gsub("\\D", "", FormGroup))) %>%
        # group the data by patient name
        group_by(PatientName) %>%
        # complete the sequence of month measurements by 6 month intervals
        complete(FormGroup=full_seq(FormGroup, 6)) %>%
        # create 5 new columns that keep track of the row number (just the index),
        # the previous recorded visit date, the next recorded visit date, the row
        # number of the previously recorded visit date, and the row number of the 
        # next recorded visit date
        mutate(
            row = row_number(),
            # initilize as just the current visit date, even if it is NA
            prev_date = visit_date,
            # initalize as just the current visit date, even if it is NA
            next_date = visit_date,
            # the previous row number is the current row if visit_date is observed and
            # NA if visit_date is not observed
            prev_row = if_else(!is.na(visit_date), row, NA),
            # the next row number is the current row if visit_date is observed and
            # NA if visit_date is not observed
            next_row = if_else(!is.na(visit_date), row, NA)
        ) %>%
        # fill in missing values of column prev_date and prev_row by using the last observed value 
        fill(prev_date, prev_row, .direction = "down") %>%
        # fill in missing values of column next_date and next_row using the the next observed value
        fill(next_date, next_row, .direction = "up") %>%
        # fill in the missing visit dates as the dates between the most recently observed and next
        # observed value evenly spaced out
        mutate(visit_date = if_else(is.na(visit_date),
                                    as.Date( round( as.numeric(prev_date) / 86400 + (as.numeric(next_date) / 86400 - as.numeric(prev_date) / 86400) * (row-prev_row) / (next_row-prev_row) ) , origin = "1970-01-01" ),
                                    visit_date)) %>%
        # remove the columns we no longer need
        select(-c(row, prev_date, next_date, prev_row, next_row, FormGroup))

    # save the min and max values for EDSS
    edss_min <- 0
    edss_max <- 9.5

    # combine the interim visits and regular visits back together
    edss_data <- regular_edss %>%
        # combine the month edss emasurements with the interim measurements
        bind_rows(interim_edss) %>%
        # sort by patient name and visit date
        arrange(PatientName, visit_date) %>%
        # merge the treatment data
        left_join(treatment_data, by="PatientName")

    if (impute_method == "A best, B worst") {
        # impute missing values of edss as the minimum possible edss value if treatment is 0
        # and the maximum possible value of edss otherwise
        edss_data <- edss_data %>%
            mutate(total_edss_score = ifelse(is.na(total_edss_score), ifelse(treatment_group == 0, edss_min, edss_max), total_edss_score))
    } else if (impute_method == "A worst, B best") {
        # impute missing values of edss as the maximum possible edss value if treatment is 0
        # and the minimum possible value of edss otherwise
        edss_data <- edss_data %>%
            mutate(total_edss_score = ifelse(is.na(total_edss_score), ifelse(treatment_group == 0, edss_max, edss_min), total_edss_score))
    }

    edss_data %>%
        select(c(PatientName, visit_date, total_edss_score, treatment_group)) %>%
        filter(PatientName == "0100-014") %>%
        print(width=Inf)

    saveRDS(edss_data, file="imputed_edss_data.RDS")
}

