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
    mutate(FormGroup = if_else(FormGroup == "Interim Information", "Z", FormGroup)) %>%
    # sort by FormGroup
    arrange(PatientName, FormGroup) %>%
    # keep only measurements on different visit dates because there are duplicates
    # technically, we are supposed to keep the month with the correct window, but this
    # heuristic works for now (I manually checked)
    distinct(PatientName, visit_date, .keep_all=TRUE) %>%
    # change Z values back to Interim Information in FormGroup
    mutate(FormGroup = if_else(FormGroup == "Z", "Interim Information", FormGroup)) %>%
    # sort by patient name and the visit date
    arrange(PatientName, visit_date)

# add the list of patients we drop to the consort
add_to_consort(c("0256-013"), 
                "recorded in EDSS, but not in baseline chars", "impute_edss_pdds.R")

# if we are not imputing any values, then just perform a couple more pre-processing steps
# and save the edss data
if (impute_method == "none") {
    edss_data <- edss_data %>%
        # remove the form group column
        select(-FormGroup)

    # save data to an RDS file
    saveRDS(edss_data, file="imputed_edss_data.RDS")
} else {
    # impute the visit dates for the edss data
    edss_data <- impute_visit_dates(edss_data)

    # save the min and max values for EDSS
    edss_min <- 0
    edss_max <- 9.5

    # merge the treatment data
    edss_data <- edss_data %>%
        left_join(treatment_data, by="PatientName") 

    if (impute_method == "A best, B worst") {
        # impute missing values of edss as the minimum possible edss value if treatment is 0
        # and the maximum possible value of edss otherwise
        edss_data <- edss_data %>%
            mutate(total_edss_score = if_else(is.na(total_edss_score), if_else(treatment_group == 0, edss_min, edss_max), total_edss_score))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit <- edss_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the edss score to the best/worst value depending on the treatment
                # group
                total_edss_score = if_else(treatment_group == 0, edss_min, edss_max),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )
    } else if (impute_method == "A worst, B best") {
        # impute missing values of edss as the maximum possible edss value if treatment is 0
        # and the minimum possible value of edss otherwise
        edss_data <- edss_data %>%
            mutate(total_edss_score = if_else(is.na(total_edss_score), if_else(treatment_group == 0, edss_max, edss_min), total_edss_score))

        # for each patient, create an extra row with the visit date 161 days after and the
        # best/worst value
        extra_visit <- edss_data %>%
            # keep only the most recent visit for each patient
            slice_max(visit_date, n=1) %>%
            mutate(
                # change the edss score to the best/worst value depending on the treatment
                # group
                total_edss_score = if_else(treatment_group == 0, edss_max, edss_min),
                # update the visit date to be 161 days after the most recent visit date
                visit_date = visit_date + 161*60*60*24
            )
    }

    # combine the rows of the edss data with the extra visits
    edss_data <- edss_data %>%
        bind_rows(extra_visit) 

    # save data to an RDS file
    saveRDS(edss_data, file="imputed_edss_data.RDS")
}

