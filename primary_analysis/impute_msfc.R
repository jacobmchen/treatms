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
    stop(paste(c("User did not supply argument for how to impute EDSS values.",
         "Usage is: Rscript impute_edss_pdds.R \"mice\"",
         "Possible imputation options are \"none\", \"mice\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

imputation_options <- c("none", "mice", "A best, B worst", "A worst, B best")
impute_method <- args[1]

# make sure that the user entered a valid imputation option
if (!(impute_method %in% imputation_options)) {
    stop(paste(c("User did not input a valid imputation option for EDSS.",
                 "Possible imputation options are \"none\", \"mice\", \"A best, B worst\", \"A worst, B best\""), collapse="\n"))
}

# read the treatment assignemnts
treatment_data <- readRDS("treatment_data.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# read the data for censoring times, which was computed separately 
censoring_times <- readRDS("censoring_times.RDS")

# get the censoring times for t25fw, nhpt
# these censoring times are needed for pre-processing of MSFC values
t25fw_censoring_time <- censoring_times %>% select(PatientName, t25fw_censor)
hpt_censoring_time <- censoring_times %>% select(PatientName, hpt_censor)

# define a new function for imputing MSFC values; we don't reuse
# the function from imputing edss and pdds because the exact min
# and max values are different, and the relevant columns are different
impute_msfc_values <- function(data, impute_method) {

    # define max and min values for t25fw and nhpt
    t25fw_max <- 80
    t25fw_min <- 0
    nhpt_max <- 200
    nhpt_min <- 0

    if (impute_method == "mice") {
        # use MICE to impute missing values for EDSS and PDDS in between visits
        # NOTE: the run time may take a while, but that is expected because we are
        # assuming MAR where all observed covariates are necessary to impute the 
        # missing data
        imp <- mice(data, m=1, maxit=20, seed=0)

        # retrieve the imputed data
        imputed_data <- complete(imp, action=1)
 
        # return the mice imputed data directly
        return(imputed_data)
    } else if (impute_method == "none") {
        # if we see this option, do not impute EDSS nor PDDS
        # in fact, just return the observed data

        return(data)
    } else if (impute_method == "A best, B worst") {
        # give the best possible value of EDSS for treatment A (0) and the
        # worst possible value of EDSS for treatment B (1) for values of
        # EDSS that were originally missing
        # the best possible value is the smaller value because bigger means
        # worse for these two metrics
        data <- data %>%
            inner_join(treatment_data, by="PatientName") %>%
            mutate(trial_average_seconds = ifelse(is.na(trial_average_seconds), ifelse(treatment_group == 0, t25fw_min, t25fw_max), trial_average_seconds)) %>%
            mutate(hand_average_seconds = ifelse(is.na(hand_average_seconds), ifelse(treatment_group == 0, nhpt_min, nhpt_max), hand_average_seconds)) %>%
            select(-treatment_group)

        return(data)
    } else if (impute_method == "A worst, B best") {
        data <- data %>%
            inner_join(treatment_data, by="PatientName") %>%
            mutate(trial_average_seconds = ifelse(is.na(trial_average_seconds), ifelse(treatment_group == 0, t25fw_max, t25fw_min), trial_average_seconds)) %>%
            mutate(hand_average_seconds = ifelse(is.na(hand_average_seconds), ifelse(treatment_group == 0, nhpt_max, nhpt_min), hand_average_seconds)) %>%
            select(-treatment_group)

        return(data)
    } else {
        # safety check for invalid imputation method
        stop("Invalid imputation method.")
    }

}

# read the msfc data
msfc_data <- data.frame(read_excel(data_file_name, sheet="msfc"))
msfc_data <- compute_average_msfc(msfc_data)

# pre-process the t25fw data
t25fw <- msfc_data %>%
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
    select(-t25fw_censor) 
   
# make a copy of the original dataset that just keeps track of which
# values will be imputed
t25fw_copy <- t25fw %>%
    ungroup() %>%
    select(c(PatientName, month, trial_average_seconds)) %>%
    mutate(t25fw_missing=ifelse(is.na(trial_average_seconds), 1, 0)) %>%
    select(-trial_average_seconds)

# pre-process the nhpt data
nhpt <- msfc_data %>%
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
    select(-hpt_censor)
   
# make a copy of the original dataset that just keeps track of which
# values will be imputed
nhpt_copy <- nhpt %>%
    ungroup() %>%
    select(c(PatientName, month, hand_average_seconds)) %>%
    mutate(nhpt_missing=ifelse(is.na(hand_average_seconds), 1, 0)) %>%
    select(-hand_average_seconds)

# merge the t25fw and nhpt datasets
t25fw_nhpt <- t25fw %>%
    left_join(nhpt, by=c("PatientName", "month")) %>%
    # add the baseline covariate data for each individual to allow for
    # missing data imputation via MICE, if desired
    full_join(baseline_data, by="PatientName")

# impute t25fw and nhpt values based on the user-specified imputation method
t25fw_nhpt_imputed <- impute_msfc_values(t25fw_nhpt, impute_method)

t25fw_nhpt_imputed %>% slice_head(n=10) %>% print()

# get just t25fw data
t25fw_imputed <- t25fw_nhpt_imputed %>%
    select(-hand_average_seconds)

# save as RDS file the imputed t25fw data for use as a secondary outcome
saveRDS(t25fw_imputed, file="t25fw_data.RDS")

# join back whether values were imputed to the imputed dataset
annotated_t25fw_data <- t25fw_imputed %>%
    inner_join(t25fw_copy, by=c("PatientName", "month"))
# save annotated data as RDS
saveRDS(annotated_t25fw_data, file="annotated_t25fw_data.RDS")

# get just nhpt data
nhpt_imputed <- t25fw_nhpt_imputed %>%
    select(-trial_average_seconds)

# save as RDS file the nhpt data for use as a secondary outcome
saveRDS(nhpt_imputed, file="nhpt_data.RDS")

# join back whether values were imputed to the imputed dataset
annotated_nhpt_data <- nhpt_imputed %>%
    inner_join(nhpt_copy, by=c("PatientName", "month"))
# save annotated data as RDS
saveRDS(annotated_nhpt_data, file="annotated_nhpt_data.RDS")


