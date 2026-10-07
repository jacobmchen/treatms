# Impute missing values for EDSS and PDDS, then save
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

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))

# read the data for PDDS
pdds_data <- data.frame(read_excel(data_file_name, sheet="pdds"))

# read the data for censoring times, which was computed separately 
censoring_times <- readRDS("censoring_times.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

# read the treatment assignemnts
treatment_data <- readRDS("treatment_data.RDS")

# get the censoring times for EDSS
edss_censoring_time <- censoring_times %>% select(PatientName, edss_censor)

# prepare pdds data to get it ready for imputation
pdds_data <- pdds_data %>%
    # get rid of rows that do not have clearly labeled form groups for month number
    filter(!(FormGroup %in% c("Interim Information", "Interim ePro", "Supplemental", "End of Trial"))) %>%
    # change baseline to month 0
    mutate(FormGroup = ifelse(FormGroup == "Baseline", 0, FormGroup)) %>%
    # convert the formgroup data to numbers
    mutate(FormGroup = as.integer(gsub("\\D", "", FormGroup))) %>%
    # keep only the months that are divisible by 6 to match with the edss data
    filter(FormGroup %% 6 == 0) %>%
    # rename FormGroup to month
    rename(month = FormGroup) %>%
    # remove problematic patient
    filter(PatientName != "0256-013") %>%
    # group the data by patient name
    group_by(PatientName) %>%
    # fill in gaps if there are any for the 6-month intervals
    complete(month=full_seq(month, 6)) %>%
    # sort the patient names by month
    arrange(PatientName, month) %>%
    # select only the patient name, month, and pdds score columns
    select(c(PatientName, month, pdds_total_score)) %>%
    # some patient names and months are duplicated, so just keep the first
    # occurrence
    distinct(PatientName, month, .keep_all=TRUE)

# create a function that gets edss data ready for imputation
# and also merges it with pdds data; this function allows
# us to specify whether to include observations with exception findings, and
# it is set to TRUE by default
prepare_edss_data <- function(edss_data, pdds_data, edss_censoring_time, include_exceptions=TRUE) {

    # if we aren't including values with an exception finding,
    # remove such rows from the edss_data
    if (include_exceptions == FALSE) {
        edss_data <- edss_data %>% filter(fs_finding == "No")
    }

    # prepare edss data to get it ready for imputation
    edss_pdds_data <- edss_data %>%
        # replace Month with empty string then cast the string as an integer
        mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
        # after the above operation, get rid of all rows with a missing value
        filter(!is.na(month)) %>%
        # keep only patient name, month, and edss score columns (including
        # sub-functional system scores)
        select(PatientName, month, total_edss_score,
               fs_cfss_total, fsvs_on_total, fs_bfss_total, total_pyramidal_score,
               sensory_system_score_total, cerebellar_system_score_total, bowel_bladder_sys_score_total) %>%
        # this column is being read as a string for some reason, so cast it
        # as a numeric
        mutate(fsvs_on_total = as.numeric(fsvs_on_total)) %>%
        mutate(bowel_bladder_sys_score_total = as.numeric(bowel_bladder_sys_score_total)) %>%
        # remove problematic patient for whom we have no data
        filter(PatientName != "0256-013") %>%
        # change every instance of Not Obtained to a missing value in the data
        mutate(across(where(is.character), ~ na_if(.x, "Not Obtained"))) %>%
        # group the data by the patient name
        group_by(PatientName) %>%
        # some patients may not have an entry for every 6-month interval;
        # this makes sure that every month at 6-month intervals are in the 
        # data, starting from month 0; new inserted months have a missing value for the edss score
        complete(month=full_seq(c(0, month), 6)) %>%
        # this sorts the patient names and months
        arrange(PatientName, month) %>%
        # append pdds scores to the data
        left_join(pdds_data, by=c("PatientName", "month")) %>%
        # make a new column with the censoring times
        left_join(edss_censoring_time, by="PatientName") %>%
        # remove all rows of data there are after the censoring times for each
        # individual
        filter(month <= edss_censor) %>%
        # remove the censoring times for each individual
        select(-edss_censor) %>%
        # add the baseline covariate data for each individual to allow for
        # missing data imputation
        left_join(baseline_data, by="PatientName")

    return(edss_pdds_data)
}

# get the edss and pdds data that is ready for imputation
edss_pdds_data <- prepare_edss_data(edss_data, pdds_data, edss_censoring_time)

edss_pdds_data %>% print(width=Inf)

# create a separate dataframe that remembers whether edss and pdds are missing
# for the purpose of spaghetti plots
data_copy <- edss_pdds_data

# keep only the relevant rows and create columns that keep track of which
# values will be imputed
# we have to track these values in a separate dataset because we don't want
# them to be used in the MICE imputation
edss_pdds_missing <- data_copy %>%
    ungroup() %>%
    select(c(PatientName, month, total_edss_score, pdds_total_score)) %>%
    mutate(edss_missing=ifelse(is.na(total_edss_score), 1, 0)) %>%
    mutate(pdds_missing=ifelse(is.na(pdds_total_score), 1, 0)) %>%
    select(-c(total_edss_score, pdds_total_score))

edss_pdds_missing %>% print(width=Inf)

# define a function that imputes missing values based on the user-specified
# imputation method
impute_missing_values <- function(edss_pdds_data, impute_method) {

    # save the min and max values for EDSS
    edss_min <- 0
    edss_max <- 9.5

    # save the min and max values for PDDS
    pdds_min <- 0
    pdds_max <- 8

   if (impute_method == "mice") {
        # use MICE to impute missing values for EDSS and PDDS in between visits
        # NOTE: the run time may take a while, but that is expected because we are
        # assuming MAR where all observed covariates are necessary to impute the 
        # missing data
        imp <- mice(edss_pdds_data, m=1, maxit=20, seed=0)

        # retrieve the imputed data
        imputed_data <- complete(imp, action=1)
 
        # return the mice imputed data directly
        return(imputed_data)
    } else if (impute_method == "none") {
        # if we see this option, do not impute EDSS nor PDDS
        # in fact, just return the observed data

        return(edss_pdds_data)
    } else if (impute_method == "A best, B worst") {
        # give the best possible value of EDSS for treatment A (0) and the
        # worst possible value of EDSS for treatment B (1) for values of
        # EDSS that were originally missing
        # the best possible value is the smaller value because bigger means
        # worse for these two metrics
        edss_pdds_data <- edss_pdds_data %>%
            inner_join(treatment_data, by="PatientName") %>%
            mutate(total_edss_score = ifelse(is.na(total_edss_score), ifelse(treatment_group == 0, edss_min, edss_max), total_edss_score)) %>%
            mutate(pdds_total_score = ifelse(is.na(pdds_total_score), ifelse(treatment_group == 0, pdds_min, pdds_max), pdds_total_score)) %>%
            select(-treatment_group)

        return(edss_pdds_data)
    } else if (impute_method == "A worst, B best") {
        edss_pdds_data <- edss_pdds_data %>%
            inner_join(treatment_data, by="PatientName") %>%
            mutate(total_edss_score = ifelse(is.na(total_edss_score), ifelse(treatment_group == 0, edss_max, edss_min), total_edss_score)) %>%
            mutate(pdds_total_score = ifelse(is.na(pdds_total_score), ifelse(treatment_group == 0, pdds_max, pdds_min), pdds_total_score)) %>%
            select(-treatment_group)

        return(edss_pdds_data)
    } else {
        # safety check for invalid imputation method
        stop("Invalid imputation method.")
    }
}

# impute the missing values using the user-specified imputation method
imputed_data <- impute_missing_values(edss_pdds_data, impute_method)
# debug statement below
imputed_data %>% inner_join(treatment_data, by="PatientName") %>% inner_join(edss_pdds_missing, by=c("PatientName", "month")) %>% slice_head(n=50) %>% select(PatientName, total_edss_score, treatment_group, edss_missing) %>% print()

# save the imputed dataset to an RDS file
saveRDS(imputed_data, file="imputed_edss_pdds_data.RDS")

# create a copy of the imputed data for comparison purposes
copy <- imputed_data

# join back whether values were imputed to the imputed dataset
imputed_data <- imputed_data %>%
    inner_join(edss_pdds_missing, by=c("PatientName", "month"))
saveRDS(imputed_data, file="annotated_imputed_edss_pdds_data.RDS")

################################################
# now repeat the same processes except for only values of EDSS that
# are not exception findings

# read the data for censoring times, which was computed separately 
censoring_times <- readRDS("censoring_times_no_exceptions.RDS")

# get the censoring times for EDSS
edss_censoring_time <- censoring_times %>% select(PatientName, edss_censor)

# get the edss and pdds data that is ready for imputation
edss_pdds_data <- prepare_edss_data(edss_data, pdds_data, edss_censoring_time, include_exceptions=FALSE)

# impute the missing values using the user-specified imputation method
imputed_data <- impute_missing_values(edss_pdds_data, impute_method)
saveRDS(imputed_data, file="imputed_edss_pdds_data_no_exceptions.RDS")

# copy %>% slice_head(n=10) %>% print()
# imputed_data %>% slice_head(n=10) %>% print()

# this print statement is for debugging and viewing
# edss progression for a single patient
# imputed_data %>% full_join(copy, by=c("PatientName", "month")) %>%
#     filter(PatientName == "0100-014") %>% select(c(PatientName, month, total_edss_score.x, total_edss_score.y)) %>% print()
