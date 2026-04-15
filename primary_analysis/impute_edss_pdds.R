# Impute missing values for EDSS and PDDS, then save
# to an RDS file for use in computation of other outcomes.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)

source("global_variables.R")

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))

# read the data for PDDS
pdds_data <- data.frame(read_excel(data_file_name, sheet="pdds"))

# read the data for censoring times, which was computed separately 
censoring_times <- readRDS("censoring_times.RDS")

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("baseline_data_merge_states.RDS")

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

# prepare edss data to get it ready for imputation
edss_pdds_data <- edss_data %>%
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns (including
    # sub-functional system scores)
    select(PatientName, month, total_edss_score,
           fs_cfss_total, fsvs_on_total, fs_bfss_total, total_pyramidal_score,
           sensory_system_score_total, cerebellar_system_score_total, bowel_bladder_sys_score_total) %>%
    # this column is being read as a string for some reason, so cast it
    # as a numeric
    mutate(fsvs_on_total = as.numeric(fsvs_on_total)) %>%
    # remove problematic patient for whom we have no data
    filter(PatientName != "0256-013") %>%
    # change every instance of Not Obtained to a missing value in the data
    mutate(across(where(is.character), ~ na_if(.x, "Not Obtained"))) %>%
    # group the data by the patient name
    group_by(PatientName) %>%
    # some patients may not have an entry for every 6-month interval;
    # this makes sure that every month at 6-month intervals are in the 
    # data; new inserted months have a missing value for the edss score
    complete(month=full_seq(month, 6)) %>%
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

print(head(edss_pdds_data))

# use MICE to impute missing values for EDSS in between visits
# NOTE: the run time may take a while, but that is expected because we are
# assuming MAR where all observed covariates are necessary to impute the 
# missing data
imp <- mice(edss_pdds_data, m=1, maxit=20, seed=0)

# save the imputed data into a separate file
imputed_data <- complete(imp, action=1)
saveRDS(imputed_data, file="imputed_edss_pdds_data.RDS")


